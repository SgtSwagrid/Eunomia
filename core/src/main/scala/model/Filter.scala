package com.alecdorrington.eunomia
package model

import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*

/**
  * A composable predicate on the items of a list, naming fields rather than
  * reading them, so that it can be evaluated in memory by a [[ListSchema]],
  * translated into a database query, or sent over the wire. Build filters from
  * typed [[Field]]s where possible.
  *
  * A comparison with an absent value never holds, as with `NULL` in SQL; use
  * [[Missing]] to ask for absence.
  */
enum Filter:

  /**
    * A filter holding wherever another does not.
    *
    * @param filter
    *   The filter negated.
    */
  case Not(filter: Filter)

  /**
    * A filter holding wherever all the given filters do.
    *
    * @param filters
    *   The filters to conjoin; with none, the filter holds everywhere.
    */
  case And(filters: List[Filter])

  /**
    * A filter holding wherever any one of the given filters does.
    *
    * @param filters
    *   The filters to disjoin; with none, the filter holds nowhere.
    */
  case Or(filters: List[Filter])

  /**
    * A filter holding wherever a field's value compares with a given value as
    * stated.
    *
    * @param field
    *   The name of the field.
    *
    * @param operator
    *   The operator comparing the field's value with `value`.
    *
    * @param value
    *   The value compared with.
    */
  case Compare
    (
      field: String,
      operator: Operator,
      value: Value,
    )

  /**
    * A filter holding wherever a text field contains the given text, ignoring
    * case.
    *
    * @param field
    *   The name of the text field.
    *
    * @param text
    *   The text sought.
    */
  case Contains(field: String, text: String)

  /**
    * A filter holding wherever a field's value equals any one of the given
    * values.
    *
    * @param field
    *   The name of the field.
    *
    * @param values
    *   The values the field may equal.
    */
  case OneOf(field: String, values: List[Value])

  /**
    * A filter holding wherever a field has no value.
    *
    * @param field
    *   The name of the field.
    */
  case Missing(field: String)

  /**
    * Conjoins this filter with another.
    *
    * @param other
    *   The other filter.
    *
    * @return
    *   A filter holding wherever both do.
    */
  def && (other: Filter): Filter = Filter.all(List(this, other))

  /**
    * Disjoins this filter with another.
    *
    * @param other
    *   The other filter.
    *
    * @return
    *   A filter holding wherever either does.
    */
  def || (other: Filter): Filter = Filter.any(List(this, other))

  /** The filter holding wherever this one does not. */
  def unary_! : Filter = this match
    case Filter.Not(filter) => filter
    case _                  => Filter.Not(this)

  /**
    * Interprets this filter, e.g. as a truth value for one item or as a
    * condition in a database query. The cases holding filters of their own are
    * folded through, so an interpreter gives only the meaning of each case.
    *
    * @tparam A
    *   The type of the interpretation.
    *
    * @param not
    *   The negation of an interpreted filter.
    *
    * @param all
    *   The conjunction of interpreted filters, holding where there are none.
    *
    * @param any
    *   The disjunction of interpreted filters, not holding where there are
    *   none.
    *
    * @param compare
    *   The interpretation of [[Compare]], given its field, operator and value.
    *
    * @param contains
    *   The interpretation of [[Contains]], given its field and text.
    *
    * @param oneOf
    *   The interpretation of [[OneOf]], given its field and values.
    *
    * @param missing
    *   The interpretation of [[Missing]], given its field.
    *
    * @return
    *   An interpretation of this filter.
    */
  def fold[A]
    (
      not: A => A,
      all: List[A] => A,
      any: List[A] => A,
      compare: (String, Operator, Value) => A,
      contains: (String, String) => A,
      oneOf: (String, List[Value]) => A,
      missing: String => A,
    )
    : A =
    def of(filter: Filter): A = filter.fold(
      not,
      all,
      any,
      compare,
      contains,
      oneOf,
      missing,
    )
    this match
      case Filter.Not(filter)                     => not(of(filter))
      case Filter.And(filters)                    => all(filters.map(of))
      case Filter.Or(filters)                     => any(filters.map(of))
      case Filter.Compare(field, operator, value) =>
        compare(field, operator, value)
      case Filter.Contains(field, text) => contains(field, text)
      case Filter.OneOf(field, values)  => oneOf(field, values)
      case Filter.Missing(field)        => missing(field)

object Filter:

  /** The filter that holds everywhere. */
  val always: Filter = And(List.empty)

  /** The filter that holds nowhere. */
  val never: Filter = Or(List.empty)

  /**
    * Conjoins the given filters, flattening nested conjunctions.
    *
    * @param filters
    *   The filters to conjoin.
    *
    * @return
    *   A filter holding wherever all of them do, or everywhere if there are
    *   none.
    */
  def all(filters: Iterable[Filter]): Filter = joined(
    filters.toList.flatMap(conjuncts),
    And(_),
  )

  /**
    * Disjoins the given filters, flattening nested disjunctions.
    *
    * @param filters
    *   The filters to disjoin.
    *
    * @return
    *   A filter holding wherever any one of them does, or nowhere if there are
    *   none.
    */
  def any(filters: Iterable[Filter]): Filter = joined(
    filters.toList.flatMap(disjuncts),
    Or(_),
  )

  private def conjuncts(filter: Filter): List[Filter] = filter match
    case And(inner) => inner
    case _          => List(filter)

  private def disjuncts(filter: Filter): List[Filter] = filter match
    case Or(inner) => inner
    case _         => List(filter)

  private def joined
    (
      filters: List[Filter],
      join: List[Filter] => Filter,
    )
    : Filter = filters match
    case List(only) => only
    case many       => join(many)

  given Encoder[Filter] = Encoder.instance:
    case Not(filter)                     => Json.obj("not" -> filter.asJson)
    case And(filters)                    => Json.obj("and" -> filters.asJson)
    case Or(filters)                     => Json.obj("or" -> filters.asJson)
    case Compare(field, operator, value) => Json.obj(
        "field" -> field.asJson,
        "is"    -> operator.asJson,
        "value" -> value.asJson,
      )
    case Contains(field, text) => Json.obj(
        "field"    -> field.asJson,
        "contains" -> text.asJson,
      )
    case OneOf(field, values) => Json.obj(
        "field" -> field.asJson,
        "oneOf" -> values.asJson,
      )
    case Missing(field) => Json.obj(
        "field"   -> field.asJson,
        "missing" -> true.asJson,
      )

  given Decoder[Filter] = Decoder.instance(cursor =>
    shapes
      .find((key, _) => cursor.downField(key).succeeded)
      .map((_, shape) => shape(cursor))
      .getOrElse(Left(DecodingFailure("Unrecognised filter.", cursor.history))),
  )

  /** The decoder for each shape of filter, by the key that identifies it. */
  private lazy val shapes: List[(String, Decoder[Filter])] = List(
    "not" -> Decoder[Filter].at("not").map(Not(_)),
    "and" -> Decoder[List[Filter]].at("and").map(And(_)),
    "or"  -> Decoder[List[Filter]].at("or").map(Or(_)),
    "is"  -> Decoder.forProduct3[Filter, String, Operator, Value](
      "field",
      "is",
      "value",
    )(Compare(_, _, _)),
    "contains" ->
      Decoder.forProduct2[Filter, String, String]("field", "contains")(
        Contains(_, _),
      ),
    "oneOf" ->
      Decoder.forProduct2[Filter, String, List[Value]]("field", "oneOf")(
        OneOf(_, _),
      ),
    "missing" -> Decoder[String].at("field").map(Missing(_)),
  )
