package com.alecdorrington.eunomia
package model

import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*

/**
  * A composable predicate on the items of a list, naming fields rather than
  * reading them, so that it can be evaluated in memory by a [[Schema]],
  * translated into a database query, or sent over the wire.
  *
  * A comparison with a field whose value is absent never holds, just as a
  * comparison with `NULL` never does in SQL; use [[Missing]] to ask for
  * absence. Build filters from typed [[Field]]s rather than by hand where
  * possible.
  */
enum Filter:

  /** Holds wherever the given filter does not. */
  case Not(filter: Filter)

  /** Holds wherever every one of the given filters does, including none. */
  case And(filters: List[Filter])

  /** Holds wherever any one of the given filters does, excluding none. */
  case Or(filters: List[Filter])

  /** Holds wherever a field's value compares with the given value as stated. */
  case Compare
    (
      field: String,
      comparison: Comparison,
      value: Value,
    )

  /** Holds wherever a text field contains the given text, ignoring case. */
  case Contains(field: String, text: String)

  /** Holds wherever a field's value equals any one of the given values. */
  case OneOf(field: String, values: List[Value])

  /** Holds wherever a field has no value. */
  case Missing(field: String)

  /** Holds wherever both this filter and the other do. */
  def && (other: Filter): Filter = Filter.all(List(this, other))

  /** Holds wherever either this filter or the other does. */
  def || (other: Filter): Filter = Filter.any(List(this, other))

  /** Holds wherever this filter does not. */
  def unary_! : Filter = this match
    case Filter.Not(filter) => filter
    case _                  => Filter.Not(this)

object Filter:

  /** The filter that holds everywhere. */
  val always: Filter = And(List.empty)

  /** The filter that holds nowhere. */
  val never: Filter = Or(List.empty)

  /**
    * Holds wherever every one of the given filters does. Nested conjunctions
    * are flattened into this one, so that composing filters piecemeal does not
    * deepen them.
    */
  def all(filters: Iterable[Filter]): Filter = joined(
    filters.toList.flatMap(conjuncts),
    And(_),
  )

  /**
    * Holds wherever any one of the given filters does, flattening nested
    * disjunctions as [[all]] does conjunctions.
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

  /** A lone filter as itself, and any other number joined as given. */
  private def joined
    (
      filters: List[Filter],
      join: List[Filter] => Filter,
    )
    : Filter = filters match
    case List(only) => only
    case many       => join(many)

  /**
    * Filters are sent as small JSON objects, told apart by their keys, e.g.
    * `{"and":[{"field":"rating","is":">=","value":50},{"not":{...}}]}`.
    */
  given Encoder[Filter] = Encoder.instance:
    case Not(filter)                       => Json.obj("not" -> filter.asJson)
    case And(filters)                      => Json.obj("and" -> filters.asJson)
    case Or(filters)                       => Json.obj("or" -> filters.asJson)
    case Compare(field, comparison, value) => Json.obj(
        "field" -> field.asJson,
        "is"    -> comparison.asJson,
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
      .fold[Decoder.Result[Filter]](Left(
        DecodingFailure("Unrecognised filter.", cursor.history),
      ))((_, shape) => shape(cursor)),
  )

  /** The decoder for each shape of filter, by the key that identifies it. */
  private lazy val shapes: List[(String, Decoder[Filter])] = List(
    "not" -> Decoder[Filter].at("not").map(Not(_)),
    "and" -> Decoder[List[Filter]].at("and").map(And(_)),
    "or"  -> Decoder[List[Filter]].at("or").map(Or(_)),
    "is"  -> Decoder.forProduct3[Filter, String, Comparison, Value](
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
