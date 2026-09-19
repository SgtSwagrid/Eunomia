package com.alecdorrington.eunomia
package model

/**
  * One named, typed field of the items in a list: how to read it from an item,
  * and the vocabulary for filtering and ordering by it. The name is what
  * queries refer to, so a server and a client describing the same list must
  * agree on it.
  *
  * {{{
  * val rating = Field.of[Book]("rating", _.rating) // A `Field[Book, Long]`.
  * val query  = ListQuery(rating >= 50L && rating.present, List(rating.descending))
  * }}}
  *
  * @tparam X
  *   The type of the items.
  *
  * @tparam B
  *   The type of the field's values, which may be absent from some items.
  *
  * @param name
  *   The name queries refer to this field by.
  *
  * @param read
  *   Reads this field's value from one item, if it has one.
  */
final class Field[-X, B : Scalar as B](val name: String, read: X => Option[B]):

  /** The kind of value this field holds. */
  def kind: Kind = B.kind

  /** This field's value in one item, if it has one. */
  def valueOf(item: X): Option[Value] = read(item).map(B.encode)

  /** Holds wherever this field equals the given value. */
  def is(value: B): Filter = compare(Comparison.Eq, value)

  /** Holds wherever this field has a value, and it differs from the given one. */
  def isNot(value: B): Filter = compare(Comparison.Ne, value)

  /** Holds wherever this field is less than the given value. */
  def < (bound: B): Filter = compare(Comparison.Lt, bound)

  /** Holds wherever this field is at most the given value. */
  def <= (bound: B): Filter = compare(Comparison.Le, bound)

  /** Holds wherever this field is greater than the given value. */
  def > (bound: B): Filter = compare(Comparison.Gt, bound)

  /** Holds wherever this field is at least the given value. */
  def >= (bound: B): Filter = compare(Comparison.Ge, bound)

  /** Holds wherever this field equals any one of the given values. */
  def oneOf(values: B*): Filter =
    Filter.OneOf(name, values.toList.map(B.encode))

  /** Holds wherever this text field contains the given text, ignoring case. */
  def contains(text: String)(using B =:= String): Filter =
    Filter.Contains(name, text)

  /** Holds wherever this field has no value. */
  def missing: Filter = Filter.Missing(name)

  /** Holds wherever this field has a value. */
  def present: Filter = !missing

  /** Orders by this field, least first. */
  def ascending: Order = Order(name)

  /** Orders by this field, greatest first. */
  def descending: Order = Order(name, descending = true)

  private def compare(comparison: Comparison, value: B): Filter = Filter
    .Compare(name, comparison, B.encode(value))

object Field:

  /** Starts a field of items of type `X`, so that only its reader is inferred. */
  def of[X]: Builder[X] = Builder()

  /** Builds fields of items of type `X`. */
  final class Builder[X]:

    /**
      * A field read from each item by the given function, which may return
      * either a value or an optional one.
      *
      * @param name
      *   The name queries refer to the field by.
      *
      * @param get
      *   Reads the field from one item.
      */
    def apply[V, B]
      (name: String, get: X => V)
      (
        using nullable: Nullable[V, B],
        scalar: Scalar[B],
      )
      : Field[X, B] = Field(name, get.andThen(nullable(_)))
