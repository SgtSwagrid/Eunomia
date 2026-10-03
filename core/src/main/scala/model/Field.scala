package com.alecdorrington.eunomia
package model

/**
  * A named, typed field of the items in a list, with the vocabulary for
  * filtering and ordering by it. A server and a client describing the same list
  * must agree on its name.
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
  *   The function reading this field's value from one item, if it has one.
  */
final class Field[-X, B : Scalar as B](val name: String, read: X => Option[B]):

  /** The kind of value this field holds. */
  def kind: Kind = B.kind

  /**
    * Reads this field's value from one item.
    *
    * @param item
    *   The item to read.
    *
    * @return
    *   An option holding the value, or `None` where the item has none.
    */
  def valueOf(item: X): Option[Value] = read(item).map(B.toValue)

  /**
    * Creates a filter holding wherever this field equals a value.
    *
    * @param value
    *   The value to equal.
    *
    * @return
    *   A filter on this field.
    */
  def is(value: B): Filter = compare(Operator.Equal, value)

  /**
    * Creates a filter holding wherever this field has a value that differs from
    * the given one.
    *
    * @param value
    *   The value to differ from.
    *
    * @return
    *   A filter on this field.
    */
  def isNot(value: B): Filter = compare(Operator.Unequal, value)

  /**
    * Creates a filter holding wherever this field is less than a bound.
    *
    * @param bound
    *   The exclusive upper bound.
    *
    * @return
    *   A filter on this field.
    */
  def < (bound: B): Filter = compare(Operator.Less, bound)

  /**
    * Creates a filter holding wherever this field is at most a bound.
    *
    * @param bound
    *   The inclusive upper bound.
    *
    * @return
    *   A filter on this field.
    */
  def <= (bound: B): Filter = compare(Operator.AtMost, bound)

  /**
    * Creates a filter holding wherever this field is greater than a bound.
    *
    * @param bound
    *   The exclusive lower bound.
    *
    * @return
    *   A filter on this field.
    */
  def > (bound: B): Filter = compare(Operator.Greater, bound)

  /**
    * Creates a filter holding wherever this field is at least a bound.
    *
    * @param bound
    *   The inclusive lower bound.
    *
    * @return
    *   A filter on this field.
    */
  def >= (bound: B): Filter = compare(Operator.AtLeast, bound)

  /**
    * Creates a filter holding wherever this field equals any one of the given
    * values.
    *
    * @param values
    *   The values to equal.
    *
    * @return
    *   A filter on this field, holding nowhere if `values` is empty.
    */
  def oneOf(values: B*): Filter =
    Filter.OneOf(name, values.toList.map(B.toValue))

  /**
    * Creates a filter holding wherever this text field contains the given text,
    * ignoring case.
    *
    * @param text
    *   The text to seek.
    *
    * @return
    *   A filter on this field.
    */
  def contains(text: String)(using B =:= String): Filter =
    Filter.Contains(name, text)

  /** The filter holding wherever this field has no value. */
  def missing: Filter = Filter.Missing(name)

  /** The filter holding wherever this field has a value. */
  def present: Filter = !missing

  /** The key ordering by this field, least first. */
  def ascending: Order = Order(name)

  /** The key ordering by this field, greatest first. */
  def descending: Order = Order(name, descending = true)

  private def compare(operator: Operator, value: B): Filter =
    Filter.Compare(name, operator, B.toValue(value))

object Field:

  /**
    * Starts a field of items of type `X`, so that only its reader is inferred.
    *
    * @tparam X
    *   The type of the items.
    *
    * @return
    *   A builder of fields of `X`.
    */
  def of[X]: Builder[X] = Builder()

  /**
    * A builder of fields of items of type `X`.
    *
    * @tparam X
    *   The type of the items.
    */
  final class Builder[X]:

    /**
      * Creates a field read from each item by the given function.
      *
      * @tparam V
      *   The type the function returns: `B`, or `Option[B]`.
      *
      * @tparam B
      *   The type of the field's values.
      *
      * @param name
      *   The name queries refer to the field by.
      *
      * @param read
      *   The function reading the field from one item.
      *
      * @return
      *   A field of `X`.
      */
    def apply[V, B]
      (name: String, read: X => V)
      (
        using nullable: Nullable[V, B],
        scalar: Scalar[B],
      )
      : Field[X, B] = Field(name, read.andThen(nullable(_)))
