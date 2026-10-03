package com.alecdorrington.eunomia
package model

import io.circe.Codec

/**
  * A key a list is ordered by. A list is ordered by a sequence of these, each
  * breaking the ties left by those before it. Items with no value for the field
  * come last, in either direction.
  *
  * @param field
  *   The name of the field ordered by.
  *
  * @param descending
  *   Whether the greatest values come first.
  */
final case class Order
  (field: String, descending: Boolean = false)
  derives Codec.AsObject:

  /** The same key in the opposite direction. */
  def reversed: Order = copy(descending = !descending)

  /** The key as written in an `order` parameter, with `-` if descending. */
  def text: String = if descending then s"-$field" else field

object Order:

  /**
    * Parses one key as written by [[Order.text]].
    *
    * @param text
    *   The key as written.
    *
    * @return
    *   A key.
    */
  def parse(text: String): Order = text match
    case s"-$field" => Order(field, descending = true)
    case field      => Order(field)

  /**
    * Parses a comma-separated sequence of keys.
    *
    * @param text
    *   The keys as written, most significant first.
    *
    * @return
    *   A list of keys, most significant first.
    */
  def parseAll(text: String): List[Order] = text
    .split(',')
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(parse)
    .toList

  /**
    * Writes a sequence of keys as [[parseAll]] reads them.
    *
    * @param keys
    *   The keys, most significant first.
    *
    * @return
    *   A comma-separated string of keys.
    */
  def textOf(keys: List[Order]): String = keys.map(_.text).mkString(",")

  /**
    * Updates the keys after a field's column heading is clicked: a field that
    * is not the most significant key becomes it, ascending; ascending becomes
    * descending; and descending is dropped.
    *
    * @param keys
    *   The keys before the click, most significant first.
    *
    * @param field
    *   The name of the field whose heading was clicked.
    *
    * @return
    *   A list of the keys after the click, most significant first.
    */
  def toggled(keys: List[Order], field: String): List[Order] = keys match
    case key :: rest if key.field == field =>
      if key.descending then rest else key.reversed :: rest
    case _ => Order(field) :: keys.filterNot(_.field == field)
