package com.alecdorrington.eunomia
package model

import io.circe.Codec

/**
  * One key a list is ordered by. A list is ordered by a sequence of these, each
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

  /** This key, in the opposite direction. */
  def reversed: Order = copy(descending = !descending)

  /** This key as written in a `sort` parameter: its field, `-` if descending. */
  def text: String = if descending then s"-$field" else field

object Order:

  /** Reads one key as written by [[Order.text]]. */
  def parse(text: String): Order = text match
    case s"-$field" => Order(field, descending = true)
    case field      => Order(field)

  /** Reads a comma-separated sequence of keys, most significant first. */
  def parseAll(text: String): List[Order] = text
    .split(',')
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(parse)
    .toList

  /** Writes a sequence of keys as [[parseAll]] reads them. */
  def textOf(orders: List[Order]): String = orders.map(_.text).mkString(",")

  /**
    * The keys after one field's column heading is clicked: a field that is not
    * already the most significant key becomes it, ascending, with the other
    * keys breaking its ties; ascending becomes descending; and descending is
    * dropped.
    *
    * @param keys
    *   The keys before the click, most significant first.
    *
    * @param field
    *   The name of the field whose heading was clicked.
    */
  def toggled(keys: List[Order], field: String): List[Order] = keys match
    case key :: rest if key.field == field =>
      if key.descending then rest else key.reversed :: rest
    case _ => Order(field) :: keys.filterNot(_.field == field)
