package com.alecdorrington.eunomia
package client

import com.alecdorrington.eunomia.model.{
  CellFilter, Filter, Kind, ListQuery, Order, Page, Paged, Schema,
}
import com.raquo.laminar.api.L.*

/**
  * The browser-side state of one list as a person narrows and orders it: the
  * text typed into each column's header cell, the keys chosen by clicking
  * headings, and the window shown. Headless, so that the same state can drive a
  * [[Table]] or any other rendering.
  *
  * @param schema
  *   The fields of the items, as shared with the server.
  *
  * @param source
  *   Where the items come from.
  *
  * @param narrowing
  *   A filter the host application applies beneath whatever is typed, such as a
  *   switch narrowing the list to the user's own items.
  *
  * @param initial
  *   The query the list starts with. Its filter is added to [[narrowing]].
  */
final class ListView[X]
  (
    schema: Schema[X],
    source: ListSource[X],
    narrowing: Signal[Filter] = Val(Filter.always),
    initial: ListQuery = ListQuery(),
  ):

  private val cells: Var[Map[String, String]] = Var(Map.empty)

  private val keys: Var[List[Order]] = Var(initial.order)

  private val window: Var[Option[Page]] = Var(initial.page)

  /**
    * Everything the items shown must satisfy. A cell whose text cannot be read
    * narrows nothing until it can; [[cellProblem]] says why.
    */
  val filter: Signal[Filter] = cells
    .signal
    .combineWith(narrowing)
    .mapN((typed, beneath) =>
      Filter.all(
        initial.filter :: beneath ::
          typed.toList.flatMap((field, text) => parsed(field, text).toOption),
      ),
    )

  /** The keys the items are ordered by, most significant first. */
  val order: Signal[List[Order]] = keys.signal

  /** The window shown, or `None` while every item is. */
  val page: Signal[Option[Page]] = window.signal

  /** The whole query, as it changes. */
  val query: Signal[ListQuery] = filter
    .combineWith(order, page)
    .mapN(ListQuery(_, _, _))

  private val loaded: Signal[Either[String, Paged[X]]] =
    source.load(schema, query)

  /** The items shown, in order. */
  val items: Signal[List[X]] = loaded.map(_.fold(_ => List.empty, _.items))

  /** The number of items matching, across every window. */
  val total: Signal[Int] = loaded.map(_.fold(_ => 0, _.total))

  /** Why the query could not be run, if it could not. */
  val problem: Signal[Option[String]] = loaded.map(_.left.toOption)

  /** The kind of the named field, if the list has such a field. */
  def kindOf(field: String): Option[Kind] = schema.kinds.get(field)

  /** The text typed into one field's header cell. */
  def cell(field: String): Signal[String] = cells
    .signal
    .map(_.getOrElse(field, ""))

  /** Why the text in one field's header cell cannot be read, if it cannot. */
  def cellProblem(field: String): Signal[Option[String]] =
    cell(field).map(parsed(field, _).left.toOption)

  /**
    * Replaces the text in one field's header cell, returning to the first
    * window.
    */
  def typeInto(field: String, text: String): Unit =
    cells.update(_.updated(field, text))
    rewind()

  /** How the list is ordered by one field, if it is. */
  def orderOf(field: String): Signal[Option[Order]] = keys
    .signal
    .map(_.find(_.field == field))

  /**
    * Responds to a click on one field's heading, as [[Order.toggled]]
    * describes.
    */
  def toggleOrder(field: String): Unit =
    keys.update(Order.toggled(_, field))
    rewind()

  /** Moves to the next window. */
  def nextPage(): Unit = window.update(_.map(_.next))

  /** Moves to the previous window. */
  def previousPage(): Unit = window.update(_.map(_.previous))

  private def rewind(): Unit = window.update(_.map(_.copy(offset = 0)))

  private def parsed(field: String, text: String): Either[String, Filter] =
    kindOf(field)
      .toRight(s"No such field `$field`.")
      .flatMap(CellFilter.parse(field, _, text))
