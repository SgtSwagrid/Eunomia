package com.alecdorrington.eunomia
package client

import com.alecdorrington.eunomia.model.{
  CellFilter, Filter, Kind, ListQuery, ListSchema, Order, Page, Window,
}
import com.raquo.laminar.api.L.*

/**
  * The headless browser state of one list as a person narrows and orders it:
  * the text in each column's header cell, the keys chosen by clicking headings,
  * and the page shown. It can drive a [[Table]] or any other rendering.
  *
  * @tparam X
  *   The type of the items.
  *
  * @param schema
  *   The fields of the items, as shared with the server.
  *
  * @param source
  *   The source of the items.
  *
  * @param narrowing
  *   The filter the host applies beneath whatever is typed, e.g. to show only
  *   the user's own items.
  *
  * @param initial
  *   The query the list starts with, its filter added to `narrowing`.
  */
final class ListState[X]
  (
    schema: ListSchema[X],
    source: ListSource[X],
    narrowing: Signal[Filter] = Val(Filter.always),
    initial: ListQuery = ListQuery(),
  ):

  private val cells: Var[Map[String, String]] = Var(Map.empty)

  private val keys: Var[List[Order]] = Var(initial.order)

  private val asked: Var[Option[Page]] = Var(initial.page)

  /** Each header cell's text as read: a filter, or why it is none. */
  private val readings: Signal[Map[String, Either[String, Filter]]] = cells
    .signal
    .map(_.transform(parsed))

  /**
    * The filter the items shown must satisfy. A cell whose text cannot be read
    * narrows nothing until it can (see [[cellProblem]]).
    */
  val filter: Signal[Filter] = readings
    .combineWith(narrowing)
    .mapN((read, beneath) =>
      Filter.all(
        initial.filter :: beneath :: read.values.flatMap(_.toOption).toList,
      ),
    )

  /** The keys the items are ordered by, most significant first. */
  val order: Signal[List[Order]] = keys.signal

  /** The whole query. */
  val query: Signal[ListQuery] = filter
    .combineWith(order, asked.signal)
    .mapN(ListQuery(_, _, _))

  private val loaded: Signal[Either[String, Window[X]]] =
    source.load(schema, query)

  /**
    * The page shown, as answered rather than as asked for, since a server may
    * page a long list unasked or cap the page's size. `None` while every item
    * is shown.
    */
  val page: Signal[Option[Page]] = loaded.map(_.toOption.flatMap(_.page))

  /** The items shown, in order. */
  val items: Signal[List[X]] = loaded.map(_.fold(_ => List.empty, _.items))

  /** The number of matching items, across every page. */
  val total: Signal[Int] = loaded.map(_.fold(_ => 0, _.total))

  /** The reason the query could not be run, if it could not. */
  val problem: Signal[Option[String]] = loaded.map(_.left.toOption)

  /**
    * Looks up the kind of a field.
    *
    * @param field
    *   The name of the field.
    *
    * @return
    *   An option holding the kind, or `None` if the list has no such field.
    */
  def kindOf(field: String): Option[Kind] = schema.kinds.get(field)

  /**
    * Follows the text in one field's header cell.
    *
    * @param field
    *   The name of the field.
    *
    * @return
    *   A signal of the text, empty if none was typed.
    */
  def cell(field: String): Signal[String] = cells
    .signal
    .map(_.getOrElse(field, ""))

  /**
    * Follows whether the text in one field's header cell can be read.
    *
    * @param field
    *   The name of the field.
    *
    * @return
    *   A signal of the reason the text cannot be read, or `None` if it can.
    */
  def cellProblem(field: String): Signal[Option[String]] =
    readings.map(_.getOrElse(field, parsed(field, "")).left.toOption)

  /**
    * Replaces the text in one field's header cell, returning to the first page.
    *
    * @param field
    *   The name of the field.
    *
    * @param text
    *   The new text.
    */
  def typeInto(field: String, text: String): Unit =
    rewinding(cells)(_.updated(field, text))

  /**
    * Follows how the list is ordered by one field.
    *
    * @param field
    *   The name of the field.
    *
    * @return
    *   A signal of the field's key, or `None` if the list is not ordered by it.
    */
  def orderOf(field: String): Signal[Option[Order]] = keys
    .signal
    .map(_.find(_.field == field))

  /**
    * Responds to a click on one field's heading, as [[Order.toggled]]
    * describes, returning to the first page.
    *
    * @param field
    *   The name of the field.
    */
  def toggleOrder(field: String): Unit =
    rewinding(keys)(Order.toggled(_, field))

  /**
    * Shows a page of the list.
    *
    * @param page
    *   The page, best derived from [[ListState.page]], which gives the size the
    *   server answers with.
    */
  def showPage(page: Page): Unit = asked.set(Some(page))

  /** Rewinds in the same transaction, so that the query changes once. */
  private def rewinding[A](state: Var[A])(change: A => A): Unit =
    Var.update(state -> change, asked -> rewound)

  private def rewound(page: Option[Page]): Option[Page] =
    page.map(_.copy(offset = 0))

  private def parsed(field: String, text: String): Either[String, Filter] =
    kindOf(field)
      .toRight(s"No such field `$field`.")
      .flatMap(CellFilter.parse(field, _, text))
