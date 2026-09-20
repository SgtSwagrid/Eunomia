package com.alecdorrington.eunomia
package model

import cats.syntax.all.*
import com.alecdorrington.eunomia.model.Filter.*

/**
  * What to show of a list: which items, in what order, and which window of
  * them.
  *
  * @param filter
  *   The filter every item shown must satisfy.
  *
  * @param order
  *   The keys the items are ordered by, most significant first. Without any,
  *   items keep the order they were stored in.
  *
  * @param page
  *   The window of items shown, or `None` for all of them.
  */
final case class ListQuery
  (
    filter: Filter = Filter.always,
    order: List[Order] = List.empty,
    page: Option[Page] = None,
  ):

  /** This query, further narrowed by the given filter. */
  def where(narrower: Filter): ListQuery = copy(filter = filter && narrower)

  /** This query, ordered by the given keys instead. */
  def sortedBy(keys: Order*): ListQuery = copy(order = keys.toList)

  /** This query, showing only the given window. */
  def paged(window: Page): ListQuery = copy(page = Some(window))

  /**
    * This query, showing no more than the given number of items: a request for
    * more is reduced, and a request for all of them gets the first window.
    */
  def limited(max: Int): ListQuery =
    copy(page = Some(page.fold(Page.first(max))(_.capped(max))))

  /**
    * This query, checked against the fields of the list it is run on: every
    * field it names exists, every value it compares against can be read as the
    * kind of that field (and is converted to it, so `50.0` compares as `50`
    * with a whole-number field), text is only sought in text fields, and its
    * window is well-formed.
    *
    * @param kinds
    *   The kind of each field of the list, by name.
    *
    * @return
    *   A checked query, or why it cannot be run.
    */
  def checked(kinds: Map[String, Kind]): Either[String, ListQuery] = (
    ListQuery.check(filter, kinds),
    order.traverse(key => ListQuery.kindOf(key.field, kinds).as(key)),
    Either.cond(
      page.forall(_.valid),
      page,
      "The page is malformed.",
    ),
  ).mapN(ListQuery(_, _, _))

object ListQuery:

  /** Checks one filter as [[ListQuery.checked]] describes. */
  private def check
    (filter: Filter, kinds: Map[String, Kind])
    : Either[String, Filter] = filter.fold[Either[String, Filter]](
    not = _.map(Not(_)),
    all = _.sequence.map(And(_)),
    any = _.sequence.map(Or(_)),
    compare = (field, comparison, value) =>
      kindOf(field, kinds)
        .flatMap(convert(field, value, _))
        .map(Compare(field, comparison, _)),
    contains = (field, text) =>
      kindOf(field, kinds).flatMap(kind =>
        Either.cond(
          kind == Kind.Text,
          Contains(field, text),
          s"`$field` does not hold text.",
        ),
      ),
    oneOf = (field, values) =>
      kindOf(field, kinds)
        .flatMap(kind => values.traverse(convert(field, _, kind)))
        .map(OneOf(field, _)),
    missing = field => kindOf(field, kinds).as(Missing(field)),
  )

  /** The kind of the named field, or a complaint that there is no such field. */
  private def kindOf
    (field: String, kinds: Map[String, Kind])
    : Either[String, Kind] = kinds
    .get(field)
    .toRight(s"No such field `$field`.")

  /** A value as one of the given kind, or a complaint that it is not one. */
  private def convert
    (field: String, value: Value, kind: Kind)
    : Either[String, Value] = value
    .as(kind)
    .toRight(s"`$field` holds ${ kind.noun }, not ${ value.kind.noun }.")
