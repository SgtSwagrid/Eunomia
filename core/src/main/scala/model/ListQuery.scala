package com.alecdorrington.eunomia
package model

import cats.syntax.all.*
import com.alecdorrington.eunomia.model.Filter.*

/**
  * A query on a list: which items to show, in what order, and which page of
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
  *   The page of items shown, or `None` for all of them.
  */
final case class ListQuery
  (
    filter: Filter = Filter.always,
    order: List[Order] = List.empty,
    page: Option[Page] = None,
  ):

  /**
    * Narrows this query by another filter.
    *
    * @param narrower
    *   The filter every item shown must also satisfy.
    *
    * @return
    *   A query showing only the items that satisfy both filters.
    */
  def narrowed(narrower: Filter): ListQuery = copy(filter = filter && narrower)

  /**
    * Reorders this query.
    *
    * @param keys
    *   The keys to order by, most significant first, replacing the current
    *   ones.
    *
    * @return
    *   A query ordered by `keys`.
    */
  def orderedBy(keys: Order*): ListQuery = copy(order = keys.toList)

  /**
    * Restricts this query to one page.
    *
    * @param page
    *   The page to show.
    *
    * @return
    *   A query showing only `page`.
    */
  def paged(page: Page): ListQuery = copy(page = Some(page))

  /**
    * Caps the number of items this query shows. A larger page is reduced, and a
    * query for every item gets the first page.
    *
    * @param max
    *   The greatest number of items to show.
    *
    * @return
    *   A query showing at most `max` items.
    */
  def capped(max: Int): ListQuery =
    copy(page = Some(page.fold(Page.first(max))(_.capped(max))))

  /**
    * Checks this query against the fields of a list: every field named exists,
    * every value compared with can be read as its field's kind (and is
    * converted to it, so `50.0` compares as `50` with an integer field), text
    * is only sought in text fields, and the page is well-formed.
    *
    * @param kinds
    *   The kind of each field of the list, by name.
    *
    * @return
    *   Either a message saying why the query cannot be run, or the checked
    *   query.
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

  private def check
    (filter: Filter, kinds: Map[String, Kind])
    : Either[String, Filter] = filter.fold[Either[String, Filter]](
    not = _.map(Not(_)),
    all = _.sequence.map(And(_)),
    any = _.sequence.map(Or(_)),
    compare = (field, operator, value) =>
      kindOf(field, kinds)
        .flatMap(convert(field, value, _))
        .map(Compare(field, operator, _)),
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

  private def kindOf
    (field: String, kinds: Map[String, Kind])
    : Either[String, Kind] = kinds
    .get(field)
    .toRight(s"No such field `$field`.")

  private def convert
    (field: String, value: Value, kind: Kind)
    : Either[String, Value] = value
    .convertedTo(kind)
    .toRight(s"`$field` holds ${ kind.noun }, not ${ value.kind.noun }.")
