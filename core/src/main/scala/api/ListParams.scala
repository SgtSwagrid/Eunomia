package com.alecdorrington.eunomia
package api

import com.alecdorrington.eunomia.model.{Filter, ListQuery, Order}
import io.circe.parser.decode
import io.circe.syntax.*

/**
  * A [[ListQuery]] as the query parameters of a request, and back:
  *
  * {{{
  * ?filter={"field":"name","contains":"x"}&sort=-rating,name&offset=0&limit=50
  * }}}
  *
  * [[ListApi]] describes an endpoint taking exactly these, and reads and writes
  * them through here. A browser building a request of its own uses these
  * directly, so that asking for a list does not link a description of the
  * endpoint serving it.
  */
object ListParams:

  /** The `filter` parameter, absent where the filter matches everything. */
  def filter(filter: Filter): Option[String] =
    Option.when(filter != Filter.always)(filter.asJson.noSpaces)

  /** The filter a `filter` parameter holds, or why it cannot be read. */
  def parseFilter(text: String): Either[String, Filter] = decode[Filter](text)
    .left
    .map(_.getMessage)

  /** The `sort` parameter, absent where the list is in stored order. */
  def sort(order: List[Order]): Option[String] =
    Option.when(order.nonEmpty)(Order.textOf(order))

  /**
    * Every parameter one query is written as, omitting those whose absence says
    * the same thing. Values are not yet URL-encoded.
    */
  def of(query: ListQuery): List[(String, String)] = filter(query.filter)
    .map("filter" -> _)
    .toList ++ sort(query.order).map("sort" -> _) ++
    query
      .page
      .toList
      .flatMap(window =>
        List(
          "offset" -> window.offset.toString,
          "limit"  -> window.limit.toString,
        ),
      )
