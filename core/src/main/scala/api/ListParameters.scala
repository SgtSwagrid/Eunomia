package com.alecdorrington.eunomia
package api

import cats.syntax.all.*
import com.alecdorrington.eunomia.model.{Filter, ListQuery, Order, Page}
import io.circe.parser.decode
import io.circe.syntax.*

/**
  * A [[ListQuery]] as the query parameters of a request, and back:
  *
  * {{{
  * ?filter={"field":"name","contains":"x"}&order=-rating,name&offset=0&limit=50
  * }}}
  *
  * A client writes a query with [[of]], and a server reads it with [[parse]].
  * Each parameter can also be written and read on its own, for endpoint
  * descriptions that name them one by one.
  */
object ListParameters:

  /**
    * Writes the `filter` parameter.
    *
    * @param filter
    *   The filter.
    *
    * @return
    *   An option holding the filter as JSON, or `None` if it matches
    *   everything.
    */
  def filter(filter: Filter): Option[String] =
    Option.when(filter != Filter.always)(filter.asJson.noSpaces)

  /**
    * Parses the `filter` parameter.
    *
    * @param text
    *   The parameter's value, or `None` if absent.
    *
    * @return
    *   Either a message saying why it cannot be read, or the filter, matching
    *   everything if absent.
    */
  def parseFilter(text: Option[String]): Either[String, Filter] =
    text.fold[Either[String, Filter]](Right(Filter.always))(
      decode[Filter](_).left.map(_.getMessage),
    )

  /**
    * Writes the `order` parameter.
    *
    * @param order
    *   The keys, most significant first.
    *
    * @return
    *   An option holding the keys as written, or `None` if there are none.
    */
  def order(order: List[Order]): Option[String] =
    Option.when(order.nonEmpty)(Order.textOf(order))

  /**
    * Parses the `order` parameter.
    *
    * @param text
    *   The parameter's value, or `None` if absent.
    *
    * @return
    *   A list of keys, most significant first, empty for stored order.
    */
  def parseOrder(text: Option[String]): List[Order] =
    text.fold(Nil)(Order.parseAll)

  /**
    * Writes the `offset` and `limit` parameters.
    *
    * @param page
    *   The page, or `None` for the whole list.
    *
    * @return
    *   A pair of the offset and the limit, both `None` for the whole list.
    */
  def page(page: Option[Page]): (Option[Int], Option[Int]) =
    (page.map(_.offset), page.map(_.limit))

  /**
    * Parses the `offset` and `limit` parameters.
    *
    * @param offset
    *   The offset, or `None` to start at the first item.
    *
    * @param limit
    *   The limit, or `None` for the whole list.
    *
    * @return
    *   An option holding the page, or `None` for the whole list.
    */
  def parsePage(offset: Option[Int], limit: Option[Int]): Option[Page] = limit
    .map(Page(offset.getOrElse(0), _))

  /**
    * Writes a query as parameters, omitting those whose absence means the same.
    *
    * @param query
    *   The query.
    *
    * @return
    *   A list of parameter names and values, not yet URL-encoded.
    */
  def of(query: ListQuery): List[(String, String)] =
    val (offset, limit) = page(query.page)
    List(
      "filter" -> filter(query.filter),
      "order"  -> order(query.order),
      "offset" -> offset.map(_.toString),
      "limit"  -> limit.map(_.toString),
    ).flatMap((key, value) => value.map(key -> _))

  /**
    * Parses a query from parameters as [[of]] writes them.
    *
    * @param parameter
    *   The function giving the URL-decoded value of the parameter of a given
    *   name, or `None` where the request has none.
    *
    * @return
    *   Either a message saying why the parameters describe no query, or the
    *   query.
    */
  def parse(parameter: String => Option[String]): Either[String, ListQuery] =
    for
      filter <- parseFilter(parameter("filter"))
      offset <- integer(parameter, "offset")
      limit  <- integer(parameter, "limit")
    yield ListQuery(
      filter,
      parseOrder(parameter("order")),
      parsePage(offset, limit),
    )

  private def integer
    (
      parameter: String => Option[String],
      name: String,
    )
    : Either[String, Option[Int]] = parameter(name).traverse(text =>
    text.toIntOption.toRight(s"The `$name` parameter is not a whole number."),
  )
