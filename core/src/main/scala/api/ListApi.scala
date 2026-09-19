package com.alecdorrington.eunomia
package api

import com.alecdorrington.eunomia.model.{
  Filter, ListQuery, ListReply, Order, Page, Paged,
}
import io.circe.{Decoder, Encoder}
import io.circe.parser.decode
import io.circe.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*

/**
  * Endpoint inputs and outputs for lists served to a browser. Add [[input]] to
  * any endpoint to have it take a [[ListQuery]] as query parameters, and reply
  * with [[reply]]:
  *
  * {{{
  * GET /api/things?filter={"field":"name","contains":"x"}&sort=-rating,name&offset=0&limit=50
  * }}}
  */
object ListApi:

  /** The `filter` parameter: a [[Filter]] as JSON, matching all when absent. */
  val filter: EndpointInput.Query[Filter] = query[Option[String]]("filter")
    .mapDecode(_.fold[DecodeResult[Filter]](DecodeResult.Value(Filter.always))(
      decodeFilter,
    ))(encodeFilter)
    .description("The filter items must satisfy, as JSON.")

  /** The `sort` parameter: comma-separated fields, each `-` if descending. */
  val sort: EndpointInput.Query[List[Order]] = query[Option[String]]("sort")
    .map(_.fold(List.empty[Order])(Order.parseAll))(encodeOrder)
    .description("The fields ordered by, e.g. `-rating,name`.")

  /** The `offset` and `limit` parameters, all items being sent without a limit. */
  val page: EndpointInput[Option[Page]] = query[Option[Int]]("offset")
    .description("The number of items skipped.")
    .and(query[Option[Int]]("limit").description("The most items sent."))
    .map((offset, limit) => limit.map(Page(offset.getOrElse(0), _)))(window =>
      (window.map(_.offset), window.map(_.limit)),
    )

  /** A whole [[ListQuery]], as the parameters above. */
  val input: EndpointInput[ListQuery] = filter
    .and(sort)
    .and(page)
    .map((narrowing, keys, window) => ListQuery(narrowing, keys, window))(
      query => (query.filter, query.order, query.page),
    )

  /** A reply to a list query: the whole list, or the window asked for. */
  def reply[X : {Encoder, Decoder, Schema}]
    : EndpointIO.Body[String, ListReply[X]] = jsonBody[ListReply[X]]

  /**
    * The query parameters [[input]] reads the given query from, for clients
    * building a request by hand. Values are not yet URL-encoded.
    */
  def params(query: ListQuery): List[(String, String)] = encodeFilter(
    query.filter,
  ).map("filter" -> _).toList ++ encodeOrder(query.order).map("sort" -> _) ++
    query
      .page
      .toList
      .flatMap(window =>
        List(
          "offset" -> window.offset.toString,
          "limit"  -> window.limit.toString,
        ),
      )

  private given [X : Schema]: Schema[Paged[X]] = Schema.derived

  private given [X : Schema]: Schema[ListReply[X]] = Schema.derived

  private def decodeFilter(text: String): DecodeResult[Filter] =
    decode[Filter](text).fold(
      error => DecodeResult.Error(text, error),
      DecodeResult.Value(_),
    )

  private def encodeFilter(filter: Filter): Option[String] =
    Option.when(filter != Filter.always)(filter.asJson.noSpaces)

  private def encodeOrder(order: List[Order]): Option[String] =
    Option.when(order.nonEmpty)(Order.textOf(order))
