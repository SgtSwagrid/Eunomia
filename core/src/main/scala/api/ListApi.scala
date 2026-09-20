package com.alecdorrington.eunomia
package api

import com.alecdorrington.eunomia.model.{
  Filter, ListQuery, ListReply, Order, Page, Paged,
}
import io.circe.{Decoder, Encoder}
// Tapir describes a JSON body with a `Schema` of its own, which is not the
// [[com.alecdorrington.eunomia.model.Schema]] a list is described by:
import sttp.tapir.{Schema as JsonSchema, *}
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
    ))(ListParams.filter)
    .description("The filter items must satisfy, as JSON.")

  /** The `sort` parameter: comma-separated fields, each `-` if descending. */
  val sort: EndpointInput.Query[List[Order]] = query[Option[String]]("sort")
    .map(_.fold(List.empty[Order])(Order.parseAll))(ListParams.sort)
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
  def reply[X : {Encoder, Decoder, JsonSchema}]
    : EndpointIO.Body[String, ListReply[X]] = jsonBody[ListReply[X]]

  private given JsonSchema[Page] = JsonSchema.derived

  private given [X : JsonSchema]: JsonSchema[Paged[X]] = JsonSchema.derived

  private given [X : JsonSchema]: JsonSchema[ListReply[X]] = JsonSchema.derived

  private def decodeFilter(text: String): DecodeResult[Filter] = ListParams
    .parseFilter(text)
    .fold(
      problem => DecodeResult.Error(text, Exception(problem)),
      DecodeResult.Value(_),
    )
