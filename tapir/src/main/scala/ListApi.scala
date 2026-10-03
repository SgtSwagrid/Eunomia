package com.alecdorrington.eunomia
package tapir

import com.alecdorrington.eunomia.api.ListParameters
import com.alecdorrington.eunomia.model.{
  Filter, ListQuery, ListReply, Order, Page, Window,
}
import io.circe.{Decoder, Encoder}
import sttp.tapir.*
import sttp.tapir.json.circe.*

/**
  * Tapir endpoint inputs and outputs for lists, as [[ListParameters]] writes
  * and reads them. Add [[query]] to an endpoint to take a [[ListQuery]] as
  * query parameters, and [[reply]] to send the answer:
  *
  * {{{
  * GET /api/things?filter={"field":"name","contains":"x"}&order=-rating,name&offset=0&limit=50
  * }}}
  */
object ListApi:

  // Tapir's `query` is written in full, as this object's own hides it.

  /** The `filter` parameter: a [[Filter]] as JSON, matching all when absent. */
  val filter: EndpointInput.Query[Filter] = sttp
    .tapir
    .query[Option[String]]("filter")
    .mapDecode(decodeFilter)(ListParameters.filter)
    .description("The filter items must satisfy, as JSON.")

  /** The `order` parameter: comma-separated fields, with `-` if descending. */
  val order: EndpointInput.Query[List[Order]] = sttp
    .tapir
    .query[Option[String]]("order")
    .map(ListParameters.parseOrder)(ListParameters.order)
    .description("The fields ordered by, e.g. `-rating,name`.")

  /** The `offset` and `limit` parameters, the whole list without a limit. */
  val page: EndpointInput[Option[Page]] = sttp
    .tapir
    .query[Option[Int]]("offset")
    .description("The number of items skipped.")
    .and(
      sttp.tapir.query[Option[Int]]("limit").description("The most items sent."),
    )
    .map((offset, limit) => ListParameters.parsePage(offset, limit))(
      ListParameters.page,
    )

  /** A whole [[ListQuery]], as [[filter]], [[order]] and [[page]]. */
  val query: EndpointInput[ListQuery] = filter
    .and(order)
    .and(page)
    .map(ListQuery(_, _, _))(asked => (asked.filter, asked.order, asked.page))

  /**
    * The JSON body of a [[ListReply]].
    *
    * @tparam X
    *   The type of the items.
    */
  def reply[X : {Encoder, Decoder, Schema}]
    : EndpointIO.Body[String, ListReply[X]] = jsonBody[ListReply[X]]

  private given Schema[Page] = Schema.derived

  private given [X : Schema]: Schema[Window[X]] = Schema.derived

  /** A reply as its codec writes it: an object of one `window` or `whole`. */
  private given [X : Schema]: Schema[ListReply[X]] =
    val window = Schema.wrapWithSingleFieldProduct(
      summon[Schema[Window[X]]],
      FieldName("window"),
    )
    val whole = Schema.wrapWithSingleFieldProduct(
      summon[Schema[List[X]]],
      FieldName("whole"),
    )
    Schema(SchemaType.SCoproduct[ListReply[X]](List(window, whole), None) {
      case ListReply.Window(answer) =>
        Some(SchemaType.SchemaWithValue(window, answer))
      case ListReply.Whole(items) =>
        Some(SchemaType.SchemaWithValue(whole, items))
    })

  private def decodeFilter(text: Option[String]): DecodeResult[Filter] =
    ListParameters
      .parseFilter(text)
      .fold(
        problem => DecodeResult.Error(text.getOrElse(""), Exception(problem)),
        DecodeResult.Value(_),
      )
