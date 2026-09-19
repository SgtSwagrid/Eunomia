package com.alecdorrington.eunomia
package client

import com.alecdorrington.eunomia.api.ListApi
import com.alecdorrington.eunomia.model.{ListQuery, ListReply, Paged, Schema}
import com.raquo.laminar.api.L.*
import io.circe.{Decoder, Encoder}
import io.circe.parser.decode
import io.laminext.fetch.circe.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.URIUtils.encodeURIComponent

/**
  * Where the items of a list come from. Whether a query then runs in the
  * browser or on the server is decided by the source, never by its user.
  */
trait ListSource[X]:

  /**
    * The window of items answering the latest query, or why it cannot be run.
    *
    * @param schema
    *   The fields of the items.
    *
    * @param query
    *   The query, as it changes.
    */
  def load
    (
      schema: Schema[X],
      query: Signal[ListQuery],
    )
    : Signal[Either[String, Paged[X]]]

object ListSource:

  /**
    * A list whose items the application already holds in the browser.
    *
    * @param all
    *   Every item of the list, in stored order.
    */
  def items[X](all: Signal[List[X]]): ListSource[X] = new ListSource[X]:
    override def load
      (
        schema: Schema[X],
        query: Signal[ListQuery],
      ) = query.combineWith(all).mapN(schema.run)

  /**
    * A list served by an endpoint taking
    * [[com.alecdorrington.eunomia.api.ListApi.input]] and replying with
    * [[com.alecdorrington.eunomia.api.ListApi.reply]]. The first request tells
    * whether the list is short: if it is, it arrives whole, and every query
    * after runs here without another request; if not, each query is sent to the
    * server, once it has stopped changing for a moment, and a reply to one
    * since superseded is discarded.
    *
    * @param url
    *   The endpoint, without query parameters.
    *
    * @param revision
    *   Changes whenever the list may have changed on the server, reloading it.
    *
    * @param debounceMs
    *   How long a query must stay unchanged before it is sent, in milliseconds.
    */
  def endpoint[X : {Encoder, Decoder}]
    (
      url: String,
      revision: Signal[Any] = Val(()),
      debounceMs: Int = 250,
    )
    : ListSource[X] = new ListSource[X]:
    override def load
      (
        schema: Schema[X],
        query: Signal[ListQuery],
      ) = revision.flatMapSwitch(_ => loaded(url, debounceMs, schema, query))

  /**
    * One load of an endpoint's list, from the first request on. The first
    * request carries the current query, so that a long list's first window is
    * the one asked for and is shown rather than fetched again.
    */
  private def loaded[X : {Encoder, Decoder}]
    (
      url: String,
      debounceMs: Int,
      schema: Schema[X],
      query: Signal[ListQuery],
    )
    : Signal[Either[String, Paged[X]]] = EventStream
    .fromValue(())
    .sample(query)
    .flatMapSwitch(first =>
      request[X](url, first).flatMapSwitch:
        case Right(ListReply.Whole(all)) => query.map(schema.run(_, all))
        case Right(window @ ListReply.Window(_)) => remote(
            url,
            debounceMs,
            schema,
            query,
            window.answer(schema, first),
          )
        case Left(problem) => Val[Either[String, Paged[X]]](Left(problem)),
    )
    .startWith(Right(Paged.empty[X]))

  /**
    * Answers each further query on the server, as a long list must be, starting
    * from the window the first request already brought.
    */
  private def remote[X : {Encoder, Decoder}]
    (
      url: String,
      debounceMs: Int,
      schema: Schema[X],
      query: Signal[ListQuery],
      first: Either[String, Paged[X]],
    )
    : Signal[Either[String, Paged[X]]] = query
    .changes
    .debounce(debounceMs)
    .flatMapSwitch(current =>
      request[X](url, current).map(_.flatMap(_.answer(schema, current))),
    )
    .startWith(first)

  /** Sends one query, turning any refusal into its reason. */
  private def request[X : {Encoder, Decoder}]
    (url: String, query: ListQuery)
    : EventStream[Either[String, ListReply[X]]] = Fetch
    .get(address(url, query))
    .text
    .map(response =>
      if response.status >= 400 then Left(response.data)
      else decode[ListReply[X]](response.data).left.map(_.getMessage),
    )
    .recover { case error => Some(Left(error.getMessage)) }

  /** The address of one query against an endpoint. */
  private def address(url: String, query: ListQuery): String =
    val params = ListApi
      .params(query)
      .map((key, value) => s"$key=${ encodeURIComponent(value) }")
    if params.isEmpty then url
    else
      params.mkString(
        if url.contains('?') then s"$url&" else s"$url?",
        "&",
        "",
      )
