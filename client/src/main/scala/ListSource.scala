package com.alecdorrington.eunomia
package client

import com.alecdorrington.eunomia.api.ListParams
import com.alecdorrington.eunomia.model.{ListQuery, ListReply, Paged, Schema}
import com.raquo.laminar.api.L.*
import io.circe.{Decoder, Encoder}
import io.circe.parser.decode
import io.laminext.fetch.circe.*
import org.scalajs.dom
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
  def items[X](all: Signal[List[X]]): ListSource[X] =
    (schema, query) => query.combineWith(all).mapN(schema.run)

  /**
    * A list served by an endpoint taking
    * [[com.alecdorrington.eunomia.api.ListApi.input]] and replying with
    * [[com.alecdorrington.eunomia.api.ListApi.reply]]. The first request tells
    * whether the list is short: if it is, it arrives whole, and every query
    * after runs here without another request; if not, each query is sent to the
    * server, once it has stopped changing for a moment, and a reply to one
    * since superseded is discarded. A query changed while the first request was
    * on its way is sent once that request is answered, and after a request that
    * fails, the next query is sent as after any other.
    *
    * The list is loaded again whenever `reloads` emits, as when the host hears
    * that it may have changed on the server: the query as it then stands is
    * asked again, and the items shown stay on screen until the answer arrives,
    * so that however often a list is reloaded, it never empties meanwhile. A
    * host with no way to hear of changes can reload a list whenever the page is
    * looked at again, with [[reshown]].
    *
    * @param url
    *   The endpoint, without query parameters.
    *
    * @param reloads
    *   Emits whenever the list may have changed on the server, reloading it.
    *
    * @param debounceMs
    *   How long a query must stay unchanged before it is sent, in milliseconds.
    */
  def endpoint[X : {Encoder, Decoder}]
    (
      url: String,
      reloads: EventStream[Any] = EventStream.empty,
      debounceMs: Int = 250,
    )
    : ListSource[X] = served[X](request[X](url, _), reloads, debounceMs)

  /**
    * A list served as [[endpoint]] describes, by whatever answers `send`.
    *
    * @param send
    *   Sends one query, replying with the list or why there is none.
    *
    * @param reloads
    *   Emits whenever the list may have changed on the server, reloading it.
    *
    * @param debounceMs
    *   How long a query must stay unchanged before it is sent, in milliseconds.
    */
  private[client] def served[X]
    (
      send: ListQuery => EventStream[Either[String, ListReply[X]]],
      reloads: EventStream[Any],
      debounceMs: Int,
    )
    : ListSource[X] = (schema, query) =>
    EventStream
      .merge(
        EventStream.fromValue(()),
        reloads.mapToUnit,
      )
      .flatMapSwitch(_ => loaded(send, debounceMs, schema, query))
      .startWith(Right(Paged.empty[X]))

  /**
    * Emits whenever the page is looked at again: shown after being hidden, as
    * when its tab is returned to, or focused after another window was. For
    * reloading an [[endpoint]]'s list where nothing tells the host when it has
    * changed, as it then may have while nobody was looking.
    */
  def reshown: EventStream[Unit] = EventStream
    .merge(
      documentEvents(_.onVisibilityChange)
        .filter(_ => !dom.document.hidden)
        .mapToUnit,
      windowEvents(_.onFocus).mapToUnit,
    )
    // Returning to a tab both shows and focuses it: one reload is enough.
    .debounce(together)

  /**
    * One load of a served list, from the first request on, which answers
    * nothing until that request has. The first request carries the current
    * query, so that a long list's first window is the one asked for and is
    * shown rather than fetched again.
    */
  private def loaded[X]
    (
      send: ListQuery => EventStream[Either[String, ListReply[X]]],
      debounceMs: Int,
      schema: Schema[X],
      query: Signal[ListQuery],
    )
    : EventStream[Either[String, Paged[X]]] = EventStream
    .fromValue(())
    .sample(query)
    .flatMapSwitch(first =>
      send(first).flatMapSwitch:
        case Right(ListReply.Whole(all)) => query.map(schema.run(_, all))
        case reply                       => remote(
            send,
            debounceMs,
            schema,
            query,
            first,
            reply.flatMap(_.answer(schema, first)),
          ),
    )

  /**
    * Answers each further query on the server, as a long list must be, starting
    * from the answer to the first request: its window, or why there is none.
    * The query as it stands is sent at once if it changed while that request
    * was on its way, and every later one once it has stopped changing.
    *
    * @param asked
    *   The query the first request carried.
    *
    * @param answer
    *   The answer to the first request.
    */
  private def remote[X]
    (
      send: ListQuery => EventStream[Either[String, ListReply[X]]],
      debounceMs: Int,
      schema: Schema[X],
      query: Signal[ListQuery],
      asked: ListQuery,
      answer: Either[String, Paged[X]],
    )
    : Signal[Either[String, Paged[X]]] = EventStream
    .merge(
      EventStream.fromValue(()).sample(query).filter(_ != asked),
      query.changes.debounce(debounceMs),
    )
    .flatMapSwitch(current =>
      send(current).map(_.flatMap(_.answer(schema, current))),
    )
    .startWith(answer)

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

  /**
    * How long apart two signs that the page is looked at again may be to count
    * as one, in milliseconds.
    */
  private val together = 100

  /** The address of one query against an endpoint. */
  private def address(url: String, query: ListQuery): String =
    val params = ListParams
      .of(query)
      .map((key, value) => s"$key=${ encodeURIComponent(value) }")
    if params.isEmpty then url
    else
      params.mkString(
        if url.contains('?') then s"$url&" else s"$url?",
        "&",
        "",
      )
