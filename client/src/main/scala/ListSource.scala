package com.alecdorrington.eunomia
package client

import com.alecdorrington.eunomia.api.ListParameters
import com.alecdorrington.eunomia.model.{
  ListQuery, ListReply, ListSchema, Window,
}
import com.raquo.laminar.api.L.*
import io.circe.{Decoder, Encoder}
import io.circe.parser.decode
import io.laminext.fetch.circe.*
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.URIUtils.encodeURIComponent

/**
  * A source of the items of a list. The source, not its user, decides whether a
  * query runs in the browser or on the server.
  *
  * @tparam X
  *   The type of the items.
  */
trait ListSource[X]:

  /**
    * Loads the window answering the latest query.
    *
    * @param schema
    *   The fields of the items.
    *
    * @param query
    *   The query, as it changes.
    *
    * @return
    *   A signal of either a message saying why the query cannot be run, or the
    *   window answering it.
    */
  def load
    (
      schema: ListSchema[X],
      query: Signal[ListQuery],
    )
    : Signal[Either[String, Window[X]]]

object ListSource:

  /**
    * Creates a source of items already held in the browser.
    *
    * @tparam X
    *   The type of the items.
    *
    * @param all
    *   Every item of the list, in stored order.
    *
    * @return
    *   A source running every query in the browser.
    */
  def items[X](all: Signal[List[X]]): ListSource[X] =
    (schema, query) => query.combineWith(all).mapN(schema.run)

  /**
    * Creates a source served by an endpoint that reads its query as
    * [[ListParameters]] and replies with a [[ListReply]] as JSON. If the first
    * reply holds the whole list, every later query runs in the browser;
    * otherwise each is sent once it stops changing, and replies to superseded
    * queries are discarded. While a reload is on its way, the items shown stay
    * on screen.
    *
    * @tparam X
    *   The type of the items.
    *
    * @param url
    *   The endpoint, without query parameters.
    *
    * @param reloads
    *   The events on which the list may have changed on the server, each of
    *   which reloads it, e.g. [[reshown]].
    *
    * @param debounce
    *   The time a query must stay unchanged before it is sent, in milliseconds.
    *
    * @return
    *   A source served by the endpoint.
    */
  def endpoint[X : {Encoder, Decoder}]
    (
      url: String,
      reloads: EventStream[Any] = EventStream.empty,
      debounce: Int = 250,
    )
    : ListSource[X] = served[X](request[X](url, _), reloads, debounce)

  /** Serves a list as [[endpoint]] does, by whatever answers `send`. */
  private[client] def served[X]
    (
      send: ListQuery => EventStream[Either[String, ListReply[X]]],
      reloads: EventStream[Any],
      debounce: Int,
    )
    : ListSource[X] = (schema, query) =>
    EventStream
      .merge(
        EventStream.fromValue(()),
        reloads.mapToUnit,
      )
      .flatMapSwitch(_ => loaded(send, debounce, schema, query))
      .startWith(Right(Window.empty[X]))

  /**
    * The events of the page being looked at again: shown after being hidden, or
    * focused after another window was. Use it to reload an [[endpoint]]'s list
    * where nothing else says when it has changed.
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
    * The first request carries the current query, so that a long list's first
    * window is not fetched twice.
    */
  private def loaded[X]
    (
      send: ListQuery => EventStream[Either[String, ListReply[X]]],
      debounce: Int,
      schema: ListSchema[X],
      query: Signal[ListQuery],
    )
    : EventStream[Either[String, Window[X]]] = EventStream
    .fromValue(())
    .sample(query)
    .flatMapSwitch(first =>
      send(first).flatMapSwitch:
        case Right(ListReply.Whole(all)) => query.map(schema.run(_, all))
        case reply                       => remote(
            send,
            debounce,
            schema,
            query,
            first,
            reply.flatMap(_.answer(schema, first)),
          ),
    )

  /**
    * Answers later queries on the server, starting from the `answer` to the
    * first request: at once if the query changed since it was `asked`, then
    * each once it stops changing.
    */
  private def remote[X]
    (
      send: ListQuery => EventStream[Either[String, ListReply[X]]],
      debounce: Int,
      schema: ListSchema[X],
      query: Signal[ListQuery],
      asked: ListQuery,
      answer: Either[String, Window[X]],
    )
    : Signal[Either[String, Window[X]]] = EventStream
    .merge(
      EventStream.fromValue(()).sample(query).filter(_ != asked),
      query.changes.debounce(debounce),
    )
    .flatMapSwitch(current =>
      send(current).map(_.flatMap(_.answer(schema, current))),
    )
    .startWith(answer)

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

  /** The time within which a show and a focus count as one, in milliseconds. */
  private val together = 100

  private def address(url: String, query: ListQuery): String =
    val parameters = ListParameters
      .of(query)
      .map((key, value) => s"$key=${ encodeURIComponent(value) }")
    if parameters.isEmpty then url
    else
      parameters.mkString(
        if url.contains('?') then s"$url&" else s"$url?",
        "&",
        "",
      )
