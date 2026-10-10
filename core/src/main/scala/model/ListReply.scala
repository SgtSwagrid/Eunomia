package com.alecdorrington.eunomia
package model

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

/**
  * A server's reply to a [[ListQuery]]. The server decides where the query
  * runs: a short list is sent whole, once, for later queries to run in the
  * browser; a long one is queried in the database, and only the window of the
  * requested page is sent. Callers need not know which happened.
  *
  * @tparam X
  *   The type of the items.
  */
enum ListReply[X]:

  /**
    * The window of the list answering the query, run in the database.
    *
    * @param window
    *   The window.
    */
  case Window(window: model.Window[X])

  /**
    * The whole list, unfiltered and in stored order, for the receiver to query.
    *
    * @param items
    *   Every item of the list.
    */
  case Whole(items: List[X])

  /**
    * Transforms each item of this reply.
    *
    * @tparam Y
    *   The type of the transformed items.
    *
    * @param transform
    *   The transformation of one item.
    *
    * @return
    *   A reply of the same shape holding the transformed items.
    */
  def map[Y](transform: X => Y): ListReply[Y] = this match
    case ListReply.Window(window) => ListReply.Window(window.map(transform))
    case ListReply.Whole(items)   => ListReply.Whole(items.map(transform))

  /**
    * Answers a query from this reply, running it here if the whole list was
    * sent.
    *
    * @param schema
    *   The fields of the items.
    *
    * @param query
    *   The query this is the reply to, or any later one.
    *
    * @return
    *   Either a message saying why the query cannot be run, or the window
    *   answering it.
    */
  def answer
    (schema: ListSchema[X], query: ListQuery)
    : Either[String, model.Window[X]] = this match
    case ListReply.Window(window) => Right(window)
    case ListReply.Whole(items)   => schema.run(query, items)

object ListReply:

  given [X : Encoder]: Encoder[ListReply[X]] = Encoder.instance:
    case Window(window) => Json.obj("window" -> window.asJson)
    case Whole(items)   => Json.obj("whole" -> items.asJson)

  given [X : Decoder]: Decoder[ListReply[X]] = Decoder[model.Window[X]]
    .at("window")
    .map(Window(_))
    .or(Decoder[List[X]].at("whole").map(Whole(_)))
