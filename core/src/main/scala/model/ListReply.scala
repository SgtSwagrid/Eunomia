package com.alecdorrington.eunomia
package model

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

/**
  * A server's reply to a [[ListQuery]]. The server, which alone knows how long
  * the list is, decides where the query runs: a short list is sent whole, once,
  * for every later query to run in the browser without another request; a long
  * one is queried in the database, and only the requested window is sent.
  * Neither side's caller need know which happened.
  */
enum ListReply[X]:

  /** The window of the list answering the query, run in the database. */
  case Window(paged: Paged[X])

  /**
    * The whole list, short enough to be sent entire, in stored order and not
    * yet filtered: the query is left for the receiver to run.
    */
  case Whole(items: List[X])

  /** The same reply, with each item transformed. */
  def map[Y](transform: X => Y): ListReply[Y] = this match
    case ListReply.Window(paged) => ListReply.Window(paged.map(transform))
    case ListReply.Whole(items)  => ListReply.Whole(items.map(transform))

  /**
    * The answer to a query, running it here first if the whole list was sent.
    *
    * @param schema
    *   The fields of the items.
    *
    * @param query
    *   The query this is the reply to, or any later one.
    */
  def answer(schema: Schema[X], query: ListQuery): Either[String, Paged[X]] =
    this match
      case ListReply.Window(paged) => Right(paged)
      case ListReply.Whole(items)  => schema.run(query, items)

object ListReply:

  given [X : Encoder]: Encoder[ListReply[X]] = Encoder.instance:
    case Window(paged) => Json.obj("window" -> paged.asJson)
    case Whole(items)  => Json.obj("whole" -> items.asJson)

  given [X : Decoder]: Decoder[ListReply[X]] = Decoder[Paged[X]]
    .at("window")
    .map(Window(_))
    .or(Decoder[List[X]].at("whole").map(Whole(_)))
