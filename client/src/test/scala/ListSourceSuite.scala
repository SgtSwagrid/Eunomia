package com.alecdorrington.eunomia
package client

import com.alecdorrington.eunomia.model.{
  Field, ListQuery, ListReply, ListSchema,
}
import com.raquo.airstream.ownership.ManualOwner
import com.raquo.laminar.api.L.*
import munit.FunSuite
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js.timers.setTimeout

/** The server is a stub that answers each request only when a test says how. */
class ListSourceSuite extends FunSuite:

  private final case class Book(name: String, rating: Option[Long])

  private type Reply = Either[String, ListReply[Book]]

  private final case class Request
    (
      id: Int,
      query: ListQuery,
      answer: Reply => Unit,
    )

  private val books = List(
    Book("Alpha", Some(72L)),
    Book("beta edition", Some(45L)),
    Book("Gamma", None),
  )

  private val name   = Field.of[Book]("name", _.name)
  private val rating = Field.of[Book]("rating", _.rating)
  private val schema = ListSchema(name, rating)

  /** The debounce, in milliseconds. */
  private val settling = 10

  private given ExecutionContext = munitExecutionContext

  private final class Server:

    private given owner: ManualOwner = ManualOwner()

    private var waiting = List.empty[Request]

    private var sent = 0

    private val reloads = new EventBus[Unit]

    val list: ListState[Book] = ListState(
      schema,
      ListSource.served[Book](send, reloads.events, settling),
    )

    private val shown = list.items.observe

    private val refusal = list.problem.observe

    private val standing = list.query.observe

    /** The queries sent and neither answered nor abandoned, oldest first. */
    def asked: List[ListQuery] = waiting.map(_.query)

    def query: ListQuery = standing.now()

    def names: List[String] = shown.now().map(_.name)

    def problem: Option[String] = refusal.now()

    def window(): Unit =
      answer(carried => schema.run(carried, books).map(ListReply.Window(_)))

    def whole(): Unit = answer(_ => Right(ListReply.Whole(books)))

    def refuse(reason: String): Unit = answer(_ => Left(reason))

    def reload(): Unit = reloads.emit(())

    def stop(): Unit = owner.killSubscriptions()

    private def send(query: ListQuery): EventStream[Reply] =
      sent += 1
      val id = sent
      EventStream.fromCustomSource[Reply](
        start = (fire, _, _, _) => waiting :+= Request(id, query, fire),
        stop = _ => waiting = waiting.filterNot(_.id == id),
      )

    private def answer(reply: ListQuery => Reply): Unit =
      val oldest = waiting.head
      waiting = waiting.tail
      oldest.answer(reply(oldest.query))

  private def serving(check: Server => Future[Unit]): Future[Unit] =
    val server = Server()
    Future
      .delegate(check(server))
      .andThen:
        case _ => server.stop()

  private def settled(): Future[Unit] =
    val done = Promise[Unit]()
    setTimeout(settling * 5.0)(done.success(()))
    done.future

  test("the first request carries the query as it stands"):
    serving: server =>
      Future(assertEquals(server.asked, List(server.query)))

  test("a query changed during the first request is sent once that answers"):
    serving: server =>
      server.list.typeInto("name", "edition")
      settled().map: _ =>
        server.window()
        assertEquals(server.asked, List(server.query))
        server.window()
        assertEquals(server.names, List("beta edition"))

  test("a failed first request gives way to the next query"):
    serving: server =>
      server.refuse("The list is unavailable.")
      assertEquals(
        server.problem,
        Some("The list is unavailable."),
      )
      server.list.typeInto("name", "edition")
      settled().map: _ =>
        assertEquals(server.asked, List(server.query))
        server.window()
        assertEquals(server.problem, None)
        assertEquals(server.names, List("beta edition"))

  test("a list sent whole is queried here, without another request"):
    serving: server =>
      server.whole()
      assertEquals(server.names, books.map(_.name))
      server.list.typeInto("name", "edition")
      assertEquals(server.names, List("beta edition"))
      settled().map(_ => assertEquals(server.asked, List.empty))

  test("a query superseded while on its way is abandoned"):
    serving: server =>
      server.window()
      server.list.typeInto("name", "a")
      settled().flatMap: _ =>
        server.list.typeInto("name", "edition")
        settled().map: _ =>
          assertEquals(server.asked, List(server.query))
          server.window()
          assertEquals(server.names, List("beta edition"))

  test("a reload keeps the items shown until it is answered"):
    serving: server =>
      server.window()
      server.reload()
      assertEquals(server.asked, List(server.query))
      assertEquals(server.names, books.map(_.name))
      server.window()
      Future(assertEquals(server.names, books.map(_.name)))
