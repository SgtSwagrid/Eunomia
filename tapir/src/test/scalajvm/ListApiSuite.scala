package com.alecdorrington.eunomia
package tapir

import com.alecdorrington.eunomia.api.ListParameters
import com.alecdorrington.eunomia.model.*
import io.circe.parser.decode
import munit.FunSuite
import sttp.client3.{basicRequest, Identity, UriContext}
import sttp.client3.testing.SttpBackendStub
import sttp.model.StatusCode
import sttp.tapir.{endpoint, stringBody, stringToPath, SchemaType}
import sttp.tapir.server.stub.TapirStubInterpreter

class ListApiSuite extends FunSuite:

  private val n      = Field.of[Int]("n", identity[Int])
  private val schema = ListSchema(n)

  private val numbers = endpoint
    .get
    .in("numbers")
    .in(ListApi.query)
    .errorOut(stringBody)
    .out(ListApi.reply[Int])
    .serverLogic[Identity](query =>
      schema
        .run(query, List(1, 2, 3, 4, 5))
        .map(found =>
          query
            .page
            .fold(ListReply.Whole(found.items))(_ => ListReply.Window(found)),
        ),
    )

  private val backend = TapirStubInterpreter(SttpBackendStub.synchronous)
    .whenServerEndpointRunLogic(numbers)
    .backend()

  private def ask(query: ListQuery): Either[String, ListReply[Int]] =
    basicRequest
      .get(uri"http://test/numbers?${ ListParameters.of(query).toMap }")
      .send(backend)
      .body
      .flatMap(decode[ListReply[Int]](_).left.map(_.getMessage))

  test("a query written by the client is the query the server reads"):
    val query = ListQuery(
      n > 1,
      List(n.descending),
      Some(Page(1, 2)),
    )
    assertEquals(
      ask(query),
      Right(ListReply.Window(Window(List(4, 3), 4, Some(Page(1, 2))))),
    )

  test("a query without a page is answered whole"):
    assertEquals(
      ask(ListQuery(n < 3)),
      Right(ListReply.Whole(List(1, 2))),
    )

  test("a reply is described as its codec writes it"):
    val keys = ListApi.reply[Int].codec.schema.schemaType match
      case SchemaType.SCoproduct(subtypes, _) => subtypes
          .map(_.schemaType)
          .collect { case SchemaType.SProduct(fields) =>
            fields.map(_.name.name)
          }
      case _ => Nil
    assertEquals(
      keys,
      List(List("window"), List("whole")),
    )

  test("a filter that cannot be read is refused"):
    val answer = basicRequest
      .get(uri"http://test/numbers?filter=${ "{" }")
      .send(backend)
    assertEquals(answer.code, StatusCode.BadRequest)
