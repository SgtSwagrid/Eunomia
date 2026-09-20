package com.alecdorrington.eunomia
package server

import com.alecdorrington.eunomia.model.*
import munit.FunSuite
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api.*

/** One row of the table under test. */
final case class Row
  (
    id: Long,
    name: String,
    rating: Option[Long],
    score: Double,
    inPrint: Boolean,
  )

/** The table under test, with a nullable column and a column of each kind. */
final class Rows(tag: Tag) extends Table[Row](tag, "list_rows"):

  def id      = column[Long]("id", O.PrimaryKey)
  def name    = column[String]("name")
  def rating  = column[Option[Long]]("rating")
  def score   = column[Double]("score")
  def inPrint = column[Boolean]("inPrint")

  override def * = (id, name, rating, score, inPrint).mapTo[Row]

/** A note on at most one row, for testing lists drawn from a join. */
final case class Note(rowId: Long, mark: Long)

/** The table of notes, which some rows have none of. */
final class Notes(tag: Tag) extends Table[Note](tag, "list_notes"):

  def rowId = column[Long]("row_id")
  def mark  = column[Long]("mark")

  override def * = (rowId, mark).mapTo[Note]

/**
  * Checks that running a query in the database gives exactly what running it in
  * memory over the same items does, so that a list can move between the two
  * without changing what it shows.
  */
class SqlListsSuite extends FunSuite:

  private val rows = TableQuery[Rows]

  private val notes = TableQuery[Notes]

  private val noted = List(Note(1, 3), Note(4, 1), Note(5, 3))

  private val data = List(
    Row(
      1,
      "Alpha book",
      Some(72L),
      7.5,
      inPrint = true,
    ),
    Row(
      2,
      "beta draft",
      Some(45L),
      4.5,
      inPrint = false,
    ),
    Row(
      3,
      "Gamma 100% done",
      None,
      9.0,
      inPrint = false,
    ),
    Row(
      4,
      "delta_book",
      Some(90L),
      9.0,
      inPrint = true,
    ),
    Row(
      5,
      "Epsilon",
      Some(45L),
      3.25,
      inPrint = true,
    ),
  )

  private val name    = Field.of[Row]("name", _.name)
  private val rating  = Field.of[Row]("rating", _.rating)
  private val score   = Field.of[Row]("score", _.score)
  private val inPrint = Field.of[Row]("inPrint", _.inPrint)

  private val schema = Schema(name, rating, score, inPrint)

  /** Lists that always run queries in the database, however short. */
  private val lists = SqlLists(H2Profile, wholeUpTo = 0)

  private val columns = lists.columns(schema)(
    lists.text[Rows]("name")(_.name.?),
    lists.whole[Rows]("rating")(_.rating),
    lists.real[Rows]("score")(_.score.?),
    lists.flag[Rows]("inPrint")(_.inPrint.?),
  )

  private val db = Database.forURL(
    "jdbc:h2:mem:lists;DB_CLOSE_DELAY=-1",
    driver = "org.h2.Driver",
  )

  override def beforeAll(): Unit = Await.result(
    db.run(DBIO.seq(
      (rows.schema ++ notes.schema).create,
      rows ++= data,
      notes ++= noted,
    )),
    10.seconds,
  )

  override def afterAll(): Unit = db.close()

  private val queries: List[(String, ListQuery)] = List(
    "text, ignoring case"       -> ListQuery(name.contains("BOOK")),
    "a literal percent sign"    -> ListQuery(name.contains("%")),
    "a literal underscore"      -> ListQuery(name.contains("_")),
    "excluded text"             -> ListQuery(!name.contains("book")),
    "a comparison skips NULLs"  -> ListQuery(rating >= 45L),
    "a negation keeps NULLs"    -> ListQuery(!(rating >= 50L)),
    "an inequality skips NULLs" -> ListQuery(rating.isNot(45L)),
    "absence"                   -> ListQuery(rating.missing),
    "one of several"            -> ListQuery(rating.oneOf(45L, 90L)),
    "one of none"               -> ListQuery(rating.oneOf()),
    "real numbers"              -> ListQuery(score > 4.5 && score <= 9.0),
    "truth values"              -> ListQuery(inPrint.is(false) || rating > 80L),
    "nothing"                   -> ListQuery(Filter.never),
    "NULLs last, descending"    ->
      ListQuery(order = List(rating.descending, name.ascending)),
    "NULLs last, ascending" ->
      ListQuery(order = List(rating.ascending, name.descending)),
    "ties kept in stored order" -> ListQuery(order = List(score.descending)),
    "a window, and the total"   -> ListQuery(
      inPrint.is(true),
      List(name.ascending),
      Some(Page(1, 2)),
    ),
    "a window past the end"         -> ListQuery(page = Some(Page(4, 10))),
    "a value of a convertible kind" -> ListQuery(Filter.Compare(
      "rating",
      Comparison.Eq,
      Value.Real(45.0),
    )),
    "a cell typed into a header" -> ListQuery(
      CellFilter
        .parse("rating", Kind.Whole, "40..80 | ?")
        .getOrElse(Filter.never),
    ),
  )

  queries.foreach((label, query) =>
    test(s"the database agrees with memory: $label"):
      val expected = schema.run(query, data).fold(fail(_), identity)
      val action   =
        lists.run(rows.sortBy(_.id), columns, query).fold(fail(_), identity)
      db.run(action).map(assertEquals(_, expected)),
  )

  test(
    "a query naming an unknown column is refused before reaching the database",
  ):
    assert(
      lists
        .run(
          rows,
          columns,
          ListQuery(Filter.Missing("author")),
        )
        .isLeft,
    )
    assert(
      lists
        .run(
          rows,
          columns,
          ListQuery(Filter.Contains("rating", "4")),
        )
        .isLeft,
    )

  test("the base query restricts the list before filtering and counting"):
    val action = lists
      .run(
        rows.filter(_.inPrint).sortBy(_.id),
        columns,
        ListQuery(rating > 50L),
      )
      .fold(fail(_), identity)
    db.run(action)
      .map(paged => assertEquals(paged.items.map(_.id), List(1L, 4L)))

  test("columns must store exactly the fields of the schema"):
    intercept[IllegalArgumentException](
      lists.columns(schema)(lists.text[Rows]("name")(_.name.?)),
    )
    intercept[IllegalArgumentException](lists.columns(schema)(
      lists.text[Rows]("name")(_.name.?),
      lists.real[Rows]("rating")(_.score.?),
      lists.real[Rows]("score")(_.score.?),
      lists.flag[Rows]("inPrint")(_.inPrint.?),
    ))

  test("a short list is sent whole, in stored order, whatever was asked"):
    val short  = SqlLists(H2Profile, wholeUpTo = data.size)
    val stored = short.columns(schema)(
      short.text[Rows]("name")(_.name.?),
      short.whole[Rows]("rating")(_.rating),
      short.real[Rows]("score")(_.score.?),
      short.flag[Rows]("inPrint")(_.inPrint.?),
    )
    val query  = ListQuery(rating > 50L, page = Some(Page(0, 1)))
    val action = short
      .answer(rows.sortBy(_.id), stored, query)
      .fold(fail(_), identity)
    db.run(action)
      .map: reply =>
        assertEquals(reply, ListReply.Whole(data))
        assertEquals(
          reply.answer(schema, query),
          schema.run(query, data),
        )

  test("a long list is sent only the window asked for, capped"):
    val long   = SqlLists(H2Profile, wholeUpTo = 1, maxWindow = 2)
    val stored = long.columns(schema)(
      long.text[Rows]("name")(_.name.?),
      long.whole[Rows]("rating")(_.rating),
      long.real[Rows]("score")(_.score.?),
      long.flag[Rows]("inPrint")(_.inPrint.?),
    )
    val action = long
      .answer(rows.sortBy(_.id), stored, ListQuery())
      .fold(fail(_), identity)
    db.run(action)
      .map(reply =>
        assertEquals(
          reply,
          ListReply.Window(Paged(
            data.take(2),
            data.size,
            Some(Page(0, 2)),
          )),
        ),
      )

  test("a window narrowed by the server says which window it is"):
    val long   = SqlLists(H2Profile, wholeUpTo = 1, maxWindow = 2)
    val stored = long.columns(schema)(
      long.text[Rows]("name")(_.name.?),
      long.whole[Rows]("rating")(_.rating),
      long.real[Rows]("score")(_.score.?),
      long.flag[Rows]("inPrint")(_.inPrint.?),
    )
    val action = long
      .answer(
        rows.sortBy(_.id),
        stored,
        ListQuery(page = Some(Page(1, 500))),
      )
      .fold(fail(_), identity)
    db.run(action)
      .map(reply =>
        assertEquals(
          reply,
          ListReply.Window(Paged(
            data.slice(1, 3),
            data.size,
            Some(Page(1, 2)),
          )),
        ),
      )

  test("the length of a list is decided from a bounded read"):
    val sql = SqlLists(H2Profile, wholeUpTo = 200)
      .probe(rows.sortBy(_.id))
      .result
      .statements
      .head
    assert(sql.contains("limit 201"), sql)

  test("a list one row too long to be sent whole is paged instead"):
    val long   = SqlLists(H2Profile, wholeUpTo = data.size - 1)
    val stored = long.columns(schema)(
      long.text[Rows]("name")(_.name.?),
      long.whole[Rows]("rating")(_.rating),
      long.real[Rows]("score")(_.score.?),
      long.flag[Rows]("inPrint")(_.inPrint.?),
    )
    val action = long
      .answer(rows.sortBy(_.id), stored, ListQuery())
      .fold(fail(_), identity)
    db.run(action)
      .map(assertEquals(
        _,
        ListReply.Window(Paged(data, data.size, Some(Page(0, 100)))),
      ))

  test("a list drawn from a left join is queried by the joined columns"):
    val name   = Field.of[(Row, Option[Note])]("name", _._1.name)
    val mark   = Field.of[(Row, Option[Note])]("mark", _._2.map(_.mark))
    val joined = Schema(name, mark)
    val stored = lists.columns(joined)(
      lists.text[(Rows, Rep[Option[Notes]])]("name")((row, _) => row.name.?),
      lists.whole[(Rows, Rep[Option[Notes]])]("mark")((_, note) =>
        note.map(_.mark),
      ),
    )
    val base = rows
      .joinLeft(notes)
      .on(_.id === _.rowId)
      .sortBy((row, _) => row.id)
    val items = data.map(row => (row, noted.find(_.rowId == row.id)))
    val query = ListQuery(
      !(mark < 2L),
      List(mark.ascending, name.descending),
    )
    val expected = joined.run(query, items).fold(fail(_), identity)
    val action   = lists.run(base, stored, query).fold(fail(_), identity)
    db.run(action).map(assertEquals(_, expected))
