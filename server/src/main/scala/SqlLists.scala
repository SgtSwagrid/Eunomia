package com.alecdorrington.eunomia
package server

import com.alecdorrington.eunomia.model.{
  Filter, Kind, ListQuery, ListReply, ListSchema, Operator, Order, Value, Window,
}
import java.util.Locale
import scala.concurrent.ExecutionContext
import slick.ast.{BaseTypedType, Ordering as SqlOrdering}
import slick.jdbc.JdbcProfile
import slick.lifted.{ColumnOrdered, Ordered}

/**
  * Answers [[ListQuery]]s over the rows of a database table. A short list is
  * sent whole, for the browser to query without asking again; a long one is
  * filtered, ordered and paged in SQL, so that only the page asked for is
  * loaded.
  *
  * The results agree with those of a [[ListSchema]] over the same items:
  * `NULL`s fail every comparison and come last in either direction, and text is
  * sought ignoring case.
  *
  * {{{
  * val lists   = SqlLists(H2Profile)
  * val columns = lists.columns(Book.schema)(
  *   lists.text[Books]("name")(_.name.?),
  *   lists.integer[Books]("rating")(_.rating), // Already optional.
  * )
  * lists.answer(books.filter(_.owner === user).sortBy(_.id), columns, query)
  * }}}
  *
  * @param profile
  *   The Slick profile of the host's database.
  *
  * @param wholeUpTo
  *   The greatest number of rows a list may have to be sent whole.
  *
  * @param maxWindow
  *   The greatest number of rows sent in one window of a longer list, however
  *   many are asked for.
  */
final class SqlLists
  (
    val profile: JdbcProfile,
    wholeUpTo: Int = 200,
    maxWindow: Int = 100,
  ):

  import profile.api.*

  /** Parasitic, as every combinator run on it is trivial. */
  private given ExecutionContext = ExecutionContext.parasitic

  /**
    * A column of a table that a list may be filtered and ordered by, under the
    * name of the field it stores.
    *
    * @tparam E
    *   The type of the table's rows, as Slick sees them.
    *
    * @param name
    *   The name of the field the column stores.
    *
    * @param kind
    *   The kind of the field the column stores.
    */
  sealed abstract class Column[-E](val name: String, val kind: Kind):

    /**
      * Compares the column with a value.
      *
      * @param row
      *   The row.
      *
      * @param operator
      *   The operator comparing the column with `value`.
      *
      * @param value
      *   The value compared with.
      *
      * @return
      *   A condition holding where the operator does, never where the column is
      *   `NULL`.
      */
    def compare(row: E, operator: Operator, value: Value): Rep[Boolean]

    /**
      * Tests the column against several values.
      *
      * @param row
      *   The row.
      *
      * @param values
      *   The values the column may equal.
      *
      * @return
      *   A condition holding where the column equals any one of `values`.
      */
    def oneOf(row: E, values: List[Value]): Rep[Boolean]

    /**
      * Tests the column for `NULL`.
      *
      * @param row
      *   The row.
      *
      * @return
      *   A condition holding where the column is `NULL`.
      */
    def missing(row: E): Rep[Boolean]

    /**
      * Searches the column for text, ignoring case.
      *
      * @param row
      *   The row.
      *
      * @param text
      *   The text sought.
      *
      * @return
      *   A condition holding where the column contains `text`, never for a
      *   column that is not text.
      */
    def contains(row: E, text: String): Rep[Boolean] = LiteralColumn(false)

    /**
      * Orders by the column, with `NULL`s last.
      *
      * @param row
      *   The row.
      *
      * @param descending
      *   Whether the greatest values come first.
      *
      * @return
      *   An ordering of rows by the column.
      */
    def ordered(row: E, descending: Boolean): Ordered

  /**
    * The columns storing every field of one list, and nothing else, as checked
    * by [[columns]], their only source.
    *
    * @tparam E
    *   The type of the table's rows, as Slick sees them.
    */
  final class Columns[E] private[SqlLists] (
    private[SqlLists] val byName: Map[String, Column[E]],
    private[SqlLists] val kinds: Map[String, Kind],
  )

  /**
    * Checks the columns storing the fields of a list against its schema. Throws
    * an `IllegalArgumentException` unless each field is stored by exactly one
    * column of the same name and kind, so call it at startup.
    *
    * @tparam E
    *   The type of the table's rows, as Slick sees them.
    *
    * @param schema
    *   The fields of the list, as shared with the client.
    *
    * @param stored
    *   The columns storing the fields.
    *
    * @return
    *   The checked columns.
    */
  def columns[E](schema: ListSchema[?])(stored: Column[E]*): Columns[E] =
    val byName = stored.map(column => column.name -> column).toMap
    val kinds  = byName.view.mapValues(_.kind).toMap
    require(
      byName.size == stored.size && kinds == schema.kinds,
      s"Columns $kinds do not store exactly the fields ${ schema.kinds }.",
    )
    new Columns(byName, kinds)

  /**
    * Creates a column of text. It is read as nullable, so that any text column
    * fits; use `.?` on one that is not.
    *
    * @tparam E
    *   The type of the table's rows, as Slick sees them.
    *
    * @param name
    *   The name of the field the column stores.
    *
    * @param column
    *   The function selecting the column from a row.
    *
    * @return
    *   A column of [[Kind.Text]].
    */
  def text[E](name: String)(column: E => Rep[Option[String]]): Column[E] =
    TextColumn(name, column)

  /**
    * Creates a column of integers, read as nullable.
    *
    * @tparam E
    *   The type of the table's rows, as Slick sees them.
    *
    * @param name
    *   The name of the field the column stores.
    *
    * @param column
    *   The function selecting the column from a row.
    *
    * @return
    *   A column of [[Kind.Integer]].
    */
  def integer[E](name: String)(column: E => Rep[Option[Long]]): Column[E] =
    TypedColumn(
      name,
      Kind.Integer,
      column,
      { case Value.Integer(number) => number },
    )

  /**
    * Creates a column of real numbers, read as nullable.
    *
    * @tparam E
    *   The type of the table's rows, as Slick sees them.
    *
    * @param name
    *   The name of the field the column stores.
    *
    * @param column
    *   The function selecting the column from a row.
    *
    * @return
    *   A column of [[Kind.Real]].
    */
  def real[E](name: String)(column: E => Rep[Option[Double]]): Column[E] =
    TypedColumn(
      name,
      Kind.Real,
      column,
      { case Value.Real(number) => number },
    )

  /**
    * Creates a column of truth values, read as nullable.
    *
    * @tparam E
    *   The type of the table's rows, as Slick sees them.
    *
    * @param name
    *   The name of the field the column stores.
    *
    * @param column
    *   The function selecting the column from a row.
    *
    * @return
    *   A column of [[Kind.Flag]].
    */
  def flag[E](name: String)(column: E => Rep[Option[Boolean]]): Column[E] =
    TypedColumn(
      name,
      Kind.Flag,
      column,
      { case Value.Flag(flag) => flag },
    )

  /**
    * Answers a query over the rows of a base query, which is where the host
    * restricts a list to what its user may see.
    *
    * @tparam E
    *   The type of the table's rows, as Slick sees them.
    *
    * @tparam U
    *   The type of the rows as loaded.
    *
    * @param rows
    *   The rows the list is drawn from, in stored order. Without an order, the
    *   order of every window is undefined.
    *
    * @param columns
    *   The columns storing the list's fields.
    *
    * @param query
    *   The query to answer.
    *
    * @return
    *   Either a message saying why the query cannot be answered, or an action
    *   loading the reply.
    */
  def answer[E, U]
    (
      rows: Query[E, U, Seq],
      columns: Columns[E],
      query: ListQuery,
    )
    : Either[String, DBIO[ListReply[U]]] = run(
    rows,
    columns,
    query.capped(maxWindow),
  ).map(window =>
    probe(rows)
      .result
      .flatMap(head =>
        if head.sizeIs <= wholeUpTo then
          DBIO.successful(ListReply.Whole(head.toList))
        else window.map(ListReply.Window(_)),
      ),
  )

  /**
    * One row more than a list sent whole may have: enough to tell whether it is
    * short, without counting every row.
    */
  private[server] def probe[E, U](rows: Query[E, U, Seq]): Query[E, U, Seq] =
    rows.take(wholeUpTo + 1)

  /** Runs a query in SQL, whatever the length of the list. */
  private[server] def run[E, U]
    (
      rows: Query[E, U, Seq],
      columns: Columns[E],
      query: ListQuery,
    )
    : Either[String, DBIO[Window[U]]] = query
    .checked(columns.kinds)
    .map(checked =>
      fetch(
        rows.filter(holds(columns.byName, checked.filter)),
        columns.byName,
        checked,
      ),
    )

  private def fetch[E, U]
    (
      matching: Query[E, U, Seq],
      columns: Map[String, Column[E]],
      query: ListQuery,
    )
    : DBIO[Window[U]] =
    val ordered = orderedBy(matching, columns, query.order)
    // Counted first, so that a page past the end moves back as it does in
    // memory (see `Page.within`).
    matching
      .length
      .result
      .flatMap: total =>
        val page = query.page.map(_.within(total))
        page
          .fold(ordered)(shown => ordered.drop(shown.offset).take(shown.limit))
          .result
          .map(items => Window(items.toList, total, page))

  private def holds[E]
    (
      columns: Map[String, Column[E]],
      filter: Filter,
    )
    (row: E)
    : Rep[Boolean] = filter.fold[Rep[Boolean]](
    not = !_,
    all = _.reduceOption(_ && _).getOrElse(LiteralColumn(true)),
    any = _.reduceOption(_ || _).getOrElse(LiteralColumn(false)),
    compare =
      (field, operator, value) => columns(field).compare(row, operator, value),
    contains = (field, text) => columns(field).contains(row, text),
    oneOf = (field, values) => columns(field).oneOf(row, values),
    missing = field => columns(field).missing(row),
  )

  private def orderedBy[E, U]
    (
      rows: Query[E, U, Seq],
      columns: Map[String, Column[E]],
      keys: List[Order],
    )
    : Query[E, U, Seq] =
    if keys.isEmpty then rows
    else
      rows.sortBy(row =>
        Ordered(
          keys
            .toVector
            .flatMap(key =>
              columns(key.field).ordered(row, key.descending).columns,
            ),
        ),
      )(using identity)

  private class TypedColumn[E, B : BaseTypedType]
    (
      name: String,
      kind: Kind,
      column: E => Rep[Option[B]],
      literal: PartialFunction[Value, B],
    )
    extends Column[E](name, kind):

    override def compare
      (row: E, operator: Operator, value: Value)
      : Rep[Boolean] = literal
      .lift(value)
      .fold(LiteralColumn(false): Rep[Boolean])(bound =>
        compared(
          column(row),
          operator,
          LiteralColumn(bound).bind,
        ).getOrElse(false),
      )

    override def oneOf(row: E, values: List[Value]): Rep[Boolean] =
      val bounds = values.flatMap(literal.lift)
      if bounds.isEmpty then LiteralColumn(false)
      else column(row).inSetBind(bounds).getOrElse(false)

    override def missing(row: E): Rep[Boolean] = column(row).isEmpty

    override def ordered(row: E, descending: Boolean): Ordered = ColumnOrdered(
      column(row),
      SqlOrdering(
        if descending then SqlOrdering.Desc else SqlOrdering.Asc,
        SqlOrdering.NullsLast,
      ),
    )

    private def compared
      (
        selected: Rep[Option[B]],
        operator: Operator,
        bound: Rep[B],
      )
      : Rep[Option[Boolean]] = operator match
      case Operator.Equal   => selected === bound
      case Operator.Unequal => selected =!= bound
      case Operator.Less    => selected < bound
      case Operator.AtMost  => selected <= bound
      case Operator.Greater => selected > bound
      case Operator.AtLeast => selected >= bound

  private final class TextColumn[E]
    (
      name: String,
      column: E => Rep[Option[String]],
    )
    extends TypedColumn[E, String](
      name,
      Kind.Text,
      column,
      { case Value.Text(text) => text },
    ):

    override def contains(row: E, text: String): Rep[Boolean] = column(row)
      .toLowerCase
      .like(
        LiteralColumn(SqlLists.pattern(text)).bind,
        SqlLists.escape,
      )
      .getOrElse(false)

object SqlLists:

  private val escape: Char = '\\'

  private val special: Set[Char] = Set('%', '_', escape)

  /**
    * Folds the text to lower case by a fixed locale; the column it is matched
    * against is folded by the database's own rules.
    */
  private def pattern(text: String): String =
    val literal = text
      .toLowerCase(Locale.ROOT)
      .flatMap(char => if special(char) then s"$escape$char" else char.toString)
    s"%$literal%"
