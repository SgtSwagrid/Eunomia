package com.alecdorrington.eunomia
package server

import com.alecdorrington.eunomia.model.{
  Comparison, Filter, Kind, ListQuery, ListReply, Order, Paged, Schema, Value,
}
import java.util.Locale
import scala.concurrent.ExecutionContext
import slick.ast.{BaseTypedType, Ordering as SqlOrdering}
import slick.jdbc.JdbcProfile
import slick.lifted.{ColumnOrdered, Ordered}

/**
  * Answers [[ListQuery]]s over the rows of a database table. A short list is
  * sent whole, for the browser to query as it pleases without asking again; a
  * long one is filtered, ordered and paged in SQL, so that only the window
  * asked for is ever loaded. Which applies is decided here, per request, so no
  * caller need know.
  *
  * The results agree with those of a [[Schema]] over the same items: absent
  * values (`NULL`s) fail every comparison and come last in either direction,
  * and text is sought ignoring case.
  *
  * {{{
  * val lists   = SqlLists(H2Profile)
  * val columns = lists.columns(Book.schema)(
  *   lists.text[Books]("name")(_.name.?),
  *   lists.whole[Books]("rating")(_.rating), // Already optional.
  * )
  * lists.answer(books.filter(_.owner === user).sortBy(_.id), columns, query)
  * }}}
  *
  * @param profile
  *   The Slick profile of the host application's database.
  *
  * @param wholeUpTo
  *   The most rows a list may have to be sent whole.
  *
  * @param maxWindow
  *   The most rows sent in any one window of a longer list, however many are
  *   asked for.
  */
final class SqlLists
  (
    val profile: JdbcProfile,
    wholeUpTo: Int = 200,
    maxWindow: Int = 100,
  ):

  import profile.api.*

  /** The context the actions' combinators run on: inline, as each is trivial. */
  private given ExecutionContext = ExecutionContext.parasitic

  /**
    * One column of a table that a list may be filtered and ordered by, under
    * the name of the field it stores.
    *
    * @tparam E
    *   The type of the table's rows, as Slick sees them.
    */
  sealed abstract class Column[-E](val name: String, val kind: Kind):

    /** Whether the column compares with the given value as stated. */
    def compare
      (
        row: E,
        comparison: Comparison,
        value: Value,
      )
      : Rep[Boolean]

    /** Whether the column equals any one of the given values. */
    def oneOf(row: E, values: List[Value]): Rep[Boolean]

    /** Whether the column is `NULL`. */
    def missing(row: E): Rep[Boolean]

    /** Whether the column contains the given text, ignoring case. */
    def contains(row: E, text: String): Rep[Boolean] = LiteralColumn(false)

    /** Orders by the column, with `NULL`s last. */
    def ordered(row: E, descending: Boolean): Ordered

  /**
    * The columns storing every field of one list, and nothing else. Obtained
    * only from [[columns]], which checks them against the list's schema.
    */
  final class Columns[E] private[SqlLists] (
    private[SqlLists] val byName: Map[String, Column[E]],
    private[SqlLists] val kinds: Map[String, Kind],
  )

  /**
    * The columns storing the fields of a list, checked against its schema: each
    * field must be stored by exactly one column of the same name and kind. A
    * mismatch is a programming error, so it is raised at once, when the server
    * starts, rather than when some request first names the field.
    *
    * @param schema
    *   The fields of the list, as shared with the client.
    */
  def columns[E](schema: Schema[?])(stored: Column[E]*): Columns[E] =
    val byName = stored.map(column => column.name -> column).toMap
    val kinds  = byName.view.mapValues(_.kind).toMap
    require(
      byName.size == stored.size && kinds == schema.kinds,
      s"Columns $kinds do not store exactly the fields ${ schema.kinds }.",
    )
    new Columns(byName, kinds)

  /** A column of text, read as nullable so that any text column fits. */
  def text[E](name: String)(rep: E => Rep[Option[String]]): Column[E] =
    TextColumn(name, rep)

  /** A column of whole numbers, read as nullable. */
  def whole[E](name: String)(rep: E => Rep[Option[Long]]): Column[E] = Typed(
    name,
    Kind.Whole,
    rep,
    { case Value.Whole(number) => number },
  )

  /** A column of real numbers, read as nullable. */
  def real[E](name: String)(rep: E => Rep[Option[Double]]): Column[E] = Typed(
    name,
    Kind.Real,
    rep,
    { case Value.Real(number) => number },
  )

  /** A column of truth values, read as nullable. */
  def flag[E](name: String)(rep: E => Rep[Option[Boolean]]): Column[E] = Typed(
    name,
    Kind.Flag,
    rep,
    { case Value.Flag(flag) => flag },
  )

  /**
    * Answers a query over the rows of the given base query, which is where the
    * host application restricts a list to what its user may see.
    *
    * @param rows
    *   The rows the list is drawn from, in their stored order. Order them, or
    *   the order of the rows sent, and so of every window, is undefined.
    *
    * @param columns
    *   The columns storing the list's fields.
    *
    * @param query
    *   The query to answer.
    *
    * @return
    *   An action loading the reply, or why the query cannot be answered.
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
    query.limited(maxWindow),
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
    * The start of a list, one row longer than a list may be to be sent whole,
    * which is all it takes to tell which of the two it is. A short list is then
    * already loaded, and a long one has cost a bounded read rather than a count
    * of every row its user may see.
    *
    * @param rows
    *   The rows the list is drawn from, in their stored order.
    */
  private[server] def probe[E, U](rows: Query[E, U, Seq]): Query[E, U, Seq] =
    rows.take(wholeUpTo + 1)

  /**
    * Runs a query in the database, whatever the length of the list, loading the
    * window of matching rows together with the number matching in all.
    */
  private[server] def run[E, U]
    (
      rows: Query[E, U, Seq],
      columns: Columns[E],
      query: ListQuery,
    )
    : Either[String, DBIO[Paged[U]]] = query
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
    : DBIO[Paged[U]] =
    val ordered = sorted(matching, columns, query.order)
    val window  = query
      .page
      .fold(ordered)(page => ordered.drop(page.offset).take(page.limit))
    window
      .result
      .zip(matching.length.result)
      .map((items, total) => Paged(items.toList, total, query.page))

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
    compare = (field, comparison, value) =>
      columns(field).compare(row, comparison, value),
    contains = (field, text) => columns(field).contains(row, text),
    oneOf = (field, values) => columns(field).oneOf(row, values),
    missing = field => columns(field).missing(row),
  )

  private def sorted[E, U]
    (
      rows: Query[E, U, Seq],
      columns: Map[String, Column[E]],
      keys: List[Order],
    )
    : Query[E, U, Seq] =
    if keys.isEmpty then rows
    else
      rows.sortBy(row =>
        new Ordered(
          keys
            .map(key => columns(key.field).ordered(row, key.descending).columns)
            .reduce(_ ++ _),
        ),
      )(using identity)

  /**
    * A column of some type `B` that the database compares natively.
    *
    * @param literal
    *   Reads a checked value, which is always of this column's kind, as a `B`.
    */
  private class Typed[E, B : BaseTypedType]
    (
      name: String,
      kind: Kind,
      rep: E => Rep[Option[B]],
      literal: PartialFunction[Value, B],
    )
    extends Column[E](name, kind):

    override def compare
      (
        row: E,
        comparison: Comparison,
        value: Value,
      )
      : Rep[Boolean] = literal
      .lift(value)
      .fold(LiteralColumn(false): Rep[Boolean])(bound =>
        compared(
          rep(row),
          comparison,
          LiteralColumn(bound).bind,
        ).getOrElse(false),
      )

    override def oneOf(row: E, values: List[Value]): Rep[Boolean] =
      val bounds = values.flatMap(literal.lift)
      if bounds.isEmpty then LiteralColumn(false)
      else rep(row).inSetBind(bounds).getOrElse(false)

    override def missing(row: E): Rep[Boolean] = rep(row).isEmpty

    override def ordered(row: E, descending: Boolean): Ordered = ColumnOrdered(
      rep(row),
      SqlOrdering(
        if descending then SqlOrdering.Desc else SqlOrdering.Asc,
        SqlOrdering.NullsLast,
      ),
    )

    private def compared
      (
        column: Rep[Option[B]],
        comparison: Comparison,
        bound: Rep[B],
      )
      : Rep[Option[Boolean]] = comparison match
      case Comparison.Eq => column === bound
      case Comparison.Ne => column =!= bound
      case Comparison.Lt => column < bound
      case Comparison.Le => column <= bound
      case Comparison.Gt => column > bound
      case Comparison.Ge => column >= bound

  /** A column of text, which alone may be searched within. */
  private final class TextColumn[E]
    (
      name: String,
      rep: E => Rep[Option[String]],
    )
    extends Typed[E, String](
      name,
      Kind.Text,
      rep,
      { case Value.Text(text) => text },
    ):

    override def contains(row: E, text: String): Rep[Boolean] = rep(row)
      .toLowerCase
      .like(
        LiteralColumn(SqlLists.pattern(text)).bind,
        SqlLists.escape,
      )
      .getOrElse(false)

object SqlLists:

  /** The character that makes the next in a `LIKE` pattern literal. */
  private val escape = '\\'

  /**
    * The `LIKE` pattern matching text which contains the given text, folded to
    * lower case by a fixed locale. The column it is matched against is folded
    * by the database instead, whose own rules apply there.
    */
  private def pattern(text: String): String =
    val literal = text
      .toLowerCase(Locale.ROOT)
      .flatMap:
        case special @ ('%' | '_' | '\\') => s"$escape$special"
        case plain                        => plain.toString
    s"%$literal%"
