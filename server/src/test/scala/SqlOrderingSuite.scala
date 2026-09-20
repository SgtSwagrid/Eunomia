package com.alecdorrington.eunomia
package server

import munit.FunSuite
import slick.jdbc.{
  H2Profile, JdbcProfile, MySQLProfile, PostgresProfile, SQLServerProfile,
}

/**
  * Checks that a list orders absent values last whatever the profile, which
  * [[SqlListsSuite]] can only show for the one it runs against.
  *
  * Not every dialect has `NULLS LAST`: MySQL and SQL Server sort `NULL` first
  * when ascending, and Slick makes up the difference with a leading key of its
  * own. The clauses are matched in full, so that a profile which stops doing so
  * is caught here rather than by a list which quietly orders the wrong way.
  */
class SqlOrderingSuite extends FunSuite:

  /** One nullable column, as whichever profile is asked stores it. */
  private final class Stored(val profile: JdbcProfile):

    import profile.api.*

    final class Marks
      (tag: Tag)
      extends Table[(Long, Option[Long])](tag, "list_marks"):

      def id = column[Long]("id")

      def mark = column[Option[Long]]("mark")

      override def * = (id, mark)

    private val rows  = TableQuery[Marks]
    private val lists = SqlLists(profile)
    private val mark  = lists.whole[Marks]("mark")(_.mark)

    /** The `ORDER BY` clause of a list ordered by the nullable column. */
    def orderBy(descending: Boolean): String =
      val sql = rows
        .sortBy(row => mark.ordered(row, descending))(using identity)
        .result
        .statements
        .head
      sql.substring(sql.indexOf("order by"))

  private def check
    (
      profile: JdbcProfile,
      ascending: String,
      descending: String,
    )
    : Unit =
    val stored = Stored(profile)
    assertEquals(
      stored.orderBy(descending = false),
      ascending,
    )
    assertEquals(
      stored.orderBy(descending = true),
      descending,
    )

  test("a profile with NULLS LAST is simply asked for it"):
    check(
      H2Profile,
      """order by "mark" nulls last""",
      """order by "mark" desc nulls last""",
    )
    check(
      PostgresProfile,
      """order by "mark" nulls last""",
      """order by "mark" desc nulls last""",
    )

  test("a profile without it sorts by absence first, descending aside"):
    check(
      MySQLProfile,
      "order by isnull(`mark`),`mark`",
      "order by `mark` desc",
    )
    check(
      SQLServerProfile,
      """order by case when ("mark") is null then 1 else 0 end,"mark"""",
      """order by "mark" desc""",
    )
