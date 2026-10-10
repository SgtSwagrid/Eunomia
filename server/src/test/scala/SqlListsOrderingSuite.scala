package com.alecdorrington.eunomia
package server

import munit.FunSuite
import slick.jdbc.{
  H2Profile, JdbcProfile, MySQLProfile, PostgresProfile, SQLServerProfile,
}

/**
  * MySQL and SQL Server have no `NULLS LAST`, so Slick adds a leading key of
  * its own. The clauses are matched in full to catch a profile that stops doing
  * so.
  */
class SqlListsOrderingSuite extends FunSuite:

  private final class Stored(val profile: JdbcProfile):

    import profile.api.*

    final class Ratings
      (tag: Tag)
      extends Table[(Long, Option[Long])](tag, "list_ratings"):

      def id = column[Long]("id")

      def rating = column[Option[Long]]("rating")

      override def * = (id, rating)

    private val rows   = TableQuery[Ratings]
    private val lists  = SqlLists(profile)
    private val rating = lists.integer[Ratings]("rating")(_.rating)

    def orderBy(descending: Boolean): String =
      val sql = rows
        .sortBy(row => rating.ordered(row, descending))(using identity)
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
      """order by "rating" nulls last""",
      """order by "rating" desc nulls last""",
    )
    check(
      PostgresProfile,
      """order by "rating" nulls last""",
      """order by "rating" desc nulls last""",
    )

  test("a profile without it sorts by absence first, descending aside"):
    check(
      MySQLProfile,
      "order by isnull(`rating`),`rating`",
      "order by `rating` desc",
    )
    check(
      SQLServerProfile,
      """order by case when ("rating") is null then 1 else 0 end,"rating"""",
      """order by "rating" desc""",
    )
