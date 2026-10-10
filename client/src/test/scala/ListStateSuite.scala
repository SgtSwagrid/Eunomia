package com.alecdorrington.eunomia
package client

import com.alecdorrington.eunomia.model.{
  Field, Filter, ListQuery, ListSchema, Order, Page,
}
import com.raquo.laminar.api.L.*
import munit.FunSuite

class ListStateSuite extends FunSuite:

  private final case class Book(name: String, rating: Option[Long])

  private val books = List(
    Book("Alpha", Some(72L)),
    Book("beta edition", Some(45L)),
    Book("Gamma", None),
  )

  private val name   = Field.of[Book]("name", _.name)
  private val rating = Field.of[Book]("rating", _.rating)
  private val schema = ListSchema(name, rating)

  private given owner: Owner = new Owner {}

  private def now[A](signal: Signal[A]): A = signal.observe.now()

  private def state
    (
      narrowing: Signal[Filter] = Val(Filter.always),
      initial: ListQuery = ListQuery(),
    )
    : ListState[Book] = ListState(
    schema,
    ListSource.items(Val(books)),
    narrowing,
    initial,
  )

  private def names(list: ListState[Book]): List[String] =
    now(list.items).map(_.name)

  test("a blank cell narrows nothing"):
    val list = state()
    assertEquals(now(list.filter), Filter.always)
    assertEquals(names(list), books.map(_.name))

  test("text typed into a cell filters by that field"):
    val list = state()
    list.typeInto("name", "a")
    assertEquals(now(list.filter), name.contains("a"))
    assertEquals(
      names(list),
      List("Alpha", "beta edition", "Gamma"),
    )
    list.typeInto("name", "edition")
    assertEquals(names(list), List("beta edition"))

  test("a cell which cannot be read narrows nothing, and says why"):
    val list = state()
    list.typeInto("rating", "abc")
    assertEquals(now(list.filter), Filter.always)
    assertEquals(names(list), books.map(_.name))
    assert(now(list.cellProblem("rating")).isDefined)
    assertEquals(now(list.cellProblem("name")), None)

  test("cells narrow the list together"):
    val list = state()
    list.typeInto("name", "a")
    list.typeInto("rating", ">50")
    assertEquals(names(list), List("Alpha"))

  test("the host's own filter is applied beneath what is typed"):
    val list = state(Val(rating.present))
    assertEquals(
      names(list),
      List("Alpha", "beta edition"),
    )
    list.typeInto("name", "gamma")
    assertEquals(names(list), List.empty)

  test("a heading cycles its field through ascending, descending and off"):
    val list = state()
    list.toggleOrder("rating")
    assertEquals(now(list.order), List(Order("rating")))
    assertEquals(
      names(list),
      List("beta edition", "Alpha", "Gamma"),
    )
    list.toggleOrder("rating")
    assertEquals(
      now(list.order),
      List(Order("rating", descending = true)),
    )
    assertEquals(
      names(list),
      List("Alpha", "beta edition", "Gamma"),
    )
    list.toggleOrder("rating")
    assertEquals(now(list.order), List.empty)
    assertEquals(names(list), books.map(_.name))

  test("the total counts every match, not only those shown"):
    val list = state(initial = ListQuery(page = Some(Page(0, 2))))
    assertEquals(
      names(list),
      List("Alpha", "beta edition"),
    )
    assertEquals(now(list.total), 3)

  test("typing or ordering returns to the first window in one change"):
    val list    = state(initial = ListQuery(page = Some(Page(2, 1))))
    val changes = list
      .query
      .changes
      .scanLeft(0)((count, _) => count + 1)
      .observe
    list.typeInto("name", "a")
    assertEquals(now(list.query).page, Some(Page(0, 1)))
    list.showPage(Page(1, 1))
    list.toggleOrder("rating")
    assertEquals(now(list.query).page, Some(Page(0, 1)))
    assertEquals(changes.now(), 3)

  test("a field the list has no column for is refused before typing"):
    assert(now(state().cellProblem("author")).isDefined)
    assertEquals(now(state().cellProblem("name")), None)

  test("a field the list has no column for is refused"):
    val list = state()
    list.typeInto("author", "x")
    assert(now(list.cellProblem("author")).isDefined)
    assertEquals(now(list.filter), Filter.always)
