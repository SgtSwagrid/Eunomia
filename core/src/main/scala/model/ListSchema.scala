package com.alecdorrington.eunomia
package model

/**
  * The fields of the items in a list, for running [[ListQuery]]s in memory over
  * a list already at hand.
  *
  * @tparam X
  *   The type of the items.
  *
  * @param fields
  *   The fields a query may filter and order by, no two of one name.
  */
final class ListSchema[X](val fields: List[Field[X, ?]]):

  private val byName: Map[String, Field[X, ?]] = fields
    .map(field => field.name -> field)
    .toMap

  require(
    repeated.isEmpty,
    s"More than one field is named ${ repeated.mkString("`", "`, `", "`") }.",
  )

  /** The kind of each field, by name. */
  val kinds: Map[String, Kind] = byName.view.mapValues(_.kind).toMap

  /**
    * Runs a query over the given items.
    *
    * @param query
    *   The query to run.
    *
    * @param items
    *   The items, in stored order.
    *
    * @return
    *   Either a message saying why the query cannot be run, or the window of
    *   matching items.
    */
  def run(query: ListQuery, items: List[X]): Either[String, Window[X]] = query
    .checked(kinds)
    .map(evaluate(_, items))

  /**
    * Compiles an unchecked filter into a test of one item, in which a condition
    * on an unknown field never holds. Fields are looked up once, not per item.
    *
    * @param filter
    *   The filter.
    *
    * @return
    *   A function deciding whether an item satisfies the filter.
    */
  def matches(filter: Filter): X => Boolean = filter.fold[X => Boolean](
    not = test => item => !test(item),
    all = tests => item => tests.forall(_(item)),
    any = tests => item => tests.exists(_(item)),
    compare = (field, operator, value) =>
      holding(field)(actual =>
        operator.holds(Ordering[Value].compare(actual, value)),
      ),
    contains = (field, text) => holding(field)(contains(_, text)),
    oneOf = (field, values) =>
      holding(field)(actual => values.exists(Ordering[Value].equiv(actual, _))),
    missing = field => reader(field).andThen(_.isEmpty),
  )

  /**
    * Ignores case character by character, by no locale's rules, as browsers
    * have no locales without a library and the server must agree with them.
    */
  private def contains(value: Value, text: String): Boolean = value match
    case Value.Text(actual) => (0 to actual.length - text.length).exists(
        start => actual.regionMatches(true, start, text, 0, text.length),
      )
    case _ => false

  /**
    * Creates an ordering of items by the given keys, placing absent values
    * last.
    *
    * @param keys
    *   The keys, most significant first.
    *
    * @return
    *   An ordering of items.
    */
  def ordering(keys: List[Order]): Ordering[X] = keys
    .map(ordered)
    .reduceOption(_.orElse(_))
    .getOrElse(ListSchema.unordered[X])

  private def evaluate(query: ListQuery, items: List[X]): Window[X] =
    val matching = items
      .filter(matches(query.filter))
      .sorted(using ordering(query.order))
    val total = matching.size
    val page  = query.page.map(_.within(total))
    Window(
      page.fold(matching)(_.slice(matching)),
      total,
      page,
    )

  /** Places absent values last in either direction. */
  private def ordered(key: Order): Ordering[X] =
    val values =
      if key.descending then Ordering[Value].reverse else Ordering[Value]
    Ordering.by(reader(key.field))(using ListSchema.absentLast(values))

  private def holding(field: String)(test: Value => Boolean): X => Boolean =
    reader(field).andThen(_.exists(test))

  /** Finds nothing for an unknown field. */
  private def reader(field: String): X => Option[Value] = byName
    .get(field)
    .fold[X => Option[Value]](_ => None)(_.valueOf)

  private def repeated: List[String] = fields
    .groupBy(_.name)
    .collect { case (name, alike) if alike.sizeIs > 1 => name }
    .toList

object ListSchema:

  /**
    * Creates a schema of the given fields. Throws an `IllegalArgumentException`
    * if two share a name.
    *
    * @tparam X
    *   The type of the items.
    *
    * @param fields
    *   The fields, no two of one name.
    *
    * @return
    *   A schema of the fields.
    */
  def apply[X](fields: Field[X, ?]*): ListSchema[X] =
    new ListSchema(fields.toList)

  private def unordered[X]: Ordering[X] = (_, _) => 0

  private def absentLast(values: Ordering[Value]): Ordering[Option[Value]] =
    (left, right) =>
      (left, right) match
        case (Some(a), Some(b)) => values.compare(a, b)
        case _                  => left.isEmpty.compare(right.isEmpty)
