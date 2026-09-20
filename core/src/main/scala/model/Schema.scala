package com.alecdorrington.eunomia
package model

import java.util.Locale

/**
  * The fields of the items in a list, for running [[ListQuery]]s over the items
  * in memory. Use it where the whole list is already at hand; where it is too
  * large to be, the same queries can be run in a database instead.
  *
  * @param fields
  *   The fields a query may filter and order by, no two of one name.
  */
final class Schema[X](val fields: List[Field[X, ?]]):

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
    * Runs a query over the given items, which are taken to be in stored order.
    *
    * @return
    *   The window of matching items, or why the query cannot be run.
    */
  def run(query: ListQuery, items: List[X]): Either[String, Paged[X]] = query
    .checked(kinds)
    .map(checked => evaluate(checked, items))

  /**
    * Whether one item satisfies a filter. Unchecked, so a comparison with an
    * unknown field simply never holds.
    */
  def matches(filter: Filter)(item: X): Boolean = filter.fold(
    not = !_,
    all = _.forall(identity),
    any = _.exists(identity),
    compare = (field, comparison, value) =>
      valueOf(field, item).exists(actual =>
        comparison.holds(Ordering[Value].compare(actual, value)),
      ),
    contains = (field, text) => valueOf(field, item).exists(within(_, text)),
    oneOf = (field, values) =>
      valueOf(field, item).exists(actual =>
        values.exists(Ordering[Value].equiv(actual, _)),
      ),
    missing = field => valueOf(field, item).isEmpty,
  )

  /** Whether a value is text holding the given text, ignoring case. */
  private def within(value: Value, text: String): Boolean = value match
    case Value.Text(actual) =>
      actual.toLowerCase(Locale.ROOT).contains(text.toLowerCase(Locale.ROOT))
    case _ => false

  /** Orders items by the given keys, placing absent values last. */
  def ordering(keys: List[Order]): Ordering[X] = (left, right) =>
    keys.iterator.map(compare(_, left, right)).find(_ != 0).getOrElse(0)

  private def evaluate(query: ListQuery, items: List[X]): Paged[X] =
    val matching = items
      .filter(matches(query.filter))
      .sorted(using ordering(query.order))
    Paged(
      query.page.fold(matching)(_.slice(matching)),
      matching.size,
      query.page,
    )

  private def compare(key: Order, left: X, right: X): Int =
    (valueOf(key.field, left), valueOf(key.field, right)) match
      case (Some(a), Some(b)) if key.descending => Ordering[Value].compare(b, a)
      case (Some(a), Some(b))                   => Ordering[Value].compare(a, b)
      case (a, b)                               => a.isEmpty.compare(b.isEmpty)

  private def valueOf(field: String, item: X): Option[Value] = byName
    .get(field)
    .flatMap(_.valueOf(item))

  /** The names given to more than one field, which a schema may not have. */
  private def repeated: List[String] = fields
    .groupBy(_.name)
    .collect { case (name, alike) if alike.sizeIs > 1 => name }
    .toList

object Schema:

  /** The schema made of the given fields. */
  def apply[X](fields: Field[X, ?]*): Schema[X] = new Schema(fields.toList)
