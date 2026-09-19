package com.alecdorrington.eunomia
package model

import com.alecdorrington.eunomia.model.Filter.*

/**
  * The fields of the items in a list, for running [[ListQuery]]s over the items
  * in memory. Use it where the whole list is already at hand; where it is too
  * large to be, the same queries can be run in a database instead.
  *
  * @param fields
  *   The fields a query may filter and order by.
  */
final class Schema[X](val fields: List[Field[X, ?]]):

  /** The kind of each field, by name. */
  val kinds: Map[String, Kind] = fields
    .map(field => field.name -> field.kind)
    .toMap

  private val byName: Map[String, Field[X, ?]] = fields
    .map(field => field.name -> field)
    .toMap

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
  def matches(filter: Filter)(item: X): Boolean = filter match
    case Not(inner)                        => !matches(inner)(item)
    case And(inners)                       => inners.forall(matches(_)(item))
    case Or(inners)                        => inners.exists(matches(_)(item))
    case Compare(field, comparison, value) => valueOf(field, item).exists(
        actual => comparison.holds(Ordering[Value].compare(actual, value)),
      )
    case Contains(field, text) => valueOf(field, item).exists:
        case Value.Text(actual) => actual.toLowerCase.contains(text.toLowerCase)
        case _                  => false
    case OneOf(field, values) => valueOf(field, item).exists(actual =>
        values.exists(Ordering[Value].equiv(actual, _)),
      )
    case Missing(field) => valueOf(field, item).isEmpty

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
    )

  private def compare(key: Order, left: X, right: X): Int =
    (valueOf(key.field, left), valueOf(key.field, right)) match
      case (Some(a), Some(b)) if key.descending => Ordering[Value].compare(b, a)
      case (Some(a), Some(b))                   => Ordering[Value].compare(a, b)
      case (a, b)                               => a.isEmpty.compare(b.isEmpty)

  private def valueOf(field: String, item: X): Option[Value] = byName
    .get(field)
    .flatMap(_.valueOf(item))

object Schema:

  /** The schema made of the given fields. */
  def apply[X](fields: Field[X, ?]*): Schema[X] = new Schema(fields.toList)
