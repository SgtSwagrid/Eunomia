package com.alecdorrington.eunomia
package client

import com.alecdorrington.eunomia.client.Visibility.visibleWhen
import com.alecdorrington.eunomia.model.{Field, Kind, Order, Page, Value}
import com.raquo.laminar.api.L.*

/**
  * A table over a [[ListState]]: a heading per column, which orders the list by
  * the column's field when clicked; a header cell per column, which reads a
  * filter in [[com.alecdorrington.eunomia.model.CellFilter]] syntax; a row per
  * item; and, while the list is paged, a pager.
  *
  * Unstyled: the host styles it through these classes:
  *   - `list-table`, the table, inside `list-table-frame`, which also holds the
  *     pager and the notes;
  *   - `list-sort`, a heading's ordering button, with `list-sorted-asc` or
  *     `list-sorted-desc` while the list is ordered by its field;
  *   - `list-filter`, a header cell's input, with `list-filter-invalid` while
  *     its text cannot be read;
  *   - `list-row`, each row, with `list-row-selected` on the selected one;
  *   - `list-empty` and `list-problem`, the notes shown for an empty list and a
  *     query that could not be run; and `list-pager`, `list-page`,
  *     `list-range`.
  */
object Table:

  /**
    * A column of a table.
    *
    * @tparam X
    *   The type of the items.
    *
    * @param label
    *   The heading.
    *
    * @param render
    *   The contents of this column's cell in one row, given that row's item.
    *
    * @param field
    *   The name of the field this column shows, if the list may be ordered and
    *   filtered by it.
    *
    * @param filterable
    *   Whether a filter on the field may be typed into the header cell.
    */
  final case class Column[X]
    (
      label: String,
      render: Signal[X] => Modifier[HtmlElement],
      field: Option[String] = None,
      filterable: Boolean = true,
    )

  object Column:

    /**
      * Creates a column showing one field's value as plain text, ordered and
      * filtered by it.
      *
      * @tparam X
      *   The type of the items.
      *
      * @param label
      *   The heading.
      *
      * @param field
      *   The field shown.
      *
      * @return
      *   A column of the field.
      */
    def of[X](label: String, field: Field[X, ?]): Column[X] =
      formatted(label, field)(item => field.valueOf(item).fold("")(plain))

    /**
      * Creates a column showing each item as text, ordered and filtered by one
      * field.
      *
      * @tparam X
      *   The type of the items.
      *
      * @param label
      *   The heading.
      *
      * @param field
      *   The field ordered and filtered by.
      *
      * @param format
      *   The text shown for one item.
      *
      * @return
      *   A column of the field.
      */
    def formatted[X]
      (label: String, field: Field[X, ?])
      (format: X => String)
      : Column[X] = Column(
      label,
      item => text <-- item.map(format),
      Some(field.name),
    )

    private def plain(value: Value): String = value match
      case Value.Text(text)      => text
      case Value.Integer(number) => number.toString
      case Value.Real(number)    => number.toString
      case Value.Flag(flag)      => if flag then "✓" else ""

  /**
    * Renders a table over a list.
    *
    * @tparam X
    *   The type of the items.
    *
    * @tparam K
    *   The type of the items' keys.
    *
    * @param list
    *   The list.
    *
    * @param columns
    *   The columns, in reading order.
    *
    * @param key
    *   The function identifying each item, so that a row survives its item
    *   changing.
    *
    * @param select
    *   The response to a click on a row, or `None` if rows cannot be selected.
    *
    * @param selected
    *   The key of the selected item, if any.
    *
    * @param emptyNote
    *   The note shown in place of the rows while none match.
    *
    * @return
    *   An element holding the table, its notes and its pager.
    */
  def apply[X, K]
    (
      list: ListState[X],
      columns: List[Column[X]],
      key: X => K,
      select: Option[K => Unit] = None,
      selected: Signal[Option[K]] = Val(None),
      emptyNote: String = "Nothing to show.",
    )
    : HtmlElement = div(
    cls := "list-table-frame",
    table(
      cls := "list-table",
      thead(
        tr(columns.map(heading(list, _))),
        Option.when(columns.exists(filterField(_).isDefined))(tr(
          columns.map(filterCell(list, _)),
        )),
      ),
      tbody(
        children <--
          list
            .items
            .split(key)((id, _, item) =>
              row(columns, id, item, select, selected),
            ),
      ),
    ),
    p(
      emptyNote,
      cls := "list-empty",
      visibleWhen(list.items.map(_.isEmpty)),
    ),
    child.maybe <-- list.problem.map(_.map(p(_, cls := "list-problem"))),
    pager(list),
  )

  private def heading[X](list: ListState[X], column: Column[X]): HtmlElement =
    th(
      column.field match
        case None        => span(column.label)
        case Some(field) => button(
            column.label,
            cls := "list-sort",
            cls <-- list.orderOf(field).map(sortedClass),
            onClick --> (_ => list.toggleOrder(field)),
          ),
    )

  private def filterCell[X]
    (list: ListState[X], column: Column[X])
    : HtmlElement = th(filterField(column).map(filterInput(list, _)))

  private def filterInput[X](list: ListState[X], field: String): HtmlElement =
    val problem = list.cellProblem(field)
    input(
      typ         := "text",
      cls         := "list-filter",
      placeholder := list.kindOf(field).fold("")(hint),
      cls("list-filter-invalid") <-- problem.map(_.isDefined),
      title <-- problem.map(_.getOrElse("")),
      controlled(
        value <-- list.cell(field),
        onInput.mapToValue --> (list.typeInto(field, _)),
      ),
    )

  private def row[X, K]
    (
      columns: List[Column[X]],
      id: K,
      item: Signal[X],
      select: Option[K => Unit],
      selected: Signal[Option[K]],
    )
    : HtmlElement = tr(
    cls := "list-row",
    cls("list-row-selected") <-- selected.map(_.contains(id)),
    select.map(choose => onClick --> (_ => choose(id))),
    columns.map(column => td(column.render(item))),
  )

  private def pager[X](list: ListState[X]): HtmlElement =
    val shown = list.page.combineWith(list.total)
    div(
      cls := "list-pager",
      visibleWhen(list.page.map(_.isDefined)),
      turner(
        list,
        "‹",
        list.page.map(_.forall(_.offset == 0)),
      )(_.previous),
      span(
        cls := "list-range",
        text <-- shown.mapN(range),
      ),
      turner(list, "›", shown.mapN(atEnd))(_.next),
    )

  private def turner[X]
    (
      list: ListState[X],
      arrow: String,
      blocked: Signal[Boolean],
    )
    (turn: Page => Page)
    : HtmlElement = button(
    arrow,
    cls := "list-page",
    disabled <-- blocked,
    onClick.compose(_.sample(list.page)) -->
      (_.foreach(page => list.showPage(turn(page)))),
  )

  private def filterField[X](column: Column[X]): Option[String] = column
    .field
    .filter(_ => column.filterable)

  private def sortedClass(key: Option[Order]): String = key.fold("")(key =>
    if key.descending then "list-sorted-desc" else "list-sorted-asc",
  )

  private def hint(kind: Kind): String = kind match
    case Kind.Text                => "Contains…"
    case Kind.Integer | Kind.Real => ">50, 10..20"
    case Kind.Flag                => "yes / no"

  private[client] def range(page: Option[Page], total: Int): String = page match
    case Some(shown) if total > 0 =>
      s"${ shown.offset + 1 }–${ last(shown, total) } of $total"
    case _ => total.toString

  /** Never sums the offset and limit, which may overflow. */
  private[client] def atEnd(page: Option[Page], total: Int): Boolean = page
    .forall(shown => shown.limit >= total - shown.offset)

  private def last(shown: Page, total: Int): Int = shown.offset +
    (total - shown.offset).min(shown.limit)
