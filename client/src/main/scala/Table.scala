package com.alecdorrington.eunomia
package client

import com.alecdorrington.eunomia.client.Visibility.visibleWhen
import com.alecdorrington.eunomia.model.{Field, Kind, Order, Page, Value}
import com.raquo.laminar.api.L.*

/**
  * A table over a [[ListView]]: a heading per column, which orders the list by
  * the column's field when clicked; a header cell per column, into which a
  * filter on that field can be typed in
  * [[com.alecdorrington.eunomia.model.CellFilter]] syntax; a row per item; and,
  * while the list is paged, a pager.
  *
  * Unstyled: the host application styles it through these classes:
  *   - `list-table`, the table, inside `list-table-frame`, which also holds the
  *     pager and the notes;
  *   - `list-sort`, a heading's ordering button, with `list-sorted-asc` or
  *     `list-sorted-desc` while the list is ordered by its field;
  *   - `list-filter`, a header cell's input, with `list-filter-invalid` while
  *     its text cannot be read;
  *   - `item-row`, each row, with `item-selected` on the selected one;
  *   - `list-empty` and `list-problem`, the notes shown for an empty list and a
  *     query that could not be run; and `list-pager`, `list-page`,
  *     `list-range`.
  */
object Table:

  /**
    * One column of a table.
    *
    * @param label
    *   The heading.
    *
    * @param render
    *   The contents of this column's cell in one row, given that row's item.
    *
    * @param field
    *   The field this column shows, if the list may be ordered and filtered by
    *   it.
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
      * A column showing one field's value as plain text, ordered and filtered
      * by it.
      */
    def of[X](label: String, field: Field[X, ?]): Column[X] =
      shown(label, field)(item => field.valueOf(item).fold("")(plain))

    /**
      * A column showing one field as the given text, ordered and filtered by
      * it.
      */
    def shown[X]
      (label: String, field: Field[X, ?])
      (show: X => String)
      : Column[X] = Column(
      label,
      item => text <-- item.map(show),
      Some(field.name),
    )

    /** A field value as plain text. */
    private def plain(value: Value): String = value match
      case Value.Text(text)    => text
      case Value.Whole(number) => number.toString
      case Value.Real(number)  => number.toString
      case Value.Flag(flag)    => if flag then "✓" else ""

  /**
    * A table over a list.
    *
    * @param view
    *   The list.
    *
    * @param columns
    *   The columns, left to right.
    *
    * @param key
    *   Identifies each item, so that a row survives its item changing.
    *
    * @param select
    *   Responds to a click on a row, if rows may be selected.
    *
    * @param selected
    *   The key of the selected item, if any.
    *
    * @param empty
    *   Shown in place of the rows while none match.
    */
  def apply[X, K]
    (
      view: ListView[X],
      columns: List[Column[X]],
      key: X => K,
      select: Option[K => Unit] = None,
      selected: Signal[Option[K]] = Val(None),
      empty: String = "Nothing to show.",
    )
    : HtmlElement = div(
    cls := "list-table-frame",
    table(
      cls := "list-table",
      thead(
        tr(columns.map(heading(view, _))),
        Option.when(columns.exists(filterField(_).isDefined))(tr(
          columns.map(filterCell(view, _)),
        )),
      ),
      tbody(
        children <--
          view
            .items
            .split(key)((id, _, item) =>
              row(columns, id, item, select, selected),
            ),
      ),
    ),
    p(
      empty,
      cls := "list-empty",
      visibleWhen(view.items.map(_.isEmpty)),
    ),
    child.maybe <-- view.problem.map(_.map(p(_, cls := "list-problem"))),
    pager(view),
  )

  private def heading[X](view: ListView[X], column: Column[X]): HtmlElement =
    th(
      column.field match
        case None        => span(column.label)
        case Some(field) => button(
            column.label,
            cls := "list-sort",
            cls <-- view.orderOf(field).map(sortedClass),
            onClick --> (_ => view.toggleOrder(field)),
          ),
    )

  private def filterCell[X](view: ListView[X], column: Column[X]): HtmlElement =
    th(filterField(column).map(field =>
      input(
        typ         := "text",
        cls         := "list-filter",
        placeholder := view.kindOf(field).fold("")(hint),
        cls("list-filter-invalid") <-- view.cellProblem(field).map(_.isDefined),
        title <-- view.cellProblem(field).map(_.getOrElse("")),
        controlled(
          value <-- view.cell(field),
          onInput.mapToValue --> (view.typeInto(field, _)),
        ),
      ),
    ))

  private def row[X, K]
    (
      columns: List[Column[X]],
      id: K,
      item: Signal[X],
      select: Option[K => Unit],
      selected: Signal[Option[K]],
    )
    : HtmlElement = tr(
    cls := "item-row",
    cls("item-selected") <-- selected.map(_.contains(id)),
    select.map(choose => onClick --> (_ => choose(id))),
    columns.map(column => td(column.render(item))),
  )

  private def pager[X](view: ListView[X]): HtmlElement = div(
    cls := "list-pager",
    visibleWhen(view.page.map(_.isDefined)),
    button(
      "‹",
      cls := "list-page",
      disabled <-- view.page.map(_.forall(_.offset == 0)),
      onClick.compose(_.sample(view.page)) -->
        (_.foreach(window => view.showPage(window.previous))),
    ),
    span(
      cls := "list-range",
      text <-- view.page.combineWith(view.total).mapN(range),
    ),
    button(
      "›",
      cls := "list-page",
      disabled <-- view.page.combineWith(view.total).mapN(atEnd),
      onClick.compose(_.sample(view.page)) -->
        (_.foreach(window => view.showPage(window.next))),
    ),
  )

  /** The field a filter may be typed for in this column's header cell, if any. */
  private def filterField[X](column: Column[X]): Option[String] = column
    .field
    .filter(_ => column.filterable)

  private def sortedClass(key: Option[Order]): String = key.fold("")(key =>
    if key.descending then "list-sorted-desc" else "list-sorted-asc",
  )

  /** A reminder of the syntax a header cell reads, for a field of one kind. */
  private def hint(kind: Kind): String = kind match
    case Kind.Text              => "Contains…"
    case Kind.Whole | Kind.Real => ">50, 10..20"
    case Kind.Flag              => "yes / no"

  private def range(page: Option[Page], total: Int): String = page match
    case Some(window) if total > 0 =>
      s"${ window.offset + 1 }–${ (window.offset + window.limit).min(
          total,
        ) } of $total"
    case _ => s"$total"

  private def atEnd(page: Option[Page], total: Int): Boolean =
    page.forall(window => window.offset + window.limit >= total)
