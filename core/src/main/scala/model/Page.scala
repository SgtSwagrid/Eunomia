package com.alecdorrington.eunomia
package model

import io.circe.{Codec, Decoder, Encoder}

/**
  * A page of an ordered list.
  *
  * @param offset
  *   The number of items skipped before the page starts.
  *
  * @param limit
  *   The greatest number of items on the page.
  */
final case class Page(offset: Int, limit: Int) derives Codec.AsObject:

  /**
    * Selects the items of a whole list that fall on this page.
    *
    * @tparam X
    *   The type of the items.
    *
    * @param items
    *   The whole list.
    *
    * @return
    *   A list of the items on this page.
    */
  def slice[X](items: List[X]): List[X] = items.drop(offset).take(limit)

  /** The page immediately after this one, its offset at most `Int.MaxValue`. */
  def next: Page = copy(offset =
    (offset.toLong + limit).min(Int.MaxValue.toLong).toInt,
  )

  /** The page immediately before this one, stopping at the start. */
  def previous: Page = copy(offset = (offset - limit).max(0))

  /**
    * Caps the size of this page.
    *
    * @param max
    *   The greatest number of items the page may hold.
    *
    * @return
    *   A page at the same offset holding at most `max` items.
    */
  def capped(max: Int): Page = copy(limit = limit.min(max))

  /** Whether this page has a non-negative offset and a positive limit. */
  def valid: Boolean = offset >= 0 && limit > 0

  /**
    * Fits this page to a list of the given length, so that a list that shrank
    * while its last page was read stays on its last page.
    *
    * @param total
    *   The number of items in the list.
    *
    * @return
    *   This page, unless it starts past the end of the list; then the last page
    *   of its size that holds anything, or the first if the list is empty.
    */
  def within(total: Int): Page =
    if offset < total then this
    else copy(offset = ((total - 1).max(0) / limit) * limit)

object Page:

  /**
    * Creates the first page of a given size.
    *
    * @param limit
    *   The greatest number of items on the page.
    *
    * @return
    *   A page at offset `0`.
    */
  def first(limit: Int): Page = Page(0, limit)

/**
  * A window of a filtered list: the items on one page of it.
  *
  * @tparam X
  *   The type of the items.
  *
  * @param items
  *   The items in the window, in order.
  *
  * @param total
  *   The number of items in the whole filtered list, across every page.
  *
  * @param page
  *   The page sent, which may differ from the one requested: a server may send
  *   a smaller one, and a page past the end of the list is answered with the
  *   last (see [[Page.within]]). `None` where the items are the whole filtered
  *   list.
  */
final case class Window[X]
  (
    items: List[X],
    total: Int,
    page: Option[Page] = None,
  ):

  /**
    * Transforms each item of this window.
    *
    * @tparam Y
    *   The type of the transformed items.
    *
    * @param transform
    *   The transformation of one item.
    *
    * @return
    *   The same window holding the transformed items.
    */
  def map[Y](transform: X => Y): Window[Y] =
    Window(items.map(transform), total, page)

object Window:

  /**
    * Creates the window of an empty list.
    *
    * @tparam X
    *   The type of the items.
    *
    * @return
    *   An empty window.
    */
  def empty[X]: Window[X] = Window(List.empty, 0)

  /**
    * Wraps a whole list as one window.
    *
    * @tparam X
    *   The type of the items.
    *
    * @param items
    *   The whole list.
    *
    * @return
    *   A window holding every item, with no page.
    */
  def whole[X](items: List[X]): Window[X] = Window(items, items.size)

  // Written by hand, as a derived codec would demand a whole codec of `X`.
  given [X : Encoder]: Encoder[Window[X]] =
    Encoder.forProduct3("items", "total", "page")(window =>
      (window.items, window.total, window.page),
    )

  given [X : Decoder]: Decoder[Window[X]] =
    Decoder.forProduct3("items", "total", "page")(Window.apply[X])
