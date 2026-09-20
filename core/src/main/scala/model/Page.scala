package com.alecdorrington.eunomia
package model

import io.circe.{Codec, Decoder, Encoder}

/**
  * One window onto an ordered list.
  *
  * @param offset
  *   The number of items skipped before the window starts.
  *
  * @param limit
  *   The greatest number of items in the window.
  */
final case class Page(offset: Int, limit: Int) derives Codec.AsObject:

  /** The items of a whole list that fall within this window. */
  def slice[X](items: List[X]): List[X] = items.slice(offset, offset + limit)

  /** The window immediately after this one. */
  def next: Page = copy(offset = offset + limit)

  /** The window immediately before this one, stopping at the start. */
  def previous: Page = copy(offset = (offset - limit).max(0))

  /** This window, holding no more than the given number of items. */
  def capped(max: Int): Page = copy(limit = limit.min(max))

  /** Whether this window is well-formed, starting at or after the beginning. */
  def valid: Boolean = offset >= 0 && limit > 0

object Page:

  /** The first window of the given size. */
  def first(limit: Int): Page = Page(0, limit)

/**
  * One window of a filtered list.
  *
  * @param items
  *   The items in the window, in order.
  *
  * @param total
  *   The number of items in the whole filtered list, across every window.
  *
  * @param page
  *   The window these items are of, as it was answered, which need not be the
  *   one asked for: a server may send a smaller window than was requested, and
  *   says here which it sent. `None` where the items are the whole of the
  *   filtered list.
  */
final case class Paged[X]
  (
    items: List[X],
    total: Int,
    page: Option[Page] = None,
  ):

  /** The same window, with each item transformed. */
  def map[Y](transform: X => Y): Paged[Y] =
    Paged(items.map(transform), total, page)

object Paged:

  /** The window of an empty list. */
  def empty[X]: Paged[X] = Paged(List.empty, 0)

  /** A whole list, as one window. */
  def whole[X](items: List[X]): Paged[X] = Paged(items, items.size)

  // Written by hand, as a derived codec would demand a whole codec of `X`.
  given [X : Encoder]: Encoder[Paged[X]] =
    Encoder.forProduct3("items", "total", "page")(paged =>
      (paged.items, paged.total, paged.page),
    )

  given [X : Decoder]: Decoder[Paged[X]] =
    Decoder.forProduct3("items", "total", "page")(Paged.apply[X])
