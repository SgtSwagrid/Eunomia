package com.alecdorrington.eunomia
package client

import com.raquo.laminar.api.L.*

/** Shows and hides elements by a condition, keeping them mounted either way. */
private[client] object Visibility:

  /**
    * Hides the element it is applied to while a condition is `false`, leaving
    * it its own `display` while it is `true`.
    *
    * @param shown
    *   Whether the element is shown.
    *
    * @return
    *   A modifier binding the element's `display`.
    */
  def visibleWhen(shown: Signal[Boolean]): Modifier[HtmlElement] = display <--
    shown.map(if _ then "" else "none")
