package com.alecdorrington.eunomia
package client

import com.raquo.laminar.api.L.*

/** Shows and hides elements by a condition, keeping them mounted either way. */
object Visibility:

  /**
    * Hides the element it is applied to while the condition is `false`.
    *
    * @param shown
    *   Whether the element is shown.
    *
    * @param as
    *   The CSS `display` value while it is shown. By default, the element's
    *   own.
    */
  def visibleWhen
    (shown: Signal[Boolean], as: String = "")
    : Modifier[HtmlElement] = display <-- shown.map(if _ then as else "none")
