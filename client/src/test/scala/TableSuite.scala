package com.alecdorrington.eunomia
package client

import com.alecdorrington.eunomia.model.Page
import munit.FunSuite

class TableSuite extends FunSuite:

  test("the pager reads a huge page without overflowing"):
    val huge = Some(Page(2, Int.MaxValue))
    assertEquals(Table.range(huge, 5), "3–5 of 5")
    assert(Table.atEnd(huge, 5))

  test("the pager is at the end only when the last item is shown"):
    assertEquals(
      Table.range(Some(Page(0, 2)), 5),
      "1–2 of 5",
    )
    assert(!Table.atEnd(Some(Page(2, 2)), 5))
    assert(Table.atEnd(Some(Page(4, 2)), 5))
