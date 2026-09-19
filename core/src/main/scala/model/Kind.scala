package com.alecdorrington.eunomia
package model

/**
  * The kind of value a field holds, which fixes how it is compared, ordered and
  * filtered.
  *
  * @param noun
  *   How a value of this kind is described to a person, e.g. in a complaint
  *   that some text is not one.
  */
enum Kind(val noun: String):

  /** Text, filtered by substring as well as compared lexicographically. */
  case Text extends Kind("text")

  /** Whole numbers. */
  case Whole extends Kind("whole number")

  /** Real numbers. */
  case Real extends Kind("number")

  /** Truth values, with `false` ordered before `true`. */
  case Flag extends Kind("yes or no")
