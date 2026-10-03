package com.alecdorrington.eunomia
package model

/**
  * The kind of value a field holds, which fixes how it is compared, ordered and
  * filtered.
  *
  * @param noun
  *   The name of the kind as shown to a person, e.g. in an error message.
  */
enum Kind(val noun: String):

  /** Text, filtered by substring as well as compared lexicographically. */
  case Text extends Kind("text")

  /** Integers. */
  case Integer extends Kind("whole number")

  /** Real numbers. */
  case Real extends Kind("number")

  /** Truth values, with `false` ordered before `true`. */
  case Flag extends Kind("yes or no")
