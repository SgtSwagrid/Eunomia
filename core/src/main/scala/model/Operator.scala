package com.alecdorrington.eunomia
package model

import io.circe.{Decoder, Encoder}

/**
  * An operator comparing a field value with a given value.
  *
  * @param symbol
  *   The symbol this operator is written as, on the wire and in a header cell.
  */
enum Operator(val symbol: String):

  /** Equal to. */
  case Equal extends Operator("=")

  /** Not equal to. */
  case Unequal extends Operator("!=")

  /** Less than. */
  case Less extends Operator("<")

  /** Less than or equal to. */
  case AtMost extends Operator("<=")

  /** Greater than. */
  case Greater extends Operator(">")

  /** Greater than or equal to. */
  case AtLeast extends Operator(">=")

  /**
    * Decides whether this operator holds, given how two values are ordered.
    *
    * @param sign
    *   The result of comparing the field value with the given value: negative,
    *   zero or positive.
    *
    * @return
    *   Whether the operator holds.
    */
  def holds(sign: Int): Boolean = this match
    case Operator.Equal   => sign == 0
    case Operator.Unequal => sign != 0
    case Operator.Less    => sign < 0
    case Operator.AtMost  => sign <= 0
    case Operator.Greater => sign > 0
    case Operator.AtLeast => sign >= 0

object Operator:

  /**
    * Finds the operator written as the given symbol.
    *
    * @param symbol
    *   The symbol, e.g. `<=`.
    *
    * @return
    *   An option holding the operator, or `None` for an unknown symbol.
    */
  def fromSymbol(symbol: String): Option[Operator] =
    values.find(_.symbol == symbol)

  /**
    * The operators, longest symbols first, so that the first whose symbol
    * prefixes some text is the one it was written with (`<=`, not `<`).
    */
  val longestFirst: List[Operator] = values.toList.sortBy(-_.symbol.length)

  given Encoder[Operator] = Encoder.encodeString.contramap(_.symbol)

  given Decoder[Operator] = Decoder
    .decodeString
    .emap(symbol => fromSymbol(symbol).toRight(s"Unknown operator `$symbol`."))
