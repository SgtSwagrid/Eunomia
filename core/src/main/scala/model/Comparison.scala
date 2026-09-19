package com.alecdorrington.eunomia
package model

import io.circe.{Decoder, Encoder}

/**
  * How a field value is compared with a given value.
  *
  * @param symbol
  *   The operator this comparison is written as, on the wire and in a header
  *   cell alike.
  */
enum Comparison(val symbol: String):

  case Eq extends Comparison("=")
  case Ne extends Comparison("!=")
  case Lt extends Comparison("<")
  case Le extends Comparison("<=")
  case Gt extends Comparison(">")
  case Ge extends Comparison(">=")

  /**
    * Whether this comparison holds between two values, given how they are
    * ordered.
    *
    * @param order
    *   Negative, zero or positive as the field value is less than, equal to or
    *   greater than the given value.
    */
  def holds(order: Int): Boolean = this match
    case Comparison.Eq => order == 0
    case Comparison.Ne => order != 0
    case Comparison.Lt => order < 0
    case Comparison.Le => order <= 0
    case Comparison.Gt => order > 0
    case Comparison.Ge => order >= 0

object Comparison:

  /** The comparison written as the given operator, if there is one. */
  def fromSymbol(symbol: String): Option[Comparison] =
    values.find(_.symbol == symbol)

  /**
    * The comparisons, longest operators first, so that the first whose operator
    * prefixes some text is the one it was written with (`<=` rather than `<`).
    */
  val bySymbolLength: List[Comparison] = values.toList.sortBy(-_.symbol.length)

  given Encoder[Comparison] = Encoder.encodeString.contramap(_.symbol)

  given Decoder[Comparison] = Decoder
    .decodeString
    .emap(symbol =>
      fromSymbol(symbol).toRight(s"Unknown comparison `$symbol`."),
    )
