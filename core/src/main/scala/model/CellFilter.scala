package com.alecdorrington.eunomia
package model

import cats.syntax.all.*
import com.alecdorrington.eunomia.model.Comparison.*
import com.alecdorrington.eunomia.model.Filter.*

/**
  * The syntax of the filter a person types into one column's header cell, where
  * the field is fixed, so only the condition on it need be written.
  *
  * Alternatives separated by `|` match wherever any one of them does. Each
  * alternative is written according to the kind of the field:
  *
  *   - Any kind: `?` matches an absent value, and `!?` a present one.
  *   - Text: `abc` matches text containing `abc`, ignoring case; `!abc` text
  *     that does not; `=abc` exactly `abc`; and `!=abc` anything else.
  *   - Numbers: conditions separated by spaces, all of which must hold, each
  *     one of `50` or `=50`, `!=50`, `<50`, `<=50`, `>50`, `>=50`, or the
  *     inclusive range `50..80`.
  *   - Truth values: `yes`, `y`, `true` or `1`, and `no`, `n`, `false` or `0`.
  *
  * A blank cell matches everything.
  */
object CellFilter:

  /**
    * Reads what was typed into one column's header cell.
    *
    * @param field
    *   The name of the column's field.
    *
    * @param kind
    *   The kind of the column's field.
    *
    * @param input
    *   What was typed.
    *
    * @return
    *   The filter written, or why it cannot be read.
    */
  def parse(field: String, kind: Kind, input: String): Either[String, Filter] =
    val alternatives = input.split('|').map(_.trim).filter(_.nonEmpty).toList
    if alternatives.isEmpty then Right(Filter.always)
    else alternatives.traverse(alternative(field, kind, _)).map(Filter.any)

  private def alternative
    (field: String, kind: Kind, text: String)
    : Either[String, Filter] = (text, kind) match
    case ("?", _)                    => Right(Missing(field))
    case ("!?", _)                   => Right(!Missing(field))
    case (_, Kind.Text)              => Right(textual(field, text))
    case (_, Kind.Flag)              => flag(field, text)
    case (_, Kind.Whole | Kind.Real) => text
        .split("\\s+")
        .toList
        .traverse(condition(field, kind, _))
        .map(Filter.all)

  private def textual(field: String, text: String): Filter = text match
    case s"!=$exact" => Compare(field, Ne, Value.Text(exact))
    case s"=$exact"  => Compare(field, Eq, Value.Text(exact))
    case s"!$part"   => !Contains(field, part)
    case part        => Contains(field, part)

  private def flag(field: String, text: String): Either[String, Filter] =
    text.toLowerCase match
      case "yes" | "y" | "true" | "1" =>
        Right(Compare(field, Eq, Value.Flag(true)))
      case "no" | "n" | "false" | "0" =>
        Right(Compare(field, Eq, Value.Flag(false)))
      case _ => Left(s"`$text` is not ${ Kind.Flag.noun }.")

  private def condition
    (field: String, kind: Kind, token: String)
    : Either[String, Filter] = token match
    case s"$low..$high" => (number(kind, low), number(kind, high)).mapN(
        (min, max) => Compare(field, Ge, min) && Compare(field, Le, max),
      )
    case _ => Comparison
        .bySymbolLength
        .find(comparison => token.startsWith(comparison.symbol))
        .fold(number(kind, token).map(Compare(field, Eq, _)))(comparison =>
          number(
            kind,
            token.drop(comparison.symbol.length),
          ).map(Compare(field, comparison, _)),
        )

  private def number(kind: Kind, text: String): Either[String, Value] = text
    .toDoubleOption
    .flatMap(Value.Real(_).as(kind))
    .toRight(s"`$text` is not a ${ kind.noun }.")
