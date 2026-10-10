package com.alecdorrington.eunomia
package model

import cats.syntax.all.*
import com.alecdorrington.eunomia.model.Filter.*
import com.alecdorrington.eunomia.model.Operator.*
import scala.util.matching.Regex

/**
  * The syntax of the filter a person types into one column's header cell, where
  * the field is fixed and only the condition on it is written.
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
    * Parses what was typed into one column's header cell.
    *
    * @param field
    *   The name of the column's field.
    *
    * @param kind
    *   The kind of the column's field.
    *
    * @param input
    *   The text typed.
    *
    * @return
    *   Either the filter written, or a message saying why it cannot be read.
    */
  def parse(field: String, kind: Kind, input: String): Either[String, Filter] =
    val alternatives = input.split('|').map(_.trim).filter(_.nonEmpty).toList
    if alternatives.isEmpty then Right(Filter.always)
    else alternatives.traverse(alternative(field, kind, _)).map(Filter.any)

  private def alternative
    (field: String, kind: Kind, text: String)
    : Either[String, Filter] = (text, kind) match
    case ("?", _)                      => Right(Missing(field))
    case ("!?", _)                     => Right(!Missing(field))
    case (_, Kind.Text)                => Right(textual(field, text))
    case (_, Kind.Flag)                => flag(field, text)
    case (_, Kind.Integer | Kind.Real) => spaces
        .split(text)
        .toList
        .traverse(condition(field, kind, _))
        .map(Filter.all)

  private val spaces: Regex = "\\s+".r

  private def textual(field: String, text: String): Filter = text match
    case s"!=$exact" => Compare(field, Unequal, Value.Text(exact))
    case s"=$exact"  => Compare(field, Equal, Value.Text(exact))
    case s"!$part"   => !Contains(field, part)
    case part        => Contains(field, part)

  /**
    * Case is folded letter by letter, by no locale's rules, as [[ListSchema]]
    * folds it, so that the JVM reads a flag as a browser does.
    */
  private def flag(field: String, text: String): Either[String, Filter] =
    text.map(_.toLower) match
      case "yes" | "y" | "true" | "1" =>
        Right(Compare(field, Equal, Value.Flag(true)))
      case "no" | "n" | "false" | "0" =>
        Right(Compare(field, Equal, Value.Flag(false)))
      case _ => Left(s"`$text` is not ${ Kind.Flag.noun }.")

  private def condition
    (field: String, kind: Kind, token: String)
    : Either[String, Filter] = token match
    case s"$low..$high"              => range(field, kind, low, high)
    case Prefixed(operator, operand) =>
      number(kind, operand).map(Compare(field, operator, _))
    case exact => number(kind, exact).map(Compare(field, Equal, _))

  /** Refuses bounds the wrong way round, as they are almost always a typo. */
  private def range
    (
      field: String,
      kind: Kind,
      low: String,
      high: String,
    )
    : Either[String, Filter] = (number(kind, low), number(kind, high))
    .tupled
    .flatMap((least, greatest) =>
      Either.cond(
        Ordering[Value].lteq(least, greatest),
        Compare(field, AtLeast, least) && Compare(field, AtMost, greatest),
        s"`$low..$high` holds nothing, as `$low` is above `$high`.",
      ),
    )

  private object Prefixed:

    def unapply(token: String): Option[(Operator, String)] = Operator
      .longestFirst
      .find(operator => token.startsWith(operator.symbol))
      .map(operator => (operator, token.drop(operator.symbol.length)))

  /** Tries a `Long` first, as a `Double` loses precision on large numbers. */
  private def number(kind: Kind, text: String): Either[String, Value] =
    if text.isEmpty then Left(s"A ${ kind.noun } is missing.")
    else
      text
        .toLongOption
        .map(Value.Integer(_))
        .orElse(text.toDoubleOption.map(Value.Real(_)))
        .flatMap(_.convertedTo(kind))
        .toRight(s"`$text` is not a ${ kind.noun }.")
