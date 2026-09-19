package com.alecdorrington.eunomia
package model

import io.circe.{Decoder, Encoder, Json}

/**
  * One field value as it travels inside a [[Filter]]: detached from the type of
  * the item it was read from, so that one query can be evaluated in memory,
  * translated into SQL, or sent over the wire alike.
  */
enum Value:

  /** A piece of text. */
  case Text(text: String)

  /** A whole number. */
  case Whole(number: Long)

  /** A real number. */
  case Real(number: Double)

  /** A truth value. */
  case Flag(flag: Boolean)

  /** The kind of field this value belongs to. */
  def kind: Kind = this match
    case Value.Text(_)  => Kind.Text
    case Value.Whole(_) => Kind.Whole
    case Value.Real(_)  => Kind.Real
    case Value.Flag(_)  => Kind.Flag

  /**
    * This value as a value of the given kind, if it can be read as one without
    * loss. Whole numbers are real numbers too, and a real number with no
    * fractional part is a whole number, so the two convert freely.
    */
  def as(target: Kind): Option[Value] = (this, target) match
    case (Value.Whole(number), Kind.Real) => Some(Value.Real(number.toDouble))
    case (Value.Real(number), Kind.Whole) =>
      Option.when(number.isWhole)(Value.Whole(number.toLong))
    case _ => Option.when(kind == target)(this)

object Value:

  /**
    * Orders two values of the same kind: text lexicographically, numbers
    * numerically, and `false` before `true`. Values of different kinds, which a
    * checked query never compares, are ordered by kind.
    */
  given Ordering[Value] = (left, right) =>
    (left, right) match
      case (Text(a), Text(b))   => a.compareTo(b)
      case (Whole(a), Whole(b)) => a.compare(b)
      case (Flag(a), Flag(b))   => a.compare(b)
      case _                    => (numeric(left), numeric(right)) match
          case (Some(a), Some(b)) => a.compare(b)
          case _ => left.kind.ordinal.compare(right.kind.ordinal)

  /** Values are sent as bare JSON scalars. */
  given Encoder[Value] = Encoder.instance:
    case Text(text)    => Json.fromString(text)
    case Whole(number) => Json.fromLong(number)
    case Real(number)  => Json.fromDoubleOrString(number)
    case Flag(flag)    => Json.fromBoolean(flag)

  /**
    * Values are read back from bare JSON scalars. JSON does not distinguish
    * whole from real numbers, so any number without a fractional part is read
    * as whole; as [[Value.as]] converts between the two, nothing is lost.
    */
  given Decoder[Value] = Decoder
    .decodeJson
    .emap(json =>
      json
        .asString
        .map(Text(_))
        .orElse(json.asBoolean.map(Flag(_)))
        .orElse(
          json
            .asNumber
            .map(number => number.toLong.fold(Real(number.toDouble))(Whole(_))),
        )
        .toRight("Expected a string, number or boolean."),
    )

  /** A value as a real number, if it is a number at all. */
  private def numeric(value: Value): Option[Double] = value match
    case Whole(number) => Some(number.toDouble)
    case Real(number)  => Some(number)
    case _             => None
