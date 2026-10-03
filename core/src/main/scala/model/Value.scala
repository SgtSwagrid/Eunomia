package com.alecdorrington.eunomia
package model

import io.circe.{Decoder, Encoder, Json}

/**
  * A field value detached from the type of the item it was read from, so that
  * one [[Filter]] can be evaluated in memory, translated into SQL or sent over
  * the wire.
  */
enum Value:

  /**
    * A piece of text.
    *
    * @param text
    *   The text.
    */
  case Text(text: String)

  /**
    * An integer.
    *
    * @param number
    *   The number.
    */
  case Integer(number: Long)

  /**
    * A real number.
    *
    * @param number
    *   The number.
    */
  case Real(number: Double)

  /**
    * A truth value.
    *
    * @param flag
    *   The truth value.
    */
  case Flag(flag: Boolean)

  /** The kind of field this value belongs to. */
  def kind: Kind = this match
    case Value.Text(_)    => Kind.Text
    case Value.Integer(_) => Kind.Integer
    case Value.Real(_)    => Kind.Real
    case Value.Flag(_)    => Kind.Flag

  /**
    * Converts this value to the given kind without loss. Integers and real
    * numbers convert freely, provided a real number has no fractional part.
    *
    * @param target
    *   The kind to convert to.
    *
    * @return
    *   An option holding the converted value, or `None` where it cannot be
    *   converted without loss.
    */
  def convertedTo(target: Kind): Option[Value] = (this, target) match
    case (Value.Integer(number), Kind.Real) => Some(Value.Real(number.toDouble))
    case (Value.Real(number), Kind.Integer) =>
      Option.when(number.isWhole)(Value.Integer(number.toLong))
    case _ => Option.when(kind == target)(this)

object Value:

  /**
    * Orders text lexicographically, numbers numerically, and `false` before
    * `true`. Values of different kinds, which a checked query never compares,
    * are ordered by kind.
    */
  given Ordering[Value] = (left, right) =>
    (left, right) match
      case (Text(a), Text(b))       => a.compareTo(b)
      case (Integer(a), Integer(b)) => a.compare(b)
      case (Flag(a), Flag(b))       => a.compare(b)
      case _                        => (numeric(left), numeric(right)) match
          case (Some(a), Some(b)) => a.compare(b)
          case _ => left.kind.ordinal.compare(right.kind.ordinal)

  given Encoder[Value] = Encoder.instance:
    case Text(text)      => Json.fromString(text)
    case Integer(number) => Json.fromLong(number)
    case Real(number)    => Json.fromDoubleOrString(number)
    case Flag(flag)      => Json.fromBoolean(flag)

  /** Reads any number without a fractional part as an integer. */
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
            .map(number =>
              number.toLong.fold(Real(number.toDouble))(Integer(_)),
            ),
        )
        .toRight("Expected a string, number or boolean."),
    )

  private def numeric(value: Value): Option[Double] = value match
    case Integer(number) => Some(number.toDouble)
    case Real(number)    => Some(number)
    case _               => None
