package com.alecdorrington.eunomia
package model

/**
  * Evidence that values of type `B` can be read as field values of one fixed
  * [[Kind]]. Instances exist for the common primitive types; derive others with
  * [[by]], e.g. a date as its epoch millisecond.
  *
  * @tparam B
  *   The type of the values read.
  */
trait Scalar[B]:

  /** The kind of field value every `B` is read as. */
  def kind: Kind

  /**
    * Reads one `B` as a field value.
    *
    * @param value
    *   The value to read.
    *
    * @return
    *   A field value of this instance's [[kind]].
    */
  def toValue(value: B): Value

  /**
    * Derives an instance for `A` by converting each `A` to a `B`.
    *
    * @tparam A
    *   The type to derive an instance for.
    *
    * @param convert
    *   The conversion from `A` to `B`.
    *
    * @return
    *   An instance of the same kind for `A`.
    */
  def by[A](convert: A => B): Scalar[A] = Scalar(kind, convert.andThen(toValue))

object Scalar:

  /**
    * Creates an instance from a kind and a reading function.
    *
    * @tparam B
    *   The type of the values read.
    *
    * @param of
    *   The kind of field value every `B` is read as.
    *
    * @param read
    *   The function reading one `B` as a field value of that kind.
    *
    * @return
    *   An instance for `B`.
    */
  def apply[B](of: Kind, read: B => Value): Scalar[B] = new Scalar[B]:
    override val kind: Kind               = of
    override def toValue(value: B): Value = read(value)

  given Scalar[String] = Scalar(Kind.Text, Value.Text(_))

  given Scalar[Long] = Scalar(Kind.Integer, Value.Integer(_))

  given Scalar[Int] = Scalar(
    Kind.Integer,
    number => Value.Integer(number.toLong),
  )

  given Scalar[Double] = Scalar(Kind.Real, Value.Real(_))

  given Scalar[Boolean] = Scalar(Kind.Flag, Value.Flag(_))

/**
  * Evidence that a field of type `V` holds values of the [[Scalar]] type `B`,
  * possibly absent: either `V` is `B` itself, or `V` is `Option[B]`.
  *
  * @tparam V
  *   The type of the field as read from an item.
  *
  * @tparam B
  *   The type of the values held.
  */
trait Nullable[V, B]:

  /**
    * Extracts the value held, if one is.
    *
    * @param value
    *   The field as read from an item.
    *
    * @return
    *   An option holding the value, or `None` where it is absent.
    */
  def apply(value: V): Option[B]

object Nullable:

  given plain[B : Scalar]: Nullable[B, B] = Some(_)

  given optional[B : Scalar]: Nullable[Option[B], B] = identity(_)
