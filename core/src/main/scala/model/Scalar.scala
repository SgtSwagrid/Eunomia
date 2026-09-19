package com.alecdorrington.eunomia
package model

/**
  * Evidence that values of type `B` can be read as field values of one fixed
  * [[Kind]]. Instances exist for the common primitive types; derive one for any
  * other type from an existing instance with [[by]], e.g. a date as its epoch
  * millisecond.
  */
trait Scalar[B]:

  /** The kind of field value every `B` is read as. */
  def kind: Kind

  /** Reads one `B` as a field value. */
  def encode(value: B): Value

  /** An instance for `A`, reading each `A` as the `B` it is converted to. */
  def by[A](convert: A => B): Scalar[A] = Scalar(kind, convert.andThen(encode))

object Scalar:

  /** An instance of the given kind, reading values with the given function. */
  def apply[B](of: Kind, read: B => Value): Scalar[B] = new Scalar[B]:
    override val kind: Kind              = of
    override def encode(value: B): Value = read(value)

  given Scalar[String] = Scalar(Kind.Text, Value.Text(_))

  given Scalar[Long] = Scalar(Kind.Whole, Value.Whole(_))

  given Scalar[Int] = Scalar(
    Kind.Whole,
    number => Value.Whole(number.toLong),
  )

  given Scalar[Double] = Scalar(Kind.Real, Value.Real(_))

  given Scalar[Boolean] = Scalar(Kind.Flag, Value.Flag(_))

/**
  * Evidence that a field of type `V` holds values of the [[Scalar]] type `B`,
  * possibly absent: either `V` is `B` itself, or `V` is `Option[B]`.
  */
trait Nullable[V, B]:

  /** The value held, if one is. */
  def apply(value: V): Option[B]

object Nullable:

  given present[B : Scalar]: Nullable[B, B] = Some(_)

  given absent[B : Scalar]: Nullable[Option[B], B] = identity(_)
