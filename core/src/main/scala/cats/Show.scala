/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats

import java.util.UUID
import scala.collection.immutable.{BitSet, Queue, Seq, SortedMap, SortedSet}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Try

/**
 * A type class to provide textual representation. It is meant to be a
 * better "toString". Whereas toString exists for any Object,
 * regardless of whether or not the creator of the class explicitly
 * made a toString method, a Show instance will only exist if someone
 * explicitly provided one.
 */
trait Show[T] extends Show.ContravariantShow[T]

/**
 * Hand rolling the type class boilerplate due to scala/bug#6260 and scala/bug#10458
 */
object Show extends ScalaVersionSpecificShowInstances with ShowInstances {

  def apply[A](using instance: Show[A]): Show[A] = instance

  trait ContravariantShow[-T] extends Serializable {
    def show(t: T): String
  }

  trait Ops[A] {
    def typeClassInstance: Show[A]
    def self: A
    def show: String = typeClassInstance.show(self)
  }

  trait ToShowOps {
    implicit def toShow[A](target: A)(using tc: Show[A]): Ops[A] =
      new Ops[A] {
        val self = target
        val typeClassInstance = tc
      }
  }

  /**
   * creates an instance of [[Show]] using the provided function
   */
  def show[A](f: A => String): Show[A] = f(_)

  /**
   * creates an instance of [[Show]] using object toString
   */
  def fromToString[A]: Show[A] = _.toString

  final case class Shown(override val toString: String) extends AnyVal
  object Shown {
    implicit def mat[A](x: A)(using z: ContravariantShow[A]): Shown = Shown(z.show(x))
  }

  final case class ShowInterpolator(_sc: StringContext) extends AnyVal {
    def show(args: Shown*): String = _sc.s(args: _*)
  }

  given catsContravariantForShow: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] = b => fa.show(f(b))
  }

  given catsShowForUnit: Show[Unit] = cats.instances.unit.catsStdShowForUnit
  given catsShowForBoolean: Show[Boolean] = cats.instances.boolean.catsStdShowForBoolean
  given catsShowForByte: Show[Byte] = cats.instances.byte.catsStdShowForByte
  given catsShowForShort: Show[Short] = cats.instances.short.catsStdShowForShort
  given catsShowForInt: Show[Int] = cats.instances.int.catsStdShowForInt
  given catsShowForLong: Show[Long] = cats.instances.long.catsStdShowForLong
  given catsShowForFloat: Show[Float] = cats.instances.float.catsStdShowForFloat
  given catsShowForDouble: Show[Double] = cats.instances.double.catsStdShowForDouble
  given catsShowForBigInt: Show[BigInt] = cats.instances.bigInt.catsStdShowForBigInt
  given catsShowForBigDecimal: Show[BigDecimal] = cats.instances.bigDecimal.catsStdShowForBigDecimal
  given catsShowForChar: Show[Char] = cats.instances.char.catsStdShowForChar
  given catsShowForSymbol: Show[Symbol] = cats.instances.symbol.catsStdShowForSymbol
  given catsShowForString: Show[String] = cats.instances.string.catsStdShowForString
  given catsShowForUUID: Show[UUID] = cats.instances.uuid.catsStdShowForUUID
  given catsShowForDuration: Show[Duration] = cats.instances.duration.catsStdShowForDurationUnambiguous
  given catsShowForBitSet: Show[BitSet] = cats.instances.bitSet.catsStdShowForBitSet

  given catsShowForOption[A: Show]: Show[Option[A]] = cats.instances.option.catsStdShowForOption[A]
  given catsShowForTry[A: Show]: Show[Try[A]] = cats.instances.try_.catsStdShowForTry[A]
  given catsShowForList[A: Show]: Show[List[A]] = cats.instances.list.catsStdShowForList[A]
  given catsShowForVector[A: Show]: Show[Vector[A]] = cats.instances.vector.catsStdShowForVector[A]
  given catsShowForQueue[A: Show]: Show[Queue[A]] = cats.instances.queue.catsStdShowForQueue[A]
  given catsShowForEither[A: Show, B: Show]: Show[Either[A, B]] =
    cats.instances.either.catsStdShowForEither[A, B]
  given catsShowForSortedMap[K: Show, V: Show]: Show[SortedMap[K, V]] =
    cats.instances.sortedMap.catsStdShowForSortedMap[K, V]

  @deprecated("Use catsStdShowForTuple2 in cats.instances.NTupleShowInstances", "2.4.0")
  def catsShowForTuple2[A: Show, B: Show]: Show[(A, B)] = cats.instances.tuple.catsStdShowForTuple2[A, B]
}

private[cats] trait ShowInstances extends cats.instances.NTupleShowInstances with ShowInstances0 {
  given catsShowForFiniteDuration: Show[FiniteDuration] =
    cats.instances.finiteDuration.catsStdShowForFiniteDurationUnambiguous

  given catsShowForSortedSet[A: Show]: Show[SortedSet[A]] = cats.instances.sortedSet.catsStdShowForSortedSet[A]
}

private[cats] trait ShowInstances0 {
  given catsShowForSeq[A: Show]: Show[Seq[A]] = cats.instances.seq.catsStdShowForSeq[A]
  given catsShowForMap[K: Show, V: Show]: Show[Map[K, V]] = cats.instances.map.catsStdShowForMap[K, V]
  given catsShowForSet[A: Show]: Show[Set[A]] = cats.instances.set.catsStdShowForSet[A]
}
