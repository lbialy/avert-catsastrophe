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

package cats.kernel

import java.util.UUID
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.{specialized => sp}

/**
 * A type class used to name the lower limit of a type.
 */
trait LowerBounded[@sp A] {
  def partialOrder: PartialOrder[A]

  /**
   * Returns the lower limit of a type.
   */
  def minBound: A
}

trait LowerBoundedFunctions[L[T] <: LowerBounded[T]] {
  def minBound[@sp A](implicit ev: L[A]): A = ev.minBound
}

object LowerBounded extends LowerBoundedFunctions[LowerBounded] {
  @inline def apply[A](implicit l: LowerBounded[A]): LowerBounded[A] = l

  given catsKernelLowerBoundedForUnit: LowerBounded[Unit] = cats.kernel.instances.unit.catsKernelStdOrderForUnit
  given catsKernelLowerBoundedForBoolean: LowerBounded[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdOrderForBoolean
  given catsKernelLowerBoundedForByte: LowerBounded[Byte] = cats.kernel.instances.byte.catsKernelStdOrderForByte
  given catsKernelLowerBoundedForInt: LowerBounded[Int] = cats.kernel.instances.int.catsKernelStdOrderForInt
  given catsKernelLowerBoundedForShort: LowerBounded[Short] =
    cats.kernel.instances.short.catsKernelStdOrderForShort
  given catsKernelLowerBoundedForLong: LowerBounded[Long] = cats.kernel.instances.long.catsKernelStdOrderForLong
  given catsKernelLowerBoundedForDuration: LowerBounded[Duration] =
    cats.kernel.instances.duration.catsKernelStdOrderForDuration
  given catsKernelLowerBoundedForFiniteDuration: LowerBounded[FiniteDuration] =
    cats.kernel.instances.all.catsKernelStdOrderForFiniteDuration
  given catsKernelLowerBoundedForChar: LowerBounded[Char] = cats.kernel.instances.char.catsKernelStdOrderForChar
  given catsKernelLowerBoundedForString: LowerBounded[String] =
    cats.kernel.instances.string.catsKernelStdOrderForString
  given catsKernelLowerBoundedForSymbol: LowerBounded[Symbol] =
    cats.kernel.instances.symbol.catsKernelStdOrderForSymbol
  given catsKernelLowerBoundedForUUID: LowerBounded[UUID] = cats.kernel.instances.uuid.catsKernelStdOrderForUUID
}

/**
 * A type class used to name the upper limit of a type.
 */
trait UpperBounded[@sp A] {
  def partialOrder: PartialOrder[A]

  /**
   * Returns the upper limit of a type.
   */
  def maxBound: A
}

trait UpperBoundedFunctions[U[T] <: UpperBounded[T]] {
  def maxBound[@sp A](implicit ev: U[A]): A = ev.maxBound
}

object UpperBounded extends UpperBoundedFunctions[UpperBounded] {
  @inline def apply[A](implicit u: UpperBounded[A]): UpperBounded[A] = u

  given catsKernelUpperBoundedForUnit: UpperBounded[Unit] = cats.kernel.instances.unit.catsKernelStdOrderForUnit
  given catsKernelUpperBoundedForBoolean: UpperBounded[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdOrderForBoolean
  given catsKernelUpperBoundedForByte: UpperBounded[Byte] = cats.kernel.instances.byte.catsKernelStdOrderForByte
  given catsKernelUpperBoundedForInt: UpperBounded[Int] = cats.kernel.instances.int.catsKernelStdOrderForInt
  given catsKernelUpperBoundedForShort: UpperBounded[Short] =
    cats.kernel.instances.short.catsKernelStdOrderForShort
  given catsKernelUpperBoundedForLong: UpperBounded[Long] = cats.kernel.instances.long.catsKernelStdOrderForLong
  given catsKernelUpperBoundedForDuration: UpperBounded[Duration] =
    cats.kernel.instances.duration.catsKernelStdOrderForDuration
  given catsKernelUpperBoundedForFiniteDuration: UpperBounded[FiniteDuration] =
    cats.kernel.instances.all.catsKernelStdOrderForFiniteDuration
  given catsKernelUpperBoundedForChar: UpperBounded[Char] = cats.kernel.instances.char.catsKernelStdOrderForChar
  given catsKernelUpperBoundedForUUID: UpperBounded[UUID] = cats.kernel.instances.uuid.catsKernelStdOrderForUUID
}
