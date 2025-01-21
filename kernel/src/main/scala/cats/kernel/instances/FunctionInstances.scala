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
package instances

import cats.kernel.compat.scalaVersionSpecific._
import scala.util.control.TailCalls.{done, tailcall, TailRec}

@suppressUnusedImportWarningForScalaVersionSpecific
trait FunctionInstances extends FunctionInstances0 {

  given catsKernelOrderForFunction0[A](using ev: Order[A]): Order[() => A] =
    Order.by(_.apply())

  given catsKernelCommutativeGroupForFunction0[A](using G: CommutativeGroup[A]): CommutativeGroup[() => A] =
    new Function0Group[A] with CommutativeGroup[() => A] { def A: Group[A] = G }

  given catsKernelCommutativeGroupForFunction1[A, B](using G: CommutativeGroup[B]): CommutativeGroup[A => B] =
    new Function1Group[A, B] with CommutativeGroup[A => B] { def B: Group[B] = G }
}

private[instances] trait FunctionInstances0 extends FunctionInstances1 {

  given catsKernelHashForFunction0[A](using ev: Hash[A]): Hash[() => A] =
    new Hash[() => A] {
      def hash(x: () => A) = ev.hash(x())
      def eqv(x: () => A, y: () => A) = ev.eqv(x(), y())
    }

  given catsKernelPartialOrderForFunction0[A](using ev: PartialOrder[A]): PartialOrder[() => A] =
    PartialOrder.by(_.apply())

  given catsKernelGroupForFunction0[A](using G: Group[A]): Group[() => A] =
    new Function0Group[A] { def A: Group[A] = G }

  given catsKernelGroupForFunction1[A, B](using G: Group[B]): Group[A => B] =
    new Function1Group[A, B] { def B: Group[B] = G }

  given catsKernelBoundedSemilatticeForFunction0[A](using
    G: BoundedSemilattice[A]
  ): BoundedSemilattice[() => A] =
    new Function0Monoid[A] with BoundedSemilattice[() => A] { def A: Monoid[A] = G }

  given catsKernelBoundedSemilatticeForFunction1[A, B](using
    G: BoundedSemilattice[B]
  ): BoundedSemilattice[A => B] =
    new Function1Monoid[A, B] with BoundedSemilattice[A => B] { def B: Monoid[B] = G }
}

private[instances] trait FunctionInstances1 extends FunctionInstances2 {

  given catsKernelEqForFunction0[A](using ev: Eq[A]): Eq[() => A] =
    Eq.by(_.apply())

  given catsKernelCommutativeMonoidForFunction0[A](using
    M: CommutativeMonoid[A]
  ): CommutativeMonoid[() => A] =
    new Function0Monoid[A] with CommutativeMonoid[() => A] { def A: Monoid[A] = M }

  given catsKernelCommutativeMonoidForFunction1[A, B](using
    M: CommutativeMonoid[B]
  ): CommutativeMonoid[A => B] =
    new Function1Monoid[A, B] with CommutativeMonoid[A => B] { def B: Monoid[B] = M }

  given catsKernelSemilatticeForFunction0[A](using M: Semilattice[A]): Semilattice[() => A] =
    new Function0Semigroup[A] with Semilattice[() => A] { def A: Semigroup[A] = M }

  given catsKernelSemilatticeForFunction1[A, B](using M: Semilattice[B]): Semilattice[A => B] =
    new Function1Semigroup[A, B] with Semilattice[A => B] { def B: Semigroup[B] = M }
}

private[instances] trait FunctionInstances2 extends FunctionInstances3 {

  given catsKernelMonoidForFunction0[A](using M: Monoid[A]): Monoid[() => A] =
    new Function0Monoid[A] { def A: Monoid[A] = M }

  given catsKernelMonoidForFunction1[A, B](using M: Monoid[B]): Monoid[A => B] =
    new Function1Monoid[A, B] { def B: Monoid[B] = M }

  given catsKernelBandForFunction0[A](using S: Band[A]): Band[() => A] =
    new Function0Semigroup[A] with Band[() => A] { def A: Semigroup[A] = S }

  given catsKernelBandForFunction1[A, B](using S: Band[B]): Band[A => B] =
    new Function1Semigroup[A, B] with Band[A => B] { def B: Semigroup[B] = S }
}

private[instances] trait FunctionInstances3 extends FunctionInstances4 {

  given catsKernelCommutativeSemigroupForFunction0[A](using
    S: CommutativeSemigroup[A]
  ): CommutativeSemigroup[() => A] =
    new Function0Semigroup[A] with CommutativeSemigroup[() => A] { def A: Semigroup[A] = S }

  given catsKernelCommutativeSemigroupForFunction1[A, B](using
    S: CommutativeSemigroup[B]
  ): CommutativeSemigroup[A => B] =
    new Function1Semigroup[A, B] with CommutativeSemigroup[A => B] { def B: Semigroup[B] = S }
}

private[instances] trait FunctionInstances4 {

  given catsKernelSemigroupForFunction0[A](using S: Semigroup[A]): Semigroup[() => A] =
    new Function0Semigroup[A] { def A: Semigroup[A] = S }

  given catsKernelSemigroupForFunction1[A, B](using S: Semigroup[B]): Semigroup[A => B] =
    new Function1Semigroup[A, B] { def B: Semigroup[B] = S }
}

final private[instances] case class CombineFunction1[A, B](left: A => B, right: A => B, semiB: Semigroup[B])
    extends (A => B) {
  private def call(fn: A => B, a: A): TailRec[B] =
    fn match {
      case ref: CombineFunction1[A, B] @unchecked =>
        for {
          lb <- tailcall(call(ref.left, a))
          rb <- tailcall(call(ref.right, a))
        } yield ref.semiB.combine(lb, rb)
      case _ => done(fn(a))
    }

  final override def apply(a: A): B = call(this, a).result
}

trait Function1Semigroup[A, B] extends Semigroup[A => B] {
  given B: Semigroup[B]

  override def combine(x: A => B, y: A => B): A => B =
    CombineFunction1(x, y, B)

  override def combineAllOption(fns: IterableOnce[A => B]): Option[A => B] =
    if (fns.iterator.isEmpty) None
    else
      Some { (a: A) =>
        B.combineAllOption(fns.iterator.map(_.apply(a))).get
      }
}

trait Function1Monoid[A, B] extends Function1Semigroup[A, B] with Monoid[A => B] {
  given B: Monoid[B]

  val empty: A => B =
    (_: A) => B.empty
}

trait Function1Group[A, B] extends Function1Monoid[A, B] with Group[A => B] {
  given B: Group[B]

  def inverse(x: A => B): A => B =
    (a: A) => B.inverse(x(a))
}

final private[instances] case class CombineFunction0[A](left: () => A, right: () => A, semiA: Semigroup[A])
    extends (() => A) {
  private def call(fn: () => A): TailRec[A] =
    fn match {
      case ref: CombineFunction0[A] @unchecked =>
        for {
          la <- tailcall(call(ref.left))
          ra <- tailcall(call(ref.right))
        } yield ref.semiA.combine(la, ra)
      case _ => done(fn())
    }

  final override def apply(): A = call(this).result
}

trait Function0Semigroup[A] extends Semigroup[() => A] {
  given A: Semigroup[A]

  override def combine(x: () => A, y: () => A): () => A =
    CombineFunction0(x, y, A)

  override def combineAllOption(fns: IterableOnce[() => A]): Option[() => A] =
    if (fns.iterator.isEmpty) None
    else
      Some { () =>
        A.combineAllOption(fns.iterator.map(_.apply())).get
      }
}

trait Function0Monoid[A] extends Function0Semigroup[A] with Monoid[() => A] {
  given A: Monoid[A]

  val empty: () => A =
    () => A.empty
}

trait Function0Group[A] extends Function0Monoid[A] with Group[() => A] {
  given A: Group[A]

  def inverse(x: () => A): () => A =
    () => A.inverse(x())
}
