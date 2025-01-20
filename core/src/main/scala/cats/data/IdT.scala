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
package data

/**
 * `IdT[F[_], A]` is the identity monad transformer.
 */
final case class IdT[F[_], A](value: F[A]) {

  /**
   * Example:
   * {{{
   *  scala> import cats.data.IdT
   *
   *  scala> val idT: IdT[List, Int] = IdT(List(1, 2, 3))
   *  scala> idT.map(_ + 1)
   *  res0: IdT[List, Int] = IdT(List(2, 3, 4))
   * }}}
   */
  def map[B](f: A => B)(using F: Functor[F]): IdT[F, B] =
    IdT(F.map(value)(f))

  /**
   * Modify the context `F` using transformation `f`.
   *
   * Example:
   * {{{
   *  scala> import cats.~>
   *  scala> import cats.data.IdT
   *
   *  scala> val vectorToList: Vector ~> List = new ~>[Vector, List] { override def apply[A](v: Vector[A]): List[A] = v.toList }
   *  scala> val idT: IdT[Vector, Int] = IdT(Vector(1, 2, 3))
   *  scala> idT.mapK[List](vectorToList)
   *  res0: IdT[List, Int] = IdT(List(1, 2, 3))
   * }}}
   */
  def mapK[G[_]](f: F ~> G): IdT[G, A] =
    IdT[G, A](f(value))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.IdT
   *
   *  scala> val idT: IdT[List, Int] = IdT(List(1, 2, 3))
   *  scala> idT.flatMap(x => IdT(List(x + 1)))
   *  res0: IdT[List, Int] = IdT(List(2, 3, 4))
   * }}}
   */
  def flatMap[B](f: A => IdT[F, B])(using F: FlatMap[F]): IdT[F, B] =
    IdT(F.flatMap(value)(a => f(a).value))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.IdT
   *
   *  scala> val idT: IdT[List, Int] = IdT(List(1, 2, 3))
   *  scala> idT.flatMapF(x => List(Option(x).filter(_ % 2 == 0)))
   *  res0: IdT[List, Option[Int]] = IdT(List(None, Some(2), None))
   * }}}
   */
  def flatMapF[B](f: A => F[B])(using F: FlatMap[F]): IdT[F, B] =
    IdT(F.flatMap(value)(f))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.IdT
   *
   *  scala> val idT: IdT[List, Int] = IdT(List(1, 2, 3))
   *  scala> idT.foldLeft(0)((acc, x) => acc + x)
   *  res0: Int = 6
   * }}}
   */
  def foldLeft[B](b: B)(f: (B, A) => B)(using F: Foldable[F]): B =
    F.foldLeft(value, b)(f)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.IdT
   *  scala> import cats.Eval
   *
   *  scala> val idT: IdT[List, Int] = IdT(List(1, 2, 3))
   *  scala> idT.foldRight(Eval.Zero)((x, acc) => Eval.later(x + acc.value)).value
   *  res0: Int = 6
   * }}}
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(using F: Foldable[F]): Eval[B] =
    F.foldRight(value, lb)(f)

  def reduceLeftTo[B](f: A => B)(g: (B, A) => B)(using F: Reducible[F]): B =
    F.reduceLeftTo(value)(f)(g)

  def reduceRightTo[B](f: A => B)(g: (A, Eval[B]) => Eval[B])(using F: Reducible[F]): Eval[B] =
    F.reduceRightTo(value)(f)(g)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.IdT
   *
   *  scala> val idT: IdT[List, Int] = IdT(List(1, 2, 3))
   *  scala> idT.traverse[Option, Int](x => Option(x + 1))
   *  res0: Option[IdT[List, Int]] = Some(IdT(List(2, 3, 4)))
   * }}}
   */
  def traverse[G[_], B](f: A => G[B])(using F: Traverse[F], G: Applicative[G]): G[IdT[F, B]] =
    G.map(F.traverse(value)(f))(IdT(_))

  def nonEmptyTraverse[G[_], B](f: A => G[B])(using F: NonEmptyTraverse[F], G: Apply[G]): G[IdT[F, B]] =
    G.map(F.nonEmptyTraverse(value)(f))(IdT(_))

  def ap[B](f: IdT[F, A => B])(using F: Apply[F]): IdT[F, B] =
    IdT(F.ap(f.value)(value))

}

object IdT extends IdTInstances {

  def pure[F[_], A](a: A)(using F: Applicative[F]): IdT[F, A] =
    IdT(F.pure(a))
}

sealed private[data] trait IdTFunctor[F[_]] extends Functor[IdT[F, *]] {
  given F0: Functor[F]

  override def map[A, B](fa: IdT[F, A])(f: A => B): IdT[F, B] =
    fa.map(f)
}

sealed private[data] trait IdTApply[F[_]] extends Apply[IdT[F, *]] with IdTFunctor[F] {
  given F0: Apply[F]

  override def ap[A, B](ff: IdT[F, A => B])(fa: IdT[F, A]): IdT[F, B] = fa.ap(ff)

  override def map2Eval[A, B, Z](fa: IdT[F, A], fb: Eval[IdT[F, B]])(f: (A, B) => Z): Eval[IdT[F, Z]] =
    F0.map2Eval(fa.value, fb.map(_.value))(f) // if F0 has a lazy map2Eval, leverage it
      .map(IdT(_))
}

sealed private[data] trait IdTApplicative[F[_]] extends Applicative[IdT[F, *]] with IdTApply[F] {
  given F0: Applicative[F]

  def pure[A](a: A): IdT[F, A] = IdT.pure(a)
}

sealed private[data] trait IdTContravariantMonoidal[F[_]] extends ContravariantMonoidal[IdT[F, *]] {
  given F0: ContravariantMonoidal[F]

  override def unit: IdT[F, Unit] = IdT(F0.unit)

  override def contramap[A, B](fa: IdT[F, A])(f: B => A): IdT[F, B] =
    IdT(F0.contramap(fa.value)(f))

  override def product[A, B](fa: IdT[F, A], fb: IdT[F, B]): IdT[F, (A, B)] =
    IdT(F0.product(fa.value, fb.value))
}

sealed private[data] trait IdTFlatMap[F[_]] extends FlatMap[IdT[F, *]] with IdTApply[F] {
  given F0: FlatMap[F]

  def flatMap[A, B](fa: IdT[F, A])(f: A => IdT[F, B]): IdT[F, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(f: A => IdT[F, Either[A, B]]): IdT[F, B] =
    IdT(F0.tailRecM(a)(f(_).value))
}

sealed private[data] trait IdTMonad[F[_]] extends Monad[IdT[F, *]] with IdTApplicative[F] with IdTFlatMap[F] {
  given F0: Monad[F]
}

sealed private[data] trait IdTFoldable[F[_]] extends Foldable[IdT[F, *]] {
  given F0: Foldable[F]

  def foldLeft[A, B](fa: IdT[F, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: IdT[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)

  override def size[A](fa: IdT[F, A]): Long =
    F0.size(fa.value)

  override def get[A](fa: IdT[F, A])(idx: Long): Option[A] =
    F0.get(fa.value)(idx)
}

sealed private[data] trait IdTTraverse[F[_]] extends Traverse[IdT[F, *]] with IdTFoldable[F] with IdTFunctor[F] {
  given F0: Traverse[F]

  def traverse[G[_]: Applicative, A, B](fa: IdT[F, A])(f: A => G[B]): G[IdT[F, B]] =
    fa.traverse(f)

  override def mapAccumulate[S, A, B](init: S, fa: IdT[F, A])(f: (S, A) => (S, B)): (S, IdT[F, B]) = {
    val (snext, fb) = F0.mapAccumulate(init, fa.value)(f)
    (snext, IdT(fb))
  }
}

sealed private[data] trait IdTNonEmptyTraverse[F[_]]
    extends IdTTraverse[F]
    with NonEmptyTraverse[IdT[F, *]]
    with IdTFunctor[F] {
  given F0: NonEmptyTraverse[F]

  def nonEmptyTraverse[G[_]: Apply, A, B](fa: IdT[F, A])(f: A => G[B]): G[IdT[F, B]] =
    fa.nonEmptyTraverse(f)

  def reduceLeftTo[A, B](fa: IdT[F, A])(f: A => B)(g: (B, A) => B): B =
    fa.reduceLeftTo(f)(g)

  def reduceRightTo[A, B](fa: IdT[F, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.reduceRightTo(f)(g)
}

sealed abstract private[data] class IdTInstances8 {
  given catsDataCommutativeFlatMapForIdT[F[_]](using
    F: CommutativeFlatMap[F]
  ): CommutativeFlatMap[IdT[F, *]] =
    new IdTFlatMap[F] with CommutativeFlatMap[IdT[F, *]] { given F0: CommutativeFlatMap[F] = F }
}

sealed abstract private[data] class IdTInstances7 extends IdTInstances8 {
  given catsDataCommutativeMonadForIdT[F[_]](using F: CommutativeMonad[F]): CommutativeMonad[IdT[F, *]] =
    new IdTMonad[F] with CommutativeMonad[IdT[F, *]] { given F0: CommutativeMonad[F] = F }
}

sealed abstract private[data] class IdTInstances6 extends IdTInstances7 {
  given catsDataContravariantMonoidalForIdT[F[_]](using
    F: ContravariantMonoidal[F]
  ): ContravariantMonoidal[IdT[F, *]] =
    new IdTContravariantMonoidal[F] { given F0: ContravariantMonoidal[F] = F }
}

sealed abstract private[data] class IdTInstances5 extends IdTInstances6 {
  given catsDataFunctorForIdT[F[_]](using F: Functor[F]): Functor[IdT[F, *]] =
    new IdTFunctor[F] { given F0: Functor[F] = F }
}

sealed abstract private[data] class IdTInstances4 extends IdTInstances5 {
  given catsDataApplyForIdT[F[_]](using F: Apply[F]): Apply[IdT[F, *]] =
    new IdTApply[F] { given F0: Apply[F] = F }
}

sealed abstract private[data] class IdTInstances3 extends IdTInstances4 {
  given catsDataApplicativeForIdT[F[_]](using F: Applicative[F]): Applicative[IdT[F, *]] =
    new IdTApplicative[F] { given F0: Applicative[F] = F }
}

sealed abstract private[data] class IdTInstances2 extends IdTInstances3 {
  given catsDataFlatMapForIdT[F[_]](using F: FlatMap[F]): FlatMap[IdT[F, *]] =
    new IdTFlatMap[F] { given F0: FlatMap[F] = F }
}

sealed abstract private[data] class IdTInstances1 extends IdTInstances2 {
  given catsDataMonadForIdT[F[_]](using F: Monad[F]): Monad[IdT[F, *]] =
    new IdTMonad[F] { given F0: Monad[F] = F }

  given catsDataFoldableForIdT[F[_]](using F: Foldable[F]): Foldable[IdT[F, *]] =
    new IdTFoldable[F] { given F0: Foldable[F] = F }
}

sealed abstract private[data] class IdTInstances0 extends IdTInstances1 {

  given catsDataTraverseForIdT[F[_]](using F: Traverse[F]): Traverse[IdT[F, *]] =
    new IdTTraverse[F] { given F0: Traverse[F] = F }

  given catsDataEqForIdT[F[_], A](using F: Eq[F[A]]): Eq[IdT[F, A]] =
    Eq.by[IdT[F, A], F[A]](_.value)
}

sealed abstract private[data] class IdTInstances extends IdTInstances0 {

  given catsDataNonEmptyTraverseForIdT[F[_]](using F: NonEmptyTraverse[F]): NonEmptyTraverse[IdT[F, *]] =
    new IdTNonEmptyTraverse[F] { given F0: NonEmptyTraverse[F] = F }

  given catsDataOrderForIdT[F[_], A](using F: Order[F[A]]): Order[IdT[F, A]] =
    Order.by[IdT[F, A], F[A]](_.value)

  given catsDataShowForIdT[F[_], A](using F: Show[F[A]]): Show[IdT[F, A]] =
    Contravariant[Show].contramap(F)(_.value)

}
