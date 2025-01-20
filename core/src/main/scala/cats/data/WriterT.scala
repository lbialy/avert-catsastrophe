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

import cats.Foldable
import cats.kernel.CommutativeMonoid

final case class WriterT[F[_], L, V](run: F[(L, V)]) {

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val writer = WriterT.liftF[Option, List[String], Int](Some(123))
   * scala> writer.tell(List("a","b","c")).tell(List("d","e","f"))
   * res0: WriterT[Option, List[String], Int] = WriterT(Some((List(a, b, c, d, e, f),123)))
   * }}}
   */
  def tell(l: L)(using functorF: Functor[F], semigroupL: Semigroup[L]): WriterT[F, L, V] =
    mapWritten(semigroupL.combine(_, l))

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val writer: WriterT[Option, List[String], Int] = WriterT.liftF(Some(123))
   * scala> writer.tell(List("a","b","c")).written.getOrElse(Nil)
   * res0: List[String] = List(a, b, c)
   * }}}
   */
  def written(using functorF: Functor[F]): F[L] =
    functorF.map(run)(_._1)

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val writer: WriterT[Option, List[String], Int] = WriterT.liftF(Some(123))
   * scala> val wt: WriterT[Option, List[String], Int] = writer.tell(List("error"))
   * res0: WriterT[Option, List[String], Int] = WriterT(Some((List(error),123)))
   *
   * scala> wt.value
   * res1: Option[Int] = Some(123)
   * }}}
   */
  def value(using functorF: Functor[F]): F[V] =
    functorF.map(run)(_._2)

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val writer: WriterT[Option, String, Int] = WriterT.liftF(Some(123))
   * scala> val wt: WriterT[Option, String, Int] = writer.tell("error").tell(" log")
   * res0: WriterT[Option, String, Int] = WriterT(Some((error log,123)))
   *
   * scala> wt.listen
   * res1: WriterT[Option, String, (Int,String)] = WriterT(Some((error log,(123,error log))))
   * }}}
   */
  def listen(using F: Functor[F]): WriterT[F, L, (V, L)] =
    WriterT(F.map(run) { case (l, v) =>
      (l, (v, l))
    })

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val writer: WriterT[Option, String, Int] = WriterT.liftF(Some(123))
   * scala> val wt: WriterT[Option, String, Int] = writer.tell("error")
   * res0: WriterT[Option, String, Int] = WriterT(Some((error,123)))
   *
   * scala> val func = WriterT.liftF[Option, String, Int => List[Int]](Some(i => List(i)))
   * scala> val func2 = func.tell("log")
   *
   * scala> wt.ap(func2)
   * res1: WriterT[Option, String, List[Int]] = WriterT(Some((logerror,List(123))))
   * }}}
   */
  def ap[Z](f: WriterT[F, L, V => Z])(using F: Apply[F], L: Semigroup[L]): WriterT[F, L, Z] =
    WriterT(F.map2(f.run, run) { case ((l1, fvz), (l2, v)) =>
      (L.combine(l1, l2), fvz(v))
    })

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val wr1: WriterT[Option, String, Int] = WriterT.liftF(None)
   * scala> val wr2 = wr1.tell("error")
   * res0: WriterT[Option, String, Int] = WriterT(None)
   *
   * scala> wr2.map(_ * 2)
   * res1: WriterT[Option, String, Int] = WriterT(None)
   *
   * scala> val wr3: WriterT[Option, String, Int] = WriterT.liftF(Some(456))
   * scala> val wr4 = wr3.tell("error")
   * scala> wr4.map(_ * 2)
   * res2: WriterT[Option, String, Int] = WriterT(Some((error,912)))
   * }}}
   */
  def map[Z](fn: V => Z)(using functorF: Functor[F]): WriterT[F, L, Z] =
    WriterT {
      functorF.map(run) { z =>
        (z._1, fn(z._2))
      }
    }

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val wr1: WriterT[Option, String, Int] = WriterT.liftF(Some(123))
   * scala> val wr2 = wr1.tell("log...")
   * scala> wr2.imap(_ * 2)(_ / 2)
   * res0: WriterT[Option, String, Int] = WriterT(Some((log...,246)))
   * }}}
   */
  def imap[Z](f: V => Z)(g: Z => V)(using F: Invariant[F]): WriterT[F, L, Z] =
    WriterT {
      F.imap(run)(z => (z._1, f(z._2)))(z => (z._1, g(z._2)))
    }

  /**
   * Modify the context `F` using transformation `f`.
   *
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.arrow.FunctionK
   * scala> import cats.syntax.all._
   *
   * scala> val optionWriter = WriterT.liftF[Option, String, Int](Some(123)).tell("log")
   * res0: WriterT[Option, String, Int](Some((log,123)))
   *
   * scala> def toList[A](option: Option[A]): List[A] = option.toList
   * scala> val listWriter = optionWriter.mapK(FunctionK.lift(toList _))
   * res1: WriterT[List, String, Int](List((log,123)))
   * }}}
   */
  def mapK[G[_]](f: F ~> G): WriterT[G, L, V] =
    WriterT[G, L, V](f(run))

  def contramap[Z](fn: Z => V)(using F: Contravariant[F]): WriterT[F, L, Z] =
    WriterT {
      F.contramap(run) { z =>
        (z._1, fn(z._2))
      }
    }

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val wr1 = WriterT.liftF[Option, String, Int](Some(123)).tell("error")
   * res0: WriterT[Option, String, Int] = WriterT(Some(error,123))
   * scala> val func = (i:Int) => WriterT.liftF[Option, String, Int](Some(i * 2)).tell(i.show)
   *
   * scala> wr1.flatMap(func)
   * res1: WriterT[Option, String, Int] = WriterT(Some((error123,246)))
   * }}}
   */
  def flatMap[U](f: V => WriterT[F, L, U])(using flatMapF: FlatMap[F], semigroupL: Semigroup[L]): WriterT[F, L, U] =
    WriterT {
      flatMapF.flatMap(run) { lv =>
        flatMapF.map(f(lv._2).run) { lv2 =>
          (semigroupL.combine(lv._1, lv2._1), lv2._2)
        }
      }
    }

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val wr1 = WriterT.liftF[Option, String, Int](Some(123)).tell("quack")
   * res0: WriterT[Option, String, Int] = WriterT(Some(quack,123))
   *
   * scala> wr1.mapBoth((s,i) => (s + " " + s, i * 2))
   * res1: WriterT[Option, String, Int] = WriterT(Some((quack quack,246)))
   * }}}
   */
  def mapBoth[M, U](f: (L, V) => (M, U))(using functorF: Functor[F]): WriterT[F, M, U] =
    WriterT(functorF.map(run)(f.tupled))

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val wr1 = WriterT.liftF[Option, String, Int](Some(123)).tell("456")
   * res0: WriterT[Option, String, Int] = WriterT(Some(456,123))
   *
   * scala> wr1.bimap(_.toInt, _.show)
   * res1: WriterT[Option, Int, String] = WriterT(Some((456,123)))
   * }}}
   */
  def bimap[M, U](f: L => M, g: V => U)(using functorF: Functor[F]): WriterT[F, M, U] =
    mapBoth((l, v) => (f(l), g(v)))

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val writer = WriterT.liftF[Option, String, Int](Some(246)).tell("error")
   * res0: WriterT[Option, String, Int] = WriterT(Some((error,246)))
   *
   * scala> writer.mapWritten(i => List(i))
   * res1: WriterT[Option, List[String], Int] = WriterT(Some((List(error),246)))
   * }}}
   */
  def mapWritten[M](f: L => M)(using functorF: Functor[F]): WriterT[F, M, V] =
    mapBoth((l, v) => (f(l), v))

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val writer = WriterT.liftF[Option, String, Int](Some(123)).tell("log")
   * scala> writer.swap
   * res0: WriterT[Option, Int, String] = WriterT(Some((123,log)))
   * }}}
   */
  def swap(using functorF: Functor[F]): WriterT[F, V, L] =
    mapBoth((l, v) => (v, l))

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val writer = WriterT.liftF[Option, String, Int](Some(123)).tell("error")
   * scala> writer.reset
   * res0: WriterT[Option, String, Int] = WriterT(Some((,123)))
   * }}}
   */
  def reset(using monoidL: Monoid[L], functorF: Functor[F]): WriterT[F, L, V] =
    mapWritten(_ => monoidL.empty)

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val writer = WriterT.liftF[Option, String, Int](Some(456)).tell("log...")
   * scala> writer.show
   * res0: String = Some((log...,456))
   * }}}
   */
  def show(using F: Show[F[(L, V)]]): String = F.show(run)

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val writer = WriterT.liftF[Option, String, Int](Some(123)).tell("hi")
   * scala> writer.foldLeft(456)((acc,v) => acc + v)
   * res0: Int = 579
   * }}}
   */
  def foldLeft[C](c: C)(f: (C, V) => C)(using F: Foldable[F]): C =
    F.foldLeft(run, c)((z, lv) => f(z, lv._2))

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.Eval
   * scala> import cats.syntax.all._
   *
   * scala> val writer = WriterT.liftF[Option, String, Int](Some(123)).tell("hi")
   * scala> writer
   *      |    .foldRight(Eval.now(456))((v,c) => c.map(_ + v))
   *      |    .value
   * res0: Int = 579
   * }}}
   */
  def foldRight[C](lc: Eval[C])(f: (V, Eval[C]) => Eval[C])(using F: Foldable[F]): Eval[C] =
    F.foldRight(run, lc)((lv, z) => f(lv._2, z))

  /**
   * Example:
   * {{{
   * scala> import cats.data.WriterT
   * scala> import cats.syntax.all._
   *
   * scala> val writer = WriterT.liftF[Option, String, Int](Some(123)).tell("hi")
   * scala> writer.traverse[List,Int](i => List(i))
   * res0: List[WriterT[Option, String, Int]] = List(WriterT(Some((hi,123))))
   * }}}
   */
  def traverse[G[_], V1](f: V => G[V1])(using F: Traverse[F], G: Applicative[G]): G[WriterT[F, L, V1]] =
    G.map(
      F.traverse(run)(lv => G.tupleLeft(f(lv._2), lv._1))
    )(WriterT.apply)

  def compare(that: WriterT[F, L, V])(using Ord: Order[F[(L, V)]]): Int =
    Ord.compare(run, that.run)
}

object WriterT extends WriterTInstances with WriterTFunctions with WriterTFunctions0 {

  def liftF[F[_], L, V](fv: F[V])(using monoidL: Monoid[L], F: Applicative[F]): WriterT[F, L, V] =
    WriterT(F.map(fv)(v => (monoidL.empty, v)))

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, *]]
   * scala> val b: OptionT[WriterT[Eval, String, *], Int] = a.mapK(WriterT.liftK)
   * scala> b.value.run.value
   * res0: (String, Option[Int]) = ("",Some(1))
   * }}}
   */
  def liftK[F[_], L](using monoidL: Monoid[L], F: Applicative[F]): F ~> WriterT[F, L, *] =
    new (F ~> WriterT[F, L, *]) { def apply[A](a: F[A]): WriterT[F, L, A] = WriterT.liftF(a) }

  @deprecated("Use liftF instead", "1.0.0-RC2")
  def lift[F[_], L, V](fv: F[V])(using monoidL: Monoid[L], F: Applicative[F]): WriterT[F, L, V] =
    WriterT(F.map(fv)(v => (monoidL.empty, v)))

}

sealed abstract private[data] class WriterTInstances extends WriterTInstances0 {
  given catsDataTraverseForWriterTId[L](using F: Traverse[Id]): Traverse[WriterT[Id, L, *]] =
    catsDataTraverseForWriterT[Id, L](using F)

  given catsDataDeferForWriterT[F[_], L](using F: Defer[F]): Defer[WriterT[F, L, *]] =
    new Defer[WriterT[F, L, *]] {
      def defer[A](fa: => WriterT[F, L, A]): WriterT[F, L, A] =
        WriterT(F.defer(fa.run))
    }
}

sealed abstract private[data] class WriterTInstances0 extends WriterTInstances1 {
  given catsDataCommutativeMonadForWriterT[F[_], L](using
    F: CommutativeMonad[F],
    L: CommutativeMonoid[L]
  ): CommutativeMonad[WriterT[F, L, *]] =
    new WriterTMonad[F, L] with CommutativeMonad[WriterT[F, L, *]] {
      given F0: Monad[F] = F
      given L0: Monoid[L] = L
    }
}

sealed abstract private[data] class WriterTInstances1 extends WriterTInstances2 {

  given catsDataTraverseForWriterT[F[_], L](using F: Traverse[F]): Traverse[WriterT[F, L, *]] =
    new WriterTTraverse[F, L] {
      val F0: Traverse[F] = F
    }

  given catsDataFoldableForWriterTId[L](using F: Foldable[Id]): Foldable[WriterT[Id, L, *]] =
    catsDataFoldableForWriterT[Id, L](using F)

  given catsDataOrderForWriterT[F[_], L, V](using Ord: Order[F[(L, V)]]): Order[WriterT[F, L, V]] =
    _ compare _
}

sealed abstract private[data] class WriterTInstances2 extends WriterTInstances3 {
  given catsDataMonadErrorForWriterT[F[_], L, E](using
    F: MonadError[F, E],
    L: Monoid[L]
  ): MonadError[WriterT[F, L, *], E] =
    new WriterTMonadError[F, L, E] {
      given F0: MonadError[F, E] = F
      given L0: Monoid[L] = L
    }

  given catsDataParallelForWriterT[M[_], L: Monoid](using
    P: Parallel[M]
  ): Parallel.Aux[WriterT[M, L, *], WriterT[P.F, L, *]] =
    new Parallel[WriterT[M, L, *]] {
      type F[x] = WriterT[P.F, L, x]
      given monadM: Monad[M] = P.monad

      def applicative: Applicative[WriterT[P.F, L, *]] = catsDataApplicativeForWriterT(using P.applicative, Monoid[L])
      def monad: Monad[WriterT[M, L, *]] = catsDataMonadForWriterT

      def sequential: WriterT[P.F, L, *] ~> WriterT[M, L, *] =
        new (WriterT[P.F, L, *] ~> WriterT[M, L, *]) {
          def apply[A](wfl: WriterT[P.F, L, A]): WriterT[M, L, A] = WriterT(P.sequential(wfl.run))
        }

      def parallel: WriterT[M, L, *] ~> WriterT[P.F, L, *] =
        new (WriterT[M, L, *] ~> WriterT[P.F, L, *]) {
          def apply[A](wml: WriterT[M, L, A]): WriterT[P.F, L, A] = WriterT(P.parallel(wml.run))
        }
    }

  given catsDataEqForWriterTId[L: Eq, V: Eq]: Eq[WriterT[Id, L, V]] =
    catsDataEqForWriterT[Id, L, V]

  given catsDataBifunctorForWriterT[F[_]: Functor]: Bifunctor[WriterT[F, *, *]] =
    new Bifunctor[WriterT[F, *, *]] {
      def bimap[A, B, C, D](fab: WriterT[F, A, B])(f: A => C, g: B => D): WriterT[F, C, D] =
        fab.bimap(f, g)
    }

  given catsDataShowForWriterT[F[_], L, V](using F: Show[F[(L, V)]]): Show[WriterT[F, L, V]] = _.show

  given catsDataMonoidForWriterTId[L: Monoid, V: Monoid]: Monoid[WriterT[Id, L, V]] =
    catsDataMonoidForWriterT[Id, L, V]

  given catsDataFoldableForWriterT[F[_], L](using F: Foldable[F]): Foldable[WriterT[F, L, *]] =
    new WriterTFoldable[F, L] {
      val F0: Foldable[F] = F
    }
}

sealed abstract private[data] class WriterTInstances3 extends WriterTInstances4 {
  given catsDataMonadForWriterTId[L: Monoid]: Monad[WriterT[Id, L, *]] =
    catsDataMonadForWriterT[Id, L]

  given catsDataEqForWriterT[F[_], L, V](using F: Eq[F[(L, V)]]): Eq[WriterT[F, L, V]] =
    Eq.by[WriterT[F, L, V], F[(L, V)]](_.run)

  given catsDataSemigroupForWriterTId[L: Semigroup, V: Semigroup]: Semigroup[WriterT[Id, L, V]] =
    catsDataSemigroupForWriterT[Id, L, V]

  given catsDataComonadForWriterTId[L](using F: Comonad[Id]): Comonad[WriterT[Id, L, *]] =
    catsDataComonadForWriterT[Id, L](using F)
}

sealed abstract private[data] class WriterTInstances4 extends WriterTInstances5 {
  given catsDataMonadForWriterT[F[_], L](using F: Monad[F], L: Monoid[L]): Monad[WriterT[F, L, *]] =
    new WriterTMonad[F, L] {
      given F0: Monad[F] = F
      given L0: Monoid[L] = L
    }

  given catsDataMonoidForWriterT[F[_], L, V](using W: Monoid[F[(L, V)]]): Monoid[WriterT[F, L, V]] =
    new WriterTMonoid[F, L, V] {
      given F0: Monoid[F[(L, V)]] = W
    }

  given catsDataCoflatMapForWriterTId[L]: CoflatMap[WriterT[Id, L, *]] =
    catsDataCoflatMapForWriterT[Id, L]
}

sealed abstract private[data] class WriterTInstances5 extends WriterTInstances6 {
  given catsDataFlatMapForWriterTId[L: Semigroup]: FlatMap[WriterT[Id, L, *]] =
    catsDataFlatMapForWriterT2[Id, L]
}

sealed abstract private[data] class WriterTInstances6 extends WriterTInstances7 {
  given catsDataFlatMapForWriterT1[F[_], L](using F: FlatMap[F], L: Monoid[L]): FlatMap[WriterT[F, L, *]] =
    new WriterTFlatMap1[F, L] {
      given F0: FlatMap[F] = F
      given L0: Monoid[L] = L
    }

  given catsDataSemigroupForWriterT[F[_], L, V](using W: Semigroup[F[(L, V)]]): Semigroup[WriterT[F, L, V]] =
    new WriterTSemigroup[F, L, V] {
      given F0: Semigroup[F[(L, V)]] = W
    }
}

sealed abstract private[data] class WriterTInstances7 extends WriterTInstances8 {
  given catsDataApplicativeErrorForWriterT[F[_], L, E](using
    F: ApplicativeError[F, E],
    L: Monoid[L]
  ): ApplicativeError[WriterT[F, L, *], E] =
    new WriterTApplicativeError[F, L, E] {
      given F0: ApplicativeError[F, E] = F
      given L0: Monoid[L] = L
    }
}

sealed abstract private[data] class WriterTInstances8 extends WriterTInstances9 {
  given catsDataAlternativeForWriterT[F[_], L](using
    F: Alternative[F],
    L: Monoid[L]
  ): Alternative[WriterT[F, L, *]] =
    new WriterTAlternative[F, L] {
      given F0: Alternative[F] = F
      given L0: Monoid[L] = L
    }

  given catsDataContravariantMonoidalForWriterT[F[_], L](using
    F: ContravariantMonoidal[F]
  ): ContravariantMonoidal[WriterT[F, L, *]] =
    new WriterTContravariantMonoidal[F, L] {
      given F0: ContravariantMonoidal[F] = F
    }
}

sealed abstract private[data] class WriterTInstances9 extends WriterTInstances10 {
  given catsDataMonoidKForWriterT[F[_], L](using F: MonoidK[F]): MonoidK[WriterT[F, L, *]] =
    new WriterTMonoidK[F, L] {
      given F0: MonoidK[F] = F
    }

  given catsDataFlatMapForWriterT2[F[_], L](using F: Monad[F], L: Semigroup[L]): FlatMap[WriterT[F, L, *]] =
    new WriterTFlatMap2[F, L] {
      given F0: Monad[F] = F
      given L0: Semigroup[L] = L
    }

  given catsDataContravariantForWriterT[F[_], L](using F: Contravariant[F]): Contravariant[WriterT[F, L, *]] =
    new WriterTContravariant[F, L] {
      given F0: Contravariant[F] = F
    }
}

sealed abstract private[data] class WriterTInstances10 extends WriterTInstances11 {
  given catsDataSemigroupKForWriterT[F[_], L](using F: SemigroupK[F]): SemigroupK[WriterT[F, L, *]] =
    new WriterTSemigroupK[F, L] {
      given F0: SemigroupK[F] = F
    }

  given catsDataApplicativeForWriterT[F[_], L](using
    F: Applicative[F],
    L: Monoid[L]
  ): Applicative[WriterT[F, L, *]] =
    new WriterTApplicative[F, L] {
      given F0: Applicative[F] = F
      given L0: Monoid[L] = L
    }

  given catsDataInvariantForWriterT[F[_], L](using F0: Invariant[F]): Invariant[WriterT[F, L, *]] =
    new WriterTInvariant[F, L] { given F: Invariant[F] = F0 }
}

sealed abstract private[data] class WriterTInstances11 extends WriterTInstances12 {
  given catsDataApplyForWriterT[F[_], L](using F: Apply[F], L: Semigroup[L]): Apply[WriterT[F, L, *]] =
    new WriterTApply[F, L] {
      given F0: Apply[F] = F
      given L0: Semigroup[L] = L
    }
}

sealed abstract private[data] class WriterTInstances12 extends WriterTInstances13 {
  given catsDataComonadForWriterT[F[_], L](using F: Comonad[F]): Comonad[WriterT[F, L, *]] =
    new WriterTComonad[F, L] {
      given F0: Comonad[F] = F
    }
}

sealed abstract private[data] class WriterTInstances13 {
  given catsDataCoflatMapForWriterT[F[_], L](using F: Functor[F]): CoflatMap[WriterT[F, L, *]] =
    new WriterTCoflatMap[F, L] {
      given F0: Functor[F] = F
    }
}

sealed private[data] trait WriterTFunctor[F[_], L] extends Functor[WriterT[F, L, *]] {
  given F0: Functor[F]

  override def map[A, B](fa: WriterT[F, L, A])(f: A => B): WriterT[F, L, B] =
    fa.map(f)
}

sealed private[data] trait WriterTContravariant[F[_], L] extends Contravariant[WriterT[F, L, *]] {
  given F0: Contravariant[F]

  override def contramap[A, B](fa: WriterT[F, L, A])(f: B => A): WriterT[F, L, B] =
    fa.contramap(f)
}

sealed private[data] trait WriterTInvariant[F[_], L] extends Invariant[WriterT[F, L, *]] {
  given F: Invariant[F]

  override def imap[A, B](fa: WriterT[F, L, A])(f: A => B)(g: B => A): WriterT[F, L, B] =
    fa.imap(f)(g)
}

sealed private[data] trait WriterTApply[F[_], L] extends WriterTFunctor[F, L] with Apply[WriterT[F, L, *]] {
  implicit override def F0: Apply[F]
  given L0: Semigroup[L]

  def ap[A, B](f: WriterT[F, L, A => B])(fa: WriterT[F, L, A]): WriterT[F, L, B] =
    fa.ap(f)

  override def map2Eval[A, B, Z](fa: WriterT[F, L, A], fb: Eval[WriterT[F, L, B]])(
    f: (A, B) => Z
  ): Eval[WriterT[F, L, Z]] =
    F0.map2Eval(fa.run, fb.map(_.run)) { case ((la, a), (lb, b)) => (L0.combine(la, lb), f(a, b)) }
      .map(WriterT(_)) // F0 may have a lazy map2Eval

  override def product[A, B](fa: WriterT[F, L, A], fb: WriterT[F, L, B]): WriterT[F, L, (A, B)] =
    WriterT(F0.map(F0.product(fa.run, fb.run)) { case ((l1, a), (l2, b)) => (L0.combine(l1, l2), (a, b)) })
}

sealed private[data] trait WriterTFlatMap1[F[_], L] extends WriterTApply[F, L] with FlatMap[WriterT[F, L, *]] {
  implicit override def F0: FlatMap[F]
  given L0: Monoid[L]

  override def ap[A, B](f: WriterT[F, L, A => B])(fa: WriterT[F, L, A]): WriterT[F, L, B] =
    super[WriterTApply].ap(f)(fa)

  def flatMap[A, B](fa: WriterT[F, L, A])(f: A => WriterT[F, L, B]): WriterT[F, L, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(fn: A => WriterT[F, L, Either[A, B]]): WriterT[F, L, B] = {

    def step(la: (L, A)): F[Either[(L, A), (L, B)]] = {
      val flv: F[(L, Either[A, B])] = fn(la._2).run
      F0.map(flv) {
        case (l, Left(a)) =>
          val combineL = L0.combine(la._1, l)
          Left((combineL, a))
        case (l, Right(b)) =>
          val combineL = L0.combine(la._1, l)
          Right((combineL, b))
      }
    }

    WriterT(F0.tailRecM((L0.empty, a))(step))
  }
}

sealed private[data] trait WriterTFlatMap2[F[_], L] extends WriterTApply[F, L] with FlatMap[WriterT[F, L, *]] {
  implicit override def F0: Monad[F]
  given L0: Semigroup[L]

  def flatMap[A, B](fa: WriterT[F, L, A])(f: A => WriterT[F, L, B]): WriterT[F, L, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(fn: A => WriterT[F, L, Either[A, B]]): WriterT[F, L, B] = {

    def step(la: (L, A)): F[Either[(L, A), (L, B)]] = {
      val flv: F[(L, Either[A, B])] = fn(la._2).run
      F0.map(flv) {
        case (l, Left(a)) =>
          val combineL = L0.combine(la._1, l)
          Left((combineL, a))
        case (l, Right(b)) =>
          val combineL = L0.combine(la._1, l)
          Right((combineL, b))
      }
    }

    val init = fn(a).run
    val res: F[(L, B)] = F0.flatMap(init) {
      case (l, Right(b)) => F0.pure((l, b))
      case (l, Left(a))  => F0.tailRecM((l, a))(step)
    }
    WriterT(res)
  }
}

sealed private[data] trait WriterTApplicative[F[_], L] extends WriterTApply[F, L] with Applicative[WriterT[F, L, *]] {
  implicit override def F0: Applicative[F]
  implicit override def L0: Monoid[L]

  def pure[A](a: A): WriterT[F, L, A] =
    WriterT.value[F, L, A](a)
}

sealed private[data] trait WriterTMonad[F[_], L]
    extends WriterTApplicative[F, L]
    with WriterTFlatMap1[F, L]
    with Monad[WriterT[F, L, *]] {
  implicit override def F0: Monad[F]
  implicit override def L0: Monoid[L]
}

sealed private[data] trait WriterTApplicativeError[F[_], L, E]
    extends ApplicativeError[WriterT[F, L, *], E]
    with WriterTApplicative[F, L] {
  implicit override def F0: ApplicativeError[F, E]

  def raiseError[A](e: E): WriterT[F, L, A] = WriterT(F0.raiseError[(L, A)](e))

  def handleErrorWith[A](fa: WriterT[F, L, A])(f: E => WriterT[F, L, A]): WriterT[F, L, A] =
    WriterT(F0.handleErrorWith(fa.run)(e => f(e).run))
}

sealed private[data] trait WriterTMonadError[F[_], L, E]
    extends MonadError[WriterT[F, L, *], E]
    with WriterTMonad[F, L]
    with WriterTApplicativeError[F, L, E] {
  implicit override def F0: MonadError[F, E]
}

sealed private[data] trait WriterTSemigroupK[F[_], L] extends SemigroupK[WriterT[F, L, *]] {
  given F0: SemigroupK[F]

  def combineK[A](x: WriterT[F, L, A], y: WriterT[F, L, A]): WriterT[F, L, A] =
    WriterT(F0.combineK(x.run, y.run))

  override def combineKEval[A](x: WriterT[F, L, A], y: Eval[WriterT[F, L, A]]): Eval[WriterT[F, L, A]] =
    F0.combineKEval(x.run, y.map(_.run)).map(WriterT(_))
}

sealed private[data] trait WriterTMonoidK[F[_], L] extends MonoidK[WriterT[F, L, *]] with WriterTSemigroupK[F, L] {
  implicit override def F0: MonoidK[F]

  def empty[A]: WriterT[F, L, A] = WriterT(F0.empty)
}

sealed private[data] trait WriterTAlternative[F[_], L]
    extends Alternative[WriterT[F, L, *]]
    with WriterTMonoidK[F, L]
    with WriterTApplicative[F, L] {
  implicit override def F0: Alternative[F]
}

sealed private[data] trait WriterTContravariantMonoidal[F[_], L] extends ContravariantMonoidal[WriterT[F, L, *]] {
  given F0: ContravariantMonoidal[F]

  override def unit: WriterT[F, L, Unit] = WriterT(F0.trivial[(L, Unit)])

  override def contramap[A, B](fa: WriterT[F, L, A])(f: B => A): WriterT[F, L, B] =
    fa.contramap(f)

  override def product[A, B](fa: WriterT[F, L, A], fb: WriterT[F, L, B]): WriterT[F, L, (A, B)] =
    WriterT(
      F0.contramap(F0.product(fa.run, fb.run))((t: (L, (A, B))) =>
        t match {
          case (l, (a, b)) => ((l, a), (l, b))
        }
      )
    )
}

sealed private[data] trait WriterTSemigroup[F[_], L, A] extends Semigroup[WriterT[F, L, A]] {
  given F0: Semigroup[F[(L, A)]]

  def combine(x: WriterT[F, L, A], y: WriterT[F, L, A]): WriterT[F, L, A] =
    WriterT(F0.combine(x.run, y.run))
}

sealed private[data] trait WriterTMonoid[F[_], L, A] extends Monoid[WriterT[F, L, A]] with WriterTSemigroup[F, L, A] {
  implicit override def F0: Monoid[F[(L, A)]]

  def empty: WriterT[F, L, A] = WriterT(F0.empty)
}

sealed private[data] trait WriterTCoflatMap[F[_], L] extends CoflatMap[WriterT[F, L, *]] with WriterTFunctor[F, L] {

  def coflatMap[A, B](fa: WriterT[F, L, A])(f: WriterT[F, L, A] => B): WriterT[F, L, B] = fa.map(_ => f(fa))
}

sealed private[data] trait WriterTFoldable[F[_], L] extends Foldable[WriterT[F, L, *]] {

  given F0: Foldable[F]

  def foldLeft[A, B](fa: WriterT[F, L, A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
  def foldRight[A, B](fa: WriterT[F, L, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)
}

sealed private[data] trait WriterTTraverse[F[_], L]
    extends Traverse[WriterT[F, L, *]]
    with WriterTFoldable[F, L]
    with WriterTFunctor[F, L] {

  implicit override def F0: Traverse[F]

  def traverse[G[_]: Applicative, A, B](fa: WriterT[F, L, A])(f: A => G[B]): G[WriterT[F, L, B]] = fa.traverse(f)
}

sealed private[data] trait WriterTComonad[F[_], L] extends Comonad[WriterT[F, L, *]] with WriterTCoflatMap[F, L] {

  implicit override def F0: Comonad[F]

  def extract[A](fa: WriterT[F, L, A]): A = F0.extract(F0.map(fa.run)(_._2))
}

// new trait for binary compatibility
private[data] trait WriterTFunctions0 {
  def listen[F[_], L, V](writerTFLV: WriterT[F, L, V])(using functorF: Functor[F]): WriterT[F, L, (V, L)] =
    writerTFLV.listen
}

private[data] trait WriterTFunctions {
  def putT[F[_], L, V](vf: F[V])(l: L)(using functorF: Functor[F]): WriterT[F, L, V] =
    WriterT(functorF.map(vf)(v => (l, v)))

  def put[F[_], L, V](v: V)(l: L)(using applicativeF: Applicative[F]): WriterT[F, L, V] =
    WriterT.putT[F, L, V](applicativeF.pure(v))(l)

  def tell[F[_], L](l: L)(using applicativeF: Applicative[F]): WriterT[F, L, Unit] =
    WriterT.put[F, L, Unit](())(l)

  def value[F[_], L, V](v: V)(using applicativeF: Applicative[F], monoidL: Monoid[L]): WriterT[F, L, V] =
    WriterT.put[F, L, V](v)(monoidL.empty)

  def valueT[F[_], L, V](vf: F[V])(using functorF: Functor[F], monoidL: Monoid[L]): WriterT[F, L, V] =
    WriterT.putT[F, L, V](vf)(monoidL.empty)

  /**
   * Lifts a FunctionK operating on effects to a FunctionK operating on WriterT with these effects.
   */
  def liftFunctionK[F[_], G[_], A](f: F ~> G): WriterT[F, A, *] ~> WriterT[G, A, *] =
    new (WriterT[F, A, *] ~> WriterT[G, A, *]) { def apply[B](k: WriterT[F, A, B]): WriterT[G, A, B] = k.mapK(f) }
}
