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
package laws
package discipline
import kernel.compat.scalaVersionSpecific._
import cats.data.NonEmptyList.ZipNonEmptyList
import cats.data.NonEmptyVector.ZipNonEmptyVector

import scala.util.{Failure, Success, Try}
import scala.collection.immutable.{Seq, SortedMap, SortedSet}
import cats.data._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary}

/**
 * Arbitrary instances for cats.data
 */
@suppressUnusedImportWarningForScalaVersionSpecific
object arbitrary extends ArbitraryInstances0 with ScalaVersionSpecific.ArbitraryInstances {

  // this instance is not available in ScalaCheck 1.13.2.
  // remove this once a newer version is available.
  given catsLawsCogenForThrowable: Cogen[Throwable] =
    Cogen[String].contramap(_.toString)

  // this instance is not available in ScalaCheck 1.13.2.
  // remove this once a newer version is available.
  given catsLawsCogenForTry[A](using A: Cogen[A]): Cogen[Try[A]] =
    Cogen((seed, x) =>
      x match {
        case Success(a) => A.perturb(seed, a)
        case Failure(e) => Cogen[Throwable].perturb(seed, e)
      }
    )

  // this instance is not available in ScalaCheck 1.13.2.
  // remove this once a newer version is available.
  given catsLawsCogenForFunction0[A](using A: Cogen[A]): Cogen[Function0[A]] =
    A.contramap(_())

  given catsLawsArbitraryForConst[A, B](using A: Arbitrary[A]): Arbitrary[Const[A, B]] =
    Arbitrary(A.arbitrary.map(Const[A, B]))

  given catsLawsCogenForConst[A, B](using A: Cogen[A]): Cogen[Const[A, B]] =
    A.contramap(_.getConst)

  given catsLawsArbitraryForOneAnd[F[_], A](using
    A: Arbitrary[A],
    F: Arbitrary[F[A]]
  ): Arbitrary[OneAnd[F, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => A.arbitrary.map(a => OneAnd(a, fa))))

  given catsLawsCogenForOneAnd[F[_], A](using A: Cogen[A], F: Cogen[F[A]]): Cogen[OneAnd[F, A]] =
    Cogen((seed, x) => F.perturb(A.perturb(seed, x.head), x.tail))

  given catsLawsArbitraryForNonEmptySeq[A](using A: Arbitrary[A]): Arbitrary[NonEmptySeq[A]] =
    Arbitrary(implicitly[Arbitrary[Seq[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptySeq(a, fa))))

  given catsLawsCogenForNonEmptySeq[A](using A: Cogen[A]): Cogen[NonEmptySeq[A]] =
    Cogen[Seq[A]].contramap(_.toSeq)

  given catsLawsArbitraryForNonEmptyVector[A](using A: Arbitrary[A]): Arbitrary[NonEmptyVector[A]] =
    Arbitrary(implicitly[Arbitrary[Vector[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptyVector(a, fa))))

  given catsLawsCogenForNonEmptyVector[A](using A: Cogen[A]): Cogen[NonEmptyVector[A]] =
    Cogen[Vector[A]].contramap(_.toVector)

  given catsLawsArbitraryForNonEmptySet[A: Order](using A: Arbitrary[A]): Arbitrary[NonEmptySet[A]] =
    Arbitrary(implicitly[Arbitrary[SortedSet[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptySet(a, fa))))

  given catsLawsCogenForNonEmptySet[A: Order: Cogen]: Cogen[NonEmptySet[A]] =
    Cogen[SortedSet[A]].contramap(_.toSortedSet)

  given catsLawsArbitraryForZipSeq[A](using A: Arbitrary[A]): Arbitrary[ZipSeq[A]] =
    Arbitrary(implicitly[Arbitrary[Seq[A]]].arbitrary.map(v => new ZipSeq(v)))

  given catsLawsArbitraryForZipVector[A](using A: Arbitrary[A]): Arbitrary[ZipVector[A]] =
    Arbitrary(implicitly[Arbitrary[Vector[A]]].arbitrary.map(v => new ZipVector(v)))

  given catsLawsArbitraryForZipList[A](using A: Arbitrary[A]): Arbitrary[ZipList[A]] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary.map(v => new ZipList(v)))

  given catsLawsArbitraryForZipNonEmptyVector[A](using A: Arbitrary[A]): Arbitrary[ZipNonEmptyVector[A]] =
    Arbitrary(implicitly[Arbitrary[NonEmptyVector[A]]].arbitrary.map(nev => new ZipNonEmptyVector(nev)))

  given catsLawsArbitraryForNonEmptyList[A](using A: Arbitrary[A]): Arbitrary[NonEmptyList[A]] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptyList(a, fa))))

  given catsLawsCogenForNonEmptyList[A](using A: Cogen[A]): Cogen[NonEmptyList[A]] =
    Cogen[List[A]].contramap(_.toList)

  given catsLawsArbitraryForNonEmptyChain[A](using A: Arbitrary[A]): Arbitrary[NonEmptyChain[A]] =
    Arbitrary(implicitly[Arbitrary[Chain[A]]].arbitrary.flatMap { chain =>
      NonEmptyChain.fromChain(chain) match {
        case None     => A.arbitrary.map(NonEmptyChain.one)
        case Some(ne) => Gen.const(ne)
      }
    })

  given catsLawsCogenForNonEmptyChain[A](using A: Cogen[A]): Cogen[NonEmptyChain[A]] =
    Cogen[Chain[A]].contramap(_.toChain)

  given catsLawsArbitraryForZipNonEmptyList[A](using A: Arbitrary[A]): Arbitrary[ZipNonEmptyList[A]] =
    Arbitrary(implicitly[Arbitrary[NonEmptyList[A]]].arbitrary.map(nel => new ZipNonEmptyList(nel)))

  given arbNonEmptyMap[K: Order, A](using A: Arbitrary[A], K: Arbitrary[K]): Arbitrary[NonEmptyMap[K, A]] =
    Arbitrary(for {
      fa <- implicitly[Arbitrary[SortedMap[K, A]]].arbitrary
      k <- K.arbitrary
      a <- A.arbitrary
    } yield NonEmptyMap((k, a), fa))

  @deprecated("Preserved for bincompat", "2.9.0")
  def cogenNonEmptyMap[K, A](kOrder: Order[K],
                             kCogen: Cogen[K],
                             aOrder: Order[A],
                             aCogen: Cogen[A]
  ): Cogen[NonEmptyMap[K, A]] = {
    given orderingK: Order[K] = kOrder
    given cogenK: Cogen[K] = kCogen
    given cogenA: Cogen[A] = aCogen

    cogenNonEmptyMap[K, A]
  }

  given cogenNonEmptyMap[K: Order: Cogen, A: Cogen]: Cogen[NonEmptyMap[K, A]] =
    Cogen[SortedMap[K, A]].contramap(_.toSortedMap)

  given catsLawsArbitraryForEitherT[F[_], A, B](using
    F: Arbitrary[F[Either[A, B]]]
  ): Arbitrary[EitherT[F, A, B]] =
    Arbitrary(F.arbitrary.map(EitherT(_)))

  given catsLawsCogenForEitherT[F[_], A, B](using F: Cogen[F[Either[A, B]]]): Cogen[EitherT[F, A, B]] =
    F.contramap(_.value)

  given catsLawsArbitraryForValidated[A, B](using
    A: Arbitrary[A],
    B: Arbitrary[B]
  ): Arbitrary[Validated[A, B]] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Validated.invalid), B.arbitrary.map(Validated.valid)))

  given catsLawsCogenForValidated[A, B](using A: Cogen[A], B: Cogen[B]): Cogen[Validated[A, B]] =
    Cogen((seed, x) => x.fold(A.perturb(seed, _), B.perturb(seed, _)))

  given catsLawsArbitraryForIor[A, B](using A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Ior B] =
    Arbitrary(
      Gen.oneOf(A.arbitrary.map(Ior.left),
                B.arbitrary.map(Ior.right),
                for {
                  a <- A.arbitrary; b <- B.arbitrary
                } yield Ior.both(a, b)
      )
    )

  given catsLawsCogenForIor[A, B](using A: Cogen[A], B: Cogen[B]): Cogen[A Ior B] =
    Cogen((seed, x) => x.fold(A.perturb(seed, _), B.perturb(seed, _), (a, b) => A.perturb(B.perturb(seed, b), a)))

  given catsLawsArbitraryForIorT[F[_], A, B](using F: Arbitrary[F[Ior[A, B]]]): Arbitrary[IorT[F, A, B]] =
    Arbitrary(F.arbitrary.map(IorT(_)))

  given catsLawsCogenForIorT[F[_], A, B](using F: Cogen[F[Ior[A, B]]]): Cogen[IorT[F, A, B]] =
    F.contramap(_.value)

  given catsLawsArbitraryForOptionT[F[_], A](using F: Arbitrary[F[Option[A]]]): Arbitrary[OptionT[F, A]] =
    Arbitrary(F.arbitrary.map(OptionT.apply))

  given catsLawsCogenForOptionT[F[_], A](using F: Cogen[F[Option[A]]]): Cogen[OptionT[F, A]] =
    F.contramap(_.value)

  given catsLawsArbitraryForRepresentableStoreT[W[_], F[_], S, A](using
    W: Arbitrary[W[F[A]]],
    S: Arbitrary[S],
    F: Representable.Aux[F, S]
  ): Arbitrary[RepresentableStoreT[W, F, S, A]] =
    Arbitrary(
      for {
        runF <- W.arbitrary
        index <- S.arbitrary
      } yield RepresentableStoreT(runF, index)
    )

  given catsLawsCogenForRepresentableStoreT[W[_], F[_], S, A](using
    W: Cogen[W[F[A]]],
    S: Cogen[S]
  ): Cogen[RepresentableStoreT[W, F, S, A]] =
    Cogen((seed, st) => S.perturb(W.perturb(seed, st.runF), st.index))

  given catsLawsArbitraryForIdT[F[_], A](using F: Arbitrary[F[A]]): Arbitrary[IdT[F, A]] =
    Arbitrary(F.arbitrary.map(IdT.apply))

  given catsLawsCogenForIdT[F[_], A](using F: Cogen[F[A]]): Cogen[IdT[F, A]] =
    F.contramap(_.value)

  given catsLawsArbitraryForEval[A: Arbitrary]: Arbitrary[Eval[A]] =
    Arbitrary(
      Gen.oneOf(getArbitrary[A].map(a => Eval.now(a)),
                getArbitrary[() => A].map(f => Eval.later(f())),
                getArbitrary[() => A].map(f => Eval.always(f()))
      )
    )

  given catsLawsCogenForEval[A: Cogen]: Cogen[Eval[A]] =
    Cogen[A].contramap(_.value)

  given catsLawsArbitraryForTuple2K[F[_], G[_], A](using
    F: Arbitrary[F[A]],
    G: Arbitrary[G[A]]
  ): Arbitrary[Tuple2K[F, G, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => G.arbitrary.map(ga => Tuple2K[F, G, A](fa, ga))))

  given catsLawsArbitraryForFunc[F[_], A, B](using
    AA: Arbitrary[A],
    CA: Cogen[A],
    F: Arbitrary[F[B]]
  ): Arbitrary[Func[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => F[B]].map(Func.func))

  given catsLawsArbitraryForAppFunc[F[_], A, B](using
    AA: Arbitrary[A],
    CA: Cogen[A],
    F: Arbitrary[F[B]],
    FF: Applicative[F]
  ): Arbitrary[AppFunc[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => F[B]].map(Func.appFunc(_)))

  given catsLawsArbitraryForWriter[L: Arbitrary, V: Arbitrary]: Arbitrary[Writer[L, V]] =
    catsLawsArbitraryForWriterT[Id, L, V]

  given catsLawsCogenForWriter[L: Cogen, V: Cogen]: Cogen[Writer[L, V]] =
    Cogen[(L, V)].contramap(_.run)

  // until this is provided by ScalaCheck
  given catsLawsArbitraryForPartialFunction[A, B](using
    F: Arbitrary[A => Option[B]]
  ): Arbitrary[PartialFunction[A, B]] =
    Arbitrary(F.arbitrary.map(Function.unlift))

  given catsLawsArbitraryForEitherK[F[_], G[_], A](using
    F: Arbitrary[F[A]],
    G: Arbitrary[G[A]]
  ): Arbitrary[EitherK[F, G, A]] =
    Arbitrary(Gen.oneOf(F.arbitrary.map(EitherK.leftc[F, G, A]), G.arbitrary.map(EitherK.rightc[F, G, A])))

  given catsLawsCogenForEitherK[F[_], G[_], A](using
    F: Cogen[F[A]],
    G: Cogen[G[A]]
  ): Cogen[EitherK[F, G, A]] =
    Cogen((seed, x) => x.run.fold(F.perturb(seed, _), G.perturb(seed, _)))

  given catLawsCogenForTuple2K[F[_], G[_], A](using F: Cogen[F[A]], G: Cogen[G[A]]): Cogen[Tuple2K[F, G, A]] =
    Cogen((seed, t) => F.perturb(G.perturb(seed, t.second), t.first))

  given catsLawsArbitraryForShow[A: Arbitrary]: Arbitrary[Show[A]] =
    Arbitrary(Show.fromToString[A])

  given catsLawsArbitraryForFn0[A: Arbitrary]: Arbitrary[() => A] =
    Arbitrary(getArbitrary[A].map(() => _))

  // TODO: we should probably be using Cogen for generating Eq, Order,
  // etc. however, we'd still have to ensure that (x.## == y.##)
  // implies equal, in order to avoid producing invalid instances.

  given catsLawsArbitraryForEq[A: Arbitrary]: Arbitrary[Eq[A]] =
    Arbitrary(getArbitrary[Int => Int].map(f => Eq.by(x => f(x.##))))

  given catsLawsArbitraryForEquiv[A: Arbitrary]: Arbitrary[Equiv[A]] =
    Arbitrary(getArbitrary[Eq[A]].map(Eq.catsKernelEquivForEq(using _)))

  given catsLawsArbitraryForPartialOrder[A: Arbitrary]: Arbitrary[PartialOrder[A]] =
    Arbitrary(getArbitrary[Int => Double].map(f => PartialOrder.by(x => f(x.##))))

  given catsLawsArbitraryForPartialOrdering[A: Arbitrary]: Arbitrary[PartialOrdering[A]] =
    Arbitrary(getArbitrary[PartialOrder[A]].map(PartialOrder.catsKernelPartialOrderingForPartialOrder(using _)))

  given catsLawsArbitraryForOrder[A: Arbitrary]: Arbitrary[Order[A]] =
    Arbitrary(getArbitrary[Int => Int].map(f => Order.by(x => f(x.##))))

  given catsLawsArbitraryForSortedMap[K: Arbitrary: Order, V: Arbitrary]: Arbitrary[SortedMap[K, V]] =
    Arbitrary(getArbitrary[Map[K, V]].map(s => SortedMap.empty[K, V](implicitly[Order[K]].toOrdering) ++ s))

  @deprecated("Preserved for bincompat", "2.9.0")
  def catsLawsCogenForSortedMap[K, V](kOrder: Order[K],
                                      kCogen: Cogen[K],
                                      vOrder: Order[V],
                                      vCogen: Cogen[V]
  ): Cogen[SortedMap[K, V]] = {
    given orderingK: Order[K] = kOrder
    given cogenK: Cogen[K] = kCogen
    given cogenA: Cogen[V] = vCogen

    catsLawsCogenForSortedMap[K, V]
  }

  given catsLawsCogenForSortedMap[K: Order: Cogen, V: Cogen]: Cogen[SortedMap[K, V]] = {
    given orderingK: Ordering[K] = Order[K].toOrdering

    implicitly[Cogen[Map[K, V]]].contramap(_.toMap)
  }

  given catsLawsArbitraryForSortedSet[A: Arbitrary: Order]: Arbitrary[SortedSet[A]] =
    Arbitrary(getArbitrary[Set[A]].map(s => SortedSet.empty[A](implicitly[Order[A]].toOrdering) ++ s))

  given catsLawsCogenForSortedSet[A: Order: Cogen]: Cogen[SortedSet[A]] = {
    given orderingA: Ordering[A] = Order[A].toOrdering

    implicitly[Cogen[Set[A]]].contramap(_.toSet)
  }

  given catsLawsArbitraryForOrdering[A: Arbitrary]: Arbitrary[Ordering[A]] =
    Arbitrary(getArbitrary[Order[A]].map(Order.catsKernelOrderingForOrder(using _)))

  given catsLawsArbitraryForHash[A: Hash]: Arbitrary[Hash[A]] =
    Arbitrary(Hash.fromUniversalHashCode[A])

  given catsLawsArbitraryForNested[F[_], G[_], A](using FG: Arbitrary[F[G[A]]]): Arbitrary[Nested[F, G, A]] =
    Arbitrary(FG.arbitrary.map(Nested(_)))

  given catsLawsArbitraryForBinested[F[_, _], G[_], H[_], A, B](using
    F: Arbitrary[F[G[A], H[B]]]
  ): Arbitrary[Binested[F, G, H, A, B]] =
    Arbitrary(F.arbitrary.map(Binested(_)))

  given catsLawArbitraryForState[S: Arbitrary: Cogen, A: Arbitrary]: Arbitrary[State[S, A]] =
    catsLawArbitraryForIndexedStateT[Eval, S, S, A]

  given catsLawArbitraryForReader[A: Arbitrary: Cogen, B: Arbitrary]: Arbitrary[Reader[A, B]] =
    catsLawsArbitraryForKleisli[Id, A, B]

  given catsLawArbitraryForCokleisliId[A: Arbitrary: Cogen, B: Arbitrary]: Arbitrary[Cokleisli[Id, A, B]] =
    catsLawsArbitraryForCokleisli[Id, A, B]

  given catsLawsArbitraryForOp[Arr[_, _], A, B](using Arr: Arbitrary[Arr[B, A]]): Arbitrary[Op[Arr, A, B]] =
    Arbitrary(Arr.arbitrary.map(Op(_)))

  given catsLawsCogenForOp[Arr[_, _], A, B](using Arr: Cogen[Arr[B, A]]): Cogen[Op[Arr, A, B]] =
    Arr.contramap(_.run)

  given catsLawsArbitraryForIRWST[F[_]: Applicative, E, L, SA, SB, A](using
    F: Arbitrary[(E, SA) => F[(L, SB, A)]]
  ): Arbitrary[IndexedReaderWriterStateT[F, E, L, SA, SB, A]] =
    Arbitrary(F.arbitrary.map(IndexedReaderWriterStateT(_)))

  given catsLawsArbitraryForRepresentableStore[F[_], S, A](using
    R: Representable.Aux[F, S],
    ArbS: Arbitrary[S],
    ArbFA: Arbitrary[F[A]]
  ): Arbitrary[RepresentableStore[F, S, A]] =
    Arbitrary {
      for {
        fa <- ArbFA.arbitrary
        s <- ArbS.arbitrary
      } yield {
        RepresentableStore[F, S, A](fa, s)
      }
    }

  given catsLawsCogenForRepresentableStore[F[_]: Representable, S, A](using
    CA: Cogen[A]
  ): Cogen[RepresentableStore[F, S, A]] =
    CA.contramap(_.extract)

  given catsLawsArbitraryForAndThen[A, B](using F: Arbitrary[A => B]): Arbitrary[AndThen[A, B]] =
    Arbitrary(F.arbitrary.map(AndThen(_)))

  given catsLawsCogenForAndThen[A, B](using F: Cogen[A => B]): Cogen[AndThen[A, B]] =
    Cogen((seed, x) => F.perturb(seed, x))

  given catsLawsArbitraryForChain[A](using A: Arbitrary[A]): Arbitrary[Chain[A]] = {
    val genA = A.arbitrary

    def genSize(sz: Int): Gen[Chain[A]] = {
      val fromSeq = Gen.listOfN(sz, genA).map(Chain.fromSeq)
      val recursive =
        sz match {
          case 0 => Gen.const(Chain.nil)
          case 1 => genA.map(Chain.one)
          case n =>
            // Here we concat two chains
            for {
              n0 <- Gen.choose(1, n - 1)
              n1 = n - n0
              left <- genSize(n0)
              right <- genSize(n1)
            } yield left ++ right
        }

      // prefer to generate recursively built Chains
      // but sometimes create fromSeq
      Gen.frequency((5, recursive), (1, fromSeq))
    }

    Arbitrary(Gen.sized(genSize))
  }

  given catsLawsCogenForChain[A](using A: Cogen[A]): Cogen[Chain[A]] =
    Cogen[List[A]].contramap(_.toList)

  given catsLawsCogenForMiniInt: Cogen[MiniInt] =
    Cogen[Int].contramap(_.toInt)

  given catsLawsArbitraryForMiniInt: Arbitrary[MiniInt] =
    Arbitrary(Gen.oneOf(MiniInt.allValues))
}

sealed private[discipline] trait ArbitraryInstances0 {

  given catsLawArbitraryForIndexedStateT[F[_], SA, SB, A](using
    F: Arbitrary[F[SA => F[(SB, A)]]]
  ): Arbitrary[IndexedStateT[F, SA, SB, A]] =
    Arbitrary(F.arbitrary.map(IndexedStateT.applyF))

  given catsLawsArbitraryForWriterT[F[_], L, V](using F: Arbitrary[F[(L, V)]]): Arbitrary[WriterT[F, L, V]] =
    Arbitrary(F.arbitrary.map(WriterT(_)))

  given catsLawsCogenForWriterT[F[_], L, V](using F: Cogen[F[(L, V)]]): Cogen[WriterT[F, L, V]] =
    F.contramap(_.run)

  given catsLawsArbitraryForKleisli[F[_], A, B](using
    AA: Arbitrary[A],
    CA: Cogen[A],
    F: Arbitrary[F[B]]
  ): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => F[B]].map(Kleisli(_)))

  given catsLawsArbitraryForCokleisli[F[_], A, B](using
    AFA: Arbitrary[F[A]],
    CFA: Cogen[F[A]],
    B: Arbitrary[B]
  ): Arbitrary[Cokleisli[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[F[A] => B].map(Cokleisli(_)))
}
