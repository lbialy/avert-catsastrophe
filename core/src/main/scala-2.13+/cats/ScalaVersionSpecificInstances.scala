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

import cats.data.{ZipLazyList, ZipStream}
import scala.collection.immutable.ArraySeq

private[cats] trait ScalaVersionSpecificTraverseInstances {
  @deprecated("Use catsTraverseForLazyList", "3.0.0")
  given catsTraverseForStream: Traverse[Stream] = cats.instances.stream.catsStdInstancesForStream

  given catsTraverseForLazyList: Traverse[LazyList] = cats.instances.lazyList.catsStdInstancesForLazyList
  given catsTraverseForArraySeq: Traverse[ArraySeq] = cats.instances.arraySeq.catsStdInstancesForArraySeq
}

private[cats] trait ScalaVersionSpecificShowInstances {
  @deprecated("Use catsShowForLazyList", "3.0.0")
  given catsShowForStream[A: Show]: Show[Stream[A]] = cats.instances.stream.catsStdShowForStream[A]

  given catsShowForLazyList[A: Show]: Show[LazyList[A]] = cats.instances.lazyList.catsStdShowForLazyList[A]
  given catsShowForArraySeq[A: Show]: Show[ArraySeq[A]] = cats.instances.arraySeq.catsStdShowForArraySeq[A]
}

private[cats] trait ScalaVersionSpecificSemigroupalInstances {
  @deprecated("Use catsSemigroupalForLazyList", "3.0.0")
  given catsSemigroupalForStream: Semigroupal[Stream] = cats.instances.stream.catsStdInstancesForStream

  given catsSemigroupalForLazyList: Semigroupal[LazyList] = cats.instances.lazyList.catsStdInstancesForLazyList
  given catsSemigroupalForArraySeq: Semigroupal[ArraySeq] = cats.instances.arraySeq.catsStdInstancesForArraySeq
}

private[cats] trait ScalaVersionSpecificMonoidKInstances {
  @deprecated("Use catsMonoidKForLazyList", "3.0.0")
  given catsMonoidKForStream: MonoidK[Stream] = cats.instances.stream.catsStdInstancesForStream

  given catsMonoidKForLazyList: MonoidK[LazyList] = cats.instances.lazyList.catsStdInstancesForLazyList
  given catsMonoidKForArraySeq: MonoidK[ArraySeq] = cats.instances.arraySeq.catsStdInstancesForArraySeq
}

private[cats] trait ScalaVersionSpecificParallelInstances {
  @deprecated("Use catsStdParallelForZipLazyList", "3.0.0")
  given catsStdParallelForZipStream: Parallel.Aux[Stream, ZipStream] =
    cats.instances.parallel.catsStdParallelForZipStream

  given catsStdParallelForZipLazyList: Parallel.Aux[LazyList, ZipLazyList] =
    cats.instances.lazyList.catsStdParallelForLazyListZipLazyList
}

private[cats] trait ScalaVersionSpecificInvariantInstances {
  @deprecated("Use catsInstancesForLazyList", "3.0.0")
  given catsInstancesForStream: (Monad[Stream] & Alternative[Stream] & CoflatMap[Stream]) =
    cats.instances.stream.catsStdInstancesForStream

  given catsInstancesForLazyList: (Monad[LazyList] & Alternative[LazyList] & CoflatMap[LazyList]) =
    cats.instances.lazyList.catsStdInstancesForLazyList

  given catsInstancesForArraySeq: (Monad[ArraySeq] & Alternative[ArraySeq] & CoflatMap[ArraySeq]) =
    cats.instances.arraySeq.catsStdInstancesForArraySeq
}

private[cats] trait ScalaVersionSpecificTraverseFilterInstances {
  @deprecated("Use catsTraverseFilterForLazyList", "3.0.0")
  given catsTraverseFilterForStream: TraverseFilter[Stream] =
    cats.instances.stream.catsStdTraverseFilterForStream

  given catsTraverseFilterForLazyList: TraverseFilter[LazyList] =
    cats.instances.lazyList.catsStdTraverseFilterForLazyList

  given catsTraverseFilterForArraySeq: TraverseFilter[ArraySeq] =
    cats.instances.arraySeq.catsStdTraverseFilterForArraySeq
}

private[cats] trait ScalaVersionSpecificAlignInstances {
  @deprecated("Use catsAlignForLazyList", "3.0.0")
  given catsAlignForStream: Align[Stream] =
    cats.instances.stream.catsStdInstancesForStream

  given catsAlignForLazyList: Align[LazyList] =
    cats.instances.lazyList.catsStdInstancesForLazyList

  given catsAlignForArraySeq: Align[ArraySeq] =
    cats.instances.arraySeq.catsStdInstancesForArraySeq
}
