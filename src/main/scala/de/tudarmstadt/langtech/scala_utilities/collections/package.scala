package de.tudarmstadt.langtech.scala_utilities
package object collections {

  def to2Tuple[A](list: List[A]): (A, A) = list match {
    case List(a, b) => (a, b)
  }

  def to3Tuple[A](list: List[A]): (A, A, A) = list match {
    case List(a, b, c) => (a, b, c)
  }

  def to4Tuple[A](list: List[A]): (A, A, A, A) = list match {
    case List(a, b, c, d) => (a, b, c, d)
  }

  /** Wraps an integer into an Option, accepting only positive i */
  def optPositive(i: Int): Option[Int] = if (i >= 0) Some(i) else None

  /** Splits a list into sublists based on splitter predicate */
  def sublists[A](xs: List[A])(p: A => Boolean): List[List[A]] =
    if (xs.isEmpty) Nil
    else xs span p match { case (a, b) => a :: sublists(b)(x => !p(x)) }

  /**
   * Splits the given list on the given predicate. Per default includes the splitting element itself
   *  at the END of the previous group.
   */
  def splitOn[A](xs: List[A])(p: A => Boolean, includeSplitter: Boolean = true): List[List[A]] = {
    xs.foldRight(List(Nil: List[A])) { (e, acc) =>
      val pred = p(e)
      if (!pred) (e :: acc.head) :: acc.tail
      else (if (includeSplitter) List(e) else Nil) :: acc
    }.filterNot(_.isEmpty)
  }

  /** Extracts a mapping from a list with a leading group-identifying object separating the groups */
  def groupBySplitter[A](xs: List[A])(p: A => Boolean, acc: Map[A, List[A]] = Map.empty[A, List[A]]): Map[A, List[A]] = xs match {
    // Note: tail recursion is absolutely necessary for this to work with longer lists!
    case Nil => acc
    case group :: rest if p(group) =>
      val (taken, dropped) = rest.span(!p(_))
      groupBySplitter(dropped)(p, acc + ((group, taken)))
    case _ :: rest => groupBySplitter(rest)(p, acc)
  }

  /** @return a finite list based on unfold function f and a seed */
  def unfoldRight[A, B](seed: B)(f: B => Option[(A, B)]): List[A] = f(seed) match {
    case Some((a, b)) => a :: unfoldRight(b)(f)
    case None         => Nil
  }

  def nondeterminism[A, B](f: A => Either[List[A], B])(init: A): List[B] = {
    f(init) match {
      case Right(result) => List(result)
      case Left(options) => options.flatMap(nondeterminism(f))
    }
  }

  
  /** Computes the fixed point of function f given an init element */
  def fix[A](init: A, f: A => List[A]): List[List[A]] = {
    var current = f(init)
    var result = List(current)
    while (current.nonEmpty) {
      current = current.flatMap(f)
      result ::= current
    }
    result
  }

  def removeConsecutiveDupes[T]( // todo: needs tail-recursion
    lst: List[T],
    equal: ((T, T) => Boolean) = ((a: T, b: T) => a == b)): List[T] = {
    lst match {
      case head :: next :: tail if equal(head, next) => removeConsecutiveDupes(next :: tail, equal)
      case head :: tail                              => head :: removeConsecutiveDupes(tail, equal)
      case nil                                       => Nil
    }
  }

  def noNaN(f: Float) = if (f.isNaN) 0f else f

  def finiteNumbers(doubles: Iterable[Double]): Iterable[Double] =
    doubles.filterNot(x => x.isInfinite || x.isInfinite)

  /** Create a map A -> List[B] from the given list */
  def groupLeft[A, B](list: Seq[(A, B)]): Map[A, List[B]] = {
    list.groupBy(_._1).mapValues(_.map(_._2).toList)
  }

  /** Flattens a 1-to-N mapping X -> {Y} in a list representation, to a list of tuples (X,Y) */
  def flatten1toN[X, Y](list: List[(X, List[Y])]): List[(X, Y)] = {
    list.flatMap { case (x, ys) => ys.map(y => (x, y)) }
  }

  /** Flips this multimap around:  Map[A, Seq[B]] => Map[B, List[A]] */
  def reverseMap[A, B](multiMap: Map[A, Seq[B]]): Map[B, List[A]] = {
    val swapped = multiMap.toList.flatMap { case (w, keywords) => keywords.map((_, w)) }
    groupLeft(swapped)
  }

  /** Extracts a fixed-size context window from collection */
  def context[A](left: Int, right: Int)(items: IndexedSeq[A], index: Int): IndexedSeq[Option[A]] = {
    val indices = index - left to index + right
    indices.map(items.lift)
  }
  
  /** Creates folds for n-fold crossvalidation. Yields tuples of (heldOutData, restOfData)  */
  def crossfold[A](items: Seq[A], folds: Int): Iterable[(Seq[A], Seq[A])] = {
    val heldOutSize = items.size / folds
    for(i <- 0 until folds) yield {
      val start = i * heldOutSize
      val end = if(i == folds - 1) items.size else start + heldOutSize // the last fold will 
      val heldOut = items.slice(start, end)
      val rest = items.take(start) ++ items.drop(end)
      //println(s"held out size = ${heldOut.length}, rest = ${rest.length}")
      assert(heldOut.length + rest.length == items.length)
      (heldOut, rest)
    }
  }
  
  /** Creates a held out split on items, based on a percentage split. Yields tpule (heldOutData, restOfData) */
  def holdOut[A](items: Seq[A], holdOutPercentage: Double): (Seq[A], Seq[A]) = {
    val heldOutSize = (items.size * holdOutPercentage).toInt
    items.splitAt(heldOutSize).swap
  }
}