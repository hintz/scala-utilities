package de.tudarmstadt.langtech.scala_utilities

package object compute {
  
  /** Normalizes a set of positive real numbers */
  def normalize[I <: Iterable[Double]](values: I): I = {
    val sum = values.sum
    if (sum > 0) values.map(v => v / sum).asInstanceOf[I] else values
  }

  /** Normalizes the value of (item, value) tuples */
  def normalizeValues[A](items: Seq[(A, Double)]): Seq[(A, Double)] = {
    val (it, values) = items.unzip
    it.zip(normalize(values))
  }

  /** @return the standard mean of a collection of numbers */
  def mean[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
    val n = ts.size
    if (n > 0) num.toDouble(ts.sum) / n
    else 0d
  }

  /** @return the F-measure (harmonic mean) of two numbers */
  def fmeasure(precision: Double, recall: Double): Double = {
    (2 * recall * precision) / (recall + precision)
  }

  /** Accumulates weights in a List[(A, Weight)] for all equal elements A */
  def accumulateWeights[A](weighted: Seq[(A, Double)]): List[(A, Double)] = {
    collections.groupLeft(weighted).mapValues(_.sum).toList
  }

  /** Create frequency counts.  @param list List containing the types to count */
  def frequency[T](list: Seq[T]): Map[T, Int] = list.groupBy(identity).mapValues(_.size)

  /** Create a frequency distribution map */
  def freqDest[T](list: Seq[T]): Map[T, Double] = frequency(list).mapValues(_.toDouble / list.size)

  /** Compute tf-idf score. @param items list containing lists of items A */
  def tfidf[A](items: Seq[Seq[A]]): Seq[Seq[(A, Double)]] = {
    val termFrequency = frequency(items.map(_.distinct).flatten) // faster, but wrong: frequency(items.flatten)
    val idf = termFrequency.mapValues(f => math.log10(items.size / f.toFloat))
    val tfidf = items.map { it => it.distinct.map(k => (k, frequency(it)(k) * idf(k))) }
    tfidf
  }

  /** Compute tf-idf score and multiply existing weights */
  def wtfidf[A](items: Seq[Seq[(A, Double)]]): Seq[Seq[(A, Double)]] = {
    val termFrequency = frequency(items.map(_.map(_._1).distinct).flatten)
    val idf = termFrequency.mapValues(f => math.log10(items.size / f))
    val tfidf = items.map { it =>
      accumulateWeights(it).map { case (a, f) => (a, f * idf(a)) }
    }
    tfidf
  }

  /** Takes a number of elements from a weighted sequence until max value is reached */
  def takeMaxSum[T](list: List[(T, Double)], max: Double): List[(T, Double)] = {
    val total = list.map(_._2).scan(0d)(_ + _)
    list.zip(total).takeWhile(_._2 < max).map(_._1)
  }

  /** @return the fraction true items in this collection */
  def fraction(collection: Iterable[Boolean]): Double = {
    collection.count(identity) / collection.size.toDouble
  }
}