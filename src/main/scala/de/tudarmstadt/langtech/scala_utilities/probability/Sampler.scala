package de.tudarmstadt.langtech.scala_utilities.probability

import scala.util.Random
import de.tudarmstadt.langtech.scala_utilities.compute

case class Interval[T](val start: T, val end: T)(implicit num: Numeric[T]){
  def includes(other: T): Boolean = num.lteq(start, other) && num.lteq(other, end)
}

case class Sampler[T](probabilities: Iterable[(T, Double)]) {

  val elementLookup = {
    val (elems, probs) = probabilities.unzip
    val normalized = elems.zip(compute.normalize(probs))
    val sorted = normalized.toList.sortBy(-_._2)
    val sections = sorted.foldLeft(List.empty[(Interval[Double], T)]) {
      case (Nil, (e, prob)) => List((Interval(0d, prob), e))
      case (acc@((Interval(_, lastEnd), _) :: rest), (e, prob)) =>
        (Interval(lastEnd, lastEnd + prob), e) :: acc
    }
    sections.reverse
  }

  def sample(random: Random): T = lookup(random.nextDouble)

  private def lookup(p: Double): T = {
    val (_, e) = elementLookup.find(_._1.includes(p)).getOrElse(throw new IllegalStateException)
    //println("sampling from " + elementLookup +" with " +p +" -> " + e)
    e
  }
}