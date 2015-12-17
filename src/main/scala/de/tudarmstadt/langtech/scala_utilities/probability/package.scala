package de.tudarmstadt.langtech.scala_utilities

package object probability {
  
  def uniform[T](elements: Iterable[T]): Iterable[(T, Double)] = {
    val prob = 1d / elements.size
    elements.zip(Iterator.continually(prob).toIterable)
  }
  
}