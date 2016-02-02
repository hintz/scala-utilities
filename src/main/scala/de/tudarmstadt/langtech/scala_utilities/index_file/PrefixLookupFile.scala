package de.tudarmstadt.langtech.scala_utilities.index_file

trait PrefixLookupFile {
  
  /** Yields all lines in this file starting with the given prefix */
  def search(prefix: String): List[String]

}