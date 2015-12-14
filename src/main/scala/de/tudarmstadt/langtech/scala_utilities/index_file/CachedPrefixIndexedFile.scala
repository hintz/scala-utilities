package de.tudarmstadt.langtech.scala_utilities.index_file

import de.tudarmstadt.langtech.scala_utilities.cache.FileBackedCache

/** Wraps a PrefixIndexedFile with an additional lookup cache */
class CachedPrefixIndexedFile(val path: String, val prefixLength: Int = 5) {
  
  val prefixFile = new PrefixIndexedFile(path, prefixLength)
  val cache = FileBackedCache(prefixFile.search, path + ".cache")
  
  def search(s: String) = cache(s)
}