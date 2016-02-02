package de.tudarmstadt.langtech.scala_utilities.index_file

import de.tudarmstadt.langtech.scala_utilities.cache.FileBackedCache

/** Wraps a PrefixIndexedFile with an additional lookup cache */
class CachedPrefixIndexedFile(val path: String, val prefixLength: Int = 5, cachefile: String = null) extends PrefixLookupFile {
  
  val prefixFile = new PrefixIndexedFile(path, prefixLength)
  val cache = FileBackedCache(prefixFile.search, if(cachefile eq null) path + ".cache" else cachefile)
  
  def search(s: String) = cache(s)
}