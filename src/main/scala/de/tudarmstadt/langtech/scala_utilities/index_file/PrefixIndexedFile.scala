package de.tudarmstadt.langtech.scala_utilities.index_file

import java.io.RandomAccessFile
import scala.collection.mutable.HashMap
import java.io.File
import scala.Left
import scala.Right
import de.tudarmstadt.langtech.scala_utilities.io
import java.nio.ByteBuffer


/**
 * A reader for a *SORTED* textfile encoded in UTF-8.
 * Retrieves subset of lines for a given prefix using the search(prefix: String) method.
 * 
 * Internally uses both a lazily generated index as well as fuzzy binary search.
 */
class PrefixIndexedFile(val path: String, val prefixLength: Int = 5, byteAccuracy: Int = 2000) extends PrefixLookupFile {

  // the random access file wrapped by this PrefixIndexFile
  private val file = new RandomAccessFile(path, "r")
  
  /** Fixes behavior of RandomAccessFile::readLine:
   *  1. proper UTF-8 reading! Interprets UTF-8 after reading complete line.
   *  2. leaves the filepointer at the beginning of the next line! */
  private def readline: String = {
    var readData = false
    val bytes = Iterator.continually(file.read).takeWhile(_ match {
      case -1   => false
      case '\n' => readData = true; false
      case '\r' =>
        readData = true;
        // swallow another character, if its \n
        val cur = file.getFilePointer
        if (file.read != '\n') file.seek(cur)
        false
      case byte => 
        readData = true; 
        true
    })
    val byteArray = bytes.map(_.toByte).toArray
    val string = if(readData) new String(byteArray) else null
    string
  }
  
  /** Close this PrefixIndexedFile */
  def close { file.close }

  // lazily loaded index
  protected val index: PrefixFileIndex = {
    val indexpath = path + ".index"
    if (!new File(indexpath).exists) {
      System.err.println("Index file " + indexpath + " does not exist. Creating index..")
      val index: PrefixFileIndex = generatedFixedPrefixIndex
      System.err.println("Writing index " + indexpath)
      new java.io.ObjectOutputStream(new java.io.FileOutputStream(indexpath)).writeObject(index)
      index
    } else {
      val s = new java.io.ObjectInputStream(new java.io.FileInputStream(indexpath))
      s.readObject.asInstanceOf[PrefixFileIndex]
    }
  }

  /** Yields all lines in this file starting with the given prefix */
  def search(prefix: String): List[String] = file synchronized {
    
    val prefixLength = prefix.length
    val (begin, end) = index.search(prefix)

    var (low, high) = (begin, end)
    var lastLow = low
    
    def prefixAt(pos: Long): Option[String] = {
      file.seek(pos)
      readline // read partial line (cut off due to pos in the middle)
      val fullLine = readline // read the next full line
      if(fullLine != null)
        Some(fullLine.take(prefixLength)) 
      else None
    }
    
    // do a binary search for the exact beginning of the prefix
    var delta = 0l // how far we moved the bounds
    do {
      val mid = (low + high) / 2
      val prefixAtMid = prefixAt(mid)
      if(prefixAtMid.isEmpty){ // we've reached the end of file
        delta = 0 // force an abortion of the search
      }
      else if (prefixAtMid.get < prefix) { 
        // binary search: go to upper half
        lastLow = low; low = mid + 1
        delta = low - lastLow
      } else {
        // binary search: go to lower half
        delta = high - mid
        high = mid
      }
    } while (delta > byteAccuracy)
    file.seek(lastLow)

    // create iterator which starts reading from the exact beginning (iterator is lazy and does not yet read)
    val lines = for (
      line <- Iterator.continually(readline)
        .takeWhile(line => line != null && file.getFilePointer <= end)
    ) yield line

    // profiling..
    /*
    val foo = lines.toList
    println("Searching with prefix '%s', resulting in %d lines".format(prefix, foo.length))
    val dropped = foo.dropWhile(!_.startsWith(prefix))
    println(".. dropped left %d lines".format(foo.length - dropped.length))
    val taken = dropped.takeWhile(_.startsWith(prefix))
    println(".. dropped right %d lines".format(dropped.length - taken.length))
    return taken
    */
    
    // extract actual subset
    val cleaned = lines.dropWhile(!_.startsWith(prefix)).takeWhile(_.startsWith(prefix))
    cleaned.toList
  }

  /** Same as *search*, but does not use binary search */
  def naive_search(prefix: String): List[String] = {
    file synchronized {
      val (begin, end) = index.search(prefix)
      file.seek(begin)
      val lines = for (line <- Iterator.continually(readline)
          .takeWhile(line => line != null && file.getFilePointer <= end)) yield line
      
      val cleaned = lines.dropWhile(!_.startsWith(prefix)).takeWhile(_.startsWith(prefix))
      cleaned.toList
    }
  }

  private def generatedFixedPrefixIndex: FixedSizePrefixIndex = {
    val fileLength = file.length
    val fullPrefexIndex = new HashMap[String, Long] // prefix -> beginning
    
    // go to beginning
    var lineBegin = 0l
    file.seek(lineBegin) 
    
    // reporting
    var lastLine = -1l
    val ReportEachMillis = 5000
    var lastReport = System.currentTimeMillis
    
    for (line <- Iterator.continually(readline).takeWhile(_ != null)) {
      
      assert(lineBegin > lastLine)
      
      // reporting
      val now = System.currentTimeMillis()
      if(now - lastReport >= ReportEachMillis){
        val percentage = lineBegin.toFloat * 100 / fileLength
        System.err.println("Indexing %.1f%%".format(percentage))
        lastReport = now
      }

      for (i <- 0 to prefixLength) {
        fullPrefexIndex.getOrElseUpdate(line.take(i + 1), lineBegin)
      }
      
      lastLine = lineBegin
      lineBegin = file.getFilePointer
    }
    new FixedSizePrefixIndex(fullPrefexIndex.toMap, fileLength)
  }
}

/*
object TestPrefixIndexedFile extends App {
  val file = new PrefixIndexedFile("testfile.txt")
  file.search("testword") foreach println
}
*/