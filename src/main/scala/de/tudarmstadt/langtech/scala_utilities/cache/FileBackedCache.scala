package de.tudarmstadt.langtech.scala_utilities.cache

import scala.collection.mutable.HashMap
import java.io.File
import java.io.IOException
import java.io.ObjectInputStream
import scala.collection.mutable.ListBuffer
import java.util.IllegalFormatException
import java.io.ObjectOutputStream
import java.io.OutputStream


class AppendingObjectOutputStream(out: OutputStream) extends ObjectOutputStream(out) {
  override def writeStreamHeader {
    reset
  }
}

/** A wrapper for a function using a file-backed cache */
class FileBackedCache[In <: java.io.Serializable, Out <: java.io.Serializable](f: In => Out, filename: String) {
  
  val cache = new HashMap[In, Out]
  
  
  val outstream = {
    val file = new File(filename)
    if (!file.exists) {
      System.err.println("Cache file " + filename + " does not exist. Starting with empty cache..")
      file.getParentFile.mkdirs
      file.createNewFile
      new java.io.ObjectOutputStream(new java.io.FileOutputStream(filename, true))
    } 
    else {
      readCacheFromFile
      new AppendingObjectOutputStream(new java.io.FileOutputStream(filename, true))
    }
  }
  
  
  private def writeToCache(in: In, out: Out) {
    outstream.writeObject(in)
    outstream.writeObject(out)
    outstream.flush
    cache.put(in, out)
  }
 
  
  private def readCacheFromFile {
   
    def throwIllegalFormat = throw new RuntimeException("Illegal format in " + filename)
    
    val s = new java.io.ObjectInputStream(new java.io.FileInputStream(filename))
    val result = new ListBuffer[Either[In, Out]]
    var needValue = false
    try {
      while(true) {
        val o = s.readObject
        result += (if(needValue) Right(o.asInstanceOf[Out]) else Left(o.asInstanceOf[In]))
        needValue = !needValue
      }
    }
    catch {
      case x: java.io.EOFException => // done
    }
    finally {
      s.close
      if(needValue) throwIllegalFormat
    }
    result.grouped(2).foreach {
      case ListBuffer(Left(k), Right(v)) => cache += ((k, v))
      case _ => throwIllegalFormat
    }
    System.err.println("Read " + cache.size + " entries from cache " + filename)
  }
  
  def apply(in: In): Out = {
    cache.get(in) match {
      case Some(cached) => 
        //System.err.println("Cache-hit: " + in + " -> " + cached)
        cached
      case None =>
        val out = f(in)
        writeToCache(in, out)
        out
    }
  }
  
  def close {
    //System.err.println("Closing cachefile " + filename)
    outstream.close
  }
}

object FileBackedCache {
  
  /** Creates a FileBacked cache which is automatically closed on shutdown */
  def apply[In <: java.io.Serializable, Out <: java.io.Serializable](f: In => Out, filename: String): FileBackedCache[In, Out] = {
    val caller = Thread.currentThread
    val cache = new FileBackedCache(f, filename)
    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run {
    	  cache.close
    	  caller.join
      }
    })
    cache
  }
}


object TestFileBackedCache extends App {
  val square: Integer => Integer = x => x * x
  val cachedSquare = FileBackedCache(square, "cache/square.bin")
  println(cachedSquare(3))
  println(cachedSquare(4))
}