package de.tudarmstadt.langtech.scala_utilities
import java.io.File
import scala.collection.mutable.ListBuffer


package object io {
  
    val FileEncoding = "UTF-8"

    def lines(path: String): Iterator[String] =
      scala.io.Source.fromFile(path)(scala.io.Codec(FileEncoding)).getLines

    def slurp(path: String): String =
      scala.io.Source.fromFile(path, FileEncoding).getLines.mkString("\n")

    def write(file: String, data: String) {
      val stream = new java.io.OutputStreamWriter(new java.io.FileOutputStream(file), FileEncoding)
      stream.write(data)
      stream.close
    }

    def exists(file: String): Boolean = new File(file).exists

    /** More or less a hack. Take raw bytes and try to encode them in default encoding (=UTF-8) */
    def reencode(string: String): String = new String(string.getBytes, FileEncoding)

    /** Serialize an object in file*/
    def serialize(o: Any, file: String) {
      val s = new java.io.ObjectOutputStream(new java.io.FileOutputStream(file))
      s.writeObject(o)
      s.close
    }

    /** Load object of type T from file */
    def deserialize[T](file: String): T = {
      val s = new java.io.ObjectInputStream(new java.io.FileInputStream(file))
      val result = s.readObject.asInstanceOf[T]
      s.close
      result
    }

    /** Serialize a sequence of items of type T */
    def serializeSeq[T](file: String, seq: Iterable[T]) {
      val s = new java.io.ObjectOutputStream(new java.io.FileOutputStream(file))
      for (o <- seq) s.writeObject(o)
      s.close
    }

    def deserializeSeq[T](file: String, max: Int = -1): List[T] = {
      val result = new ListBuffer[T]

      val s = new java.io.ObjectInputStream(new java.io.FileInputStream(file))
      try {
        while (max < 0 || result.size < max)
          result += s.readObject.asInstanceOf[T]
      } catch { case x: java.io.EOFException => }
      s.close
      result.toList
    }

    /** Lazily serialize an object in a file */
    def lazySerialized[T](filecache: String)(creator: => T): T = {
      if (!new File(filecache).exists) {
        val data = creator // fixed point
        serialize(data, filecache)
        data
      } else deserialize(filecache)
    }
  }