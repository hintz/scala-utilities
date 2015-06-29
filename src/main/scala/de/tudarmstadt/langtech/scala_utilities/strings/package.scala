package de.tudarmstadt.langtech.scala_utilities

import java.text.SimpleDateFormat
import java.util.Calendar

package object strings {

  /** Parses simple expressions *LEFT* [Splitter] *RIGHT*
   *  and fails on invalid formats */
  def splitAssign(splitter: Char)(string: String): (String, String) = {
    val splitPos = string.lastIndexOf(splitter)
    if (splitPos < 0)
      throw new IllegalArgumentException("Given string %s can not be split at char '%c'".format(string, splitter))
    val (before, after) = string.splitAt(splitPos)
    (before, after.substring(1))
  }

  def md5Hash(text: String): String = {
    java.security.MessageDigest.getInstance("MD5")
      .digest(text.getBytes()).map(0xFF & _)
      .map { "%02x".format(_) }.foldLeft("") { _ + _ }
  }

  def timestring: String = new SimpleDateFormat("YYYY-MM-dd_kk-mm-ss").format(Calendar.getInstance.getTime)
}