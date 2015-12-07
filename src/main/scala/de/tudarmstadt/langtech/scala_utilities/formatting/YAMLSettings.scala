package de.tudarmstadt.langtech.scala_utilities.formatting

import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream
import java.io.FileNotFoundException

/** Provides simple read method for the given yaml settings file. */
class YamlSettings(val settingsFile: String) {
  val settings = new Yaml().load(new FileInputStream(settingsFile))
  
  /** reads a setting from the settings yaml file with given hierarchical key 
   *  e.g. ("paths", "outputfile") for the YAML document
   *  path:
   *      outputfile: "foo"
   */
  def read[Type](keys: String*): Type = {
    try {
      keys.foldLeft(settings)(_.asInstanceOf[java.util.LinkedHashMap[String, Object]].get(_))
      .asInstanceOf[Type]
    }
    catch {
      case e: NullPointerException => 
        throw new IllegalArgumentException("Settings file %s does not contain keys %s".format(settingsFile, keys.mkString("->")))
    }
  }
  
  /** reads a string from the settings file, and makes sure that it is a valid path to a file or folder */
  def path(keys: String*): String = {
    val pathString = read[String](keys :_*)
    if(!new java.io.File(pathString).exists()){
      //throw new FileNotFoundException("Could not access " + pathString + " specified in " + settingsFile)
      System.err.println("WARNING: Path " + pathString + " specified in " + settingsFile + " but no file was found.")
    }
    pathString
  }
}