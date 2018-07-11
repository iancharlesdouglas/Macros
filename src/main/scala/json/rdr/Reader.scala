package json.rdr

import json.JsonElement

import scala.collection.mutable.ListBuffer

/**
  * Created by Ian on 29/06/2018.
  */
abstract class Reader {

  def read(json: String, position: Integer = 0, identifier: String = "") : (JsonElement, Integer)

  def whitespace(what: Char): Boolean = List(' ', '\r', '\n', '\t', '\f').contains(what)

  protected def stringFromChars(charsBuffer: ListBuffer[Char]): String = charsBuffer.foldLeft("")(_ + _)
}
