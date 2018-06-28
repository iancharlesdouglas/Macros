package json.reader

import json.JsonElement

import scala.util.matching.Regex

/**
  * Created by Ian on 27/06/2018.
  */
abstract class JsonElementReader[T] extends JsonReader {

  def nextNonWhitespace(json: String):Option[String] = """[^\s]""".r.findFirstIn(json)
}
