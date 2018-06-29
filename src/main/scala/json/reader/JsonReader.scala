package json.reader

import json.JsonElement

/**
  * Created by Ian on 28/06/2018.
  */
abstract class JsonReader {

  def canRead(json: String): Boolean

  def read(json: String, id: String, parent: JsonElement, readers: List[JsonReader]): (JsonElement, String)

  //lazy val readers = List(new JsonObjectReader)

  def nextNonWhitespace(json: String):Option[String] = """[^\s]""".r.findFirstIn(json)
}
