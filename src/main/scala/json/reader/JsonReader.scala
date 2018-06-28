package json.reader

import json.JsonElement

/**
  * Created by Ian on 28/06/2018.
  */
abstract class JsonReader {

  def canRead(json: String): Boolean

  def read(json: String, id: String, parent: JsonElement): (JsonElement, String)

  val readers = List(new JsonObjectReader)
}
