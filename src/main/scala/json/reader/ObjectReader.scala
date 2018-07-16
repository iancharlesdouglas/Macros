package json.reader
import json.exceptions.InvalidObjectFormatException
import json.{JsonElement, JsonObject}

/**
  * Created by Ian on 29/06/2018.
  */
class ObjectReader extends Reader {

  override def read(json: String, position: Integer = 0, identifier: String = ""): (JsonElement, Integer) =
    readBody(json, position, JsonObject(identifier))

  def readBody(json: String, position: Integer, jsonObject: JsonObject): (JsonObject, Integer) = {

    json.charAt(position) match {
      case chr if whitespace(chr) => readBody (json, position + 1, jsonObject)
      case '}' => (jsonObject, position + 1)
      case '"' =>
        val (field, newPosition) = new FieldReader ().read (json, position + 1)
        jsonObject.elements += field
        readBody (json, newPosition, jsonObject)
      case ',' => readBody (json, position + 1, jsonObject)
      case _ => throw new InvalidObjectFormatException (s"At: $position")
    }
  }
}
