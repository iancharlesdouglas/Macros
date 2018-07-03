package json.rdr
import json.exceptions.{InvalidObjectFormatException, JsonException}
import json.{JsonElement, JsonObject}

/**
  * Created by Ian on 29/06/2018.
  */
class ObjectReader extends Reader {

  override def read(json: String, position: Integer = 0, identifier: String = ""): (JsonElement, Integer) =
    readBody(json, position, JsonObject(identifier))

  def readBody(json: String, position: Integer, jsonObject: JsonObject): (JsonObject, Integer) = {

    if (position == json.size)
      (jsonObject, position)
    else {
      val chr = json.charAt(position)
      if (whitespace(chr))
        readBody(json, position + 1, jsonObject)
      else if (chr == '}')
        (jsonObject, position)
      else if (chr == '"') {
        val (field, newPosition) = new FieldReader().read(json, position + 1)
        jsonObject.elements += field
        readBody(json, newPosition, jsonObject)
      } else
        throw new InvalidObjectFormatException(s"At: $position")
    }
  }
}
