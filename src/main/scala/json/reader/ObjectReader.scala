package json.reader
import json.exceptions.InvalidObjectFormatException
import json.{ParseResult, JsonElement, JsonObject}

/**
  * Created by Ian on 29/06/2018.
  */
class ObjectReader extends Reader {

  override def read(json: String, position: Integer = 0, identifier: String = ""): ParseResult =
    readBody(json, position, JsonObject(identifier))

  def readBody(json: String, position: Integer, jsonObject: JsonObject): ParseResult = {

    json.charAt(position) match {
      case chr if whitespace(chr) => readBody (json, position + 1, jsonObject)
      case '}' => new ParseResult(jsonObject, true, position + 1, null)
      case '"' =>
        val result = new FieldReader ().read (json, position + 1)
        jsonObject.elements += result.it
        readBody (json, result.position, jsonObject)
      case ',' => readBody (json, position + 1, jsonObject)
      case _ => throw new InvalidObjectFormatException (s"At: $position")
    }
  }
}
