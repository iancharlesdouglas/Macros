package json.rdr

import json._
import json.exceptions.{InvalidArrayFormatException, InvalidObjectFormatException}

/**
  * Created by Ian on 11/07/2018.
  */
class ArrayReader extends Reader {

  override def read(json: String, position: Integer = 0, identifier: String = ""): (JsonElement, Integer) =
    readBody(json, position, JsonArray(identifier))

  def readBody(json: String, position: Integer, jsonArray: JsonArray): (JsonArray, Integer) = {

    if (position == json.size)
      (jsonArray, position)
    else {
      val chr = json.charAt(position)
      chr match {
        case c if whitespace(c) => readBody(json, position + 1, jsonArray)
        case ']' => (jsonArray, position + 1)
        case '{' => {
          val (field, newPosition) = new ObjectReader().read(json, position + 1)
          jsonArray.elements += field
          readBody(json, newPosition, jsonArray)
        }
        case '[' => {
          val (array, newPosition) = new ArrayReader().read(json, position + 1)
          jsonArray.elements += array
          readBody(json, newPosition, jsonArray)
        }
        case '"' => {
          val (string, newPosition) = new StrReader().read(json, position + 1)
          jsonArray.elements += string
          readBody(json, newPosition, jsonArray)
        }
        case 't' if json.length > position + 3 && json.substring(position, position + 4) == "true" => {
          jsonArray.elements += new JsonBoolean("", true)
          readBody(json, position + 4, jsonArray)
        }
        case 'f' if json.length > position + 4 && json.substring(position, position + 5) == "false" =>
          (new JsonBoolean(stringFromChars(identifier), false), position + 5)
        case 'n' if json.length > position + 3 && json.substring(position, position + 4) == "null" =>
          (new JsonNull(stringFromChars(identifier)), position + 4)
        case ',' => readBody(json, position + 1, jsonArray)
        case _ => throw new InvalidArrayFormatException(s"At: $position")
      }
      /*if (whitespace(chr))
        readBody(json, position + 1, jsonArray)
      else if (chr == ']')
        (jsonArray, position + 1)
      else if (chr == '{') {
        val (field, newPosition) = new ObjectReader().read(json, position + 1)
        jsonArray.elements += field
        readBody(json, newPosition, jsonArray)
      } else if (chr == '[') {
        val (array, newPosition) = new ArrayReader().read(json, position + 1)
        jsonArray.elements += array
        readBody(json, newPosition, jsonArray)
      } else if (chr == '"') {
        val (string, newPosition) = new StrReader().read(json, position + 1)
        jsonArray.elements += string
        readBody(json, newPosition, jsonArray)
      } else if (chr == ',')
        readBody(json, position + 1, jsonArray)
      else
        throw new InvalidArrayFormatException(s"At: $position")*/
    }
  }
}
