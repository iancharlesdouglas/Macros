package json.reader

import json._
import json.exceptions.{InvalidArrayFormatException, InvalidObjectFormatException}

/**
  * Created by Ian on 11/07/2018.
  */
class ArrayReader extends Reader {

  override def read(json: String, position: Integer = 0, identifier: String = ""): ElementParseResult =
    readBody(json, position, JsonArray(identifier))

  def readBody(json: String, position: Integer, jsonArray: JsonArray): ElementParseResult = {

    /*if (position  == json.length)
      (jsonArray, position)
    else {*/
      json.charAt(position) match {
        case chr if whitespace(chr) => readBody(json, position + 1, jsonArray)
        case ']' => new ElementParseResult(jsonArray, true, position + 1, null)
        case '{' =>
          val result = new ObjectReader().read(json, position + 1)
          jsonArray.elements += result.it
          readBody(json, result.position, jsonArray)
        case '[' =>
          val result = new ArrayReader().read(json, position + 1)
          jsonArray.elements += result.it
          readBody(json, result.position, jsonArray)
        case '"' =>
          val result = new StrReader().read(json, position + 1)
          jsonArray.elements += result.it
          readBody(json, result.position, jsonArray)
        case 't' if json.length > position + 3 && json.substring(position, position + 4) == "true" =>
          jsonArray.elements += new JsonBoolean("", true)
          readBody(json, position + 4, jsonArray)
        case 'f' if json.length > position + 4 && json.substring(position, position + 5) == "false" =>
          jsonArray.elements += new JsonBoolean("", false)
          readBody(json, position + 5, jsonArray)
        case 'n' if json.length > position + 3 && json.substring(position, position + 4) == "null" =>
          jsonArray.elements += new JsonNull("")
          readBody(json, position + 4, jsonArray)
        case ',' => readBody(json, position + 1, jsonArray)
        case _ =>
          val result = new NumberReader().read(json, position)
          jsonArray.elements += result.it
          readBody(json, result.position, jsonArray)
      }
    //}
  }
}
