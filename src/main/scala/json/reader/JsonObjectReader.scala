package json.reader

import json.exceptions.JsonException
import json.{JsonElement, JsonObject}

/**
  * Created by Ian on 27/06/2018.
  */
class JsonObjectReader extends JsonReader {

  override def canRead(json: String): Boolean =
    json != null && !json.isEmpty && nextNonWhitespace(json).getOrElse("").charAt(0) == '{'

  override def read(json: String, id: String, parent: JsonElement, readers: List[JsonReader]): (JsonObject, String) = {

    val jsonObject = new JsonObject(id)

    val body = """(\s?)([{])(.+)""".r.findFirstMatchIn(json).get.group(3)

    readBodyElement(body, jsonObject, readers)
  }

  def readBodyElement(json: String, jsonObject: JsonObject, readers: List[JsonReader]): (JsonObject, String) = {

    val terminator = """([\s|!{]?)([}]+)(.?)""".r.findFirstMatchIn(json)
    if (terminator.nonEmpty)
      (jsonObject, terminator.get.group(3))
    else {
      val delimiter = """(\s?)(,)(.+)""".r.findFirstMatchIn(json)
      if (delimiter.nonEmpty)
        readBodyElement(delimiter.get.group(3), jsonObject, readers)
      else {
        val reader = readers.filter(_.canRead(json))
        if (reader.isEmpty)
          throw new JsonException(s"Unparseable body of Json object ($jsonObject) beginning: $json")
        else {

          val id = """(\s?[{]\s?\")(.+)(\"\s?\:)(.+)""".r.findFirstMatchIn(json)
          if (id.isEmpty) throw new JsonException(s"Identifier for field in object ($jsonObject) not found")

          val (element, remainder) = reader.head.read(id.get.group(4), id.get.group(2), jsonObject, readers)
          jsonObject.elements += element
          readBodyElement(remainder, jsonObject, readers)
        }
      }
    }
  }
 }
