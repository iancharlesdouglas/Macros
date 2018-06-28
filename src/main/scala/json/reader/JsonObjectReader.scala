package json.reader

import json.exceptions.JsonException
import json.{JsonElement, JsonObject}

/**
  * Created by Ian on 27/06/2018.
  */
class JsonObjectReader extends JsonElementReader[JsonObject] {

  override def canRead(json: String): Boolean =
    json != null && !json.isEmpty && nextNonWhitespace(json).getOrElse("").charAt(0) == '{'

  override def read(json: String, objectId: String, parent: JsonElement): JsonObject = {
    val jsonObject = JsonObject(objectId)
    val remainder = """(\s?)([{])(.+)""".r.findFirstMatchIn(json).get.group(3)
    //getBody(remainder)

    val id = extractId(json)
    if (id.isEmpty) throw new JsonException("Identifier for field not found")
    null
  }

  def getBody(json: String, jsonObject: JsonObject): JsonObject = {

    val terminator = """(\s?)([}])(.+)""".r.findFirstMatchIn(json)
    if (terminator.nonEmpty)
      jsonObject
    else {
      val delimiter = """(\s?)(,)(.+)""".r.findFirstMatchIn(json)
      if (delimiter.nonEmpty)
        getBody(delimiter.get.group(3), jsonObject)
      else {
        val reader = readers.filter(_.canRead(json))
        if (reader.isEmpty)
          throw new JsonException("Unparseable body of Json object - beginning " + json)
        else {
          val (element, remainder) = reader.head.read(json, "", jsonObject)
          jsonObject.elements += element
          getBody(remainder, jsonObject)
        }
      }
    }
  }

  def extractId(json: String): Option[String] = {

    val idMatch = """(\s?[{]\s?\")(.+)(\"\s?\:)""".r.findFirstMatchIn(json)

    if (!idMatch.isEmpty)
      Some(idMatch.get.group(2))
    else
      None
  }
 }
