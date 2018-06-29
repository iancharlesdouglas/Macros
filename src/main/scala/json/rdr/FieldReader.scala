package json.rdr
import json.JsonElement
import json.exceptions.JsonException

import scala.collection.mutable.ListBuffer

/**
  * Created by Ian on 29/06/2018.
  */
class FieldReader extends Reader {

  override def read(json: String, position: Integer = 0, identifier: String = ""): (JsonElement, Integer) =
    readBody(json, position, ListBuffer[Char]())

  def readBody(json: String, position: Integer, identifier: ListBuffer[Char],
               beyondIdentifier: Boolean = false, beyondColon: Boolean = false): (JsonElement, Integer) = {
    if (position == json.size)
      throw new JsonException("Read beyond end of input in object")
    else {
      val chr = json.charAt(position)
      if (chr == '"' && !beyondIdentifier)
        readBody(json, position + 1, identifier, true)
      else if (chr == ':' && beyondIdentifier)
        readBody(json, position + 1, identifier, true, true)
      else if (!beyondColon && whitespace(chr))
        readBody(json, position + 1, identifier, beyondIdentifier)
      else if (beyondColon && !whitespace(chr)) {
        chr match {
          case '"' => new StrReader().read(json, position + 1, identifier.toString())
          case _ => readBody(json, position + 1, identifier)    // TODO
        }
      }
      else {
        identifier += chr
        readBody(json, position + 1, identifier)
      }
    }
  }
}
