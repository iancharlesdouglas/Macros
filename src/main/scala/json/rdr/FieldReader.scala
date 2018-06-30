package json.rdr
import json.JsonElement
import json.exceptions.{JsonException, ReadPastEndOfElementException}

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
      throw new ReadPastEndOfElementException(s"At: $position")
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
          case '"' => new StrReader().read(json, position + 1, idFrom(identifier))
          case '{' => new ObjectReader().read(json, position + 1, idFrom(identifier))
          case _ => readBody(json, position + 1, identifier) // TODO - Booleans, null, number
        }
      } else if (whitespace(chr))
        readBody(json, position + 1, identifier, beyondIdentifier, beyondColon)
      else {
        identifier += chr
        readBody(json, position + 1, identifier)
      }
    }
  }

  private def idFrom(identifier: ListBuffer[Char]): String = identifier.foldLeft("")(_ + _)
}
