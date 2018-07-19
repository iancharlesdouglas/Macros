package json.reader
import json.{JsonBoolean, JsonElement, JsonNull, JsonNumber}
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
      json.charAt(position) match {
        case '"' if !beyondIdentifier => readBody(json, position + 1, identifier, true)
        case ':' if beyondIdentifier => readBody(json, position + 1, identifier, true, true)
        case chr if !beyondColon && whitespace(chr) => readBody(json, position + 1, identifier, beyondIdentifier)
        case chr if beyondColon && !whitespace(chr) =>
          chr match {
            case '"' => new StrReader().read(json, position + 1, stringFromChars(identifier))
            case '{' => new ObjectReader().read(json, position + 1, stringFromChars(identifier))
            case 't' if json.length > position + 3 && json.substring(position, position + 4) == "true" =>
              (new JsonBoolean(stringFromChars(identifier), true), position + 4)
            case 'f' if json.length > position + 4 && json.substring(position, position + 5) == "false" =>
              (new JsonBoolean(stringFromChars(identifier), false), position + 5)
            case 'n' if json.length > position + 3 && json.substring(position, position + 4) == "null" =>
              (new JsonNull(stringFromChars(identifier)), position + 4)
            case '[' => new ArrayReader().read(json, position + 1, stringFromChars(identifier))
            case _ => new NumberReader().read(json, position, stringFromChars(identifier))
          }
        case chr if whitespace(chr) => readBody(json, position + 1, identifier, beyondIdentifier, beyondColon)
        case chr =>
          identifier += chr
          readBody(json, position + 1, identifier)
      }
    }
  }
}