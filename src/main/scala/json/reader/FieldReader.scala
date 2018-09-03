package json.reader
import json._
import json.exceptions.ReadPastEndOfElementException

import scala.collection.mutable.ListBuffer

/**
  * Created by Ian on 29/06/2018.
  */
class FieldReader extends Reader {

  override def read(json: String, position: Integer = 0, identifier: String = ""): ParseResult =
    readBody(json, position, ListBuffer[Char]())

  def readBody(json: String, position: Integer, identifier: ListBuffer[Char],
               beyondIdentifier: Boolean = false, beyondColon: Boolean = false): ParseResult = {
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
              new ParseResult(new JsonBoolean(stringFromChars(identifier), true), true, position + 4, null)
            case 'f' if json.length > position + 4 && json.substring(position, position + 5) == "false" =>
              new ParseResult(new JsonBoolean(stringFromChars(identifier), value = false),true, position + 5, null)
            case 'n' if json.length > position + 3 && json.substring(position, position + 4) == "null" =>
              new ParseResult(new JsonNull(stringFromChars(identifier)), true, position + 4, null)
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
