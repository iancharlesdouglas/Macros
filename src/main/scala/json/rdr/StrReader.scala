package json.rdr
import json.exceptions.{JsonException, ReadPastEndOfElementException}
import json.{JsonElement, JsonString}

import scala.collection.mutable.ListBuffer

/**
  * Created by Ian on 29/06/2018.
  */
class StrReader extends Reader {

  override def read(json: String, position: Integer, identifier: String): (JsonElement, Integer) =
    readString(json, position, identifier, ListBuffer[Char]())

  def readString(json: String, position: Integer, identifier: String, body: ListBuffer[Char],
                 escaped: Boolean = false): (JsonString, Integer) = {
    if (position == json.size)
      throw new ReadPastEndOfElementException(s"At: $position")
    else {
      val chr = json.charAt(position)
      if (chr == '"' && !escaped)
        (new JsonString(identifier, body.toString()), position + 1)
      else if (chr == '\\')
        readString(json, position + 1, identifier, body, true)
      else {
        body += chr
        readString(json, position + 1, identifier, body)
      }
    }
  }
}
