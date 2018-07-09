package json.rdr
import json.exceptions.{JsonException, ReadPastEndOfElementException, UnrecognisedEscapeSequenceException}
import json.{JsonElement, JsonString}

import scala.Char
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
        (JsonString(identifier, body.foldLeft("")(_ + _)), position + 1)
      else if (chr == '\\')
        readString(json, position + 1, identifier, body, true)
      else if (escaped) {
        val substChar = chr match {
          case 'n' => '\n'
          case 'r' => '\r'
          case 't' => '\t'
          case 'f' => '\f'
          case 'b' => '\b'
            // TODO - Unicode 16 (made of two Unicode 8s)
          case 'u' if json.length > position + 4 => Integer.valueOf(json.substring(position + 1, position + 4), 16).toChar
          case 'u' => throw new UnrecognisedEscapeSequenceException("'\\u'")
          case c => c
        }
        body += substChar
        readString(json, position + 1 + (if (chr == 'u') 3 else 0), identifier, body)
      } else {
        body += chr
        readString(json, position + 1, identifier, body)
      }
    }
  }
}
