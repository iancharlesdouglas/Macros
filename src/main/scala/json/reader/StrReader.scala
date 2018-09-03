package json.reader

import json.{ElementParseResult, JsonString}

import scala.collection.mutable.ListBuffer

/**
  * Created by Ian on 29/06/2018.
  */
class StrReader extends Reader {

  override def read(json: String, position: Integer, identifier: String): ElementParseResult =
    readString(json, position, identifier, ListBuffer[Char]())

  def readString(json: String, position: Integer, identifier: String, body: ListBuffer[Char],
                 escaped: Boolean = false): ElementParseResult = {
    if (position == json.size)
      new ElementParseResult(null, false, position, "Read past end of element")
    else {
      json.charAt(position) match {
        case '"' if !escaped => new ElementParseResult(JsonString(identifier, body.foldLeft("")(_ + _)), true, position + 1, null)
        case '\\' => readString(json, position + 1, identifier, body, true)
        case chr if escaped => {
          body += (chr match {
            case 'n' => '\n'
            case 'r' => '\r'
            case 't' => '\t'
            case 'f' => '\f'
            case 'b' => '\b'
            // TODO - Unicode 16 (made of two Unicode 8s)
            case 'u' if json.length > position + 4 => Integer.valueOf(json.substring(position + 1, position + 4), 16).toChar
            case c => c
          })
          readString(json, position + 1 + (if (chr == 'u') 3 else 0), identifier, body)
        }
        case chr => {
          body += chr
          readString(json, position + 1, identifier, body)
        }
      }
    }
  }
}
