package json.rdr
import json.JsonElement
import json.exceptions.{JsonException, UnrecognisedRootElementException}

class JsonReader extends Reader {

  override def read(json: String, position: Integer, identifier: String): (JsonElement, Integer) = readBody(json, position)

  def readBody(json: String, position: Integer): (JsonElement, Integer) = {
    val chr = json.charAt(position)
    if (whitespace(chr))
      readBody(json, position + 1)
    else if (chr == '{')
      new ObjectReader().read(json, position + 1)
    else
      throw new UnrecognisedRootElementException(s"Character: $chr")
  }
}
