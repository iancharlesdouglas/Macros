package json.reader
import json.{ParseResult, JsonElement}

case object JsonReader extends Reader {

  def read(json: String): ParseResult = read(json, 0, "")

  override def read(json: String, position: Integer, identifier: String): ParseResult =
    readBody(json, position)

  def readBody(json: String, position: Integer): ParseResult = {
    json.charAt(position) match {
      case chr if whitespace(chr) => readBody(json, position + 1)
      case '{' => new ObjectReader().read(json, position + 1)
      case '[' => new ArrayReader().read(json, position + 1)
      case chr => new ParseResult(null, false, position, s"Invalid root character: $chr")
    }
  }
}
