package json.reader
import json.{ElementParseResult, JsonElement}
import json.exceptions.UnrecognisedRootElementException

case object JsonReader extends Reader {

  def read(json: String): JsonElement = read(json, 0, "").it

  override def read(json: String, position: Integer, identifier: String): ElementParseResult =
    readBody(json, position)

  def readBody(json: String, position: Integer): ElementParseResult = {
    json.charAt(position) match {
      case chr if whitespace(chr) => readBody(json, position + 1)
      case '{' => new ObjectReader().read(json, position + 1)
      case '[' => new ArrayReader().read(json, position + 1)
      case chr => throw new UnrecognisedRootElementException(s"Character: $chr")
    }
  }
}
