package json.reader
import json.JsonElement
import json.exceptions.UnrecognisedRootElementException

case object JsonReader extends Reader {

  def read(json: String): JsonElement = read(json, 0, "")._1

  override def read(json: String, position: Integer, identifier: String): (JsonElement, Integer) =
    readBody(json, position)

  def readBody(json: String, position: Integer): (JsonElement, Integer) = {
    json.charAt(position) match {
      case chr if whitespace(chr) => readBody(json, position + 1)
      case '{' => new ObjectReader().read(json, position + 1)
      case '[' => new ArrayReader().read(json, position + 1)
      case chr => throw new UnrecognisedRootElementException(s"Character: $chr")
    }
  }
}
