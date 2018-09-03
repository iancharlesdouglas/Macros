package json

class ParseResult(val it: JsonElement, val successfully: Boolean, val position: Int, val message: String)

object ParseResult {
  def apply(it: JsonElement, successfully: Boolean, position: Int, message: String): ParseResult =
    new ParseResult(it, successfully, position, message)
}
class TypedParseResult[T](val it: T, val successfully: Boolean, val position: Int, val message: String)
