package json

abstract class ParseResult(val successfully: Boolean, val position: Int, val message: String)

class ElementParseResult(val it: JsonElement, override val successfully: Boolean, override val position: Int, override val message: String)
  extends ParseResult(successfully, position, message)

class TypedParseResult[T](val it: T, override val successfully: Boolean, override val position: Int, override val message: String)
  extends ParseResult(successfully, position, message)

/*object TypedParseResult[T] {
  def apply(it: T, successfully: Boolean, position: Int, line: Int, message: String): TypedParseResult[T] =
    new TypedParseResult[T]()
}*/