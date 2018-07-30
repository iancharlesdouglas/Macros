package json

/**
  * Created by Ian on 29/06/2018.
  */
case class JsonString(id: String, value: String) extends JsonElement(id) {

  def this(id: String, value: Char) = this(id, value.toString)
}

object JsonString {
  def apply(value: String): JsonString = new JsonString("", value)
  def apply(value: Char): JsonString = new JsonString("", value.toString)
}