package json

/**
  * Created by Ian on 29/06/2018.
  */
case class JsonString(id: String, value: String) extends JsonElement(id) {

}

object JsonString {
  def apply(value: String): JsonString = new JsonString("", value)
}