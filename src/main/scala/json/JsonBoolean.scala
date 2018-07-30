package json

/**
  * Created by Ian on 03/07/2018.
  */
case class JsonBoolean(id: String, value: Boolean) extends JsonElement(id) {}

object JsonBoolean {
  def apply(value: Boolean): JsonBoolean = new JsonBoolean("", value)
}