package json

/**
  * Created by Ian on 03/07/2018.
  */
case class JsonNull(id: String) extends JsonElement(id) {

}

object JsonNull {
  def apply(): JsonNull = new JsonNull("")
}