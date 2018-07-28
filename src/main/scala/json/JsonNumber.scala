package json

/**
  * Created by Ian on 04/07/2018.
  */
case class JsonNumber(id: String, value: BigDecimal) extends JsonElement(id) {

  override def find(fieldId: String): JsonElement = ???
}

object JsonNumber {
  def apply(value: BigDecimal): JsonNumber = {
    new JsonNumber("", value)
  }
}
