package json

/**
  * Created by Ian on 04/07/2018.
  */
case class JsonNumber(id: String, value: BigDecimal) extends JsonElement(id) {

  def this(value: BigDecimal) {
    this("", value)
  }
}

object JsonNumber {
  def apply(value: BigDecimal): JsonNumber = new JsonNumber("", value)
}
