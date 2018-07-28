package json

/**
  * Created by Ian on 27/06/2018.
  */
case class JsonObject(id: String = "") extends JsonElement(id) {

  def this(id: String, childElements: Seq[JsonElement]) = {
    this(id)
    this.elements ++= childElements
  }

  override def equals(obj: scala.Any): Boolean = {
    if (obj.isInstanceOf[JsonObject]) {
      val other: JsonObject = obj.asInstanceOf[JsonObject]
      other.id == id && other.elements.forall(this.elements.contains(_))
    } else {
      false
    }
  }

  override def find(fieldId: String): JsonElement = elements.toList.find(_.elementId == fieldId).get
}

object JsonObject {
  def apply(): JsonObject = JsonObject("")
  def apply(id: String, childElements: JsonElement*): JsonObject = new JsonObject(id, childElements)
  def apply(childElements: JsonElement*): JsonObject = new JsonObject("", childElements)
}
