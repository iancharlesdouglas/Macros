package json

/**
  * Created by Ian on 11/07/2018.
  */
case class JsonArray(id: String = "") extends JsonElement(id) {

  def this(id: String, childElements: JsonElement*) = {
    this(id)
    this.elements ++= childElements
  }

  override def equals(obj: scala.Any): Boolean = {
    if (obj.isInstanceOf[JsonArray]) {
      val other: JsonArray = obj.asInstanceOf[JsonArray]
      other.id == id && other.elements.forall(this.elements.contains(_))
    } else {
      false
    }
  }
}
