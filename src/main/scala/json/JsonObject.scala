package json

import scala.collection.mutable.ListBuffer

/**
  * Created by Ian on 27/06/2018.
  */
case class JsonObject(id: String = "") extends JsonElement(id) {

  /*def this(id: String, childElements: JsonElement*) = {
    this(id)
    this.elements ++= childElements
  }*/

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
}

object JsonObject {
  def apply(): JsonObject = JsonObject("")
  def apply(id: String, childElements: JsonElement*): JsonObject = new JsonObject(id, childElements)
  def apply(childElements: JsonElement*): JsonObject = new JsonObject("", childElements)
}
