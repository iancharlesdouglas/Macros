package json

import scala.collection.mutable.ListBuffer

/**
  * Created by Ian on 27/06/2018.
  */
case class JsonObject(id: String) extends JsonElement(id) {

  def this(id: String, childElements: JsonElement*) = {
    this(id)
    this.elements ++= childElements
  }

}
