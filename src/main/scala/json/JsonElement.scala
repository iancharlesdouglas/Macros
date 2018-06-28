package json

import scala.collection.mutable.ListBuffer

/**
  * Created by Ian on 27/06/2018.
  */
case class JsonElement(id: String) {

  val elements = ListBuffer[JsonElement]()
}
