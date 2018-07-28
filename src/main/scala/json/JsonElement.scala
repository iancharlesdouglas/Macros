package json

import scala.collection.mutable.ListBuffer

/**
  * Created by Ian on 27/06/2018.
  */
abstract class JsonElement(val elementId: String) {

  val elements = ListBuffer[JsonElement]()

  def find(fieldId: String): JsonElement
}