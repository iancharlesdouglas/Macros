package json

import scala.collection.mutable.ListBuffer

/**
  * Created by Ian on 27/06/2018.
  */
abstract class JsonElement(val elementId: String) {

  val elements = ListBuffer[JsonElement]()

}

object JsonElement {

  /*def createArray[T](id: String, values: Array[T]): JsonArray = {
    T match {
      case v: Array[Int] if v.isInstanceOf[Array[Int]] => new JsonArray(id, v.map(vl => JsonNumber(vl)))
    }
  }*/
  def createArray(id: String, values: Array[Int]): JsonArray = new JsonArray(id, values.map(v => JsonNumber(v)))
  def createArray(id: String, values: Array[Long]): JsonArray = new JsonArray(id, values.map(v => JsonNumber(v)))

  def apply(id: String, values: Array[Int]): JsonElement =
    new JsonArray(id, values.map(value => JsonNumber(value)))

  def apply(id: String, values: Array[Long]): JsonElement =
    new JsonArray(id, values.map(value => JsonNumber(value)))
}
