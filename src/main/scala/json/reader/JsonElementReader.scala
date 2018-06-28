package json.reader

import scala.util.matching.Regex

/**
  * Created by Ian on 27/06/2018.
  */
abstract class JsonElementReader[T] extends JsonReader {

  val body = new StringBuilder

  def fromJson(json: String): T

}
