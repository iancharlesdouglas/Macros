package json.reader

import scala.util.matching.Regex

/**
  * Created by Ian on 27/06/2018.
  */
abstract class JsonElementReader[T] {

  def canRead(json: String): Boolean

  val body = new StringBuilder

  def fromJson(json: String): T

  def nextNonWhitespace(json: String):Option[String] = """[^\s]""".r.findFirstIn(json)


}
