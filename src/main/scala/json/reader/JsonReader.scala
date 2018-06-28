package json.reader

/**
  * Created by Ian on 28/06/2018.
  */
abstract class JsonReader {

  def nextNonWhitespace(json: String):Option[String] = """[^\s]""".r.findFirstIn(json)

  val readers = List[JsonReader](new JsonObjectReader)

  def canRead(json: String): Boolean
}
