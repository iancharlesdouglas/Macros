package json

import language.experimental.macros
import reflect.macros.blackbox.Context

object Typer {

  /**
    * Extension methods for String type.
    * @param text String value
    */
  implicit class JsonString(val text: String) {

    /**
      * Reads a JSON string and returns an instance of the given case class or Array/List/Vector/Seq type, populated
      * with its values.
      * @tparam T Type of the case class or Array/List/Vector/Seq to populate
      * @return Populated object created from the case class
      */
    def jsonTo[T]: TypedParseResult[T] = macro CompileTimeReaderWriter.jsonTo_impl[T]
  }

  implicit class JsonRef[T](val obj: T) {

    def json[T]: String = macro CompileTimeReaderWriter.json_impl[T]//(obj)

    //private def toJs(objc: T): String = macro CompileTimeReaderWriter.json_impl[T](obj)
  }
}


