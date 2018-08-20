package json

import language.experimental.macros
import reflect.macros.blackbox.Context

object Typer {

  implicit class JsonString(val text: String) {

    /**
      * Reads a JSON string and returns an instance of the given case class or Array/List/Vector/Seq type, populated
      * with its values.
      * @tparam T Type of the case class or Array/List/Vector/Seq to populate
      * @return Populated object created from the case class
      */
    def jsonTo[T]: T = macro CompileTimeReaderWriter.jsonTo_impl[T]
  }
}


