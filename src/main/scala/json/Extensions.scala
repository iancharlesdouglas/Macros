package json

import language.experimental.macros
import reflect.macros.blackbox.Context

object Extensions {

  implicit class StringExtensions(val json: String) {
    def jsonTo[T]: T = macro CompileTimeReaderWriter.jsonTo_impl[T]
  }

  def jsonTo[T]:T = macro CompileTimeReaderWriter.jsonTo_impl[T]
}


