package json.writer

import json.JsonElement

/**
  * Created by Ian on 16/07/2018.
  */
abstract class Writer {

  def write(context: WriteContext)(element: JsonElement): WriteContext

  def writeComma(context: WriteContext): Unit = {
    context.write(",")
    if (context.indent) context.write(" ")
  }
}
