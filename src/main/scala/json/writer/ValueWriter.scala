package json.writer
import json.{JsonBoolean, JsonElement, JsonNull, JsonNumber}

/**
  * Created by Ian on 16/07/2018.
  */
class ValueWriter extends Writer {

  override def write(context: WriteContext)(element: JsonElement): WriteContext = {
    element match {
      case n: JsonNull => context.write("null")
      case b: JsonBoolean if b.value => context.write("true")
      case b: JsonBoolean if !b.value => context.write("false")
      case n: JsonNumber => context.write(n.value.toString())
    }
  }
}
