package json.writer
import json._

/**
  * Created by Ian on 16/07/2018.
  */
class ValueWriter extends Writer {

  override def write(context: WriteContext)(element: JsonElement): WriteContext = {
    writeElementId(context, element)
    element match {
      case n: JsonNull => context.write("null")
      case b: JsonBoolean if b.value => context.write("true")
      case b: JsonBoolean if !b.value => context.write("false")
      case n: JsonNumber => context.write(n.value.toString())
      case s: JsonString => writeString(s.value, context)
      case o: JsonObject => writeObject(o, context)
    }
  }

  def writeObject(obj: JsonObject, context: WriteContext): WriteContext = {
    context.write("{")
    obj.elements.zipWithIndex.foreach( elIdx => {

    })
    context.write("}")
  }

  def writeElementId(context: WriteContext, element: JsonElement): Unit = {
    if (element.elementId != null && !element.elementId.isEmpty) {
      context.write("\"")
        .write(element.elementId)
        .write("\"")
        .write(":")
      if (context.indent) context.write(" ")
    }
  }

  def writeString(value: String, context: WriteContext, position: Int = 0): WriteContext = {
    if (position < value.length) {
      context.builder.append(value.charAt(position) match {
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case '\f' => "\\f"
        case '\b' => "\\b"
        case '\\' => "\\"
        case c => c
      })
      writeString(value, context, position + 1)
    } else
      context
  }
}
