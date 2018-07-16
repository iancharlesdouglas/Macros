package json.writer
import json.JsonElement

/**
  * Created by Ian on 16/07/2018.
  */
object JsonWriter {

  def write(context: WriteContext)(element: JsonElement): String =
    new ValueWriter().write(context)(element).builder.toString()
}
