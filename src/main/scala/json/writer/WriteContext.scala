package json.writer

/**
  * Created by Ian on 16/07/2018.
  */
case class WriteContext(builder: StringBuilder, indent: Boolean = false, indentLevel: Int = 0) {

  def write(content: String): WriteContext = {
    builder.append(content)
    this
  }
}
