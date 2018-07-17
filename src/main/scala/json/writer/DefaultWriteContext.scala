package json.writer

/**
  * Created by Ian on 16/07/2018.
  */
object DefaultWriteContext {

  def apply(): WriteContext = WriteContext(new StringBuilder(), indent = false, indentLevel = 0)
}
