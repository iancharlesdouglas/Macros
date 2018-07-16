import json.writer.WriteContext

/**
  * Created by Ian on 16/07/2018.
  */
object DefaultWriteContext {

  def apply(): WriteContext = WriteContext(new StringBuilder(), false, 0)
}
