package json.exceptions

import scala.Exception

/**
  * Created by Ian on 28/06/2018.
  */
class JsonException extends Exception {

  def this(message: String) {
    super(message)
  }
}
