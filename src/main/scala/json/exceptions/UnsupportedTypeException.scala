package json.exceptions

class UnsupportedTypeException(field: String, message: String) extends JsonException(s"""Field "$field" - $message""") {

}
