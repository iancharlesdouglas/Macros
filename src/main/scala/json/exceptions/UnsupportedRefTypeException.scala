package json.exceptions

class UnsupportedRefTypeException(className: String, message: String) extends JsonException(s"""Class "$className" - $message""") {
}
