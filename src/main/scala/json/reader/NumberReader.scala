package json.reader

import json.{ElementParseResult, JsonNumber}

/**
  * Created by Ian on 09/07/2018.
  */
class NumberReader extends Reader {

  override def read(json: String, position: Integer, identifier: String): ElementParseResult =
    readNumber(json, position, position, identifier)

  def readNumber(json: String, position: Integer, startPosition: Integer, identifier: String): ElementParseResult = {
    val chr = json.charAt(position)
    if (chr == ',' || chr == '}' || chr == ']') {
      val stringValue = json.substring(startPosition, position)
      val fields = "([+-]?[\\d.]+)((?:[eE][+-]?\\d+)?)".r.findFirstMatchIn(stringValue)
      if (fields.isDefined)
        new ElementParseResult(JsonNumber(identifier, fields.get.group(1).toDouble * exponentPortion(fields.get.group(2))), true, position, null)
      else
        new ElementParseResult(null, false, position, s"""Value "$stringValue" does not conform to expected numeric pattern""")
    } else
      readNumber(json, position + 1, startPosition, identifier)
  }

  def exponentPortion(from: String): Double = {
    val expFields = "([eE])([+-]?)(\\d+)".r.findFirstMatchIn(from)
    if (expFields.isDefined)
      Math.pow(10.0, expFields.get.group(3).toDouble * expSign(expFields.get.group(2)))
    else
      1.0
  }

  def expSign(expSignValue: String): Double = {
    if (expSignValue.isEmpty || expSignValue == "+")
      1.0
    else
      -1.0
  }
}
