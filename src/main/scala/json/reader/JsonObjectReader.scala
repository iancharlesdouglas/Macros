package json.reader

import json.JsonObject

/**
  * Created by Ian on 27/06/2018.
  */
class JsonObjectReader extends JsonElementReader[JsonObject] {

  override def canRead(json: String): Boolean =
    json != null && !json.isEmpty && nextNonWhitespace(json).getOrElse("").charAt(0) == '{'

  override def fromJson(json: String): JsonObject = ???
}
