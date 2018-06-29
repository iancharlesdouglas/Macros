package json.rdr

import json.JsonElement

/**
  * Created by Ian on 29/06/2018.
  */
abstract class Reader {

  def read(json: String, position: Integer = 0, identifier: String = "") : (JsonElement, Integer)

  def whitespace(what: Char): Boolean = List(' ').contains(what)
}
