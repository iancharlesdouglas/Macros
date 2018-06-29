/**
  * Created by Ian on 29/06/2018.
  */

import json.JsonObject
import json.rdr.ObjectReader
import json.reader.JsonObjectReader
import org.scalatest.{FlatSpec, Matchers}

class ReaderTests extends FlatSpec with Matchers {

  "Object reader" should "read empty object" in {
    val json = "{}"
    val reader = new ObjectReader
    val (obj, pos) = reader.read(json)
    val x = 1
  }
  /*
  "JSON object reader" should "read an empty object" in {

    val json = "{}"
    val reader = new JsonObjectReader()
    val (obj, remain) = reader.read(json, "", null, List(reader))

    obj shouldBe JsonObject("")
  }

  it should "read an object that contains an empty child object" in {

    val json = """{ "nested": {} }"""
    val reader = new JsonObjectReader()
    val (obj, remain) = reader.read(json, "", null, List(reader))

    val x = 1
    //obj shouldBe JsonObject
  }
  */
}
