/**
  * Created by Ian on 29/06/2018.
  */

import json.{JsonObject, JsonString}
import json.rdr.JsonReader
import org.scalatest.{FlatSpec, Matchers}

class ObjectReaderTests extends FlatSpec with Matchers {

  "Object reader" should "read empty object" in {
    val json = "{}"
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject("")
  }

  it should "read nested object with id" in {
    val json = """{ "thing": {} }"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonObject("", JsonObject("thing"))
  }

  it should "read nested object with string property" in {
    val json = """{ "thing": { "id": "ABC_1" } }"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonObject("", new JsonObject("thing", JsonString("id", "ABC_1")))
  }

  it should "read string property escaped with CRLFs" in {
    val json = """{ "prop": "Line 1\r\nLine 2" }"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonObject("", JsonString("prop", "Line 1\r\nLine 2"))
  }

  it should "read string property with an escaped Unicode value" in {
    val json = """{ "prop": "Value: \u0020" }"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonObject("", JsonString("prop", "Value:  "))
  }
}
