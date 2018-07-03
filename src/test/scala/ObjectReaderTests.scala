/**
  * Created by Ian on 29/06/2018.
  */

import json.{JsonBoolean, JsonNull, JsonObject, JsonString}
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
    val json = """{ "prop": "Value: One\u0020Two" }"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonObject("", JsonString("prop", "Value: One Two"))
  }

  it should "read string property with form feed, tab and backspace directives" in {
    val json = """{ "prop": "Value: \fOne\b\tTwo" }"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonObject("", JsonString("prop", "Value: \fOne\b\tTwo"))
  }

  it should "read a Boolean property of true" in {
    val json = """{ "exists": true }"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonObject("", JsonBoolean("exists", true))
  }

  it should "read a Boolean property of false" in {
    val json = """{ "exists": false } """
    val obj = JsonReader.read(json)
    obj shouldBe new JsonObject("", JsonBoolean("exists", false))
  }

  it should "read a null property" in {
    val json = """{ "exists": null }"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonObject("", JsonNull("exists"))
  }
}
