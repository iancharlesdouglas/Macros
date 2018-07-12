/**
  * Created by Ian on 29/06/2018.
  */

import json._
import json.rdr.JsonReader
import org.scalatest.{FlatSpec, Matchers}

class ObjectReaderTests extends FlatSpec with Matchers {

  "Object reader" should "read empty object" in {
    val json = "{}"
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject()
  }

  it should "read nested object with id" in {
    val json = """{ "thing": {} }"""
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonObject("thing"))
  }

  it should "read nested object with string property" in {
    val json = """{ "thing": { "id": "ABC_1" } }"""
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonObject("thing", JsonString("id", "ABC_1")))
  }

  it should "read nested object with two string properties" in {

    val json = """{ "thing": { "id": "120E", "name": "A Name" } }"""
    val obj = JsonReader.read(json)

    obj shouldBe JsonObject(JsonObject("thing",
      JsonString("id", "120E"),
      JsonString("name", "A Name")))
  }

  it should "read nested object with a string and a numeric property" in {

    val json = """{ "thing": { "id": "120E", "price": 123E3 } }"""
    val obj = JsonReader.read(json)

    obj shouldBe JsonObject(JsonObject("thing",
      JsonString("id", "120E"),
      JsonNumber("price", BigDecimal(123000))))
  }

  it should "read string property escaped with CRLFs" in {
    val json = """{ "prop": "Line 1\r\nLine 2" }"""
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonString("prop", "Line 1\r\nLine 2"))
  }

  it should "read string property with an escaped Unicode value" in {
    val json = """{ "prop": "Value: One\u0020Two" }"""
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonString("prop", "Value: One Two"))
  }

  it should "read string property with form feed, tab and backspace directives" in {
    val json = """{ "prop": "Value: \fOne\b\tTwo" }"""
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonString("prop", "Value: \fOne\b\tTwo"))
  }

  it should "read a Boolean property of true" in {
    val json = """{ "exists": true }"""
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonBoolean("exists", true))
  }

  it should "read a Boolean property of false" in {
    val json = """{ "exists": false } """
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonBoolean("exists", false))
  }

  it should "read a null property" in {
    val json = """{ "exists": null }"""
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonNull("exists"))
  }

  it should "ignore non-space character whitespace" in {
    val formFeed = '\f'
    val tab = '\t'
    val json =
      s"""$formFeed
        |{
        |$tab  "exists": false
        |}
      """.stripMargin
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonBoolean("exists", false))
  }

  it should "read a number value" in {

    val json = """{ "number": 1.23 }"""
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonNumber("number", BigDecimal(1.23)))

    val jsonNeg = """{ "number": -1.23 }"""
    val objForNeg = JsonReader.read(jsonNeg)
    objForNeg shouldBe JsonObject(JsonNumber("number", BigDecimal(-1.23)))
  }

  it should "read a negative exponential number value" in {
    val json = """{ "number": 123e-2 }"""
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonNumber("number", BigDecimal(1.23)))
  }

  it should "read a positive exponential number value" in {

    val json = """{ "number": 1.23e+2 }"""
    val obj = JsonReader.read(json)
    obj shouldBe JsonObject(JsonNumber("number", BigDecimal(123)))

    val jsonUnsigned = """{ "number": 1.23e2 }"""
    val objForUnsgn = JsonReader.read(jsonUnsigned)
    objForUnsgn shouldBe JsonObject(JsonNumber("number", BigDecimal(123)))
  }
}
