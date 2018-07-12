import json._
import json.rdr.JsonReader
import org.scalatest.{FlatSpec, Matchers}

class ArrayReaderTests  extends FlatSpec with Matchers {

  "Array reader" should "read an array containing a single string value" in {
    val json = """["A String"]"""
    val array = JsonReader.read(json)
    array shouldBe JsonArray(JsonString("A String"))
  }

  it should "read an array containing a single numeric value" in {
    val json = """[123]"""
    val array = JsonReader.read(json)
    array shouldBe JsonArray(JsonNumber(123))
  }

  it should "read an array containing a single Boolean true value" in {
    val json = """[true]"""
    val array = JsonReader.read(json)
    array shouldBe JsonArray(JsonBoolean(true))
  }

  it should "read an array containing a single Boolean false value" in {
    val json = """[false]"""
    val array = JsonReader.read(json)
    array shouldBe JsonArray(JsonBoolean(false))
  }

  it should "read an array containing a single null value" in {
    val json = """[null]"""
    val array = JsonReader.read(json)
    array shouldBe JsonArray(JsonNull())
  }

  it should "read an array containing multiple primitive values" in {
    val json = """[ "String", 123.45e2, null, true, false ]"""
    val array = JsonReader.read(json)
    array shouldBe JsonArray(JsonString("String"), JsonNumber(12345), JsonNull(),
      JsonBoolean(true), JsonBoolean(false))
  }

  it should "read an empty array" in {
    val json = """[]"""
    val array = JsonReader.read(json)
    array shouldBe JsonArray()
  }
  // TODO - array containing object, array containing another array, object field containing array
}
