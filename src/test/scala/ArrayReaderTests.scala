import json._
import json.rdr.JsonReader
import org.scalatest.{FlatSpec, Matchers}

class ArrayReaderTests  extends FlatSpec with Matchers {

  "Array reader" should "read an array containing a single string value" in {
    val json = """["A String"]"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonArray("", JsonString("", "A String"))
  }

  it should "read an array containing a single numeric value" in {
    val json = """[123]"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonArray("", new JsonNumber(123))
  }

  it should "read an array containing a single Boolean true value" in {
    val json = """[true]"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonArray("", JsonBoolean("", true))
  }

  it should "read an array containing a single Boolean false value" in {
    val json = """[false]"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonArray("", JsonBoolean("", false))
  }

  it should "read an array containing a single null value" in {
    val json = """[null]"""
    val obj = JsonReader.read(json)
    obj shouldBe new JsonArray("", JsonNull(""))
  }
}
