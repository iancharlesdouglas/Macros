import json._
import json.reader.JsonReader
import json.writer.{DefaultWriteContext, JsonWriter, WriteContext}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Ian on 16/07/2018.
  */
class WriterTests extends FlatSpec with Matchers {

  "JSON Writer" should "write a null value as the word 'null'" in {
    val nullElement = JsonNull()
    val result = JsonWriter.write(DefaultWriteContext())(nullElement)
    result shouldBe "null"
  }

  it should "write a false Boolean as the word 'false'" in {
    val falseElement = JsonBoolean(false)
    val result = JsonWriter.write(DefaultWriteContext())(falseElement)
    result shouldBe "false"
  }

  it should "write a true Boolean as the word 'true'" in {
    val trueElement = JsonBoolean(true)
    val result = JsonWriter.write(DefaultWriteContext())(trueElement)
    result shouldBe "true"
  }

  it should "write numbers correctly" in {

    val fraction = JsonNumber(123.01)
    val result = JsonWriter.write(DefaultWriteContext())(fraction)
    result shouldBe "123.01"

    val intFrac = JsonNumber(1.0)
    val intResult = JsonWriter.write(DefaultWriteContext())(intFrac)
    intResult shouldBe "1"

    val intPure = JsonNumber(1)
    val intPureRes = JsonWriter.write(DefaultWriteContext())(intPure)
    intPureRes shouldBe "1"

    val neg = JsonNumber(-100)
    val negResult = JsonWriter.write(DefaultWriteContext())(neg)
    negResult shouldBe "-100"

    val bigDec = JsonNumber(BigDecimal(1000000))
    val bigDecResult = JsonWriter.write(DefaultWriteContext())(bigDec)
    bigDecResult shouldBe "1000000"
  }

  it should "write a string with escaped control characters" in {
    val string = JsonString("ABCDE\t100.00\r\nLine 2\fLine 3\b\\Line 4")
    val result = JsonWriter.write(DefaultWriteContext())(string)
    result shouldBe "\"ABCDE\\t100.00\\r\\nLine 2\\fLine 3\\b\\Line 4\""
  }

  it should "write the value 'null' for a string whose value is null" in {
    val result = JsonWriter.write(DefaultWriteContext())(JsonString(null))
    result shouldBe "null"
  }

  it should "write the value of a character as a single-character string" in {
    val char = 'a'
    val result = JsonWriter.write(DefaultWriteContext())(JsonString(char))
    result shouldBe "\"a\""
  }

  it should "write the identifier of a value if one is supplied" in {

    val field = JsonNumber("id", 1000)
    val result = JsonWriter.write(DefaultWriteContext())(field)
    result shouldBe """"id":1000"""

    val prettyResult = JsonWriter.write(WriteContext(new StringBuilder, indent = true))(field)
    prettyResult shouldBe """"id": 1000"""
  }

  it should "write an empty object" in {
    val result = JsonWriter.write(DefaultWriteContext())(JsonObject())
    result shouldBe "{}"
  }

  it should "write the fields of an object" in {
    val obj = JsonObject(JsonNumber("id", 1000), JsonString("name", "One"))
    val result = JsonWriter.write(DefaultWriteContext())(obj)
    result shouldBe """{"id":1000,"name":"One"}"""
  }

  it should "write the fields of a nested object" in {
    val obj = JsonObject(JsonNumber("id", 1000),
      JsonObject("obj", JsonNumber("price", 100000), JsonBoolean("active", true)))
    val result = JsonWriter.write(DefaultWriteContext())(obj)
    result shouldBe """{"id":1000,"obj":{"price":100000,"active":true}}"""
  }

  it should "write an empty array" in {
    val result = JsonWriter.write(DefaultWriteContext())(JsonArray())
    result shouldBe "[]"
  }

  it should "write the elements of an array" in {
    val array = JsonArray(JsonNumber(1), JsonNumber(2), JsonNumber(3))
    val result = JsonWriter.write(DefaultWriteContext())(array)
    result shouldBe "[1,2,3]"
  }

  it should "write a nested array" in {
    val array = JsonArray(JsonArray(JsonNumber(1), JsonNumber(2)), JsonNumber(3))
    val result = JsonWriter.write(DefaultWriteContext())(array)
    result shouldBe "[[1,2],3]"
  }

  it should "write and read a complex object symmetrically" in {
    val json = """{"id":1000,"name":"One","deals":[{"id":10101,"price":100000},{"id":10011,"price":200000}],"active":true,"parent":null}"""
    val readIn = JsonReader.read(json).it
    val writtenOut = JsonWriter.write(DefaultWriteContext())(readIn)
    writtenOut shouldBe json
  }
}
