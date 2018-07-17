import json._
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
    intResult shouldBe "1.0"

    val intPure = JsonNumber(1)
    val intPureRes = JsonWriter.write(DefaultWriteContext())(intPure)
    intPureRes shouldBe "1"

    val neg = JsonNumber(-100)
    val negResult = JsonWriter.write(DefaultWriteContext())(neg)
    negResult shouldBe "-100"
  }

  it should "write a string with escaped control characters" in {
    val string = JsonString("ABCDE\t100.00\r\nLine 2")
    val result = JsonWriter.write(DefaultWriteContext())(string)
    result shouldBe "ABCDE\\t100.00\\r\\nLine 2"
  }

  it should "write the identifier of a value if one is supplied" in {

    val field = JsonNumber("id", 1000)
    val result = JsonWriter.write(DefaultWriteContext())(field)
    result shouldBe """"id":1000"""

    val prettyResult = JsonWriter.write(WriteContext(new StringBuilder, indent = true))(field)
    prettyResult shouldBe """"id": 1000"""
  }

  it should "write an empty object" in {
    val obj = JsonObject()
    val result = JsonWriter.write(DefaultWriteContext())(obj)
    result shouldBe "{}"
  }

  it should "write the fields of an object" in {

  }
}
