package json

import org.scalatest.{FlatSpec, Matchers}

class CaseClassReaderTests extends FlatSpec with Matchers {

  import CaseClassMaterialiser._

  it should "read a simple JSON object into the fields of a case class object" in {

    case class Thing(id: Int, name: String, status: Long, typeId: Short, code: Byte,
                     roughPrice: Float, price: Double, char: Char, active: Boolean,
                     inactive: Boolean)

    val json = """{"id":1,"name":"One","status":10,"typeId":3,"code":2,""" +
      """"roughPrice":1.33,"price":1.3,"char":"X","active":true,"inactive":false}"""

    val thing = fromJson[Thing](json)

    thing shouldBe Thing(1, "One", 10L, 3.toShort, 2.toByte, 1.33F, 1.30, 'X', true, false)
  }

  it should "read a nested JSON object into the fields of a case class object" in {

    case class Country(id: Int, name: String)

    case class City(id: Int, name: String, country: Country)

    case class Place(id: Int, city: City)

    val place = fromJson[Place](
      """{"id":1,"city":{"id":1,"name":"London","country":{"id":1,"name":"UK"}}}"""
    )
    place shouldBe Place(1, City(1, "London", Country(1, "UK")))
  }

  it should "read a nullable Int field into a case class object" in {

    case class Thing(id: Int, size: Option[Int])

    val thing = fromJson[Thing]("""{"id":1,"size":10}""")

    thing shouldBe Thing(1, Some(10))

    val thingWithNull = fromJson[Thing]("""{"id":1,"size":null}""")

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read a nullable Long field into a case class object" in {

    case class Thing(id: Int, size: Option[Long])

    val thing = fromJson[Thing]("""{"id":1,"size":10}""")

    thing shouldBe Thing(1, Some(10L))

    val thingWithNull = fromJson[Thing]("""{"id":1,"size":null}""")

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read a nullable Short field into a case class object" in {

    case class Thing(id: Int, size: Option[Short])

    val thing = fromJson[Thing]("""{"id":1,"size":10}""")

    thing shouldBe Thing(1, Some(10.toShort))

    val thingWithNull = fromJson[Thing]("""{"id":1,"size":null}""")

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read a nullable Byte field into a case class object" in {

    case class Thing(id: Int, size: Option[Byte])

    val thing = fromJson[Thing]("""{"id":1,"size":10}""")

    thing shouldBe Thing(1, Some(10.toByte))

    val thingWithNull = fromJson[Thing]("""{"id":1,"size":null}""")

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read a nullable Float field into a case class object" in {

    case class Thing(id: Int, size: Option[Float])

    val thing = fromJson[Thing]("""{"id":1,"size":10}""")

    thing shouldBe Thing(1, Some(10F))

    val thingWithNull = fromJson[Thing]("""{"id":1,"size":null}""")

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read a nullable Double field into a case class object" in {

    case class Thing(id: Int, size: Option[Double])

    val thing = fromJson[Thing]("""{"id":1,"size":10}""")

    thing shouldBe Thing(1, Some(10.0))

    val thingWithNull = fromJson[Thing]("""{"id":1,"size":null}""")

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read a nullable String field into a case class object" in {

    case class Thing(id: Int, size: Option[String])

    val thing = fromJson[Thing]("""{"id":1,"size":"One"}""")

    thing shouldBe Thing(1, Some("One"))

    val thingWithNull = fromJson[Thing]("""{"id":1,"size":null}""")

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read a nullable Char field into a case class object" in {

    case class Thing(id: Int, size: Option[Char])

    val thing = fromJson[Thing]("""{"id":1,"size":"O"}""")

    thing shouldBe Thing(1, Some('O'))

    val thingWithNull = fromJson[Thing]("""{"id":1,"size":null}""")

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read a nullable Char field that contains more than one character into a case class object as a single character" in {

    case class Thing(id: Int, size: Option[Char])

    val thing = fromJson[Thing]("""{"id":1,"size":"OPQ"}""")

    thing shouldBe Thing(1, Some('O'))
  }

  it should "read a nullable Boolean field into a case class object" in {

    case class Thing(id: Int, size: Option[Boolean])

    val thing = fromJson[Thing]("""{"id":1,"size":true}""")

    thing shouldBe Thing(1, Some(true))

    val thingWithNull = fromJson[Thing]("""{"id":1,"size":null}""")

    thingWithNull shouldBe Thing(1, None)

    val thingWithFalse = fromJson[Thing]("""{"id":1,"size":false}""")

    thingWithFalse shouldBe Thing(1, Some(false))
  }
}
