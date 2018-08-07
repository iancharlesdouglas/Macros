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
}
