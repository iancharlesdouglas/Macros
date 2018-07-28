package json

import org.scalatest.{FlatSpec, Matchers}

class CaseClassWriterTests extends FlatSpec with Matchers {

  import CaseClassWriter._

  "Case class writer" should "write a fairly complex object correctly" in {

    case class Country(id: Int, name: String)
    case class City(id: Int, name: String, country: Country)
    case class Thing(id: Int)

    case class Customer(val id: Int,
                        status: Option[Boolean],
                        name: String,
                        isActive: Boolean,
                        flags: Array[Thing],
                        city: City)

    val customer = Customer(1, Some(true), "ACME Corp.", true, Array(Thing(1), Thing(2)),
      City(1, "London", Country(1, "UK")))

    val js = toJson(customer)

    js shouldBe """{"id":1,"status":true,"name":"ACME Corp.","isActive":true,"flags":[{"id":1},{"id":2}],""" +
      """"city":{"id":1,"name":"London","country":{"id":1,"name":"UK"}}}"""
  }

  it should "write an array of nested objects correctly" in {

    case class Country(name: String)
    case class City(id: Int, name: String, country: Country)

    val cities = Array(City(1, "London", Country("England")), City(2, "Aberdeen", Country("Scotland")))

    val js = toJson(cities)

    js shouldBe """[{"id":1,"name":"London","country":{"name":"England"}},{"id":2,"name":"Aberdeen","country":{"name":"Scotland"}}]"""
  }
}
