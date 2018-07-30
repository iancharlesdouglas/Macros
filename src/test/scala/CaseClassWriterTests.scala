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

  it should "write an iterable of items correctly" in {

    case class Country(name: String)

    val countries = List(Country("UK"), Country("Italy"))

    val js = toJson(countries)

    js shouldBe """[{"name":"UK"},{"name":"Italy"}]"""
  }

  it should "write an iterable within a class correctly" in {

    case class Deal(id: Int, products: List[Int])

    val deal = Deal(1, List(1, 2, 3))

    val js = toJson(deal)

    js shouldBe """{"id":1,"products":[1,2,3]}"""

    case class VecDeal(id: Int, products: Vector[Int])

    val vecDeal = VecDeal(1, Vector(1, 2, 3))

    val vecJs = toJson(vecDeal)
    vecJs shouldBe """{"id":1,"products":[1,2,3]}"""
  }

  it should "write nulls correctly" in {

    case class Country(id: Int)
    case class City(id: Int, name: String, country: Country)

    val city = City(1, null, Country(1))

    val cityJs = toJson(city)

    cityJs shouldBe """{"id":1,"name":null,"country":{"id":1}}"""

    val nullCity: City = null

    val nullJs = toJson(nullCity)

    nullJs shouldBe "null"
  }

  it should "write optional objects correctly" in {

    case class Country(id: Int)
    case class City(id: Int, name: String, country: Option[Country])

    val cityCountryless = City(1, "London", None)

    val jsCountryless = toJson(cityCountryless)

    jsCountryless shouldBe """{"id":1,"name":"London","country":null}"""

    val cityCountried = City(1, "London", Some(Country(1)))

    val jsCountried = toJson(cityCountried)

    jsCountried shouldBe """{"id":1,"name":"London","country":{"id":1}}"""
  }

  it should "write optional primitives correctly" in {

    case class Task(id: Int, name: Option[String], isCompleted: Option[Boolean])

    val minimalTask = Task(1, None, None)

    val minimalJs = toJson(minimalTask)

    minimalJs shouldBe """{"id":1,"name":null,"isCompleted":null}"""

    val maximalTask = Task(1, Some("Take out the trash"), Some(true))

    val maximalJs = toJson(maximalTask)

    maximalJs shouldBe """{"id":1,"name":"Take out the trash","isCompleted":true}"""
  }

  it should "write numbers (and options of numbers) correctly" in {

    case class NumbersReqd(id: Int, long: Long, short: Short, byte: Byte, float: Float, double: Double)

    val numbersReqd = NumbersReqd(1, 1L, 1.toShort, 1.toByte, 1F, 1.0)

    val jsNumbersReqd = toJson(numbersReqd)

    jsNumbersReqd shouldBe """{"id":1,"long":1,"short":1,"byte":1,"float":1,"double":1}"""

    case class NumbersOptn(id: Int, long: Option[Long], short: Option[Short], byte: Option[Byte], float: Option[Float],
                           double: Option[Double])

    val numbersOptn = NumbersOptn(1, Some(1L), Some(1.toShort), Some(1.toByte), Some(1F), Some(1.0))

    val jsNumbersOptn = toJson(numbersOptn)

    jsNumbersOptn shouldBe """{"id":1,"long":1,"short":1,"byte":1,"float":1,"double":1}"""

    val numbersMissing = NumbersOptn(2, None, None, None, None, None)

    val jsNumbersMissing = toJson(numbersMissing)

    jsNumbersMissing shouldBe """{"id":2,"long":null,"short":null,"byte":null,"float":null,"double":null}"""
  }

  it should "read a simple JSON object into the fields of a case class object" in {

    case class Thing(id: Int, name: String, status: Long, typeId: Short, code: Byte)

    val json = """{"id":1,"name":"One","status":10,"typeId":3,"code":2}"""

    val thing = fromJson[Thing](json)

    thing shouldBe Thing(1, "One", 10L, 3.toShort, 2.toByte)
  }
}
