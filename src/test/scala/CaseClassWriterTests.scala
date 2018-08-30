package json

import org.scalatest.{FlatSpec, Matchers}

class CaseClassWriterTests extends FlatSpec with Matchers {

  import CompileTimeReaderWriter._

  "Case class writer" should "write a simple object correctly" in {

    import Typer._

    case class Thing(id: Int)

    val thing = Thing(1)

    thing.json shouldBe """{"id":1}"""
  }

  it should "write a fairly complex object correctly" in {

    import Typer._

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

    customer.json shouldBe """{"id":1,"status":true,"name":"ACME Corp.","isActive":true,"flags":[{"id":1},{"id":2}],""" +
      """"city":{"id":1,"name":"London","country":{"id":1,"name":"UK"}}}"""
  }

  it should "write an array of nested objects correctly" in {

    import Typer._

    case class Country(name: String)

    case class City(id: Int, name: String, country: Country)

    val cities = Array(City(1, "London", Country("England")), City(2, "Aberdeen", Country("Scotland")))

    cities.json shouldBe """[{"id":1,"name":"London","country":{"name":"England"}},{"id":2,"name":"Aberdeen","country":{"name":"Scotland"}}]"""
  }

  it should "write an iterable of items correctly" in {

    import Typer._

    case class Country(name: String)

    val countries = List(Country("UK"), Country("Italy"))

    countries.json shouldBe """[{"name":"UK"},{"name":"Italy"}]"""
  }

  it should "write an iterable within a class correctly" in {

    import Typer._

    case class Deal(id: Int, products: List[Int])

    val deal = Deal(1, List(1, 2, 3))

    deal.json shouldBe """{"id":1,"products":[1,2,3]}"""

    case class VecDeal(id: Int, products: Vector[Int])

    val vecDeal = VecDeal(1, Vector(1, 2, 3))

    vecDeal.json shouldBe """{"id":1,"products":[1,2,3]}"""
  }

  it should "write nulls correctly" in {

    import Typer._

    case class Country(id: Int)
    case class City(id: Int, name: String, country: Country)

    val city = City(1, null, Country(1))

    city.json shouldBe """{"id":1,"name":null,"country":{"id":1}}"""

    val nullCity: City = null

    nullCity.json shouldBe "null"
  }

  it should "write optional objects correctly" in {

    import Typer._

    case class Country(id: Int)
    case class City(id: Int, name: String, country: Option[Country])

    val cityCountryless = City(1, "London", None)

    cityCountryless.json shouldBe """{"id":1,"name":"London","country":null}"""

    val cityCountried = City(1, "London", Some(Country(1)))

    cityCountried.json shouldBe """{"id":1,"name":"London","country":{"id":1}}"""
  }

  it should "write optional primitives correctly" in {

    import Typer._

    case class Task(id: Int, name: Option[String], isCompleted: Option[Boolean])

    val minimalTask = Task(1, None, None)

    minimalTask.json shouldBe """{"id":1,"name":null,"isCompleted":null}"""

    val maximalTask = Task(1, Some("Take out the trash"), Some(true))

    maximalTask.json shouldBe """{"id":1,"name":"Take out the trash","isCompleted":true}"""
  }

  it should "write numbers (and options of numbers) correctly" in {

    import Typer._

    case class NumbersReqd(id: Int, long: Long, short: Short, byte: Byte, float: Float, double: Double)

    val numbersReqd = NumbersReqd(1, 1L, 1.toShort, 1.toByte, 1F, 1.0)

    numbersReqd.json shouldBe """{"id":1,"long":1,"short":1,"byte":1,"float":1,"double":1}"""

    case class NumbersOptn(id: Int, long: Option[Long], short: Option[Short], byte: Option[Byte], float: Option[Float],
                           double: Option[Double])

    val numbersOptn = NumbersOptn(1, Some(1L), Some(1.toShort), Some(1.toByte), Some(1F), Some(1.0))

    numbersOptn.json shouldBe """{"id":1,"long":1,"short":1,"byte":1,"float":1,"double":1}"""

    val numbersMissing = NumbersOptn(2, None, None, None, None, None)

    numbersMissing.json shouldBe """{"id":2,"long":null,"short":null,"byte":null,"float":null,"double":null}"""
  }

  it should "write BigDecimal values correctly" in {

    import Typer._

    case class Item(id: Int, price: BigDecimal)

    val item = Item(1, BigDecimal(10))

    item.json shouldBe """{"id":1,"price":10}"""

    case class ItemOptionalBigDec(id: Int, price: Option[BigDecimal])

    val itemOptional = ItemOptionalBigDec(1, Some(BigDecimal(10)))

    itemOptional.json shouldBe """{"id":1,"price":10}"""

    val itemNone = ItemOptionalBigDec(1, None)

    itemNone.json shouldBe """{"id":1,"price":null}"""

    val amount = 1000000

    val array = Array(BigDecimal(amount))

    array.json shouldBe s"""[$amount]"""

    // TODO - arrays of optional values
  }

  // TODO - BigInts
}
