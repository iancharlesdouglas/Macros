package json

import org.scalatest.{FlatSpec, Matchers}

class CaseClassReaderTests extends FlatSpec with Matchers {

  import CompileTimeReaderWriter._

  it should "read a simple JSON object into the fields of a case class object" in {

    import Typer._

    case class Thing(id: Int, name: String, status: Long, typeId: Short, code: Byte,
                     roughPrice: Float, price: Double, char: Char, active: Boolean,
                     inactive: Boolean)

    val thing = ("""{"id":1,"name":"One","status":10,"typeId":3,"code":2,""" +
      """"roughPrice":1.33,"price":1.3,"char":"X","active":true,"inactive":false}""").jsonTo[Thing]

    thing shouldBe Thing(1, "One", 10L, 3.toShort, 2.toByte, 1.33F, 1.30, 'X', true, false)
  }

  it should "read a nested JSON object into the fields of a case class object" in {

    import Typer._

    case class Country(id: Int, name: String)

    case class City(id: Int, name: String, country: Country)

    case class Place(id: Int, city: City)

    val place = """{"id":1,"city":{"id":1,"name":"London","country":{"id":1,"name":"UK"}}}""".jsonTo[Place]

    place shouldBe Place(1, City(1, "London", Country(1, "UK")))
  }

  it should "read an optional Int field into a case class object" in {

    import Typer._

    case class Thing(id: Int, size: Option[Int])

    val thing = """{"id":1,"size":10}""".jsonTo[Thing]

    thing shouldBe Thing(1, Some(10))

    val thingWithNull = """{"id":1,"size":null}""".jsonTo[Thing]

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read an optional Long field into a case class object" in {

    import Typer._

    case class Thing(id: Int, size: Option[Long])

    val thing = """{"id":1,"size":10}""".jsonTo[Thing]

    thing shouldBe Thing(1, Some(10L))

    val thingWithNull = """{"id":1,"size":null}""".jsonTo[Thing]

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read an optional Short field into a case class object" in {

    import Typer._

    case class Thing(id: Int, size: Option[Short])

    val thing = """{"id":1,"size":10}""".jsonTo[Thing]

    thing shouldBe Thing(1, Some(10.toShort))

    val thingWithNull = """{"id":1,"size":null}""".jsonTo[Thing]

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read an optional Byte field into a case class object" in {

    import Typer._

    case class Thing(id: Int, size: Option[Byte])

    val thing = """{"id":1,"size":10}""".jsonTo[Thing]

    thing shouldBe Thing(1, Some(10.toByte))

    val thingWithNull = """{"id":1,"size":null}""".jsonTo[Thing]

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read an optional Float field into a case class object" in {

    import Typer._

    case class Thing(id: Int, size: Option[Float])

    val thing = """{"id":1,"size":10}""".jsonTo[Thing]

    thing shouldBe Thing(1, Some(10F))

    val thingWithNull = """{"id":1,"size":null}""".jsonTo[Thing]

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read an optional Double field into a case class object" in {

    import Typer._

    case class Thing(id: Int, size: Option[Double])

    val thing = """{"id":1,"size":10}""".jsonTo[Thing]

    thing shouldBe Thing(1, Some(10.0))

    val thingWithNull = """{"id":1,"size":null}""".jsonTo[Thing]

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read an optional String field into a case class object" in {

    import Typer._

    case class Thing(id: Int, size: Option[String])

    val thing = """{"id":1,"size":"One"}""".jsonTo[Thing]

    thing shouldBe Thing(1, Some("One"))

    val thingWithNull = """{"id":1,"size":null}""".jsonTo[Thing]

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read an optional Char field into a case class object" in {

    import Typer._

    case class Thing(id: Int, size: Option[Char])

    val thing = """{"id":1,"size":"O"}""".jsonTo[Thing]

    thing shouldBe Thing(1, Some('O'))

    val thingWithNull = """{"id":1,"size":null}""".jsonTo[Thing]

    thingWithNull shouldBe Thing(1, None)
  }

  it should "read an optional Char field that contains more than one character into a case class object as a single character" in {

    import Typer._

    case class Thing(id: Int, size: Option[Char])

    val thing = """{"id":1,"size":"OPQ"}""".jsonTo[Thing]

    thing shouldBe Thing(1, Some('O'))
  }

  it should "read an optional Boolean field into a case class object" in {

    import Typer._

    case class Thing(id: Int, size: Option[Boolean])

    val thing = """{"id":1,"size":true}""".jsonTo[Thing]

    thing shouldBe Thing(1, Some(true))

    val thingWithNull = """{"id":1,"size":null}""".jsonTo[Thing]

    thingWithNull shouldBe Thing(1, None)

    val thingWithFalse = """{"id":1,"size":false}""".jsonTo[Thing]

    thingWithFalse shouldBe Thing(1, Some(false))
  }

  it should "read an optional object field into a case class object" in {

    import Typer._

    case class Place(id: Int, name: String)

    case class Thing(id: Int, place: Option[Place])

    val thing = """{"id":1,"place":{"id":1,"name":"One"}}""".jsonTo[Thing]

    thing shouldBe Thing(1, Some(Place(1, "One")))

    val nullPlaceThing = """{"id":1,"place":null}""".jsonTo[Thing]

    nullPlaceThing shouldBe Thing(1, None)
  }

  it should "read an array of Int values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Array[Int])

    val thing = """{"id":1,"places":[1,2,3]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe Array(1, 2, 3)
  }

  it should "read an array of Long values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Array[Long])

    val thing = """{"id": 1, "places": [1, 2, 3]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe Array(1L, 2L, 3L)
  }

  it should "read an array of Short values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Array[Short])

    val thing = """{"id":1,"places":[1,2,3]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe Array(1.toShort, 2.toShort, 3.toShort)
  }

  it should "read an array of Byte values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Array[Byte])

    val thing = """{"id":1,"places":[1,2,3]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe Array(1.toByte, 2.toByte, 3.toByte)
  }

  it should "read an array of Float values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Array[Float])

    val thing = """{"id":1,"places":[1,2,3]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe Array(1F, 2F, 3F)
  }

  it should "read an array of Double values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Array[Double])

    val thing = """{"id":1,"places":[1,2,3]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe Array(1.0, 2.0, 3.0)
  }

  it should "read an array of String values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Array[String])

    val thing = """{"id":1,"places":["1","2","3"]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe Array("1", "2", "3")
  }

  it should "read an array of Char values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Array[Char])

    val thing = """{"id":1,"places":["1","2","3"]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe Array('1', '2', '3')
  }

  it should "read an array of Boolean values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Array[Boolean])

    val thing = """{"id":1,"places":[true,false]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe Array(true, false)
  }

  it should "read an array of case class objects into a field of a parent case class object" in {

    import Typer._

    case class Place(id: Int, name: String)
    case class Thing(id: Int, places: Array[Place])

    val thing = """{"id":1,"places":[{"id":1,"name":"UK"},{"id":2,"name":"Eire"}]}""".jsonTo[Thing]

    thing.places.size shouldBe 2
    thing.places(0) shouldBe Place(1, "UK")
    thing.places(1) shouldBe Place(2, "Eire")
  }

  it should "read a List of Int values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: List[Int])

    val thing = """{"id":1,"places":[1,2]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe List(1, 2)
  }

  it should "read a Vector of Int values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Vector[Int])

    val thing = """{"id":1,"places":[1,2]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe Vector(1, 2)
  }

  it should "read a Seq of Int values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Seq[Int])

    val thing = """{"id":1,"places":[1,2]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe Seq(1, 2)
  }

  it should "read an array of Int values from a root-level array" in {

    import Typer._

    val array = "[1,2,3]".jsonTo[Array[Int]]

    array shouldBe Array(1, 2, 3)
  }

  it should "read an optional array of Int values into a field of a case class object" in {

    import Typer._

    case class Thing(id:Int, places: Option[Array[Int]])

    val thing = """{"id":1,"places":[1,2,3]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places.get shouldBe Array(1, 2, 3)
  }

  it should "read a null array as the value None for an optional array field of a case class object" in {

    import Typer._

    case class Thing(id: Int, places: Option[Array[Int]])

    val thing = """{"id":1,"places":null}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places shouldBe None
  }

  it should "read an optional list of objects into a field of a case class object" in {

    import Typer._

    case class Place(id: Int, name: String)
    case class Thing(id: Int, places: Option[Array[Place]])

    val thing = """{"id":1,"places":[{"id":1,"name":"London"},{"id":2,"name":"UK"}]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.places.get.size shouldBe 2
    thing.places.get(0).id shouldBe 1
    thing.places.get(0).name shouldBe "London"
    thing.places.get(1).id shouldBe 2
    thing.places.get(1).name shouldBe "UK"
  }

  it should "read an optional vector of float values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, rates: Option[Vector[Float]])

    val thing = """{"id":1,"rates":[1.001, 2.002]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.rates.get shouldBe Vector(1.001F, 2.002F)
  }

  it should "read an optional seq of double values into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, prices: Option[Seq[Double]])

    val thing = """{"id":1,"prices":[10.01, 20.02]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.prices.get shouldBe Seq(10.01, 20.02)
  }

  it should "read an array of arrays into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, combinations: Array[Array[Int]])

    val thing = """{"id":1,"combinations":[[1,2],[1,3]]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.combinations shouldBe Array(Array(1, 2), Array(1, 3))
  }

  it should "read an array of lists into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, combinations: Array[List[Boolean]])

    val thing = """{"id": 1, "combinations": [[true, false], [false, false]]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.combinations shouldBe Array(List(true, false), List(false, false))
  }

  it should "read a list of vectors into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, combinations: List[Vector[Float]])

    val thing = """{"id":1,"combinations":[[1.01, 0.0002], [200.04, 2992.0, 2992.31]]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.combinations shouldBe List(Vector(1.01F, 0.0002F), Vector(200.04F, 2992F, 2992.31F))
  }

  it should "read a vector of seqs into a field of a case class object" in {

    import Typer._

    case class Thing(id: Int, flags: Vector[Seq[Boolean]])

    val thing = """{"id":1,"flags":[[true,false], [false, false, false]]}""".jsonTo[Thing]

    thing.id shouldBe 1
    thing.flags shouldBe Vector(Seq(true, false), Seq(false, false, false))
  }

  it should "read an array of optional ints" in {

    import Typer._

    val things = """[1, null, 2, null, 3]""".jsonTo[Array[Option[Int]]]

    things shouldBe Array(Some(1), None, Some(2), None, Some(3))
  }

  it should "read a case class object directly from a JSON string via a string extension method" in {

    import Typer._

    case class Thing(id: Int)

    val thing = """{"id":1}""".jsonTo[Thing]

    thing.id shouldBe 1
  }

  it should "read BigDecimal values correctly" in {

    import Typer._

    case class Item(id: Int, price: BigDecimal)

    val item = """{"id":1,"price":10"}""".jsonTo[Item]

    item.price shouldBe 10

    case class ItemOptional(id: Int, price: Option[BigDecimal])

    val itemOptional = """{"id":1,"price":10}""".jsonTo[ItemOptional]

    itemOptional.price shouldBe Some(10)

    val itemNull = """{"id":1,"price":null}""".jsonTo[ItemOptional]

    itemNull.price shouldBe None

    val arrayOptional = """[100000,2]""".jsonTo[Array[Option[BigDecimal]]]

    arrayOptional shouldBe Array(Some(BigDecimal(100000)), Some(BigDecimal(2)))
  }

  it should "read BigInt values correctly" in {

    import Typer._

    case class Item(id: Int, price: BigInt)

    val price = 1000000000

    val item = s"""{"id":1,"price":$price}""".jsonTo[Item]

    item.price shouldBe price

    case class ItemOptional(id: Int, price: Option[BigInt])

    val itemOptional = s"""{"id":1,"price":$price}""".jsonTo[ItemOptional]

    itemOptional.price shouldBe Some(price)

    val itemNull = """{"id":1,"price":null}""".jsonTo[ItemOptional]

    itemNull.price shouldBe None
  }
}
