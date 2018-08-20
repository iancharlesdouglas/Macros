package json

import org.scalatest.{FlatSpec, Matchers}

class CaseClassReaderTests extends FlatSpec with Matchers {

  import CompileTimeReaderWriter._

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

  it should "read a nullable object field into a case class object" in {

    case class Place(id: Int, name: String)

    case class Thing(id: Int, place: Option[Place])

    val thing = fromJson[Thing]("""{"id":1,"place":{"id":1,"name":"One"}}""")

    thing shouldBe Thing(1, Some(Place(1, "One")))

    val nullPlaceThing = fromJson[Thing]("""{"id":1,"place":null}""")

    nullPlaceThing shouldBe Thing(1, None)
  }

  it should "read an array of Int values into a field of a case class object" in {

    case class Thing(id: Int, places: Array[Int])

    val thing = fromJson[Thing]("""{"id":1,"places":[1,2,3]}""")

    thing.id shouldBe 1
    thing.places shouldBe Array(1, 2, 3)
  }

  it should "read an array of Long values into a field of a case class object" in {

    case class Thing(id: Int, places: Array[Long])

    val thing = fromJson[Thing]("""{"id":1,"places":[1,2,3]}""")

    thing.id shouldBe 1
    thing.places shouldBe Array(1L, 2L, 3L)
  }

  it should "read an array of Short values into a field of a case class object" in {

    case class Thing(id: Int, places: Array[Short])

    val thing = fromJson[Thing]("""{"id":1,"places":[1,2,3]}""")

    thing.id shouldBe 1
    thing.places shouldBe Array(1.toShort, 2.toShort, 3.toShort)
  }

  it should "read an array of Byte values into a field of a case class object" in {

    case class Thing(id: Int, places: Array[Byte])

    val thing = fromJson[Thing]("""{"id":1,"places":[1,2,3]}""")

    thing.id shouldBe 1
    thing.places shouldBe Array(1.toByte, 2.toByte, 3.toByte)
  }

  it should "read an array of Float values into a field of a case class object" in {

    case class Thing(id: Int, places: Array[Float])

    val thing = fromJson[Thing]("""{"id":1,"places":[1,2,3]}""")

    thing.id shouldBe 1
    thing.places shouldBe Array(1F, 2F, 3F)
  }

  it should "read an array of Double values into a field of a case class object" in {

    case class Thing(id: Int, places: Array[Double])

    val thing = fromJson[Thing]("""{"id":1,"places":[1,2,3]}""")

    thing.id shouldBe 1
    thing.places shouldBe Array(1.0, 2.0, 3.0)
  }

  it should "read an array of String values into a field of a case class object" in {

    case class Thing(id: Int, places: Array[String])

    val thing = fromJson[Thing]("""{"id":1,"places":["1","2","3"]}""")

    thing.id shouldBe 1
    thing.places shouldBe Array("1", "2", "3")
  }

  it should "read an array of Char values into a field of a case class object" in {

    case class Thing(id: Int, places: Array[Char])

    val thing = fromJson[Thing]("""{"id":1,"places":["1","2","3"]}""")

    thing.id shouldBe 1
    thing.places shouldBe Array('1', '2', '3')
  }

  it should "read an array of Boolean values into a field of a case class object" in {

    case class Thing(id: Int, places: Array[Boolean])

    val thing = fromJson[Thing]("""{"id":1,"places":[true,false]}""")

    thing.id shouldBe 1
    thing.places shouldBe Array(true, false)
  }

  it should "read an array of case class objects into a field of a parent case class object" in {

    case class Place(id: Int, name: String)
    case class Thing(id: Int, places: Array[Place])

    val thing = fromJson[Thing]("""{"id":1,"places":[{"id":1,"name":"UK"},{"id":2,"name":"Eire"}]}""")

    thing.places.size shouldBe 2
    thing.places(0) shouldBe Place(1, "UK")
    thing.places(1) shouldBe Place(2, "Eire")
  }

  it should "read a List of Int values into a field of a case class object" in {

    case class Thing(id: Int, places: List[Int])

    val thing = fromJson[Thing]("""{"id":1,"places":[1,2]}""")

    thing.id shouldBe 1
    thing.places shouldBe List(1, 2)
  }

  it should "read a Vector of Int values into a field of a case class object" in {

    case class Thing(id: Int, places: Vector[Int])

    val thing = fromJson[Thing]("""{"id":1,"places":[1,2]}""")

    thing.id shouldBe 1
    thing.places shouldBe Vector(1, 2)
  }

  it should "read a Seq of Int values into a field of a case class object" in {

    case class Thing(id: Int, places: Seq[Int])

    val thing = fromJson[Thing]("""{"id":1,"places":[1,2]}""")

    thing.id shouldBe 1
    thing.places shouldBe Seq(1, 2)
  }

  it should "read an array of Int values from a root-level array" in {
    val array = fromJson[Array[Int]]("[1,2,3]")
    array shouldBe Array(1, 2, 3)
  }

  it should "read an optional array of Int values into a field of a case class object" in {

    case class Thing(id:Int, places: Option[Array[Int]])

    val thing = fromJson[Thing]("""{"id":1,"places":[1,2,3]}""")

    thing.id shouldBe 1
    thing.places.get shouldBe Array(1, 2, 3)
  }

  it should "read a null array as the value None for an optional array field of a case class object" in {

    case class Thing(id: Int, places: Option[Array[Int]])

    val thing = fromJson[Thing]("""{"id":1,"places":null}""")

    thing.id shouldBe 1
    thing.places shouldBe None
  }

  it should "read an optional list of objects into a field of a case class object" in {

    case class Place(id: Int, name: String)
    case class Thing(id: Int, places: Option[Array[Place]])

    val thing = fromJson[Thing]("""{"id":1,"places":[{"id":1,"name":"London"},{"id":2,"name":"UK"}]}""")

    thing.id shouldBe 1
    thing.places.get.size shouldBe 2
    thing.places.get(0).id shouldBe 1
    thing.places.get(0).name shouldBe "London"
    thing.places.get(1).id shouldBe 2
    thing.places.get(1).name shouldBe "UK"
  }

  it should "read an optional vector of float values into a field of a case class object" in {

    case class Thing(id: Int, rates: Option[Vector[Float]])

    val thing = fromJson[Thing]("""{"id":1,"rates":[1.001, 2.002]}""")

    thing.id shouldBe 1
    thing.rates.get shouldBe Vector(1.001F, 2.002F)
  }

  it should "read an optional seq of double values into a field of a case class object" in {

    case class Thing(id: Int, prices: Option[Seq[Double]])

    val thing = fromJson[Thing]("""{"id":1,"prices":[10.01, 20.02]}""")

    thing.id shouldBe 1
    thing.prices.get shouldBe Seq(10.01, 20.02)
  }

  it should "read an array of arrays into a field of a case class object" in {

    case class Thing(id: Int, combinations: Array[Array[Int]])

    val thing = fromJson[Thing]("""{"id":1,"combinations":[[1,2],[1,3]]}""")

    thing.id shouldBe 1
    thing.combinations shouldBe Array(Array(1, 2), Array(1, 3))
  }

  it should "read an array of lists into a field of a case class object" in {

    case class Thing(id: Int, combinations: Array[List[Boolean]])

    val thing = fromJson[Thing]("""{"id":1,"combinations":[[true, false],[false, false]]}""")

    thing.id shouldBe 1
    thing.combinations shouldBe Array(List(true, false), List(false, false))
  }

  it should "read a list of vectors into a field of a case class object" in {

    case class Thing(id: Int, combinations: List[Vector[Float]])

    val thing = fromJson[Thing]("""{"id":1,"combinations":[[1.01,0.0002],[200.04,2992.0,2992.31]]}""")

    thing.id shouldBe 1
    thing.combinations shouldBe List(Vector(1.01F, 0.0002F), Vector(200.04F, 2992F, 2992.31F))
  }

  it should "read a vector of seqs into a field of a case class object" in {

    case class Thing(id: Int, flags: Vector[Seq[Boolean]])

    val thing = fromJson[Thing]("""{"id":1,"flags":[[true,false], [false, false, false]]}""")

    thing.id shouldBe 1
    thing.flags shouldBe Vector(Seq(true, false), Seq(false, false, false))
  }

  it should "read an array of optional ints" in {

    val things = fromJson[Array[Option[Int]]]("""[1,null,2,null,3]""")

    things shouldBe Array(Some(1), None, Some(2), None, Some(3))
  }

  it should "x" in {

    import Extensions._

    case class Thing(id: Int)

    val js = """{"id":1}"""
    val thing = js.jsonTo[Thing]
  }
  /*it should "prevent a member of type Any" in {

    case class Thing(id: Any)

    val thing = fromJson[Thing]("""{"id":1}""")
  }*/
}
