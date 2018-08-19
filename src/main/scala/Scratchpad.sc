import language.experimental.macros
import json.CompileTimeReaderWriter._

case class Country(id: Int, name: String)

case class City(id: Int, name: String, country: Country)

case class Title(id: Int)
case class Thing(id: Int, title: Title)

case class Customer(id: Int, name: String,
                    isActive: Boolean = true,
                    parentId: Option[Title] = None,
                   city: City = null,
                   flags: Array[Thing])

val customer = Customer(2, "One", false,
  Some(Title(101)),
  City(1, "London", Country(1, "UK")),
  Array(Thing(1, Title(1)), Thing(2, Title(2))))

toJson(customer)

val countries = Array(Country(1, "UK"), Country(2, "Italy"))

toJson(countries)

val country = fromJson[Country]("""{"id":1,"name":"One"}""")

country.id

val city = fromJson[City]("""{"id":1,"name":"London","country":{"id":1,"name":"UK"}}""")

case class Place(id: Int, sym: Option[Int], city: City)

val place = fromJson[Place](
  """{"id":1,"sym":12,"city":{"id":1,"name":"London","country":{"id":1,"name":"UK"}}}"""
)

case class PlaceOpt(id: Int, city: Option[City])

val placeOpt = fromJson[PlaceOpt](
  """{"id":1,"city":{"id":1,"name":"London","country":{"id":1,"name":"UK"}}}"""
)

case class PlaceArr(id: Int, cities: Array[Int])

val placeArr = fromJson[PlaceArr](
  """{"id":1,"cities":[1,2]}"""
)

placeArr.cities(0)
placeArr.cities(1)

case class PlaceObj(id: Int, titles: Array[Thing])

val placeObj = fromJson[PlaceObj](
  """{"id":1,"titles":[{"id":1,"title":{"id":10,"name":"t10"}},{"id":3,"title":{"id":30,"name":"t30"}}]}"""
)
placeObj.titles.size
placeObj.titles(0)
placeObj.titles(1)

val arr = "[1,2,3]"
fromJson[Array[Int]](arr)