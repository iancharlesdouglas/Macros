import json.CaseClassWriter._

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
