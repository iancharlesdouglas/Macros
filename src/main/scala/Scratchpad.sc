import json.DebugMacro._

case class Country(id: Int, name: String)

case class City(id: Int, name: String, country: Country)

case class Customer(id: Int, name: String,
                    isActive: Boolean = true,
                    parentId: Long = 0,
                    city: City)

val customer = Customer(1, "One",
  city = City(1, "London", Country(1, "UK")))

toJson(customer)
