import json.DebugMacro._

case class Country(id: Int, name: String)

case class City(id: Int, name: String, country: Country)

case class Customer(id: Int, name: String,
                    isActive: Boolean = true,
                    parentId: Option[Int] = None,
                   shortVal: Option[Short] = None,
                   code: Option[String] = None,
                   city: City = null)

val customer = Customer(1, "One", false, Some(1),
  None, Some("CD"), City(1, "London", Country(1, "UK")))

toJson(customer)
