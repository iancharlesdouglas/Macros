import json.DebugMacro._

case class Country(id: Int, name: String)

case class City(id: Int, name: String, country: Country)

case class Thing(id: Int)

case class Customer(id: Int, name: String,
                    isActive: Boolean = true,
                    parentId: Option[Long] = None,/*
                   shortVal: Option[Short] = None,
                   code: Option[String] = None,
                   city: City = null,*/
                   flags: Array[Thing])

val customer = Customer(2, "One", false, /*Some(2),
  None, Some("CD"),
  City(1, "London", Country(1, "UK")), */
  Some(1),
  Array(Thing(1), Thing(2)))


toJson(customer)
