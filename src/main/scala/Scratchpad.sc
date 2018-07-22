import json.DebugMacro._
import language.experimental.macros
import reflect.macros.blackbox.Context



jsStr("xss")

case class City(id: Int, name: String)

case class Customer(id: Int, name: String,
                    isActive: Boolean = true,
                    parentId: Long = 0,
                    city: City)

val customer = Customer(1, "One", city = City(1, "London"))

printMembers(customer)