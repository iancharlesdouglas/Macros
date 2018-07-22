import json.DebugMacro._
import language.experimental.macros
import reflect.macros.blackbox.Context



jsStr("xss")

case class Customer(id: Int, name: String,
                    isActive: Boolean = true,
                    parentId: Long = 0)

val customer = Customer(1, "One")

printMembers(customer)