import DebugMacro._

jsStr("xss")

case class Customer(id: Int, name: String)

printMembers[Customer]