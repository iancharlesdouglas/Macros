package json

import org.scalatest.{FlatSpec, Matchers}

class MacroTests extends FlatSpec with Matchers {

  import DebugMacro._

  "Macro" should "x" in {

    jsStr("xxx")

    case class City(id: Int, name: String)
    case class Customer(val id: Int, name: String, city: City)
    val customer = Customer(1, "ACME Corp.", City(1, "London"))
    val members = printMembers(customer)
    val x = 1
  }
}
