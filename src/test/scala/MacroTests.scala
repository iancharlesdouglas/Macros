package json

import org.scalatest.{FlatSpec, Matchers}

class MacroTests extends FlatSpec with Matchers {

  import DebugMacro._

  "Macro" should "x" in {

    jsStr("xxx")

    case class City(id: Int, name: String)
    case class Customer(val id: Int, status: Option[Int], name: String, isActive: Boolean)// city: City)
    val customer = Customer(1, None, "ACME Corp.", true)//City(1, "London"))
    val members = toJson(customer)
    val x = 1
  }
}
