package json

import org.scalatest.{FlatSpec, Matchers}

class MacroTests extends FlatSpec with Matchers {

  import DebugMacro._

  "Macro" should "x" in {

    jsStr("xxx")

    case class City(id: Int, name: String)
    case class Thing(id: Int)
    case class Customer(val id: Int, status: Option[Boolean], name: String, isActive: Boolean,
                        flags: Array[Thing])// city: City)
    val customer = Customer(1, Some(true), "ACME Corp.", true, Array(Thing(1), Thing(2)))//City(1, "London"))
    val members = toJson(customer)
    val x = 1
  }
}
