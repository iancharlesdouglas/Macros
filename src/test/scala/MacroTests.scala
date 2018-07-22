package json

import org.scalatest.{FlatSpec, Matchers}

class MacroTests extends FlatSpec with Matchers {

  import DebugMacro._

  "Macro" should "x" in {
    case class Customer(val id: Int, name: String)
    val customer = Customer(1, "ACME Corp.")
    val members = printMembers(customer)
    val x = 1
  }
}
