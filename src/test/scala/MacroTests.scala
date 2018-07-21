package json

import org.scalatest.{FlatSpec, Matchers}

class MacroTests extends FlatSpec with Matchers {

  import DebugMacro._

  "Macro" should "x" in {
    case class Customer(val id: Int, name: String)
    val members = printMembers[Customer]
    val x = 1
  }
}
