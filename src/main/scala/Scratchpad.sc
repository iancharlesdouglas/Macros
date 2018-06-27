//import DebugMacro._
import json.reader.JsonObjectReader


//hello()

//printParam(1)

//debug(2)
//val x = 1

//debug(x)


val reader = new JsonObjectReader

reader.nextNonWhitespace("   f")

reader.xx("   g")

import scala.util.matching.Regex

"""[^\s]""".r.findFirstIn("   f")