//import DebugMacro._
import json.reader.JsonObjectReader
import json.rdr.ObjectReader

import scala.collection.mutable.ListBuffer

//hello()

//printParam(1)

//debug(2)
//val x = 1

//debug(x)


val reader = new JsonObjectReader

reader.nextNonWhitespace("   f")

val jsonStr ="""{ "id": "1"},, """
val idMatch = """\s?[{]\s?\".+\"\s?\:""".r.findFirstMatchIn(jsonStr)

idMatch.isEmpty
idMatch.get.start
idMatch.get.end

val idMatchGroups = """(\s?[{]\s?\")(.+)(\"\s?\:)""".r.findFirstMatchIn(jsonStr)
idMatchGroups.get.group(2)

val start = """(\s?)([{])(.+)""".r.findFirstMatchIn(jsonStr)
start.get.group(3)

val delimJson = " , one"
"""(\s?)(,)(.+)""".r.findFirstMatchIn(delimJson).get.group(2)

var lb = ListBuffer('a', 'b', 'c')
lb.foldLeft("")(_ + _)