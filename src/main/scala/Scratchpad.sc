import scala.util.matching.Regex

//val m = "(d*)([eE][+-]d)?".r.findFirstMatchIn("1.23e+1")
val m = "([+-]?[\\d.]+)((?:[eE][+-]?\\d+)?)".r
  .findFirstMatchIn("1.23")

m.get.group(1)
m.get.group(2)
val exp = m.get.group(2).isEmpty

val e = "([eE])([+-]?)(\\d+)".r.findFirstMatchIn("e27")
e.get.group(1)
e.get.group(2)
e.get.group(3)
