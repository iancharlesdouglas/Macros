import scala.util.matching.Regex

//val m = "(d*)([eE][+-]d)?".r.findFirstMatchIn("1.23e+1")
val m = "([+-]?[\\d.]+)((?:[eE][+-]?\\d+)?)".r
  .findFirstMatchIn("1.23E+7")

m.get.group(1)
m.get.group(2)