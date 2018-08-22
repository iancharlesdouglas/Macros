package json

import json.exceptions.{InvalidObjectFormatException, UnsupportedRefTypeException, UnsupportedTypeException}

import language.experimental.macros
import reflect.macros.blackbox.Context

trait JsWriter[T] {
  def write(value: T): JsonElement
  def write(id: String, value: T): JsonElement
}

object CompileTimeReaderWriter {

  implicit object ByteWriter extends JsWriter[Byte] {
    def write(value: Byte): JsonElement = JsonNumber(value.toInt)
    def write(id: String, value: Byte): JsonElement = JsonNumber(id, value.toInt)
  }

  implicit object ShortWriter extends JsWriter[Short] {
    def write(value: Short): JsonElement = JsonNumber(value.toInt)
    def write(id: String, value: Short): JsonElement = JsonNumber(id, value.toInt)
  }

  implicit object IntWriter extends JsWriter[Int] {
    def write(value: Int): JsonElement = JsonNumber(value)
    def write(id: String, value: Int): JsonElement = JsonNumber(id, value)
  }

  implicit object LongWriter extends JsWriter[Long] {
    def write(value: Long): JsonElement = JsonNumber(value)
    def write(id: String, value: Long): JsonElement = JsonNumber(id, value)
  }

  implicit object FloatWriter extends JsWriter[Float] {
    def write(value: Float): JsonElement = JsonNumber(value.toDouble)
    def write(id: String, value: Float): JsonElement = JsonNumber(id, value.toDouble)
  }

  implicit object DoubleWriter extends JsWriter[Double] {
    def write(value: Double): JsonElement = JsonNumber(value)
    def write(id: String, value: Double): JsonElement = JsonNumber(id, value)
  }

  implicit object BooleanWriter extends JsWriter[Boolean] {
    def write(value: Boolean): JsonElement = JsonBoolean(value)
    def write(id: String, value: Boolean): JsonElement = JsonBoolean(id, value)
  }

  implicit object StringWriter extends JsWriter[String] {
    def write(value: String): JsonElement = JsonString(value)
    def write(id: String, value: String): JsonElement = JsonString(id, value)
  }

  implicit object CharWriter extends JsWriter[Char] {
    def write(value: Char): JsonElement = JsonString(value)
    def write(id: String, value: Char): JsonElement = JsonString(id, value.toString)
  }

  implicit def ArrayWriter[T: JsWriter]: JsWriter[Array[T]] = new JsWriter[Array[T]] {
    def write(value: Array[T]) = JsonArray(value.map(implicitly[JsWriter[T]].write):_*)
    def write(id: String, value: Array[T]) = JsonArray(id, value.map(implicitly[JsWriter[T]].write):_*)
  }

  def write[T: JsWriter](value: T): JsonElement = implicitly[JsWriter[T]].write(value)

  def jsonTo_impl[T: c.WeakTypeTag](c: Context): c.Tree = {

    import c.universe._

    val json = reify(c.prefix.splice.asInstanceOf[Typer.JsonString].text)

    val objType = weakTypeOf[T]

    objType.typeSymbol.name.toString match {
      case "Array" | "List" | "Vector" | "Seq" => {
        readSequence(c)(q"import json._; reader.JsonReader.read($json).elements.toArray",
          Some(objType.typeArgs.head),
          objType.typeSymbol.name.toString)
      }
      case _ => readObject(c)(objType, q"import json._; reader.JsonReader.read($json)")
    }
  }

  def json_impl[T: c.WeakTypeTag](c: Context): c.Tree = {

    import c.universe._

    val obj = reify {
      c.prefix.splice.asInstanceOf[Typer.JsonRef[T]].obj
    }

    val objType = weakTypeOf[T];
    val objTypeName = objType.erasure.typeSymbol.name.toString
    val iterable = objType.baseClasses.find(_.typeSignature.typeSymbol.name.toString == "Iterable")

    val statements =
      if (objTypeName == "Array" || iterable.isDefined) {

        val typeArg =
          if (objType.resultType.typeArgs.isEmpty)
            objType.erasure.resultType.typeArgs.headOption
          else
            objType.resultType.typeArgs.headOption

        val elements = q"..$obj"
        val values = sequenceElements(c)(typeArg, elements)

        q"""new json.JsonArray("", $values)"""

      } else {

        q"""
           if ($obj == null)
             json.JsonNull()
           else
             json.JsonObject(..${objectMembers(c)(obj.actualType/*weakTypeOf[T]*/, obj)})
         """
      }

    q"""json.writer.JsonWriter.write(json.writer.DefaultWriteContext())($statements)"""
  }

  def toJson[T](obj: T): String = macro toJson_impl[T]

  def toJson_impl[T: c.WeakTypeTag](c: Context)(obj: c.Expr[T]): c.Tree = {

    import c.universe._

    val objType = weakTypeOf[T];
    val objTypeName = objType.erasure.typeSymbol.name.toString
    val iterable = objType.baseClasses.find(_.typeSignature.typeSymbol.name.toString == "Iterable")

    val statements =
      if (objTypeName == "Array" || iterable.isDefined) {

        val typeArg =
          if (objType.resultType.typeArgs.isEmpty)
            objType.erasure.resultType.typeArgs.headOption
          else
            objType.resultType.typeArgs.headOption

        val elements = q"..$obj"
        val values = sequenceElements(c)(typeArg, elements)

        q"""new json.JsonArray("", $values)"""

      } else {

        q"""
           if ($obj == null)
             json.JsonNull()
           else
             json.JsonObject(..${objectMembers(c)(weakTypeOf[T], obj)})
         """
      }

    q"""json.writer.JsonWriter.write(json.writer.DefaultWriteContext())($statements)"""
  }

  def fromJson[T](json: String): T = macro fromJson_impl[T]

  def fromJson_impl[T: c.WeakTypeTag](c: Context)(json: c.Expr[String]): c.Tree = {

    import c.universe._

    val objType = weakTypeOf[T]

    objType.typeSymbol.name.toString match {
      case "Array" | "List" | "Vector" | "Seq" => {
        readSequence(c)(q"import json._; reader.JsonReader.read($json).elements.toArray",
          Some(objType.typeArgs.head),
          objType.typeSymbol.name.toString)
      }
      case _ => readObject(c)(objType, q"import json._; reader.JsonReader.read($json)")
    }
  }

  def readObject(c: Context)(tpe: c.Type, src: c.Tree): c.Tree = {

    import c.universe._

    if (!tpe.baseClasses.find(_.name.toString == "Product").isDefined)
      throw new UnsupportedRefTypeException(tpe.typeSymbol.name.toString, "this type is not a case class")

    val fields = classMembers(c)(tpe)

    val className = tpe.typeSymbol.name.toString
    val consSelect = Ident(TermName(className))

    val members = fields.map(field => (field._1, field._2, TermName(field._1), field._6, field._7, field._4))

    q"""
      import json._

      def readObj(source: JsonElement) =

      $consSelect(..${
      members.map { m =>
        val (fieldName, fieldType, fieldTerm, baseTypes, fieldTpe, typeArg) = m
        val fieldValue = fieldType match {
          case "Int" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonNumber].value.toInt"
          case "Long" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonNumber].value.toLong"
          case "Short" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonNumber].value.toShort"
          case "Byte" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonNumber].value.toByte"
          case "Float" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonNumber].value.toFloat"
          case "Double" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonNumber].value.toDouble"
          case "String" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonString].value"
          case "Char" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonString].value.toCharArray()(0)"
          case "Boolean" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonBoolean].value"
          case "Option" => readOption(c)(q"source.elements.find(_.elementId == $fieldName).get", typeArg)
          case "Array" | "List" | "Vector" | "Seq" =>
            readSequence(c)(q"source.elements.find(_.elementId == $fieldName).get.elements.toArray", typeArg, fieldType)
          case "Object" if !baseTypes.map(_.name.toString).contains("Product") =>
            throw new UnsupportedTypeException(fieldName, s"""Type "${fieldTpe.typeSymbol.alternatives.head.name.toString}" is not supported""")
          case _ =>
            val src = q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonObject]"
            q"${readObject(c)(fieldTpe, src)}"
        }
        q"$fieldTerm = $fieldValue"
      }})

       readObj($src)
     """
  }

  def readOption(c: Context)(option: c.Tree, typeArg: Option[c.Type]): c.Tree = {

    import c.universe._

    q"""
     val field = $option
     if (field.isInstanceOf[JsonNull])
       None
     else
     ${
      typeArg.get.typeSymbol.name.toString match {
        case "Int" => q"Some(field.asInstanceOf[JsonNumber].value.toInt)"
        case "Long" => q"Some(field.asInstanceOf[JsonNumber].value.toLong)"
        case "Short" => q"Some(field.asInstanceOf[JsonNumber].value.toShort)"
        case "Byte" => q"Some(field.asInstanceOf[JsonNumber].value.toByte)"
        case "Float" => q"Some(field.asInstanceOf[JsonNumber].value.toFloat)"
        case "Double" => q"Some(field.asInstanceOf[JsonNumber].value.toDouble)"
        case "String" => q"Some(field.asInstanceOf[JsonString].value)"
        case "Char" => q"Some(field.asInstanceOf[JsonString].value.toCharArray()(0))"
        case "Boolean" => q"Some(field.asInstanceOf[JsonBoolean].value)"
        case "Array" | "List" | "Vector" | "Seq" =>
          val seq = readSequence(c)(q"field.elements.toArray", Some(typeArg.get.typeArgs.head),
            typeArg.get.typeSymbol.name.toString)
          q"Some($seq)"
        case "Option" => throw new UnsupportedTypeException("", "Options of options are not supported")
        case _ =>
          val src = q"field.asInstanceOf[JsonObject]"
          q"Some(${readObject(c)(typeArg.get, src)})"
      }
    }"""
  }

  def readSequence(c: Context)(sequence: c.Tree, typeArg: Option[c.Type], fieldType: String): c.Tree = {

    import c.universe._

    q"""
      import json._

      val seq = ${
      typeArg.get.typeSymbol.name.toString match {
        case "Int" => q"$sequence.map(_.asInstanceOf[JsonNumber].value.toInt)"
        case "Long" => q"$sequence.map(_.asInstanceOf[JsonNumber].value.toLong)"
        case "Short" => q"$sequence.map(_.asInstanceOf[JsonNumber].value.toShort)"
        case "Byte" => q"$sequence.map(_.asInstanceOf[JsonNumber].value.toByte)"
        case "Float" => q"$sequence.map(_.asInstanceOf[JsonNumber].value.toFloat)"
        case "Double" => q"$sequence.map(_.asInstanceOf[JsonNumber].value.toDouble)"
        case "String" => q"$sequence.map(_.asInstanceOf[JsonString].value)"
        case "Char" => q"$sequence.map(_.asInstanceOf[JsonString].value.toCharArray()(0))"
        case "Boolean" => q"$sequence.map(_.asInstanceOf[JsonBoolean].value)"
        case "Array" | "List" | "Vector" | "Seq" =>
          q"""
             $sequence.map(seqnce => {
               ${val se = q"seqnce.elements.toArray"
                readSequence(c)(se, Some(typeArg.get.typeArgs.head), typeArg.get.typeSymbol.name.toString)}
              })
            """
        case "Option" =>
          q"""
             $sequence.map(opt => {
                ${val option = q"opt"
                  readOption(c)(option, Some(typeArg.get.typeArgs.head))}
               })
           """
        case _ =>
          q"""
           $sequence.map(obj => {
             ${val sc = q"obj.asInstanceOf[JsonObject]"
            readObject(c)(typeArg.get, sc)}
               })
           """
      }}
    ${if (fieldType != "Array") {
      val seqCons = Ident(TermName(fieldType))
      q"${seqCons}() ++ seq"
    } else
      q"seq"
    }"""
  }

  def toJsonAnon[T](obj: T): JsonObject = macro toJsonAnon_impl[T]

  def toJsonAnon_impl[T: c.WeakTypeTag](c: Context)(obj: c.Expr[T]): c.Tree = {

    import c.universe._

    val stmts = objectMembers(c)(weakTypeOf[T], obj)

    q"json.JsonObject(..$stmts)"
  }

  private def objectMembers(c: Context)(tpe: c.universe.Type, obj: c.Expr[Any]): Iterable[c.Tree] = {

    import c.universe._

    val fields = classMembers(c)(tpe)

    fields.map { field =>
      val (fieldName, fieldTypeName, fieldType, fieldTypeArg, iterable, bases, fieldTpe) = field
      val fieldTerm = TermName(fieldName)
      val id = q"$fieldName"
      val value = q"$obj.$fieldTerm"
      fieldTypeName match {
        case "Int" | "Long" | "Double" | "Float" | "Short" | "Byte" => q"json.JsonNumber($id, $value)"
        case "String" => q"""json.JsonString($id, $value)"""
        case "Boolean" => q"json.JsonBoolean($id, $value)"
        case "Null" => q"json.JsonNull($id)"
        case "Option" =>
          val typeArg = fieldTypeArg.get.typeSymbol.name.toString
          typeArg match {
            case "Int" | "Long" | "Double" | "Float" | "Short" | "Byte" =>
              q"if ($value.isDefined) json.JsonNumber($id, $value.get) else json.JsonNull($id)"
            case "String" => q"if ($value.isDefined) json.JsonString($id, $value.get) else json.JsonNull($id)"
            case "Boolean" => q"if ($value.isDefined) json.JsonBoolean($id, $value.get) else json.JsonNull($id)"
            case _ =>
              val optValue = q"$value.getOrElse(null)"
              q"""
                 if ($optValue != null)
                   json.JsonObject($id, ..${objectMembers(c)(fieldTypeArg.get, c.Expr(optValue))})
                 else
                   json.JsonNull($id)
               """
          }
        case "Array" =>
          val values = sequenceElements(c)(fieldTypeArg, value)
          q"new json.JsonArray($id, $values)"
        case _ if iterable.isDefined =>
          val values = sequenceElements(c)(fieldTypeArg, value)
          q"new json.JsonArray($id, $values)"
        case _ => q"json.JsonObject($id, ..${objectMembers(c)(fieldType.typeSignature, c.Expr(value))})"
      }
    }
  }

  private def classMembers(c: Context)(tpe: c.universe.Type): List[(String, String, c.universe.Symbol,
    Option[c.universe.Type], Option[c.universe.Symbol], List[c.universe.Symbol], c.universe.Type)] = {

    tpe.members.collect {
      case field if field.isMethod && field.asMethod.isCaseAccessor =>
        (field.name.toTermName.toString,
          field.typeSignature.erasure.resultType.typeSymbol.name.toString,
          field.typeSignature.erasure.resultType.typeSymbol,
          if (field.typeSignature.resultType.typeArgs.isEmpty)
            field.typeSignature.erasure.resultType.typeArgs.headOption
          else
            field.typeSignature.resultType.typeArgs.headOption,
        field.typeSignature.baseClasses.find(_.typeSignature.typeSymbol.name.toString == "Iterable"),
        field.typeSignature.baseClasses,
        field.typeSignature)
    }.toList.reverse
  }

  private def sequenceElements(c: Context)(elementType: Option[c.universe.Type], sequence: c.Tree) = {

    import c.universe._

    val typeArg = elementType.get.typeSymbol.name.toString
    typeArg match {
      case "Int" | "Long" | "Double" | "Float" | "Short" | "Byte" => q"$sequence.map(json.JsonNumber(_))"
      case "String" => q"$sequence.map(json.JsonString(_))"
      case "Boolean" => q"$sequence.map(json.JsonBoolean(_))"
      case _ => q"$sequence.map(toJsonAnon(_))"
    }
  }
}