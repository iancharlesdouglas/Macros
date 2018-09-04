package json

import json.exceptions.{UnsupportedRefTypeException, UnsupportedTypeException}

import language.experimental.macros
import reflect.macros.blackbox.Context

object CompileTimeReaderWriter {

  def jsonTo_impl[T: c.WeakTypeTag](c: Context): c.Tree = {

    import c.universe._

    val json = reify(c.prefix.splice.asInstanceOf[Typer.JsonString].text)

    val objType = weakTypeOf[T]

    objType.typeSymbol.name.toString match {
      case "Array" | "List" | "Vector" | "Seq" => {
        q"""import json._; val _seq = reader.JsonReader.read($json)
           if (_seq.successfully) {
             ${readSequence(c)(q"_seq.it", Some(objType.typeArgs.head),
          objType.typeSymbol.name.toString)}
           } else {
             TypedParseResult[${objType.typeArgs.head.typeSymbol.name.toString}](_seq.it, false, _seq.position, _seq.message)
           }
         """
      }
      case _ => readObject(c)(objType, q"import json._; reader.JsonReader.read($json)")
    }
  }

  def json_impl[T: c.WeakTypeTag](c: Context): c.Tree = {

    import c.universe._

    val wrapper = reify(c.prefix.splice).tree

    val objType = c.prefix.actualType.typeArgs.head
    val objTypeName = objType.erasure.typeSymbol.name.toString
    val obj = q"$wrapper.obj"
    val iterable = objType.baseClasses.find(_.typeSignature.typeSymbol.name.toString == "Iterable")

    val statements =
      if (objTypeName == "Array" || iterable.isDefined) {

        val typeArg =
          if (objType.resultType.typeArgs.isEmpty)
            objType.erasure.resultType.typeArgs.headOption
          else
            objType.resultType.typeArgs.headOption

        val elements = q"..$obj"
        val values = getSeqElements(c)(typeArg, elements)

        q"""new json.JsonArray("", $values)"""

      } else {

        q"""
           val __obj = $obj
           if (__obj == null)
             json.JsonNull()
           else
             json.JsonObject(..${objectMembers(c)(objType, c.Expr(q"__obj"))})
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
        val values = getSeqElements(c)(typeArg, elements)

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

  def fromJson[T](json: String): TypedParseResult[T] = macro fromJson_impl[T]

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

    if (!isCaseClass(c)(tpe))
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
          case "BigDecimal" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonNumber].value"
          case "BigInt" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonNumber].value.toBigInt"
          case "String" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonString].value"
          case "Char" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonString].value.toCharArray()(0)"
          case "Boolean" => q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonBoolean].value"
          case "Option" => readOption(c)(q"source.elements.find(_.elementId == $fieldName).get", typeArg)
          case "Array" | "List" | "Vector" | "Seq" =>
            readSequence(c)(q"source.elements.find(_.elementId == $fieldName).get.elements.toArray", typeArg, fieldType)
          /*case "Object" if !baseTypes.map(_.name.toString).contains("Product") =>
            throw new UnsupportedTypeException(fieldName, s"""Type "${fieldTpe.typeSymbol.alternatives.head.name.toString}" is not supported""")*/
          case _ =>
            val src = q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonObject]"
            q"${readObject(c)(fieldTpe, src)}"
        }
        q"$fieldTerm = $fieldValue"
      }})

       val _res = readObj($src)
       json.TypedParseResult[$className](_res.it, _res.successfully, _res.position, _res.message)
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
        case "BigDecimal" => q"Some(field.asInstanceOf[JsonNumber].value)"
        case "BigInt" => q"Some(field.asInstanceOf[JsonNumber].value.toBigInt)"
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
        case "BigDecimal" => q"$sequence.map(_.asInstanceOf[JsonNumber].value)"
        case "BigInt" => q"$sequence.map(_.asInstanceOf[JsonNumber].value.toBigInt)"
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
        case "Int" | "Long" | "Double" | "Float" | "Short" | "Byte" | "BigDecimal" =>
          q"json.JsonNumber($id, $value)"
        case "BigInt" =>
          q"json.JsonNumber($id, scala.math.BigDecimal($value))"
        case "String" => q"""json.JsonString($id, $value)"""
        case "Boolean" => q"json.JsonBoolean($id, $value)"
        case "Null" => q"json.JsonNull($id)"
        case "Option" =>
          val typeArg = fieldTypeArg.get.typeSymbol.name.toString
          typeArg match {
            case "Int" | "Long" | "Double" | "Float" | "Short" | "Byte" | "BigDecimal" =>
              q"if ($value.isDefined) json.JsonNumber($id, $value.get) else json.JsonNull($id)"
            case "BigInt" =>
              q"if ($value.isDefined) json.JsonNumber($id, scala.math.BigDecimal($value.get)) else json.JsonNull($id)"
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
          val values = getSeqElements(c)(fieldTypeArg, value)
          q"new json.JsonArray($id, $values)"
        case _ if iterable.isDefined =>
          val values = getSeqElements(c)(fieldTypeArg, value)
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

  private def isCaseClass(c: Context)(tpe: c.universe.Type): Boolean =
    tpe.members.exists(m => m.isMethod && m.asMethod.isCaseAccessor)

  private def getSeqElements(c: Context)(elementType: Option[c.universe.Type], sequence: c.Tree) = {

    import c.universe._

    val typeArg = elementType.get.typeSymbol.name.toString
    typeArg match {
      case "Int" | "Long" | "Double" | "Float" | "Short" | "Byte" | "BigDecimal" =>
        q"$sequence.map(json.JsonNumber(_))"
      case "BigInt" =>
        q"$sequence.map(value => json.JsonNumber(scala.math.BigDecimal(value)))"
      case "String" => q"$sequence.map(json.JsonString(_))"
      case "Boolean" => q"$sequence.map(json.JsonBoolean(_))"
      case "Option" | "Some" =>
        val optTypeArg = elementType.get.typeArgs.head.typeSymbol.name.toString
        optTypeArg match {
          case "Int" | "Long" | "Double" | "Float" | "Short" | "Byte" | "BigDecimal" =>
            q"$sequence.map(v => if (v.isDefined) json.JsonNumber(v.get) else json.JsonNull())"
          case "BigInt" =>
            q"$sequence.map(v => if (v.isDefined) json.JsonNumber(BigDecimal(v.get)) else json.JsonNull())"
        }
      case "None" => q"$sequence.map(json.JsonNull())"
      case _ => q"$sequence.map(toJsonAnon(_))"
    }
  }
}