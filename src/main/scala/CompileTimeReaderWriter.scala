package json

import json.exceptions.{InvalidObjectFormatException, UnsupportedTypeException}

import language.experimental.macros
import reflect.macros.blackbox.Context
/**
  * Created by Ian on 26/06/2018.
  */
object CompileTimeReaderWriter {

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

    val fields = classMembers(c)(tpe)

    val className = tpe.typeSymbol.name.toString
    val consSelect = Ident(TermName(className))

    val members = fields.map(field => (field._1, field._2, TermName(field._1), field._6, field._7, field._4))

    q"""
      import json._

      def readObj(source: JsonElement) =

      $consSelect(..${
      members.map { m =>
        //val (fieldName, fieldTypeName, fieldType, fieldTypeArg, iterable, bases, fieldTpe) = field
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
          case "Option" =>
            q"""
               val field = source.elements.find(_.elementId == $fieldName).get
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
                case _ =>
                  val src = q"field.asInstanceOf[JsonObject]"
                  q"Some(${readObject(c)(typeArg.get, src)})"
              }
            }"""
          case "Array" | "List" | "Vector" | "Seq" =>
            readSequence(c)(q"source.elements.find(_.elementId == $fieldName).get.elements.toArray", typeArg, fieldType)
          case _ if baseTypes.find(_.name.toString == "AnyVal").isDefined => throw new UnsupportedTypeException(fieldName, s"""Type "$fieldType" is not supported""")
          // TODO - unsupported primitive check (NtH)
          //case _ if baseTypes.isEmpty => throw new UnsupportedTypeException(fieldName, s"""Type "$fieldType" is not supported""")
          case _ =>
            val src = q"source.elements.find(_.elementId == $fieldName).get.asInstanceOf[JsonObject]"
            q"${readObject(c)(fieldTpe, src)}"
        }
        q"$fieldTerm = $fieldValue"
      }})

       readObj($src)
     """
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
        // TODO - array of optional values
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