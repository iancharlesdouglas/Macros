package json

import language.experimental.macros
import reflect.macros.blackbox.Context
/**
  * Created by Ian on 26/06/2018.
  */
object CaseClassWriter {

  def toJson[T](obj: T): String = macro toJson_impl[T]

  def toJson_impl[T: c.WeakTypeTag](c: Context)(obj: c.Expr[T]): c.Tree = {

    import c.universe._

    val objType = weakTypeOf[T];
    val objTypeName = objType.erasure.typeSymbol.name.toString

    val statements =
      if (objTypeName == "Array") {

        val typeArg =
          if (objType.resultType.typeArgs.isEmpty)
            objType.erasure.resultType.typeArgs.headOption
          else
            objType.resultType.typeArgs.headOption

        val elements = q"..$obj"
        val values = arrayElements(c)(typeArg, elements)

        q"""new json.JsonArray("", $values)"""

      } else {

        val stmts = getObjectMembers(c)(weakTypeOf[T], obj)

        q"json.JsonObject(..$stmts)"
      }

    q"""json.writer.JsonWriter.write(json.writer.DefaultWriteContext())($statements)"""
  }

  def toJsonAnon[T](obj: T): JsonObject = macro toJsonAnon_impl[T]

  def toJsonAnon_impl[T: c.WeakTypeTag](c: Context)(obj: c.Expr[T]): c.Tree = {

    import c.universe._

    val stmts = getObjectMembers(c)(weakTypeOf[T], obj)

    q"json.JsonObject(..$stmts)"
  }

  def getObjectMembers(c: Context)(tpe: c.universe.Type, obj: c.Expr[Any]): Iterable[c.Tree] = {

    import c.universe._

    val fields = tpe.members.collect {
      case field if field.isMethod && field.asMethod.isCaseAccessor =>
        (field.name.toTermName.toString,
          field.typeSignature.erasure.resultType.typeSymbol.name.toString,
          field.typeSignature.erasure.resultType.typeSymbol,
          if (field.typeSignature.resultType.typeArgs.isEmpty)
            field.typeSignature.erasure.resultType.typeArgs.headOption
          else
            field.typeSignature.resultType.typeArgs.headOption)
    }.toList.reverse

    fields.map { field =>
      val (fieldName, fieldTypeName, fieldType, fieldTypeArg) = field
      val fieldTerm = TermName(fieldName)
      val id = q"$fieldName"
      val value = q"$obj.$fieldTerm"
      fieldTypeName match {
        case "Int" | "Integer" | "Long" | "Double" | "Float" | "Short" | "Byte" => q"json.JsonNumber($id, $value)"
        case "String" => q"""json.JsonString($id, $value)"""
        case "Boolean" => q"json.JsonBoolean($id, $value)"
        case "Null" => q"json.JsonNull($id)"
        case "Option" =>
          val typeArg = fieldTypeArg.get.typeSymbol.name.toString
          typeArg match {
            case "Int" | "Integer" | "Long" | "Double" | "Float" | "Short" | "Byte" => q"json.JsonNumber($id, $value.get)"
            case "String" => q"json.JsonString($id, $value.get)"
            case "Boolean" => q"json.JsonBoolean($id, $value.get)"
            case _ => {
              val objValue = q"$obj.$fieldTerm.get"
              val members = getObjectMembers(c)(fieldTypeArg.get, c.Expr(objValue))
              q"json.JsonObject($id, ..$members)"
            }
          }
        case "Array" =>
          val values = arrayElements(c)(fieldTypeArg, value)
          q"new json.JsonArray($id, $values)"
        case _ =>
          val members = getObjectMembers(c)(fieldType.typeSignature, c.Expr(value))
          q"json.JsonObject($id, ..$members)"
      }
    }
  }

  private def arrayElements(c: Context)(elementType: Option[c.universe.Type], array: c.Tree) = {

    import c.universe._

    val typeArg = elementType.get.typeSymbol.name.toString
    typeArg match {
      case "Int" | "Integer" | "Long" | "Double" | "Float" | "Short" | "Byte" => q"$array.map(json.JsonNumber(_))"
      case "String" => q"$array.map(json.JsonString(_))"
      case "Boolean" => q"$array.map(json.JsonBoolean(_))"
      case _ => q"$array.map(toJsonAnon(_))"
    }
  }
}