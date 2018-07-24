package json

import language.experimental.macros
import reflect.macros.blackbox.Context
//import scala.reflect.runtime.universe._
/**
  * Created by Ian on 26/06/2018.
  */
object DebugMacro {

  def jsStr(str: String): String/*JsonString*/ = macro jsStr_impl

  def jsStr_impl(c: Context)(str: c.Expr[String]) = {
    import c.universe._

    val defaultWriter = q"json.writer.DefaultWriteContext()"

    val mems = List(q"""json.JsonString("name", $str)""", q"""json.JsonNumber("id", 1)""")
    q"json.writer.JsonWriter.write($defaultWriter)(json.JsonObject(..$mems))"
  }

  def toJson[T](obj: T): String = macro toJson_impl[T]

  def toJson_impl[T: c.WeakTypeTag](c: Context)(obj: c.Expr[T]) : c.Tree = {

    import c.universe._

    val stmts = getMems(c)(weakTypeOf[T], obj)

    q"json.writer.JsonWriter.write(json.writer.DefaultWriteContext())(json.JsonObject(..$stmts))"
  }

  def getMembers[T: c.WeakTypeTag](c: Context)(obj: c.Expr[T]): Iterable[c.Tree] = {

    import c.universe._

    val tpe = weakTypeOf[T]

    val fields = tpe.members.collect {
      case field if field.isMethod && field.asMethod.isCaseAccessor =>
        (field.name.toTermName.toString, field.typeSignature.resultType.typeSymbol.name.toString, field.typeSignature.resultType.typeSymbol)
    }

    fields.map { field =>
      val (fieldName, fieldTypeName, fieldType) = field
      val fieldRef = TermName(fieldName)
      val id = q"$fieldName"
      val extr = q"$obj.$fieldRef"
      fieldTypeName match {
        case "Int" | "Integer" | "Long" => q"json.JsonNumber($id, $extr)"
        case "String" => q"""json.JsonString($id, $extr)"""
        case "Boolean" => q"json.JsonBoolean($id, $extr)"
        case "Null" => q"json.JsonNull($id)"
        case _ => {

          //val ConstantType(Constant(tpe: Type)) = extr.tpe
          //q"val childStmts = getMembers[$tpe]($c)(${c.Expr(extr)}"
          val childStmts = getMembers(c)(c.Expr(extr))
          q"json.JsonObject(..$childStmts)"
        }
      }
    }
  }

  def getMems(c: Context)(tpe: c.universe.Type, obj: c.Expr[Any]): Iterable[c.Tree] ={
    import c.universe._

    val fields = tpe.members.collect {
      case field if field.isMethod && field.asMethod.isCaseAccessor =>
        (field.name.toTermName.toString,
          field.typeSignature.resultType.typeSymbol.name.toString,
          field.typeSignature.resultType.typeSymbol)
    }.toList.reverse

    fields.map { field =>
      val (fieldName, fieldTypeName, fieldType) = field
      val fieldTerm = TermName(fieldName)
      val id = q"$fieldName"
      val value = q"$obj.$fieldTerm"
      fieldTypeName match {
        case "Int" | "Integer" | "Long" | "Double" | "Float" | "Short" | "Byte" => q"json.JsonNumber($id, $value)"
        case "String" => q"""json.JsonString($id, $value)"""
        case "Boolean" => q"json.JsonBoolean($id, $value)"
        case "Null" => q"json.JsonNull($id)"
        case "Option" => {
          q"""$value match {
             case None => json.JsonNull($id)
             case Some(x) if x.isInstanceOf[Short] => json.JsonNumber($id, $value.get)
             case Some(x) if x.isInstanceOf[Int] => json.JsonNumber($id, $value.get)
             case Some(x) if x.isInstanceOf[Long] => json.JsonNumber($id, $value.get)
             case Some(x) if x.isInstanceOf[Double] => json.JsonNumber($id, $value.get)
             case Some(x) if x.isInstanceOf[Float] => json.JsonNumber($id, $value.get)
             case Some(x) if x.isInstanceOf[Byte] => json.JsonNumber($id, $value.get)
             }"""
        }
        case "Array" => q"json.JsonArray($id, ..$value)"
        case _ => {
          val childStmts = getMems(c)(fieldType.typeSignature, c.Expr(value))
          q"json.JsonObject($id, ..$childStmts)"
        }
      }
    }
  }

  def hello(): Unit = macro hello_impl

  def hello_impl(c: Context)(): c.Expr[Unit] = {

    import c.universe._

    reify { println("Hello World") }
  }

  def printParam(param: Any): Unit = macro printparam_impl

  def printparam_impl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {

    import c.universe._

    reify { println(param.splice) }
  }

  def debug(param: Any): Unit = macro debug_impl

  def debug_impl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {

    import c.universe._

    val paramRepr = show(param.tree)
    val paramReprTree = Literal(Constant(paramRepr))
    val paramReprExpr = c.Expr[String](paramReprTree)

    reify { println(paramReprExpr.splice + " = " + param.splice) }
  }

  def listMembers[T](obj: T): Unit = macro listMembers_impl[T]

  def listMembers_impl[T](c: Context)(obj: c.Expr[T]): c.Expr[Unit] = {

    import c.universe._

    val typeRef = weakTypeOf[T]
    val list = typeRef.members.filter(m => m.isPublic && !m.isAbstract && !m.isConstructor && m.isMethod)
      .map(_.name)
      .foldLeft("")(_ + _)

    val reprTree = Literal(Constant(list))
    val reprExpr = c.Expr[String](reprTree)

    reify { println(reprExpr.splice) }
  }
}
