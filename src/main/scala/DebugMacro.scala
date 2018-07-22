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
    //q"json.JsonString($str)"
    q"json.writer.JsonWriter.write(json.writer.DefaultWriteContext())(json.JsonString($str))"
  }

  def printMembers[T](obj: T): Any/*String*/ = macro printMembers_impl[T]

  def printMembers_impl[T: c.WeakTypeTag](c: Context)(obj: c.Expr[T]) = {//: c.Expr[String] = {
    import c.universe._
    val tt = weakTypeTag[T]
    val tm = tt.tpe.members
    val fs = tt.tpe.members.collect { case f if f.isMethod && f.asMethod.isCaseAccessor => f.name.toTermName.toString }
    val tpe = weakTypeOf[T]
    val typeExpr = q"val t: ${tpe.getClass.getName}"

    val mems = tpe.members
    val fields = tpe.members.collect {
      case field if field.isMethod && field.asMethod.isCaseAccessor => field.name.toTermName.toString
    }
    //reify { println(fields) }
    val members = fields.foldLeft("")(_ + _)
    c.Expr(q"$members")
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
