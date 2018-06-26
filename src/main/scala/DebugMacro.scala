import language.experimental.macros
import reflect.macros.whitebox.Context

/**
  * Created by Ian on 26/06/2018.
  */
object DebugMacro {

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
}
