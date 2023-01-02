package Hw0

sealed trait IntList
import scala.collection.immutable.HashMap
case class Nil() extends IntList
case class Cons(h: Int, t: IntList) extends IntList

class Env(map: HashMap[Var,Val]) {
  def update(variable: Var, value: Val): Env = {
    (new Env(map + (variable -> value)))
  }
  def get(variable: Var): Val = {
    map(variable)
  }
  override def toString(): String = {
    "[ " ++ map.mkString(", ") ++ " ]"
  }
}


case class Var(v: String)

sealed trait Expr
case class BoolExpr(b: Boolean) extends Expr
case class IntExpr(v: Int) extends Expr
case class VarExpr(v: String) extends Expr
case class Plus(a: Expr, b: Expr) extends Expr // <- E + E
case class Let(v: Var, a: Expr ,b: Expr) extends Expr // let v = a in b
case class ProcDecl(v: Var, body: Expr) extends Expr // proc x E

// 1
// 2
// x
// t


sealed trait Val
case class IntVal(v: Int) extends Val
case class BoolVal(v: Boolean) extends Val
case class ProcVal(v: Var, body: Expr, env: Env) extends Val




// Val = Int + Boolean + Procedure
// Procedure = Var X Expr X Env

object Interpreter {

  def eval(env: Env, prog: Expr): Val = {
    prog match {
      case BoolExpr(b)  => BoolVal(b)
      case IntExpr(i) => IntVal(i)
      case VarExpr(v) => {
        env.get(Var(v))
      }
      case Plus(a, b) => {
        val result_a = eval(env, a)
        val result_b = eval(env, b)
        result_a match {
          case IntVal(v_a) => {
            result_b match {
              case IntVal(v_b) => {
                IntVal(v_a + v_b)
              }
              case _ => {
                throw new Exception("Undefined Semantics")
              }
            }
          }
          case _ => {
            throw new Exception("Undefined Semantics")
          }
        }
      }
      case Let(v, a, b) => {
        val res_a = eval(env, a)
        val new_env = env.update(v, res_a)
        val res_b = eval(new_env, b)
        res_b
      }
      case ProcDecl(v, asd) => {
        ProcVal(v, asd, env)
      }
    }
  }

}

object Hw0 extends App {

  val e = new Env(new HashMap[Var,Val]())
  val prog = Let(
      Var("x"),
      Plus(IntExpr(3), IntExpr(2)),
      Let(Var("f"),
         ProcDecl(Var("y"),
        Plus(VarExpr("y"), IntExpr(1))), VarExpr("f")
      )
  )
  // let x = (3 + 2) in (let f = (proc y y + 1)) in f
  def sum(l: IntList): Int = l match {
    case Nil()=>0
    case Cons(h, t) => h+sum(t)
  }

val prog = """let x = 7
in let y = 2
in let y = let x = x-1
in x - y
in (x-8)-y"""
val Parsed.Success(value, successIndex) = LetParser(prog)
println(value)
println(InterpretLet(value))
}
