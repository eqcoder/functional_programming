package Hw2

import fastparse._
import MultiLineWhitespace._
import scala.collection.immutable.HashMap 

sealed trait Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, expr: Expr, env: Env) extends Val

case class Env(hashmap: HashMap[Var,Val]) {
  def apply(variable: Var): Val = hashmap(variable)
  def exists(v: Var): Boolean = 
    hashmap.exists((a: (Var, Val)) => a._1 == v)
  def add(v: Var, value: Val) = Env(hashmap + (v -> value))
}

sealed trait Program
sealed trait Expr extends Program
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(name: Var, value: Expr, body: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr
case class LetRec(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr

sealed trait IntExpr
case class IntConst(n: Int) extends IntExpr
case object IntVar extends IntExpr
case class IntAdd(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSub(l: IntExpr, r: IntExpr) extends IntExpr
case class IntMul(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSigma(f: IntExpr, t: IntExpr, b: IntExpr) extends IntExpr
case class IntPow(b: IntExpr, e: IntExpr) extends IntExpr



package object Hw2 {

  

}

object IntInterpreter {
  def evalInt(expr: IntExpr, env: Option[Int]): Int = expr match{
    case IntConst(n) => n
    case IntVar =>env match{
      case Some(x) => x
      case None => throw new Exception("1")
    } 
    case IntAdd(l, r) => (evalInt(l, env), evalInt(r, env)) match{
      case (l:Int, v:Int) => l+v
      case _ =>throw new Exception("Type error")
    }
    case IntSub(l, r) => (evalInt(l, env), evalInt(r, env)) match{
      case (l:Int, v:Int) => l-v
      case _ =>throw new Exception("Type error")
    }
    case IntMul(l, r) => (evalInt(l, env), evalInt(r, env)) match{
      case (l:Int, v:Int) => l*v
      case _ =>throw new Exception("Type error")
    }
    case IntSigma(f, t, b) =>(evalInt(f, env), evalInt(t, env)) match{
      case (f:Int, t:Int) =>if(f>t) 0 else evalInt(b, Some(f))+evalInt(IntSigma(IntConst(f+1), IntConst(t), b), env)
      case _ =>throw new Exception("Type error")
    }
    case IntPow(b, e) => (evalInt(b, env), evalInt(e, env)) match{
      case (b:Int, e:Int)=>if(e==0) 1 else b*evalInt(IntPow(IntConst(b), IntConst(e-1)), env)
    }
  }
  def apply(s: String): Int = {
    val parsed = IntParser(s)
    evalInt(parsed, None)
  }
}

object LetRecInterpreter {
  
  def eval(env: Env, expr: Expr): Val = expr match{
    case Add(l, r) => (eval(env, l), eval(env, r)) match{
      case (l:IntVal, r:IntVal)=>IntVal(l.n+r.n)
      case _ =>throw new Exception("Type error")
    }
    case Sub(l, r) => (eval(env, l), eval(env, r)) match{
      case (l:IntVal, r:IntVal)=>IntVal(l.n-r.n)
      case _ =>throw new Exception("Type error")
    } 
    case Var(s) => {
      if(env.exists(Var(s))) env(Var(s)) else throw new Exception("1")
    }
    case Const(n: Int) => IntVal(n)
    case Iszero(c) => eval(env, c) match{
      case x:IntVal =>BoolVal(x.n==0)
      case _ =>throw new Exception("Type error")
    }
    case Ite(c, t, f) => eval(env, c) match{
      case v: BoolVal =>if(v.b) eval(env, t) else eval(env, f)
      case _ =>throw new Exception("Type error")
    }
    case Let(name:Var, value:Expr, body:Expr) => eval(env.add(name, eval(env, value)), body)
    case Paren(expr:Expr) => eval(env, expr)
    case PCall(ftn:Expr, arg:Expr) => ftn match{
      case f:Var => env.apply(f) match{
        case RecProcVal(fv:Var, av:Var, body:Expr, expr:Expr, en:Env) => {
          eval(env.add(av, eval(env, arg)), LetRec(fv, av, body, expr))
        }
        case _=>throw new Exception("Type error")
      }
      case Proc(v:Var, expr:Expr) => eval(env.add(v, eval(env, arg)), Proc(v, expr))
      case LetRec(fname:Var, aname:Var, fbody:Expr, ibody:Expr)=> {
        val new_env=env.add(fname, RecProcVal(fname, aname, fbody, ibody, env))
        eval(new_env.add(aname, eval(env, arg)), LetRec(fname, aname, fbody, ibody))
      }
      case _=>throw new Exception("Type error")
    }
    case Proc(v:Var, expr:Expr) => if(env.exists(v)) eval(env, expr) else ProcVal(v, expr, Env(new HashMap[Var,Val]()))
    case LetRec(fname:Var, aname:Var, fbody:Expr, ibody:Expr) => if(env.exists(fname)) eval(env, fbody) else RecProcVal(fname, aname, fbody, ibody, Env(new HashMap[Var,Val]()))
  }
  
  def apply(program: String): Val = {
    val parsed = LetRecParserDriver(program)
    eval(Env(new HashMap[Var,Val]()), parsed)
  }
}

object LetRecToString {
  def apply(expr: Expr): String = expr match{
    case Const(n) => n.toString()
    case Add(l, r) => apply(l)+" + "+apply(r)
    case Sub(l, r) => apply(l)+" - "+apply(r)
    case Iszero(c) => "iszero "+apply(c)
    case Ite(c, t, f) => "if "+apply(c)+" then "+apply(t)+" else "+apply(f)
    case Let(name, value, body) => "let "+apply(name)+" = "+apply(value)+" in "+apply(body)
    case LetRec(fname, aname, fbody, ibody) => "letrec "+apply(fname)+"("+apply(aname)+") = "+apply(fbody)+" in "+apply(ibody)
    case PCall(ftn, arg) => apply(ftn)+" "+apply(arg)
    case Paren(expr) => " ("+apply(expr)+") "
    case Proc(v, expr) => "proc "+v.s+" "+apply(expr)
    case Var(s) => s
    case _=>throw new Exception("Type error")
  }
}

object Hw2App extends App {
 
}
