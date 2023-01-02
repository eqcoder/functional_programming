package hw3

import scala.collection.immutable.HashMap 
import hw3._


package object hw3 {
  type Env = HashMap[Var,Val]
  type Loc = Int
  
}

case class Mem(m: HashMap[Loc,Val], top: Loc) {

}

sealed trait Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, env: Env) extends Val
case class LocVal(l: Loc) extends Val


sealed trait Program
sealed trait Expr extends Program
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class GTExpr(l: Expr, r: Expr) extends Expr
case class GEQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class ValExpr(name: Var, value: Expr, body: Expr) extends Expr
case class VarExpr(name: Var, value: Expr, body: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class DefExpr(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr







object MiniScalaInterpreter {

  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)
  
  def doInterpret(env: Env, mem: Mem, expr: Expr): (Val, Mem) = {
    expr match{
    case Const(n)=>(IntVal(n),mem)
    case Var(s)=>{
      if(env.exists((a: (Var, Val)) => a._1 == Var(s))) env(Var(s)) match{
        case LocVal(l)=>(mem.m(l), mem)
        case _=>(env(Var(s)), mem)
      }
      else throw new Exception("1")
    }
    case Add(l, r) => {
      doInterpret(env, mem, l) match{
      case (l:IntVal, m:Mem)=> doInterpret(env, m, r) match{
        case (r:IntVal, new_m:Mem)=>(IntVal(l.n+r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case Sub(l, r) => {
      doInterpret(env, mem, l) match{
      case (l:IntVal, m:Mem)=> doInterpret(env, m, r) match{
        case (r:IntVal, new_m:Mem)=>(IntVal(l.n-r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case Mul(l, r) => {
      doInterpret(env, mem, l) match{
      case (l:IntVal, m:Mem)=> doInterpret(env, m, r) match{
        case (r:IntVal, new_m:Mem)=>(IntVal(l.n*r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case Div(l, r) => {
      doInterpret(env, mem, l) match{
      case (l:IntVal, m:Mem)=> doInterpret(env, m, r) match{
        case (r:IntVal, new_m:Mem)=>(IntVal(l.n/r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case GEQExpr(l, r) => {
      doInterpret(env, mem, l) match{
      case (l:IntVal, m:Mem)=> doInterpret(env, m, r) match{
        case (r:IntVal, new_m:Mem)=>(BoolVal(l.n>=r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case GTExpr(l, r) => {
      doInterpret(env, mem, l) match{
      case (l:IntVal, m:Mem)=> doInterpret(env, m, r) match{
        case (r:IntVal, new_m:Mem)=>(BoolVal(l.n>r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case Iszero(c) => {
      doInterpret(env, mem, c) match{
      case (x:IntVal, m:Mem) => {
        (BoolVal(x.n==0), m)
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case Ite(c, t, f) => {
      doInterpret(env, mem, c) match{
      case (v:BoolVal, m:Mem) =>{
        if(v.b) doInterpret(env, m, t) else doInterpret(env, m, f)
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case ValExpr(name, value, body) => {
      doInterpret(env, mem, value) match{
      case (v: Val, m:Mem)=>doInterpret(env+(name->v), m, body)
      case _=>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case VarExpr(name, value, body) => {
      doInterpret(env, mem, value) match{
      case (v: Val, m :Mem)=>{
        doInterpret(env+(name->LocVal(m.top)), new Mem(m.m+(m.top->v), m.top+1), body)
      }
      case _=>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case Proc(v, expr) => if(env.exists((a: (Var, Val)) => a._1 == v)) doInterpret(env, mem, expr) else (ProcVal(v, expr, new Env()), mem)
    case PCall(ftn, arg) => {
      ftn match{
      case Proc(v, expr) => doInterpret(env, mem, arg) match{
        case (va:Val, m:Mem)=>doInterpret(env+(v->va), m, expr)
      }
      case f:Var=>{
        env(f) match{
        case RecProcVal(fv, av, body, en) => doInterpret(env, mem, arg) match{
          case (va:Val, m:Mem)=>doInterpret(en+(av->va)+(fv->RecProcVal(fv, av, body, en)), m, body)
          case _=>throw new UndefinedSemantics(s"message ${expr}")
        }
        case _=>throw new UndefinedSemantics(s"message ${expr}")
        }
      }
      case _=>throw new UndefinedSemantics(s"message ${expr}")
    }
  }

    case DefExpr(fname, aname, fbody, ibody) => doInterpret(env+(fname->RecProcVal(fname, aname, fbody, env)), mem, ibody)
    case Asn(v:Var, e) => env(v) match{
      case LocVal(l)=>doInterpret(env, mem, e) match{
        case (va:Val, m:Mem)=>(va, new Mem(m.m+(l->va), mem.top+1))
        case _=>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _=>throw new UndefinedSemantics(s"message ${expr}")
    }
    case Block(f, s) => doInterpret(env, mem, f) match{
      case(v:Val, m:Mem)=> doInterpret(env, m, s)
      case _=>throw new UndefinedSemantics(s"message ${expr}")
    }
    case Paren(expr:Expr) => doInterpret(env, mem, expr)
  }
}
  
  def apply(program: String): Val = {
    val parsed = MiniScalaParserDriver(program)
    val (value, mem)=doInterpret(new Env(), Mem(new HashMap[Loc,Val],0), parsed)
    value
  }

}


object Hw3App extends App {
}
