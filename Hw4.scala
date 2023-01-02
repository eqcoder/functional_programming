package hw4

import scala.collection.immutable.HashMap 
import hw4._


package object hw4 {
  type Env = HashMap[Var,LocVal]
}

case class Mem(m: HashMap[LocVal,Val], top: Int) {
  def extended(v: Val): (Mem, LocVal) = {
    val new_mem = Mem(m.updated(LocVal(top),v), top+1)
    (new_mem,LocVal(top))
  }
  def updated(l: LocVal, new_val: Val): Option[Mem] = {
    m.get(l) match {
      case Some(v) => Some(Mem(m.updated(l, new_val), top))
      case None => None
    }
  }
  def get(l: LocVal): Option[Val] = m.get(l)
  def getLocs(): List[LocVal] = m.keySet.toList
}

sealed trait Val
case object SkipVal extends Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(args: List[Var], expr: Expr, env: Env) extends Val
case class LocVal(l: Int) extends Val
sealed trait RecordValLike extends Val
case object EmptyRecordVal extends RecordValLike
case class RecordVal(field: Var, loc: LocVal, next: RecordValLike) extends RecordValLike


sealed trait Program
sealed trait Expr extends Program
case object Skip extends Expr
case object False extends Expr
case object True extends Expr
case class NotExpr(expr: Expr) extends Expr
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr {
  override def toString = s"Var(${"\""}${s}${"\""})"
}
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class LTEExpr(l: Expr, r: Expr) extends Expr
case class EQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(i: Var, v: Expr, body: Expr) extends Expr
case class Proc(args: List[Var], expr: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class BeginEnd(expr: Expr) extends Expr
case class FieldAccess(record: Expr, field: Var) extends Expr
case class FieldAssign(record: Expr, field: Var, new_val: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCallV(ftn: Expr, arg: List[Expr]) extends Expr
case class PCallR(ftn: Expr, arg: List[Var]) extends Expr
case class WhileExpr(cond: Expr, body: Expr) extends Expr
sealed trait RecordLike extends Expr
case object EmptyRecordExpr extends RecordLike
case class RecordExpr(field: Var, initVal: Expr, next: RecordLike) extends RecordLike








object MiniCInterpreter {

  case class Result(v: Val, m: Mem)
  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)
    
  
  def eval(env: Env, mem: Mem, expr: Expr): Result = expr match{
    case Const(n)=>Result(IntVal(n),mem)
    case Skip => Result(SkipVal, mem)
    case True => Result(BoolVal(true), mem)
    case False => Result(BoolVal(false), mem)
    case NotExpr(expr) => eval(env, mem, expr) match{
      case Result(BoolVal(b), m:Mem)=>if(b==true) Result(BoolVal(false), m) else Result(BoolVal(true), m)
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
    case Var(s)=>{
      if(env.exists((a: (Var, Val)) => a._1 == Var(s))) env(Var(s)) match{
        case l: LocVal=>Result(mem.m(l), mem)
        case _=>Result(env(Var(s)), mem)
      }
      else throw new UndefinedSemantics(s"message ${expr}")
    }
    case Add(l, r) => {
      eval(env, mem, l) match{
      case Result(l:IntVal, m:Mem)=> eval(env, m, r) match{
        case Result(r:IntVal, new_m:Mem)=>Result(IntVal(l.n+r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case Sub(l, r) => {
      eval(env, mem, l) match{
      case Result(l:IntVal, m:Mem)=> eval(env, m, r) match{
        case Result(r:IntVal, new_m:Mem)=>Result(IntVal(l.n-r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case Mul(l, r) => {
      eval(env, mem, l) match{
      case Result(l:IntVal, m:Mem)=> eval(env, m, r) match{
        case Result(r:IntVal, new_m:Mem)=>Result(IntVal(l.n*r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case Div(l, r) => {
      eval(env, mem, l) match{
      case Result(l:IntVal, m:Mem)=> eval(env, m, r) match{
        case Result(r:IntVal, new_m:Mem)=>if(r.n==0) throw new UndefinedSemantics(s"message ${expr}") else Result(IntVal(l.n/r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case LTEExpr(l, r) => {
      eval(env, mem, l) match{
      case Result(l:IntVal, m:Mem)=> eval(env, m, r) match{
        case Result(r:IntVal, new_m:Mem)=>Result(BoolVal(l.n<=r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case EQExpr(l, r) => {
      eval(env, mem, l) match{
      case Result(l:IntVal, m:Mem)=> eval(env, m, r) match{
        case Result(r:IntVal, new_m:Mem)=>Result(BoolVal(l.n==r.n), new_m)
        case _ =>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case Iszero(c) => {
      eval(env, mem, c) match{
      case Result(x:IntVal, m:Mem) => {
        Result(BoolVal(x.n==0), m)
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
    case Ite(c, t, f) => {
      eval(env, mem, c) match{
      case Result(v:BoolVal, m:Mem) =>{
        if(v.b) eval(env, m, t) else eval(env, m, f)
      }
      case _ =>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
  case Let(i, v, body) => eval(env, mem, v) match{
    case Result(c:Val, m:Mem)=>{
      eval(env+(i->LocVal(m.top)), m.extended(c)._1, body)
    }
  }
  case Block(f, s) => eval(env, mem, f) match{
    case Result(v:Val, m:Mem)=> eval(env, m, s)
    case _=>throw new UndefinedSemantics(s"message ${expr}")
  }
  case Asn(v:Var, e) => eval(env, mem, e) match{
        case Result(va:Val, m:Mem)=>{
          if(env.exists((a: (Var, Val)) => a._1 == v)) {
            m.updated(env(v), va) match{
          case Some(v)=>Result(va, v)
          case None=>throw new UndefinedSemantics(s"message ${expr}")
        case _=>throw new UndefinedSemantics(s"message ${expr}")
            }
          }
            else throw new UndefinedSemantics(s"message ${expr}")
    }
  }
  case BeginEnd(expr) => eval(env, mem, expr) 
  case WhileExpr(cond, body) => eval(env, mem, cond) match{
    case Result(v:BoolVal, m:Mem)=> {
      if(v==BoolVal(true)) eval(env, mem, body) match{
        case Result(c: Val, m:Mem)=> eval(env, m, WhileExpr(cond, body))
      }
      else Result(BoolVal(false), m)
      }
    case _=>throw new UndefinedSemantics(s"message ${expr}")
  }
  case Proc(v, expr) => Result(ProcVal(v, expr, new Env()), mem)
  case PCallV(ftn, arg) => {
     ftn match{
      case f:Var =>eval(env, mem, f) match{
        case Result(p:ProcVal, mem:Mem)=>{
        def li(l:List[Var], c:List[Expr], e:Env, mem:Mem):Result=(l,c) match{
            case(head::next, h::n)=>li(next, n, e+(head->LocVal(mem.top)), mem.extended(eval(env, mem, h).v)._1)
            case (Nil, Nil)=> eval(e, Mem(mem.m.updated(env(f), ProcVal(p.args, p.expr, e)), mem.top), p.expr)
        }
        li(p.args, arg, p.env, mem)
        }
      case _=>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _=>throw new UndefinedSemantics(s"message ${expr}")
     }
     
    }
  case PCallR(ftn, arg) => {
     ftn match{
      case f:Var =>eval(env, mem, f) match{
        case Result(p:ProcVal, mem:Mem)=>{
        def li(l:List[Var], c:List[Var], e:Env):Result=(l,c) match{
            case(head::next, h::n)=>if(env.exists((a: (Var, Val)) => a._1 == h)) li(next, n, e+(head->env(h))) else throw new UndefinedSemantics(s"message ${expr}")
            case (Nil, Nil)=> {
              if(env.exists((a: (Var, Val)) => a._1 == f)) {
              mem.updated(env(f), ProcVal(p.args, p.expr, e)) match{
              case Some(v)=>eval(e, v, p.expr)
              case None=>throw new UndefinedSemantics(s"message ${expr}")
            }
          }
          else throw new UndefinedSemantics(s"message ${expr}")
        }
        }
        li(p.args, arg, p.env)
        }
        case _=>throw new UndefinedSemantics(s"message ${expr}")
      }
      case _=>throw new UndefinedSemantics(s"message ${expr}")
    }
  }
  case EmptyRecordExpr => Result(EmptyRecordVal, mem)
  case RecordExpr(field, initVal, next) => {
    def li(field:Var, initVal:Expr, next:RecordLike, e:Env, m:Mem):(RecordValLike, Mem)={
      eval(env, m, initVal) match{
        case Result(v:Val, m:Mem)=>{
          val new_mem=m.extended(v)._1
          val new_env=e+(field->LocVal(m.top))
          next match{
            case RecordExpr(f, initVal, next)=>{
              val n=(li(f, initVal, next, new_env, new_mem))
              (RecordVal(field, LocVal(m.top),n._1), n._2)
            }
            case EmptyRecordExpr => (RecordVal(field, LocVal(m.top),EmptyRecordVal), new_mem)
          }
        }
      }
    }
    val r=li(field, initVal, next, env, mem)
    Result(r._1, r._2)
  }
  case FieldAccess(record, field) => {
    def li(r:RecordVal, v:Var):Val={
          if(v==r.field) mem.m((r.loc))
          else {
            r.next match{
              case EmptyRecordVal => throw new UndefinedSemantics(s"message ${expr}")
              case rec: RecordVal => li(rec, v)
          }
        }
      }
      eval(env, mem,record) match{
      case Result(r:RecordVal, m:Mem) =>Result(li(r, field), m)
    }
  }
  case FieldAssign(record, field, new_val) => {
    def li(r:RecordVal, v:Var, n:Expr, mem:Mem):Result={
          if(v==r.field) {
            eval(env, mem, n) match{
              case Result(num:Val, me:Mem)=>{
                Result(num, Mem(me.m.updated(r.loc, num), me.top))
              }
            }
          }
          else {
            r.next match{
              case EmptyRecordVal => throw new UndefinedSemantics(s"message ${expr}")
              case rec: RecordVal => li(rec, v, n, mem)
          }
        }
      }
      eval(env, mem, record) match{
      case Result(r:RecordVal, m:Mem) =>li(r, field, new_val, m)
      }
  }
}


  def gc(env: Env, mem: Mem): Mem = {
    Mem(mem.m, mem.top)
  }
  
  def apply(program: String): (Val, Mem) = {
    val parsed = MiniCParserDriver(program)
    println(parsed)
    val res = eval(new Env(), Mem(new HashMap[LocVal,Val],0), parsed)
    (res.v, res.m)
  }

}


object Hw4App extends App {

}
