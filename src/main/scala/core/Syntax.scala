package core

object Syntax:
  type Name = String
  type Ix = Int

  enum Type:
    case TUnit
    case TBool
    case TInt
    case TFun(param: Type, retrn: Type)
  export Type.*

  enum Binop:
    case BAdd
    case BMul
    case BSub
    case BLt
  export Binop.*

  enum Expr:
    case Local(ix: Ix)
    case Global(name: Name)
    case App(fn: Expr, arg: Expr)
    case Lam(name: Name, ty: Type, body: Expr)
    case Let(name: Name, ty: Type, value: Expr, body: Expr)
    case IntLit(value: Int)
    case BoolLit(value: Boolean)
    case UnitLit
    case If(cond: Expr, ifTrue: Expr, ifFalse: Expr)
    case BinopExpr(op: Binop, left: Expr, right: Expr)
  export Expr.*
