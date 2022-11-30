package surface

object Syntax:
  type Name = String

  enum Type:
    case TInt
    case TBool
    case TUnit
    case TFun(param: Type, retrn: Type)
  export Type.*

  enum Binop:
    case BAdd
    case BMul
    case BSub
    case BLt
  export Binop.*

  enum Expr:
    case Var(name: Name)
    case App(fn: Expr, arg: Expr)
    case Lam(name: Name, ty: Option[Type], body: Expr)
    case Let(name: Name, ty: Option[Type], value: Expr, body: Expr)
    case If(cond: Expr, ifTrue: Expr, ifFalse: Expr)
    case BinopExpr(op: Binop, left: Expr, right: Expr)
    case IntLit(value: Int)
    case BoolLit(value: Boolean)
    case UnitLit
  export Expr.*

  final case class Def(name: Name, ty: Option[Type], value: Expr)
  type Defs = List[Def]
