package surface

object Syntax:
  type Name = String

  enum Type:
    case TInt
    case TBool
    case TUnit
    case TVar(name: Name)
    case TCon(name: Name, args: List[Type])
    case TFun(param: Type, retrn: Type)
    case THole
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
    case Hole
    case Case(scrut: Expr, cases: List[(Name, List[Name], Expr)])
  export Expr.*

  enum Def:
    case DDef(name: Name, ty: Option[Type], value: Expr)
    case DDecl(name: Name, ty: Type)
    case DData(name: Name, params: List[Name], cons: List[(Name, List[Type])])
  export Def.*

  type Defs = List[Def]
