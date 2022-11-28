package ir

object Syntax:
  type Name = String
  type Lvl = Int
  type Arity = Int // -1 means it's not a function

  enum Expr:
    case Local(lvl: Lvl, args: Option[List[Expr]] = None)
    case Global(name: Name, args: Option[List[Expr]] = None)
    case IntLit(value: Int)
    case BoolLit(value: Boolean)
  export Expr.*

  enum IRType:
    case TInt
    case TBool
  export IRType.*

  final case class Def(
      name: Name,
      params: Option[List[(Name, IRType)]],
      retrn: IRType,
      body: Expr
  ):
    def arity = params.map(_.size).getOrElse(-1)

  type Defs = List[Def]
