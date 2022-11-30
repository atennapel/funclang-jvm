package surface

import Syntax.*

import parsley.Parsley, Parsley._
import scala.language.implicitConversions

object Parser:
  object LangLexer:
    import parsley.token.{LanguageDef, Lexer, Predicate, Parser}
    import parsley.character.{alphaNum, isWhitespace, char, oneOf}
    import parsley.combinator.{eof, many}

    private val userOps = "`~!@$%^&*-+=|:/?><,."
    private val userOpsTail = s"${userOps}#_;"
    private val uriCharsHead = "-_~:/?#@!$&'*+,%="
    private val uriChars = "-_~:/?#@!$&'*+,%="

    val lang = LanguageDef.plain.copy(
      commentLine = "--",
      commentStart = "{-",
      commentEnd = "-}",
      nestedComments = true,
      keywords = Set(
        "let",
        "in",
        "if",
        "then",
        "else",
        "True",
        "False",
        "Int",
        "Bool"
      ),
      operators = Set("=", ":", ";", "\\", ".", "->"),
      identStart = Predicate(_.isLetter),
      identLetter =
        Predicate(c => c.isLetterOrDigit || c == '_' || c == '\'' || c == '-'),
      opStart = Predicate(userOps.contains(_)),
      opLetter = Predicate(userOpsTail.contains(_)),
      space = Predicate(isWhitespace)
    )
    val lexer = new Lexer(lang)

    def fully[A](p: => Parsley[A]): Parsley[A] = lexer.whiteSpace *> p <* eof

    val ident: Parsley[String] = lexer.identifier
    val userOp: Parsley[String] = lexer.userOp
    val natural: Parsley[Int] = lexer.natural
    val uri: Parsley[String] =
      lexer.lexeme(
        char('#') *> (((alphaNum <|> oneOf(uriCharsHead*)) <~> many(
          alphaNum <|> oneOf(uriChars*)
        )).map { case (hd, tl) =>
          s"$hd${tl.mkString}"
        } <|> lexer.stringLiteral)
      )
    def keyword(s: String): Parsley[Unit] = lexer.keyword(s)
    def symbol(s: String): Parsley[Unit] = void(lexer.symbol_(s))

    object Implicits:
      given Conversion[String, Parsley[Unit]] with
        def apply(s: String): Parsley[Unit] =
          if lang.keywords(s) then lexer.keyword(s)
          else if lang.operators(s) then lexer.maxOp(s)
          else void(lexer.symbol_(s))

  object TmParser:
    import parsley.expr.{precedence, Ops, InfixL, InfixR, Prefix, Postfix}
    import parsley.combinator.{many, some, option, sepEndBy}
    import parsley.Parsley.pos

    import LangLexer.{ident as ident0, userOp as userOp0, natural, uri}
    import LangLexer.Implicits.given

    private lazy val ident: Parsley[Name] = ident0
    private lazy val userOp: Parsley[Name] = userOp0
    private lazy val identOrOp: Parsley[Name] = ("(" *> userOp <* ")") <|> ident

    private lazy val tyAtom: Parsley[Type] =
      attempt(
        "(" <~> ")"
      ) #> TUnit <|> ("(" *> ty <* ")") <|> "Int" #> TInt <|> "Bool" #> TBool

    lazy val ty: Parsley[Type] =
      precedence[Type](tyAtom)(Ops(InfixR)("->" #> ((l, r) => TFun(l, r))))

    private lazy val atom: Parsley[Expr] =
      attempt("(" <~> ")") #> UnitLit <|>
        ("(" *> userOp.map(Var.apply) <* ")") <|>
        natural.map(IntLit.apply) <|>
        "True" #> BoolLit(true) <|>
        "False" #> BoolLit(true) <|>
        ident.map(Var.apply)

    lazy val tm: Parsley[Expr] = ifTm <|> let <|> lam <|> app

    private lazy val app: Parsley[Expr] =
      precedence[Expr](appAtom)(
        ops(
          "`@#?,.",
          "*/%",
          "+-",
          ":",
          "=!",
          "<>",
          "&",
          "^",
          "|",
          "$",
          "~"
        )*
      )

    private lazy val appAtom: Parsley[Expr] =
      (atom <~> many(arg) <~> option(let <|> lam))
        .map { case ((fn, args), opt) =>
          (args.flatten ++ opt)
            .foldLeft(fn) { case (fn, arg) => App(fn, arg) }
        }

    private type Arg = Expr

    private lazy val arg: Parsley[List[Arg]] = atom.map(t => List(t))

    private lazy val ifTm: Parsley[Expr] =
      ("if" *> tm <~> "then" *> tm <~> "else" *> tm)
        .map { case ((c, t), f) => If(c, t, f) }

    private lazy val let: Parsley[Expr] =
      ("let" *> identOrOp <~> many(defParam) <~>
        option(":" *> ty) <~> "=" *> tm <~> "in" *> tm).map {
        case ((((x, ps), ty), v), b) =>
          Let(
            x,
            ty.map(typeFromParams(ps, _)),
            lamFromDefParams(ps, v, ty.isEmpty),
            b
          )
      }

    private type DefParam = (List[Name], Option[Type])
    private lazy val defParam: Parsley[DefParam] =
      attempt(nestedParam) <|> identOrOp.map(x => (List(x), None))
    private lazy val nestedParam: Parsley[DefParam] =
      attempt("(" *> some(identOrOp) <~> ":" *> ty <* ")").map((xs, ty) =>
        (xs, Some(ty))
      ) <|> ("(" <~> ")").map(_ => (List("_"), Some(TUnit)))

    private lazy val lam: Parsley[Expr] =
      ("\\" *> many(lamParam) <~> "." *> tm).map(lamFromLamParams(_, _))

    private type LamParam = (List[Name], Option[Type])
    private lazy val lamParam: Parsley[LamParam] =
      attempt(nestedParam).map { case (xs, ty) => (xs, ty) } <|>
        identOrOp.map(x => (List(x), None))

    private def typeFromParams(ps: List[DefParam], rt: Type): Type =
      ps.foldRight(rt)((x, b) =>
        x match
          case (xs, ty) =>
            xs.foldRight(b)((_, t) => TFun(ty.get, t))
      )

    private def lamFromDefParams(
        ps: List[DefParam],
        b: Expr,
        useTypes: Boolean
    ): Expr =
      ps.foldRight(b)((x, b) =>
        x match
          case (xs, ty) =>
            xs.foldRight(b)(
              Lam(
                _,
                if useTypes then Some(ty.get) else None,
                _
              )
            )
      )

    private def lamFromLamParams(ps: List[LamParam], b: Expr): Expr =
      ps.foldRight(b)((x, b) =>
        x match
          case (xs, ty) => xs.foldRight(b)(Lam(_, ty, _))
      )

    private def userOpStart(s: String): Parsley[String] =
      userOp0.filter(_.startsWith(s))
    private def opL(o: String): Parsley[InfixL.Op[Expr]] =
      attempt(userOpStart(o).filterNot(_.endsWith(":"))).map(op =>
        (l, r) => BinopExpr(parseOp(o), l, r)
      )
    private def opR(o: String): Parsley[InfixR.Op[Expr]] =
      attempt(userOpStart(o)).map(op => (l, r) => BinopExpr(parseOp(o), l, r))

    private def opP(o: String): Parsley[Prefix.Op[Expr]] =
      attempt(userOpStart(o)).map(op =>
        t =>
          throw new Exception(
            s"unsupported unary operator $o"
          ) // UnopExpr(parseOp(o), t)
      )
    private def opLevel(s: String): List[Ops[Expr, Expr]] =
      val chars = s.toList
      List(
        Ops(Prefix)(chars.map(c => opP(c.toString))*),
        Ops(InfixL)(chars.map(c => opL(c.toString))*),
        Ops(InfixR)(chars.map(c => opR(c.toString))*)
      )
    private def ops(ss: String*): Seq[Ops[Expr, Expr]] =
      ss.map(opLevel).flatten

    private def parseOp(x: String): Binop = x match
      case "+" => BAdd
      case "*" => BMul
      case "-" => BSub
      case "<" => BLt
      case _   => throw new Exception(s"unsupported operator $x")

    lazy val defns: Parsley[Defs] = sepEndBy(defn, ";")
    lazy val defn: Parsley[Def] =
      (identOrOp <~> many(defParam) <~> option(":" *> ty) <~> "=" *> tm).map {
        case (((x, ds), ty), b) =>
          Def(
            x,
            ty.map(typeFromParams(ds, _)),
            lamFromDefParams(ds, b, ty.isEmpty)
          )
      }

  lazy val parser: Parsley[Defs] = LangLexer.fully(TmParser.defns)
