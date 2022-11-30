import ir.Syntax.*
import ir.JvmGenerator.generate

import core.Syntax as C
import core.ClosureConversion.*
import core.LambdaLifting.*
import core.Compiler.*

import java.io.BufferedOutputStream
import java.io.FileOutputStream

object Main:
  @main def run2(out: String) =
    val ds = List(
      C.Def(
        "fac",
        C.TFun(C.TInt, C.TFun(C.TInt, C.TInt)),
        C.Lam(
          "n",
          C.TInt,
          C.TFun(C.TInt, C.TInt),
          C.Lam(
            "acc",
            C.TInt,
            C.TInt,
            C.If(
              C.BinopExpr(C.BLt, C.Local(1), C.IntLit(2)),
              C.Local(0),
              C.App(
                C.App(
                  C.Global("fac"),
                  C.BinopExpr(C.BSub, C.Local(1), C.IntLit(1))
                ),
                C.BinopExpr(C.BMul, C.Local(1), C.Local(0))
              )
            )
          )
        )
      )
    )
    val ir = compile(ds)
    println(ir)
    val bs = generate("Test", ir)
    val bos = new BufferedOutputStream(new FileOutputStream(out))
    bos.write(bs)
    bos.close()

  @main def run(out: String) =
    val ds = List(
      Def("id", Some(NEL.of(TInt)), TInt, Local(0)),
      Def(
        "f",
        Some(NEL.of(TInt)),
        TInt,
        Global("id", Some(NEL.of(Local(0))))
      ),
      Def("p42", None, TInt, IntLit(42)),
      Def("p42b", None, TBool, BoolLit(true)),
      Def("pid", None, TInt, Global("id", Some(NEL.of(IntLit(1))))),
      Def("test", Some(NEL.of(TBool)), TFun, Global("id")),
      Def(
        "app",
        Some(NEL.of(TFun)),
        TInt,
        App(Local(0), NEL.of(IntLit(0)))
      ),
      Def(
        "app2",
        Some(NEL.of(TInt)),
        TInt,
        Global("test", Some(NEL.of(Local(0))))
      ),
      Def("fn2", Some(NEL.of(TInt, TInt)), TInt, Local(0)),
      Def("fn2c", Some(NEL.of(TBool)), TFun, Global("fn2")),
      Def(
        "fn2p",
        Some(NEL.of(TInt)),
        TFun,
        Global(
          "fn2",
          Some(NEL.of(Local(0)))
        )
      ),
      Def(
        "not",
        Some(NEL.of(TBool)),
        TBool,
        If(Local(0), BoolLit(false), BoolLit(true))
      ),
      Def(
        "lt",
        Some(NEL.of(TInt, TInt)),
        TBool,
        BinopExpr(BLt, Local(0), Local(1))
      ),
      Def(
        "fac",
        Some(NEL.of(TInt)),
        TInt,
        If(
          BinopExpr(BLt, Local(0), IntLit(2)),
          IntLit(1),
          BinopExpr(
            BMul,
            Local(0),
            Global("fac", Some(NEL.of(BinopExpr(BSub, Local(0), IntLit(1)))))
          )
        )
      ),
      Def(
        "fac2",
        Some(NEL.of(TInt, TInt)),
        TInt,
        If(
          BinopExpr(BLt, Local(0), IntLit(2)),
          Local(1),
          Global(
            "fac2",
            Some(
              NEL.of(
                BinopExpr(BSub, Local(0), IntLit(1)),
                BinopExpr(BMul, Local(0), Local(1))
              )
            )
          )
        )
      ),
      Def(
        "fac3",
        Some(NEL.of(TInt)),
        TInt,
        Let(TInt, Global("fac2", Some(NEL.of(Local(0), IntLit(1)))), Local(1))
      ),
      Def(
        "lets",
        Some(NEL.of(TBool)),
        TInt,
        If(
          Local(0),
          Let(TInt, IntLit(0), Local(1)),
          Let(TInt, IntLit(1), Local(1))
        )
      )
    )
    val bs = generate("Test", ds)
    val bos = new BufferedOutputStream(new FileOutputStream(out))
    bos.write(bs)
    bos.close()
