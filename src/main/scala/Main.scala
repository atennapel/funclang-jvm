import ir.Syntax.*
import ir.JvmGenerator.generate

import java.io.BufferedOutputStream
import java.io.FileOutputStream

object Main:
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
      )
    )
    val bs = generate("Test", ds)
    val bos = new BufferedOutputStream(new FileOutputStream(out))
    bos.write(bs)
    bos.close()
