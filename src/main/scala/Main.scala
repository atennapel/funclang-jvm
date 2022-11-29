import ir.Syntax.*
import ir.JvmGenerator.generate

import java.io.BufferedOutputStream
import java.io.FileOutputStream

object Main:
  @main def run(out: String) =
    val ds = List(
      Def("id", Some(List(("x", TInt))), TInt, Local(0)),
      Def(
        "f",
        Some(List(("x", TInt))),
        TInt,
        Global("id", Some(List(Local(0))))
      ),
      Def("p42", None, TInt, IntLit(42)),
      Def("p42b", None, TBool, BoolLit(true)),
      Def("pid", None, TInt, Global("id", Some(List(IntLit(1))))),
      Def("test", Some(List()), TFun, Global("id")),
      Def(
        "app",
        Some(List(("f", TFun))),
        TInt,
        App(Local(0), List(IntLit(0)))
      ),
      Def(
        "app2",
        Some(List(("x", TInt))),
        TInt,
        Global("test", Some(List(Local(0))))
      )
    )
    val bs = generate("Test", ds)
    val bos = new BufferedOutputStream(new FileOutputStream(out))
    bos.write(bs)
    bos.close()
