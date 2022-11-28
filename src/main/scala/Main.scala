import ir.Syntax.*
import ir.JvmGenerator.generate

import java.io.BufferedOutputStream
import java.io.FileOutputStream

object Main:
  @main def run(out: String) =
    val ds = List(
      Def("id", Some(List("x")), Local(0)),
      Def("f", Some(List("x")), Global("id", Some(List(Local(0)))))
    )
    val bs = generate("Test", ds)
    val bos = new BufferedOutputStream(new FileOutputStream(out))
    bos.write(bs)
    bos.close()
