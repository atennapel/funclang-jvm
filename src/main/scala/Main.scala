import surface.Syntax as S
import surface.Parser.parser
import surface.Typechecking.typecheck
import core.Compiler.compile
import ir.JvmGenerator.generate

import java.io.BufferedOutputStream
import java.io.FileOutputStream

import java.io.File
import scala.io.Source
import parsley.io.given

object Main:
  @main def run(filename: String, out: String) =
    val ds = parser.parseFromFile(new File(filename)).flatMap(_.toTry).get
    val ds2 = typecheck(ds)
    val ir = compile(ds2)
    val bs = generate("Test", ir)
    val bos = new BufferedOutputStream(new FileOutputStream(out))
    bos.write(bs)
    bos.close()
