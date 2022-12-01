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
  @main def run(filename0: String) =
    var filename = filename0
    if !filename.endsWith(".lang") then filename = s"$filename0.lang"
    val moduleName = filename.dropRight(5)
    val ds = parser.parseFromFile(new File(filename)).flatMap(_.toTry).get
    val ds2 = typecheck(ds)
    val ir = compile(ds2)
    val bs = generate(moduleName, ir)
    val bos = new BufferedOutputStream(
      new FileOutputStream(s"$moduleName.class")
    )
    bos.write(bs)
    bos.close()
