package ir

import Syntax.*

import org.objectweb.asm.*
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.commons.*

object JvmGenerator:
  final case class Ctx(
      moduleName: Name,
      arities: Map[Name, Arity],
      methods: Map[Name, Method]
  ):
    lazy val classType = Type.getType(s"L$moduleName;")

  final case class MethodCtx(arity: Arity)

  def generate(moduleName: Name, ds: Defs): Array[Byte] =
    val arities = ds.map(d => d.name -> d.arity).toMap
    val methods = ds.flatMap(d => createMethod(d).map(m => d.name -> m)).toMap
    implicit val ctx: Ctx = Ctx(moduleName, arities, methods)
    implicit val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(V1_8, ACC_PUBLIC, moduleName, null, "java/lang/Object", null)
    ds.foreach(gen)
    cw.visitEnd()
    cw.toByteArray()

  private def createMethod(d: Def): Option[Method] = d match
    case Def(x, Some(ps), _) =>
      Some(
        new Method(
          x,
          OBJECT_TYPE,
          ps.map(_ => OBJECT_TYPE).toArray
        ) // TODO: types
      )
    case _ => None

  private val OBJECT_TYPE = Type.getType(classOf[Object]) // TODO: type system

  private def gen(d: Def)(implicit ctx: Ctx, cw: ClassWriter): Unit = d match
    case Def(x, None, b) => ??? // TODO: static member
    case Def(x, Some(ps), b) =>
      val m = ctx.methods(x)
      implicit val mctx: MethodCtx = MethodCtx(ps.size)
      implicit val mg: GeneratorAdapter =
        new GeneratorAdapter(ACC_PRIVATE + ACC_STATIC, m, null, null, cw)
      gen(b)
      mg.returnValue()
      mg.endMethod()

  private def gen(
      e: Expr
  )(implicit ctx: Ctx, mctx: MethodCtx, mg: GeneratorAdapter): Unit =
    e match
      case IntLit(v)  => mg.push(v)
      case BoolLit(v) => mg.push(v)
      case Local(l, aso) =>
        mg.loadArg(l)
        aso match
          case None     => // done, local is pushed on stack
          case Some(as) => ??? // apply unknown
      case Global(x, aso) =>
        val arity = ctx.arities(x)
        (arity, aso) match
          case (-1, None)     => mg.getStatic(ctx.classType, x, OBJECT_TYPE)
          case (-1, Some(as)) => ??? // over-apply static member
          case (n, None)      => ??? // reference method
          case (n, Some(as)) if as.size < n => ??? // under-apply
          case (n, Some(as)) if as.size > n => ??? // over-apply
          case (_, Some(as)) =>
            as.foreach(gen)
            mg.invokeStatic(ctx.classType, ctx.methods(x))
