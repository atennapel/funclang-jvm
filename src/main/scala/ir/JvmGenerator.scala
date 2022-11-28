package ir

import Syntax.*

import org.objectweb.asm.*
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.commons.*

object JvmGenerator:
  final case class Ctx(
      moduleName: Name,
      arities: Map[Name, Arity],
      methods: Map[Name, Method],
      returns: Map[Name, Type]
  ):
    lazy val classType = Type.getType(s"L$moduleName;")

  final case class MethodCtx(arity: Arity)

  def generate(moduleName: Name, ds: Defs): Array[Byte] =
    val arities = ds.map(d => d.name -> d.arity).toMap
    val methods = ds.flatMap(d => createMethod(d).map(m => d.name -> m)).toMap
    val returns = ds.map(d => d.name -> descriptor(d.retrn)).toMap
    implicit val ctx: Ctx = Ctx(moduleName, arities, methods, returns)
    implicit val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(V1_8, ACC_PUBLIC, moduleName, null, "java/lang/Object", null)
    ds.foreach(gen)
    genStaticBlock(ds)
    cw.visitEnd()
    cw.toByteArray()

  private def genStaticBlock(
      ds: Defs
  )(implicit ctx: Ctx, cw: ClassWriter): Unit =
    val m = new Method("<clinit>", Type.VOID_TYPE, Nil.toArray)
    implicit val mg: GeneratorAdapter =
      new GeneratorAdapter(ACC_STATIC, m, null, null, cw)
    implicit val mctx: MethodCtx = MethodCtx(-1)
    ds.foreach(d => {
      d match
        case Def(x, None, rt, b) if constantValue(b).isEmpty =>
          gen(b)
          mg.putStatic(ctx.classType, x, descriptor(rt))
        case _ =>
    })
    mg.endMethod()

  private def createMethod(d: Def): Option[Method] = d match
    case Def(x, Some(ps), rt, _) =>
      Some(
        new Method(
          x,
          descriptor(rt),
          ps.map((_, t) => descriptor(t)).toArray
        )
      )
    case _ => None

  private def constantValue(e: Expr): Option[Any] = e match
    case IntLit(v)  => Some(v)
    case BoolLit(v) => Some(v)
    case _          => None

  private def gen(d: Def)(implicit ctx: Ctx, cw: ClassWriter): Unit = d match
    case Def(x, None, rt, b) =>
      cw.visitField(
        ACC_PUBLIC + ACC_FINAL + ACC_STATIC,
        x,
        descriptor(rt).getDescriptor(),
        null,
        constantValue(b).orNull
      )
    case Def(x, Some(ps), _, b) =>
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
        mg.loadArg(l) // TODO: local variables
        aso match
          case None     => // done, local is pushed on stack
          case Some(as) => ??? // apply unknown
      case Global(x, aso) =>
        val arity = ctx.arities(x)
        (arity, aso) match
          case (-1, None)     => mg.getStatic(ctx.classType, x, ctx.returns(x))
          case (-1, Some(as)) => ??? // over-apply static member
          case (n, None)      => ??? // reference method
          case (n, Some(as)) if as.size < n => ??? // under-apply
          case (n, Some(as)) if as.size > n =>
            as.take(n).foreach(gen)
            mg.invokeStatic(ctx.classType, ctx.methods(x))
          // apply closure: to as.drop(as.size - n)
          case (_, Some(as)) =>
            as.foreach(gen)
            mg.invokeStatic(ctx.classType, ctx.methods(x))

  private def descriptor(t: IRType): Type = t match
    case TInt  => Type.INT_TYPE
    case TBool => Type.BOOLEAN_TYPE
