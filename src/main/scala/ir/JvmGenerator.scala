package ir

import Syntax.*

import org.objectweb.asm.*
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.commons.*

import java.util.function.Function
import java.lang.invoke.LambdaMetafactory
import java.lang.reflect.Modifier
import java.lang.invoke.MethodType
import java.lang.invoke.MethodHandles

object JvmGenerator:
  var gensym: Int = 0
  def uniq: Int =
    val t = gensym
    gensym += 1
    t

  final case class Ctx(
      moduleName: Name,
      arities: Map[Name, Arity],
      methods: Map[Name, Method],
      args: Map[Name, Option[List[Type]]],
      returns: Map[Name, Type]
  ):
    lazy val classType = Type.getType(s"L$moduleName;")

  final case class MethodCtx(arity: Arity)

  def generate(moduleName: Name, ds: Defs): Array[Byte] =
    val arities = ds.map(d => d.name -> d.arity).toMap
    val methods = ds.flatMap(d => createMethod(d).map(m => d.name -> m)).toMap
    val args =
      ds.map(d => d.name -> d.params.map(as => as.map((_, t) => descriptor(t))))
        .toMap
    val returns = ds.map(d => d.name -> descriptor(d.retrn)).toMap
    implicit val ctx: Ctx = Ctx(moduleName, arities, methods, args, returns)
    implicit val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(V1_8, ACC_PUBLIC, moduleName, null, "java/lang/Object", null)
    ds.foreach(gen)
    genStaticBlock(ds)
    cw.visitEnd()
    cw.toByteArray()

  private def genStaticBlock(
      ds0: Defs
  )(implicit ctx: Ctx, cw: ClassWriter): Unit =
    val ds = ds0.filter {
      case Def(x, None, rt, b) if constantValue(b).isEmpty => true
      case _                                               => false
    }
    if ds.nonEmpty then
      val m = new Method("<clinit>", Type.VOID_TYPE, Nil.toArray)
      implicit val mg: GeneratorAdapter =
        new GeneratorAdapter(ACC_STATIC, m, null, null, cw)
      implicit val mctx: MethodCtx = MethodCtx(-1)
      ds.foreach(d => {
        d match
          case Def(x, None, rt, b) =>
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

  private val OBJECT_TYPE = Type.getType(classOf[Object])
  private val FUNCTION_TYPE = Type.getType("Ljava/util/function/Function;")

  private def descriptor(t: IRType): Type = t match
    case TInt  => Type.INT_TYPE
    case TBool => Type.BOOLEAN_TYPE
    case TFun  => FUNCTION_TYPE

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
        new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC, m, null, null, cw)
      gen(b)
      mg.returnValue()
      mg.endMethod()

  private def gen(e: Expr)(implicit
      ctx: Ctx,
      mctx: MethodCtx,
      cw: ClassWriter,
      mg: GeneratorAdapter
  ): Unit =
    e match
      case IntLit(v)   => mg.push(v)
      case BoolLit(v)  => mg.push(v)
      case Local(l)    => mg.loadArg(l) // TODO: local variables
      case App(fn, as) => gen(fn); appClos(as)
      case Global(x, aso) =>
        val arity = ctx.arities(x)
        (arity, aso) match
          case (-1, None) => mg.getStatic(ctx.classType, x, ctx.returns(x))
          case (-1, Some(as)) => // over-apply static member
            gen(Global(x, None)); appClos(as)
          case (n, None) => curry(x)
          case (n, Some(as)) if as.size < n => // under-apply
            gen(Global(x, None)); appClos(as)
          case (n, Some(as)) if as.size > n => // over-apply
            gen(Global(x, Some(as.take(n)))); appClos(as.drop(n))
          case (_, Some(as)) => // exact known call
            as.foreach(gen)
            mg.invokeStatic(ctx.classType, ctx.methods(x))

  private def appClos(as: List[Expr])(implicit
      ctx: Ctx,
      mctx: MethodCtx,
      cw: ClassWriter,
      mg: GeneratorAdapter
  ): Unit =
    as.foreach(a => {
      gen(a)
      mg.invokeInterface(
        Type.getType(classOf[Function[Object, Object]]),
        Method.getMethod("Object apply (Object)")
      )
    })

  private def curry(x: Name)(implicit
      ctx: Ctx,
      mctx: MethodCtx,
      cw: ClassWriter,
      mg: GeneratorAdapter
  ): Unit =
    val params = ctx.args(x).get
    val arity = params.size
    def go(i: Int): Method = i match
      case 0 => throw new Exception("cannot curry 0 arity function")
      case 1 =>
        /*val m = new Method(
          s"$x$$lambda$$$uniq",
          ctx.returns(x),
          params.toArray
        )
        val mg2: GeneratorAdapter =
          new GeneratorAdapter(
            ACC_PUBLIC + ACC_STATIC + ACC_SYNTHETIC,
            m,
            null,
            null,
            cw
          )
        (0 until arity).foreach(i => mg2.loadArg(i))
        mg2.invokeStatic(ctx.classType, ctx.methods(x))
        mg2.returnValue()
        mg2.endMethod()
        m*/
        ctx.methods(x)
      case i =>
        val prevMethod = go(i - 1)
        val prefix = params.dropRight(i - 1)
        val m = new Method(
          s"$x$$lambda$$$uniq",
          FUNCTION_TYPE,
          prefix.toArray
        )
        val mg2: GeneratorAdapter =
          new GeneratorAdapter(
            ACC_PUBLIC + ACC_STATIC + ACC_SYNTHETIC,
            m,
            null,
            null,
            cw
          )
        (0 until prefix.size).foreach(i => mg2.loadArg(i))
        // dynamically instantiate lambda for previous generated method
        val funDesc =
          Type.getMethodDescriptor(descriptor(TFun), prefix.toArray*)
        val funTypeASM = Type.getMethodType(descriptor(TFun), params(arity - i))
        mg2.visitInvokeDynamicInsn(
          "apply",
          funDesc,
          metaFactoryHandle,
          funTypeASM,
          new Handle(
            H_INVOKESTATIC,
            ctx.moduleName,
            prevMethod.getName,
            prevMethod.getDescriptor,
            false
          ),
          funTypeASM
        )
        mg2.returnValue()
        mg2.endMethod()
        m
    val m = go(arity)
    val funDesc = MethodType
      .methodType(classOf[Function[?, ?]])
      .toMethodDescriptorString
    val funTypeASM = Type.getMethodType(ctx.returns(x), params.head)
    mg.visitInvokeDynamicInsn(
      "apply",
      funDesc,
      metaFactoryHandle,
      funTypeASM,
      new Handle(
        H_INVOKESTATIC,
        ctx.moduleName,
        m.getName,
        m.getDescriptor,
        false
      ),
      funTypeASM
    )

  private lazy val metaFactoryHandle: Handle =
    val m = classOf[LambdaMetafactory].getDeclaredMethods
      .find(m => m.getName == "metafactory")
      .get
    new Handle(
      H_INVOKESTATIC,
      Type.getInternalName(m.getDeclaringClass),
      m.getName,
      Type.getMethodDescriptor(m),
      false
    )
