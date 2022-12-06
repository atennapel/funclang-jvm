package ir

import Syntax.*
import core.GenSym.uniq

import org.objectweb.asm.*
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.commons.*

import scala.collection.mutable

import java.util.function.Function
import java.lang.invoke.LambdaMetafactory
import java.lang.reflect.Modifier
import java.lang.invoke.MethodType
import java.lang.invoke.MethodHandles

import java.io.BufferedOutputStream
import java.io.FileOutputStream

object JvmGenerator:
  private val tcons: mutable.Map[Name, Type] = mutable.Map.empty

  final case class Ctx(
      moduleName: Name,
      arities: Map[Name, Arity],
      methods: Map[Name, Method],
      references: Map[Name, Set[Name]],
      tailRecursive: Set[Name],
      params: Map[Name, Option[NEL[Type]]],
      returns: Map[Name, Type]
  ):
    lazy val classType = Type.getType(s"L$moduleName;")

  final case class MethodCtx(
      name: Name,
      arity: Arity,
      tailRecursive: Boolean,
      lvl: Lvl,
      locals: Map[Lvl, Int]
  )

  private def onlyDefs(ds: Defs): List[DDef] =
    ds.flatMap {
      case d @ DDef(x, t, v, b) => Some(d)
      case _                    => None
    }

  // from https://stackoverflow.com/questions/8104479/how-to-find-the-longest-common-prefix-of-two-strings-in-scala
  private def longestCommonPrefix(a: String, b: String): String = {
    var same = true
    var i = 0
    while same && i < math.min(a.length, b.length) do
      if a.charAt(i) != b.charAt(i) then same = false
      else i += 1
    a.substring(0, i)
  }

  def generate(moduleName: Name, ds0: Defs): Unit =
    // println(s"generate $ds0")
    val ds = onlyDefs(ds0)
    val arities = ds.map(d => d.name -> d.arity).toMap
    val references = ds.map { case DDef(x, _, _, b) => x -> b.globals }.toMap
    val emptyCtx = Ctx(
      moduleName,
      Map.empty,
      Map.empty,
      Map.empty,
      Set.empty,
      Map.empty,
      Map.empty
    )
    val methodsTR =
      ds.flatMap(d =>
        createMethod(d, references)(emptyCtx).map((m, b) => d.name -> (m, b))
      ).toMap
    val methods = methodsTR.map { case (k, (m, _)) => k -> m }
    val tr = methodsTR.filter { case (k, (m, b)) => b }.keySet
    val params =
      ds.map(d =>
        d.name -> d.params.map(as => as.map(t => descriptor(t)(emptyCtx)))
      ).toMap
    val returns = ds.map(d => d.name -> descriptor(d.retrn)(emptyCtx)).toMap
    implicit val ctx: Ctx =
      Ctx(moduleName, arities, methods, references, tr, params, returns)
    implicit val cw: ClassWriter = new ClassWriter(
      ClassWriter.COMPUTE_MAXS + ClassWriter.COMPUTE_FRAMES
    ) {
      override protected def getCommonSuperClass(
          type1: String,
          type2: String
      ): String =
        // println(s"getCommonSuperClass $type1 $type2")
        val prefix = longestCommonPrefix(type1, type2)
        List.from(prefix).init.mkString("")
    }
    cw.visit(V1_8, ACC_PUBLIC, moduleName, null, "java/lang/Object", null)

    // empty constructor
    val con = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    con.visitVarInsn(ALOAD, 0)
    con.visitMethodInsn(
      INVOKESPECIAL,
      "java/lang/Object",
      "<init>",
      "()V",
      false
    )
    con.visitInsn(RETURN)
    con.visitMaxs(1, 1)
    con.visitEnd()
    // main method
    if ctx.methods.get(mainName).isDefined then
      val m = new Method(
        "main",
        Type.VOID_TYPE,
        List(Type.getType("[Ljava/lang/String;")).toArray
      )
      val main: GeneratorAdapter =
        new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC, m, null, null, cw)
      main.visitFieldInsn(
        GETSTATIC,
        "java/lang/System",
        "out",
        "Ljava/io/PrintStream;"
      )
      main.push(false)
      main.invokeStatic(ctx.classType, ctx.methods(mainName))
      main.invokeStatic(
        Type.getType(classOf[Integer]),
        Method.getMethod("Integer valueOf (int)")
      )
      main.invokeVirtual(
        Type.getType(classOf[Object]),
        Method.getMethod("String toString ()")
      )
      main.visitMethodInsn(
        INVOKEVIRTUAL,
        "java/io/PrintStream",
        "println",
        "(Ljava/lang/String;)V",
        false
      )
      main.visitInsn(RETURN)
      main.visitMaxs(3, 1)
      main.visitEnd
    // generate user definitions
    ds0.foreach(gen)
    genStaticBlock(ds)
    cw.visitEnd()
    val bos = new BufferedOutputStream(
      new FileOutputStream(s"$moduleName.class")
    )
    bos.write(cw.toByteArray())
    bos.close()

  private def genStaticBlock(
      ds0: Defs
  )(implicit ctx: Ctx, cw: ClassWriter): Unit =
    val ds = ds0.filter {
      case DDef(x, None, rt, b) if constantValue(b).isEmpty => true
      case _                                                => false
    }
    if ds.nonEmpty then
      val m = new Method("<clinit>", Type.VOID_TYPE, Nil.toArray)
      implicit val mg: GeneratorAdapter =
        new GeneratorAdapter(ACC_STATIC, m, null, null, cw)
      implicit val mctx: MethodCtx =
        MethodCtx("<clinit>", 0, false, 0, Map.empty)
      ds.foreach(d => {
        d match
          case DDef(x, None, rt, b) =>
            implicit val lMethodStart = new Label
            mg.visitLabel(lMethodStart)
            gen(b)
            mg.putStatic(ctx.classType, x, descriptor(rt))
          case _ =>
      })
      mg.visitInsn(RETURN)
      mg.endMethod()

  private def createMethod(
      d: Def,
      references: Map[Name, Set[Name]]
  )(implicit ctx: Ctx): Option[(Method, Boolean)] =
    d match
      case DDef(x, Some(ps), rt, b) =>
        Some(
          (
            new Method(
              x,
              descriptor(rt),
              ps.map(descriptor).toList.toArray
            ),
            isTailRecursive(x, ps.size, references, b)
          )
        )
      case _ => None

  private def isTailRecursive(
      x: Name,
      arity: Arity,
      rs: Map[Name, Set[Name]],
      e: Expr
  ): Boolean =
    e match
      case IntLit(_)  => true
      case BoolLit(_) => true
      case Local(_)   => true
      case UnitLit    => true
      case Global(y, as) if x == y =>
        as.map(_.size).getOrElse(0) == arity && !as.exists(a =>
          a.toList.exists(isRecursive(x, _))
        )
      case Global(y, as) =>
        !rs(y).contains(x) && !as.exists(a =>
          a.toList.exists(isRecursive(x, _))
        )
      case If(c, a, b) =>
        !isRecursive(x, c) && isTailRecursive(
          x,
          arity,
          rs,
          a
        ) && isTailRecursive(x, arity, rs, b)
      case BinopExpr(op, a, b) => !(isRecursive(x, a) || isRecursive(x, b))
      case App(f, a) =>
        !(isRecursive(x, f) || a.toList.exists(isRecursive(x, _)))
      case Let(ty, v, b) =>
        !isRecursive(x, v) && isTailRecursive(x, arity, rs, b)
      case Box(_, e)     => !isRecursive(x, e)
      case Unbox(_, e)   => !isRecursive(x, e)
      case Con(_, _, as) => !as.exists((e, _) => isRecursive(x, e))
      case Case(t, _, cs) =>
        !isRecursive(x, t) && cs.forall((_, _, b) =>
          isTailRecursive(x, arity, rs, b)
        )

  private def isRecursive(x: Name, e: Expr): Boolean = e match
    case IntLit(_)  => false
    case BoolLit(_) => false
    case Local(_)   => false
    case UnitLit    => false
    case Global(y, as) =>
      x == y || as.exists(a => a.toList.exists(isRecursive(x, _)))
    case If(c, a, b) =>
      isRecursive(x, c) || isRecursive(x, a) || isRecursive(x, b)
    case BinopExpr(op, a, b) => isRecursive(x, a) || isRecursive(x, b)
    case App(f, as) => isRecursive(x, f) || as.toList.exists(isRecursive(x, _))
    case Let(ty, v, b) => isRecursive(x, v) || isRecursive(x, b)
    case Box(_, e)     => isRecursive(x, e)
    case Unbox(_, e)   => isRecursive(x, e)
    case Con(_, _, as) => as.exists((e, _) => isRecursive(x, e))
    case Case(t, _, cs) =>
      isRecursive(x, t) || cs.exists((_, _, b) => isRecursive(x, b))

  private def constantValue(e: Expr): Option[Any] = e match
    case IntLit(v)  => Some(v)
    case BoolLit(v) => Some(v)
    case _          => None

  private val OBJECT_TYPE = Type.getType(classOf[Object])
  private val FUNCTION_TYPE = Type.getType("Ljava/util/function/Function;")

  private def descriptor(t: IRType)(implicit ctx: Ctx): Type = t match
    case TUnit   => Type.BOOLEAN_TYPE
    case TInt    => Type.INT_TYPE
    case TBool   => Type.BOOLEAN_TYPE
    case TFun    => FUNCTION_TYPE
    case TPoly   => OBJECT_TYPE
    case TCon(x) => Type.getType(s"L${ctx.moduleName}$$$x;")

  private def gen(d: Def)(implicit ctx: Ctx, cw: ClassWriter): Unit =
    // println(s"gen $d")
    d match
      case DDef(x, None, rt, b) =>
        cw.visitField(
          ACC_PUBLIC + ACC_FINAL + ACC_STATIC,
          x,
          descriptor(rt).getDescriptor(),
          null,
          constantValue(b).orNull
        )
      case DDef(x, Some(ps), _, b) =>
        val m = ctx.methods(x)
        val tr = ctx.tailRecursive.contains(x)
        implicit val mctx: MethodCtx =
          MethodCtx(x, ps.size, tr, ps.size, Map.empty)
        implicit val mg: GeneratorAdapter =
          new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC, m, null, null, cw)
        implicit val lMethodStart = new Label
        mg.visitLabel(lMethodStart)
        gen(b)
        mg.returnValue()
        mg.endMethod()
      case DData(x, cs) =>
        val className = s"${ctx.moduleName}$$$x"
        val datacw = new ClassWriter(
          ClassWriter.COMPUTE_MAXS + ClassWriter.COMPUTE_FRAMES
        )
        datacw.visit(
          V1_8,
          ACC_PUBLIC + ACC_ABSTRACT,
          className,
          null,
          "java/lang/Object",
          null
        )
        // private empty constructor
        val con = datacw.visitMethod(ACC_PROTECTED, "<init>", "()V", null, null)
        con.visitVarInsn(ALOAD, 0)
        con.visitMethodInsn(
          INVOKESPECIAL,
          "java/lang/Object",
          "<init>",
          "()V",
          false
        )
        con.visitInsn(RETURN)
        con.visitMaxs(1, 1)
        con.visitEnd()
        // datatype constructors
        cs.foreach((x, as) => {
          gen(className, x, as)
          datacw.visitInnerClass(
            s"$className$$$x",
            className,
            x,
            ACC_PUBLIC + ACC_STATIC + ACC_FINAL
          )
        })
        // done
        datacw.visitEnd()
        cw.visitInnerClass(
          className,
          ctx.moduleName,
          x,
          ACC_PUBLIC + ACC_ABSTRACT + ACC_STATIC
        )
        val bos = new BufferedOutputStream(
          new FileOutputStream(s"$className.class")
        )
        bos.write(datacw.toByteArray())
        bos.close()

  private def gen(superName: String, x: Name, as: List[IRType])(implicit
      ctx: Ctx
  ): Unit =
    val className = s"$superName$$$x"
    val cw = new ClassWriter(
      ClassWriter.COMPUTE_MAXS + ClassWriter.COMPUTE_FRAMES
    )
    cw.visit(
      V1_8,
      ACC_PUBLIC + ACC_STATIC + ACC_FINAL,
      className,
      null,
      superName,
      null
    )
    tcons += x -> Type.getType(s"L$className;")
    // fields
    as.zipWithIndex.foreach((ty, i) => {
      cw.visitField(
        ACC_PUBLIC + ACC_FINAL,
        s"a$i",
        descriptor(ty).getDescriptor,
        null,
        null
      )
    })
    // class constructor
    val m = new Method("<init>", Type.VOID_TYPE, as.map(descriptor).toArray)
    val mg: GeneratorAdapter =
      new GeneratorAdapter(ACC_PUBLIC, m, null, null, cw)
    mg.visitVarInsn(ALOAD, 0)
    mg.visitMethodInsn(
      INVOKESPECIAL,
      superName,
      "<init>",
      "()V",
      false
    )
    as.zipWithIndex.foreach((ty, i) => {
      mg.loadThis()
      mg.loadArg(i)
      mg.putField(Type.getType(s"L$className;"), s"a$i", descriptor(ty))
    })
    mg.visitInsn(RETURN)
    mg.visitMaxs(1, 1)
    mg.visitEnd()
    // done
    cw.visitEnd()
    val bos = new BufferedOutputStream(
      new FileOutputStream(s"$className.class")
    )
    bos.write(cw.toByteArray())
    bos.close()

  private def gen(e: Expr)(implicit
      ctx: Ctx,
      mctx: MethodCtx,
      cw: ClassWriter,
      mg: GeneratorAdapter,
      lMethodStart: Label
  ): Unit =
    e match
      case IntLit(v)                  => mg.push(v)
      case BoolLit(v)                 => mg.push(v)
      case UnitLit                    => mg.push(false)
      case Local(l) if l < mctx.arity => mg.loadArg(l)
      case Local(l)                   => mg.loadLocal(mctx.locals(l))
      case App(fn, as)                => gen(fn); appClos(as)
      case Let(ty, v, b) =>
        val local = mg.newLocal(descriptor(ty))
        gen(v)
        mg.storeLocal(local)
        val mctx2 = mctx.copy(
          lvl = mctx.lvl + 1,
          locals = mctx.locals + (mctx.lvl -> local)
        )
        gen(b)(ctx, mctx2, cw, mg, lMethodStart)
      case Global(x, aso) =>
        val arity = ctx.arities(x)
        (arity, aso) match
          case (0, None) => mg.getStatic(ctx.classType, x, ctx.returns(x))
          case (0, Some(as)) => // over-apply static member
            gen(Global(x, None)); appClos(as)
          case (n, None) => curry(x)
          case (n, Some(as)) if as.size < n => // under-apply
            gen(Global(x, None)); appClos(as)
          case (n, Some(as)) if as.size > n => // over-apply
            gen(Global(x, Some(NEL.of(as.take(n)))))
            appClos(NEL.of(as.drop(n)))
          case (_, Some(as))
              if mctx.tailRecursive && x == mctx.name => // exact known call tail-recursive
            as.foreach(gen)
            Range.inclusive(as.size - 1, 0, -1).foreach(i => mg.storeArg(i))
            mg.visitJumpInsn(GOTO, lMethodStart)
          case (_, Some(as)) => // exact known call
            as.foreach(gen)
            mg.invokeStatic(ctx.classType, ctx.methods(x))
      case If(BinopExpr(BLt, x, y), a, b) =>
        val lFalse = new Label
        val lEnd = new Label
        gen(x)
        gen(y)
        mg.ifICmp(IFGE, lFalse)
        gen(a)
        mg.visitJumpInsn(GOTO, lEnd)
        mg.visitLabel(lFalse)
        gen(b)
        mg.visitLabel(lEnd)
      case If(c, a, b) =>
        val lFalse = new Label
        val lEnd = new Label
        gen(c)
        mg.visitJumpInsn(IFEQ, lFalse)
        gen(a)
        mg.visitJumpInsn(GOTO, lEnd)
        mg.visitLabel(lFalse)
        gen(b)
        mg.visitLabel(lEnd)
      case BinopExpr(op, a, b) =>
        gen(a)
        gen(b)
        op match
          case BAdd => mg.visitInsn(IADD)
          case BMul => mg.visitInsn(IMUL)
          case BSub => mg.visitInsn(ISUB)
          case BLt =>
            val lFalse = new Label
            val lEnd = new Label
            mg.ifICmp(IFGE, lFalse)
            mg.push(true)
            mg.visitJumpInsn(GOTO, lEnd)
            mg.visitLabel(lFalse)
            mg.push(false)
            mg.visitLabel(lEnd)
      case Box(t, a)   => gen(a); box(descriptor(t))
      case Unbox(t, a) => gen(a); mg.unbox(descriptor(t))
      case Con(x, TCon(y), as) =>
        val conType = Type.getType(s"L${ctx.moduleName}$$$y$$$x;")
        mg.newInstance(conType)
        mg.dup()
        as.foreach((e, _) => gen(e))
        mg.invokeConstructor(
          conType,
          new Method(
            "<init>",
            Type.VOID_TYPE,
            as.map((_, t) => descriptor(t)).toArray
          )
        )
      case Case(scrut, rt, cs) =>
        gen(scrut)
        val lEnd = new Label
        var lNextCase = new Label
        // TODO: handle empty ADTs
        cs.init.foreach { (c, ts, b) =>
          val contype = tcons(c)
          mg.visitLabel(lNextCase)
          lNextCase = new Label
          mg.dup()
          mg.instanceOf(contype)
          mg.visitJumpInsn(IFEQ, lNextCase)
          if ts.nonEmpty then mg.checkCast(contype)
          var mctx2: MethodCtx = mctx
          ts.zipWithIndex.foreach((ty, i) => {
            val desc = descriptor(ty)
            val local = mg.newLocal(desc)
            mg.dup()
            mg.getField(contype, s"a$i", desc)
            mg.storeLocal(local)
            mctx2 = mctx2.copy(
              lvl = mctx2.lvl + 1,
              locals = mctx2.locals + (mctx2.lvl -> local)
            )
          })
          mg.pop()
          gen(b)(ctx, mctx2, cw, mg, lMethodStart)
          mg.visitJumpInsn(GOTO, lEnd)
        }
        mg.visitLabel(lNextCase)
        val (c, ts, b) = cs.last
        val contype = tcons(c)
        if ts.nonEmpty then mg.checkCast(contype)
        var mctx2: MethodCtx = mctx
        ts.zipWithIndex.foreach((ty, i) => {
          val desc = descriptor(ty)
          val local = mg.newLocal(desc)
          mg.dup()
          mg.getField(contype, s"a$i", desc)
          mg.storeLocal(local)
          mctx2 = mctx2.copy(
            lvl = mctx2.lvl + 1,
            locals = mctx2.locals + (mctx2.lvl -> local)
          )
        })
        mg.pop()
        gen(b)(ctx, mctx2, cw, mg, lMethodStart)
        mg.visitLabel(lEnd)
      case _ => throw new Exception("impossible")

  // ASM generates a constructor application, so we override it here
  // to generate simpler bytecode
  private def box(t: Type)(implicit mg: GeneratorAdapter) =
    if t == Type.INT_TYPE then
      mg.invokeStatic(
        Type.getType(classOf[Integer]),
        Method.getMethod("Integer valueOf (int)")
      )
    else if t == Type.BOOLEAN_TYPE then
      mg.invokeStatic(
        Type.getType(classOf[Boolean]),
        Method.getMethod("Boolean valueOf (boolean)")
      )
    else mg.box(t)

  private def appClos(as: NEL[Expr])(implicit
      ctx: Ctx,
      mctx: MethodCtx,
      cw: ClassWriter,
      mg: GeneratorAdapter,
      lMethodStart: Label
  ): Unit =
    as.foreach(a => {
      gen(a)
      mg.invokeInterface(
        Type.getType(classOf[Function[?, ?]]),
        Method.getMethod("Object apply (Object)")
      )
    })

  private def boxType(t: Type): Type =
    if t == Type.INT_TYPE then Type.getType(classOf[Integer])
    else if t == Type.BOOLEAN_TYPE then Type.getType(classOf[Boolean])
    else t

  private def curry(x: Name)(implicit
      ctx: Ctx,
      mctx: MethodCtx,
      cw: ClassWriter,
      mg: GeneratorAdapter
  ): Unit =
    val params = ctx.params(x).get
    val arity = params.size
    def go(i: Int): Method = i match
      case 0 => throw new Exception("cannot curry 0 arity function")
      case 1 =>
        val m = new Method(
          s"$x$$lambda$$$uniq",
          boxType(ctx.returns(x)),
          params.map(boxType).toList.toArray
        )
        val mg2: GeneratorAdapter =
          new GeneratorAdapter(
            ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC,
            m,
            null,
            null,
            cw
          )
        (0 until arity).foreach(i => {
          mg2.loadArg(i); mg2.unbox(params.toList(i))
        })
        mg2.invokeStatic(ctx.classType, ctx.methods(x))
        box(ctx.returns(x))(mg2)
        mg2.returnValue()
        mg2.endMethod()
        m
      // ctx.methods(x)
      case i =>
        val prevMethod = go(i - 1)
        val prefix = params.dropRight(i - 1)
        val m = new Method(
          s"$x$$lambda$$$uniq",
          FUNCTION_TYPE,
          prefix.map(boxType).toArray
        )
        val mg2: GeneratorAdapter =
          new GeneratorAdapter(
            ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC,
            m,
            null,
            null,
            cw
          )
        (0 until prefix.size).foreach(i => mg2.loadArg(i))
        // dynamically instantiate lambda for previous generated method
        val funDesc =
          Type.getMethodDescriptor(
            descriptor(TFun),
            prefix.map(boxType).toArray*
          )
        val funTypeASM = Type.getMethodType(OBJECT_TYPE, OBJECT_TYPE)
        val funTypeASM2 = Type.getMethodType(
          boxType(ctx.returns(x)),
          boxType(params(arity - i + 1))
        )
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
          funTypeASM2
        )
        mg2.returnValue()
        mg2.endMethod()
        m
    val m = go(arity)
    val funDesc = MethodType
      .methodType(classOf[Function[?, ?]])
      .toMethodDescriptorString
    val funTypeASM = Type.getMethodType(OBJECT_TYPE, OBJECT_TYPE)
    val funTypeASM2 = Type.getMethodType(
      boxType(ctx.returns(x)),
      boxType(params.head)
    )
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
      funTypeASM2
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
