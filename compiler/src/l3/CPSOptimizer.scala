package l3

import scala.collection.mutable.{ Map => MutableMap }
import scala.reflect.TypeTest
import scala.annotation.tailrec

abstract class CPSOptimizer[T <: SymbolicNames]
  (protected val treeModule: T)
  (using TypeTest[treeModule.Atom, treeModule.Literal],
         TypeTest[treeModule.Atom, treeModule.Name],
         TypeTest[treeModule.Tree, treeModule.Body]) {
  import treeModule._
  /** Entry point of abstract class */
  protected def rewrite(tree: Program): Tree = {
    val simplifiedTree = fixedPoint(tree)(shrink)
    val maxSize = size(simplifiedTree) * 3 / 2
    fixedPoint(simplifiedTree, 8)(`inline`(_, maxSize)) // use backticks to `inline` method instead of keyword
  }
  /** count
    * 
    * For example, shrinking inlining requires 1 `applied` and 0 `asValue`.
    * 
    * @param applied how many times a function is applied
    * @param asValue how many times a name is used as a value
    */
  private case class Count(applied: Int = 0, asValue: Int = 0)
  /** state
    *
    * @param census  how many times a name is used, for DCE and shrinking inline, is calculated at the start of the shrink phase
    * @param aSubst  record of `Atom` substitutions, for constant folding, common subexpression elimination, absorbing elements, and shrinking inline ?
    * @param cSubst  record of `Cnt`  substitutions, for inlining
    * @param eInvEnv for rewriting, e.g., CSE
    *      Pay attention to commutative operations.
    * @param cEnv for continuation inlining
    * @param fEnv for function inlining
    */
  private case class State(
    census: Map[Name, Count],
    aSubst: Subst[Atom] = emptySubst,
    cSubst: Subst[Name] = emptySubst,
    eInvEnv: Map[(ValuePrimitive, Seq[Atom]), Atom] = Map.empty,
    cEnv: Map[Name, Cnt] = Map.empty, // for inlining
    fEnv: Map[Name, Fun] = Map.empty) {
    /** DCE: verify a name is dead */
    def dead(s: Name): Boolean =
      ! census.contains(s)
    /** Shrinking inlining: verify a function/continuation is applied exactly once */
    def appliedOnce(s: Name): Boolean =
      census.get(s).contains(Count(applied = 1))
    /** Determine whether to inline a function/continuation application
      * 
      * We need to verify parameters and arguments match in that the input
      * L₃ programs can be incorrect (see 
      * https://edstem.org/eu/courses/1102/discussion/102705?answer=194371),
      * and they can be wrong during grading
      * (see https://edstem.org/eu/courses/1102/discussion/102705?comment=194427).
      * 
      * @param x function/continuation name
      * @param actualArgs arguments passed to the function/continuation
      *      application
      * @return if a function/continuation is to be inlined AND the number of
      *      parameters and arguments matches
      */
    def hasFun(fun: Name, actualArgs: Seq[Atom]): Boolean =  // to check if inlining is possible
      fEnv.get(fun).exists(_.args.length == actualArgs.length)
    def hasCnt(cnt: Name, actualArgs: Seq[Atom]): Boolean =
      cEnv.get(cnt).exists(_.args.length == actualArgs.length)

    def withASubst(from: Atom, to: Atom): State =  // augment atom substitution
      copy(aSubst = aSubst + (from -> aSubst(to)))
    def withASubst(from: Seq[Name], to: Seq[Atom]): State =
      copy(aSubst = aSubst ++ (from zip to.map(aSubst)))

    def withCSubst(from: Name, to: Name): State =  // augment continuation substitution
      copy(cSubst = cSubst + (from -> cSubst(to)))

    def withExp(atom: Atom, prim: ValuePrimitive, args: Seq[Atom]): State =
      copy(eInvEnv = eInvEnv + ((prim, args) -> atom))

    def withCnts(cnts: Seq[Cnt]): State =
      copy(cEnv = cEnv ++ (cnts.map(_.name) zip cnts))
    def withFuns(funs: Seq[Fun]): State =
      copy(fEnv = fEnv ++ (funs.map(_.name) zip funs))
  }

  // Needed for the construction of trees containing other trees,
  // which requires checking (dynamically here) it is indeed a subtype of Body.
  given Conversion[Tree, Body] = {
    case body: Body => body
    case other => sys.error(s"${other} is not a Body")
  }

  // Shrinking optimizations

  private def shrink(tree: Tree): Tree =
    shrink(tree, State(census(tree)))

  // called recursively
  private def shrink(tree: Tree, s: State): Tree = tree match { // a single pattern match, don't modularize
    case LetF(funs: Seq[Fun], body: Tree) => 
      var s_ = s
      val cen: Seq[Map[Name, Count]] = funs.map(f => census(f.body))
      val funs_ = funs.filter (f => 
        if (s.dead(f.name)) // DCE
          false
        else if (cen.exists(_.contains(f.name))) // no shrinking inlining
          true
        else if (s.appliedOnce(f.name)) // shrinking inlining
          s_ = s_.withFuns(Seq(Fun(f.name, f.retC, f.args, shrink(f.body, s))))
          false
        else
          true
      )
      val funs__ = funs_.map(f => // shrink the body of the function
        Fun(f.name, f.retC, f.args, shrink(f.body, s))
      )

      if (funs__.isEmpty)
        shrink(body, s_)
      else
        LetF(funs__, shrink(body, s_))
    
    case LetC(cnts: Seq[Cnt], body: Tree) =>
      var s_ = s
      val cen: Seq[Map[Name, Count]] = cnts.map(c => census(c.body))
      val cnts_ = cnts.filter(c => 
        if (s.dead(c.name))
          false
        else if (cen.exists(_.contains(c.name)))
          true
        else if (s.appliedOnce(c.name)) // shrinking inlining
          s_ = s_.withCnts(Seq(Cnt(c.name, c.args, shrink(c.body, s))))
          false
        else
          true
      )
      val cnts__ = cnts_.map(c => // shrink the body of the continuation
        Cnt(c.name, c.args, shrink(c.body, s))
      )

      if (cnts__.isEmpty)
        shrink(body, s_)
      else
        LetC(cnts__, shrink(body, s_))
    // side effect
    case LetP(name: Name, this.byteWrite, Seq(a: Atom), body: Tree) =>
      LetP(name, byteWrite, Seq(s.aSubst(a)), shrink(body, s)) 
    // DCE
    case LetP(name: Name, prim: ValuePrimitive, _, body: Tree) 
      if (!impure(prim) && s.dead(name)) =>
        shrink(body, s)
    // constant folding
    case LetP(name: Name, prim: ValuePrimitive, args: Seq[Atom], body: Tree) 
      if (vEvaluator.isDefinedAt((prim, args))) =>
        shrink(body, s.withASubst(name, vEvaluator((prim, args))))
    // correct?
    case LetP(name: Name, this.identity, Seq(a: Atom), body: Tree) =>
      shrink(body , s.withASubst(name, s.aSubst(a)))
    // left neutral element
    case LetP(name: Name, prim: ValuePrimitive, Seq(a: Literal, b: Atom), body: Tree) 
      if leftNeutral.contains((a, prim)) =>
        shrink(body, s.withASubst(name, s.aSubst(b)))  
    // right neutral element
    case LetP(name: Name, prim: ValuePrimitive, Seq(a: Atom, b: Literal), body: Tree)
      if rightNeutral.contains((prim, b)) =>
        shrink(body, s.withASubst(name, s.aSubst(a)))
    // left absorbing element
    case LetP(name: Name, prim: ValuePrimitive, Seq(a: Literal, b: Atom), body: Tree)
      if leftAbsorbing.contains((a, prim)) =>
        shrink(body, s.withASubst(name, a))
    // right absorbing element
    case LetP(name: Name, prim: ValuePrimitive, Seq(a: Atom, b: Literal), body: Tree)
      if rightAbsorbing.contains((prim, b)) =>
        shrink(body, s.withASubst(name, b))
    // same argument reduction
    case LetP(name: Name, prim: ValuePrimitive, Seq(a: Atom, b: Atom), body: Tree) 
      if s.aSubst(a) == s.aSubst(b) && sameArgReduce.isDefinedAt((prim, a)) => // `a` does not matter.
        shrink(body, s.withASubst(name, sameArgReduce((prim, s.aSubst(a)))))
    // CSE
    case LetP(name: Name, prim: ValuePrimitive, args: Seq[Atom], body: Tree) 
      if !(impure(prim) || unstable(prim)) && s.eInvEnv.contains((prim, args map s.aSubst)) =>
        shrink(body, s.withASubst(name, s.eInvEnv((prim, args map s.aSubst))))
    // commutative operations: +, *, &, |, and ^
    case LetP(name: Name, prim: ValuePrimitive, Seq(a: Atom, b: Atom), body: Tree)
      if commutative(prim) =>
        LetP(name, prim, Seq(s.aSubst(a), s.aSubst(b)), 
             shrink(body, s.withExp(name, prim, Seq(s.aSubst(a), s.aSubst(b))).withExp(name, prim, Seq(s.aSubst(b), s.aSubst(a)))))
    // non-commutative and unary operations without side effects
    case LetP(name: Name, prim: ValuePrimitive, args: Seq[Atom], body: Tree)
      if !(impure(prim) || unstable(prim)) =>
        LetP(name, prim, args map s.aSubst, shrink(body, s.withExp(name, prim, args map s.aSubst)))
    // impure or unstable operations
    case LetP(name: Name, prim: ValuePrimitive, args: Seq[Atom], body: Tree) =>
      LetP(name, prim, args map s.aSubst, shrink(body, s))

    case AppF(fun: Atom, retC: Name, actualArgs: Seq[Atom]) =>
      /* function as argument of `AppF` and applied in body of `AppF`:
       * substitute `fun` */
      if (s.hasFun(s.aSubst(fun).asInstanceOf[Name], actualArgs))
        s.fEnv(s.aSubst(fun).asInstanceOf[Name]) match
          case Fun(f, c, as, e) =>
            shrink(e, s.withASubst(as, actualArgs map s.aSubst).withCSubst(c, s.cSubst(retC)))
      else
        AppF(s.aSubst(fun), s.cSubst(retC), actualArgs map s.aSubst)
    
    case AppC(cnt: Name, actualArgs: Seq[Atom]) =>
      /* continuation as argument of `AppC` and applied in body of `AppC`:
       * substitute `cnt` */
      if (s.hasCnt(s.cSubst(cnt), actualArgs))
        s.cEnv(s.cSubst(cnt)) match
          case Cnt(c, as, e) =>
            shrink(e, s.withASubst(as, actualArgs map s.aSubst))
      else
        AppC(s.cSubst(cnt), actualArgs map s.aSubst)
    
    case If(cond: TestPrimitive, args: Seq[Atom], thenC: Name, elseC: Name) =>
      if (cEvaluator.isDefinedAt((cond, args))) // constant folding
        if (cEvaluator((cond, args)))
          AppC(s.cSubst(thenC), Seq.empty[Atom])
        else
          AppC(s.cSubst(elseC), Seq.empty[Atom])
      else if (sameArgReduceC.isDefinedAt(cond) && s.aSubst(args(0)) == s.aSubst(args(1))) // same argument reduction
        AppC(s.cSubst(if sameArgReduceC(cond) then thenC else elseC), Seq.empty[Atom])
      else
        If(cond, args map s.aSubst, s.cSubst(thenC), s.cSubst(elseC))
    
    case Halt(arg: Atom) =>
      Halt(s.aSubst(arg))
  }

  // (Non-shrinking) inlining
  // need to duplicate(rename) names bound in the inlined function

  private def inline(tree: Tree, maxSize: Int): Tree = {
    /** Copy a tree */
    def copyT(t: Tree, subV: Subst[Atom], subC: Subst[Name]): Tree = t match {
      case LetF(funs, body) =>
        val names = funs map (_.name)
        val names1 = names map (_.copy())
        val subV1 = subV ++ (names zip names1)
        LetF(funs map (copyF(_, subV1, subC)), copyT(body, subV1, subC))
      case LetC(cnts, body) =>
        val names = cnts map (_.name)
        val names1 = names map (_.copy())
        val subC1 = subC ++ (names zip names1)
        LetC(cnts map (copyC(_, subV, subC1)), copyT(body, subV, subC1))
      case LetP(name, prim, args, body) =>
        val name1 = name.copy()
        LetP(name1, prim, args map subV,
          copyT(body, subV + (name -> name1), subC))
      case AppF(fun, retC, args) =>
        AppF(subV(fun), subC(retC), args map subV)
      case AppC(cnt, args) =>
        AppC(subC(cnt), args map subV)
      case If(cond, args, thenC, elseC) =>
        If(cond, args map subV, subC(thenC), subC(elseC))
      case Halt(arg) =>
        Halt(subV(arg))
    }
    /** Copy a function */
    def copyF(fun: Fun, subV: Subst[Atom], subC: Subst[Name]): Fun = {
      val retC1 = fun.retC.copy()
      val subC1 = subC + (fun.retC -> retC1)
      val args1 = fun.args map (_.copy())
      val subV1 = subV ++ (fun.args zip args1)
      val funName1 = subV(fun.name).asInstanceOf[Name]
      Fun(funName1, retC1, args1, copyT(fun.body, subV1, subC1))
    }
    /** Copy a continuation */
    def copyC(cnt: Cnt, subV: Subst[Atom], subC: Subst[Name]): Cnt = {
      val args1 = cnt.args map (_.copy())
      val subV1 = subV ++ (cnt.args zip args1)
      Cnt(subC(cnt.name), args1, copyT(cnt.body, subV1, subC))
    }

    val fibonacci = Seq(1, 2, 3, 5, 8, 13)

    val trees = LazyList.iterate((0, tree), fibonacci.length) { (i, tree) =>
      val funLimit = fibonacci(i)
      val cntLimit = i

      def inlineT(tree: Tree)(using s: State): Tree = // create a stream of trees, the nth tree is the result of inlining functions of size <= fibonacci(n)
        tree match {
          case LetC(cnts: Seq[Cnt], body: Body) => {
            val cnts_ = cnts.map {
              case Cnt(c, as, e) =>
                Cnt(c, as, inlineT(e))
            }
            val _cnts = cnts_.filter((c: Cnt) => size(c.body) <= cntLimit)
            LetC(cnts_, inlineT(body)(using s.withCnts(_cnts)))
          }
          case AppC(cnt: Name, args: Seq[Atom]) => {
            if (s.hasCnt(cnt, args))
              s.cEnv(cnt) match
                case Cnt(c, as, e) =>
                  copyT(e, subst(as, args), emptySubst[Name])
            else   // Even though an inner `AppC` must correpsond to an outer
              tree // `LetC`, `cEnv` may still not contain `cnt` due to size
          }        // limit.
          case LetF(funs: Seq[Fun], body: Body) => {
            val funs_ = funs.map {
              case Fun(f, c, as, e) =>
                Fun(f, c, as, inlineT(e))
            }
            val _funs = funs_.filter((f: Fun) => size(f.body) <= funLimit)
            LetF(funs_, inlineT(body)(using s.withFuns(_funs)))
          }
          case AppF(fun: Atom, retC: Name, args: Seq[Atom]) => {
            if (s.hasFun(fun.asInstanceOf[Name], args))
              s.fEnv(fun.asInstanceOf[Name]) match
                case Fun(f, c, as, e) =>
                  copyT(e, subst(as, args), subst(c, retC))
            else   // Even though an inner `AppF` must correpsond to an outer
              tree // `LetF`, `fEnv` may still not contain `fun` due to size
          }        // limit.
          case LetP(name, prim, args, body) =>
            LetP(name, prim, args, inlineT(body))
          // `If` and `Halt`
          case t: Tree => t
        }

      (i + 1, fixedPoint(inlineT(tree)(using State(census(tree))))(shrink))
    }

    trees.takeWhile{ (_, tree) => size(tree) <= maxSize }.last._2  // take the last tree that fits in maxSize
  }

  // Census computation
  private def census(tree: Tree): Map[Name, Count] = {
    val census = MutableMap[Name, Count]().withDefault(_ => Count())
    val rhs = MutableMap[Name, Tree]()

    def incAppUse(atom: Atom): Unit = atom match {
      case n: Name =>
        val currCount = census(n)
        census(n) = currCount.copy(applied = currCount.applied + 1)
        rhs.remove(n).foreach(addToCensus)
      case _: Literal =>
    }

    def incValUse(atom: Atom): Unit = atom match {
      case n: Name =>
        val currCount = census(n)
        census(n) = currCount.copy(asValue = currCount.asValue + 1)
        rhs.remove(n).foreach(addToCensus)
      case _: Literal =>
    }

    @tailrec
    def addToCensus(tree: Tree): Unit = tree match {
      case LetF(funs, body) =>
        rhs ++= (funs map { f => (f.name, f.body) }); addToCensus(body)
      case LetC(cnts, body) =>
        rhs ++= (cnts map { c => (c.name, c.body) }); addToCensus(body)
      case LetP(_, _, args, body) =>
        args foreach incValUse; addToCensus(body)
      case AppF(fun, retC, args) =>
        incAppUse(fun); incValUse(retC); args foreach incValUse
      case AppC(cnt, args) =>
        incAppUse(cnt); args foreach incValUse
      case If(_, args, thenC, elseC) =>
        args foreach incValUse; incValUse(thenC); incValUse(elseC)
      case Halt(arg) =>
        incValUse(arg)
    }

    addToCensus(tree)
    census.toMap
  }

  private def size(tree: Tree): Int = tree match {
    case LetF(fs, body) => fs.map(_.body).map(size).sum + size(body)
    case LetC(cs, body) => cs.map(_.body).map(size).sum + size(body)
    case LetP(_, _, _, body) => size(body) + 1
    case _: (AppF | AppC | If | Halt) => 1
  }

  // up until this point, the code is common to both optimizers
  // 
  protected val impure: ValuePrimitive => Boolean
  protected val unstable: ValuePrimitive => Boolean

  protected val blockAlloc: ValuePrimitive
  protected val blockTag: ValuePrimitive
  protected val blockLength: ValuePrimitive

  protected val identity: ValuePrimitive

  protected val leftNeutral: Set[(Literal, ValuePrimitive)]
  protected val rightNeutral: Set[(ValuePrimitive, Literal)]
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)]
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)]

  protected val sameArgReduce: PartialFunction[(ValuePrimitive, Atom), Atom] 
  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean]   // changed to partial function

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Atom]),
                                            Literal]
  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Atom]),
                                            Boolean]
  protected val commutative: ValuePrimitive => Boolean

  protected val byteWrite: ValuePrimitive
}

object HighCPSOptimizer extends CPSOptimizer(HighCPSTreeModule)
    with (HighCPSTreeModule.Program => HighCPSTreeModule.Program) {
  import treeModule._
  import CL3Literal._, L3Primitive._

  def apply(program: Program): Program =
    rewrite(program)

  private[this] given Conversion[L3Int, Literal] = IntLit.apply
  private[this] given Conversion[Int, Literal] = L3Int.apply

  // TODO: check `FlatCPSOptimizer` for populating values below
  protected val impure: ValuePrimitive => Boolean =
    Set(ByteRead, ByteWrite, BlockSet)

  protected val unstable: ValuePrimitive => Boolean =
    Set(BlockAlloc, BlockGet, ByteRead)

  protected val blockAlloc: ValuePrimitive = BlockAlloc
  protected val blockTag: ValuePrimitive = BlockTag
  protected val blockLength: ValuePrimitive = BlockLength

  protected val identity: ValuePrimitive = Id

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((0, IntAdd), (1, IntMul), (~0, IntBitwiseAnd), (0, IntBitwiseOr), (0, IntBitwiseXOr))
  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((IntAdd, 0), (IntSub, 0), (IntMul, 1), (IntDiv, 1),
        (IntShiftLeft, 0), (IntShiftRight, 0),
        (IntBitwiseAnd, ~0), (IntBitwiseOr, 0), (IntBitwiseXOr, 0))

  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((0, IntMul), (0, IntDiv),
        (0, IntShiftLeft), (0, IntShiftRight),
        (0, IntBitwiseAnd), (~0, IntBitwiseOr))
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((IntMul, 0), (IntBitwiseAnd, 0), (IntBitwiseOr, ~0))

  protected val sameArgReduce: PartialFunction[(ValuePrimitive, Atom), Atom] = {
    case (IntBitwiseAnd | IntBitwiseOr, a) => a
    case (IntSub | IntMod | IntBitwiseXOr, _) => 0
    case (IntDiv, _) => 1
  }

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case IntLe | Eq => true
    case IntLt => false
  }

  def getV(literal: CL3Literal): L3Int = literal match {
    case CL3Literal.IntLit(value) => value
    case _ => throw new IllegalArgumentException("Unsupported literal type")
  }
  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Atom]),
                                            Literal] = {
    case (IntAdd, Seq(x: Literal, y: Literal)) => getV(x) + getV(y)
    case (IntSub, Seq(x: Literal, y: Literal)) => getV(x) - getV(y)
    case (IntMul, Seq(x: Literal, y: Literal)) => getV(x) * getV(y)
    case (IntDiv, Seq(x: Literal, y: Literal)) if getV(y).toInt != 0 => getV(x) / getV(y)
    case (IntMod, Seq(x: Literal, y: Literal)) if getV(y).toInt != 0 => getV(x) % getV(y)

    case (IntShiftLeft , Seq(x: Literal, y: Literal)) => getV(x) << getV(y)
    case (IntShiftRight, Seq(x: Literal, y: Literal)) => getV(x) >> getV(y)
    case (IntBitwiseAnd, Seq(x: Literal, y: Literal)) => getV(x) & getV(y)
    case (IntBitwiseOr , Seq(x: Literal, y: Literal)) => getV(x) | getV(y)
    case (IntBitwiseXOr, Seq(x: Literal, y: Literal)) => getV(x) ^ getV(y)
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Atom]),
                                            Boolean] = {
    case (IntLt, Seq(x: Literal, y: Literal)) => getV(x) <  getV(y)
    case (IntLe, Seq(x: Literal, y: Literal)) => getV(x) <= getV(y)
    case (Eq   , Seq(x: Literal, y: Literal)) => x == y
  }

  protected val commutative: ValuePrimitive => Boolean = 
    Set(IntAdd, IntMul, IntBitwiseAnd, IntBitwiseOr, IntBitwiseXOr)
  
  protected val byteWrite: ValuePrimitive = ByteWrite
}

object FlatCPSOptimizer extends CPSOptimizer(FlatCPSTreeModule)
    with (FlatCPSTreeModule.Program => FlatCPSTreeModule.Program) {
  import treeModule._
  import CPSValuePrimitive._
  import CPSTestPrimitive._

  def apply(program: Program): Program = rewrite(program) match {
    case tree: Program => tree
    case other => LetF(Seq(), other)
  }

  protected val impure: ValuePrimitive => Boolean =
    Set(BlockSet, ByteRead, ByteWrite)

  protected val unstable: ValuePrimitive => Boolean =
    Set(BlockAlloc, BlockGet, ByteRead) // why `BlockGet`?

  protected val blockAlloc: ValuePrimitive = BlockAlloc
  protected val blockTag: ValuePrimitive = BlockTag
  protected val blockLength: ValuePrimitive = BlockLength

  protected val identity: ValuePrimitive = Id

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((0, Add), (1, Mul), (~0, And), (0, Or), (0, XOr))
  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((Add, 0), (Sub, 0), (Mul, 1), (Div, 1),
        (ShiftLeft, 0), (ShiftRight, 0),
        (And, ~0), (Or, 0), (XOr, 0))

  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((0, Mul), (0, Div),
        (0, ShiftLeft), (0, ShiftRight),
        (0, And), (~0, Or))
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((Mul, 0), (And, 0), (Or, ~0))

  protected val sameArgReduce: PartialFunction[(ValuePrimitive, Atom), Atom] = {
    case (And | Or, a) => a
    case (Sub | Mod | XOr, _) => 0
    case (Div, _) => 1
  }

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case Le | Eq => true
    case Lt => false
  }

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Atom]),
                                            Literal] = {
    case (Add, Seq(x: Literal, y: Literal)) => x + y
    case (Sub, Seq(x: Literal, y: Literal)) => x - y
    case (Mul, Seq(x: Literal, y: Literal)) => x * y
    case (Div, Seq(x: Literal, y: Literal)) if y.toInt != 0 => x / y
    case (Mod, Seq(x: Literal, y: Literal)) if y.toInt != 0 => x % y

    case (ShiftLeft,  Seq(x: Literal, y: Literal)) => x << y
    case (ShiftRight, Seq(x: Literal, y: Literal)) => x >> y
    case (And, Seq(x: Literal, y: Literal)) => x & y
    case (Or,  Seq(x: Literal, y: Literal)) => x | y
    case (XOr, Seq(x: Literal, y: Literal)) => x ^ y
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Atom]),
                                            Boolean] = {
    case (Lt, Seq(x: Literal, y: Literal)) => x < y
    case (Le, Seq(x: Literal, y: Literal)) => x <= y
    case (Eq, Seq(x: Literal, y: Literal)) => x == y
  }

  protected val commutative: ValuePrimitive => Boolean =
    Set(Add, Mul, And, Or, XOr)
  
  protected val byteWrite: ValuePrimitive = ByteWrite
}
