package l3

import l3.{ SymbolicCL3TreeModule => S }
import l3.{ HighCPSTreeModule => H }

import CL3Literal._ // ???

object CL3ToCPSTranslator extends (S.Tree => H.Tree) {
  def apply(tree: S.Tree): H.Tree =
    nonTail(tree)(_ => H.Halt(IntLit(L3Int(0))))

  private def tail(tree: S.Tree, c: Symbol): H.Tree =
    given Position = tree.pos
    
    tree match {
      // optimization
      case S.App(fun: S.Tree, args: Seq[S.Tree]) => {
        def appTransform(es: Seq[S.Tree])(as: Seq[H.Atom]): H.Tree = {
          es match {
            case Seq() =>
              H.AppF(as.head, c, as.tail)
            case e +: es => nonTail(e)
                                   ((a: H.Atom) => appTransform(es)(as :+ a))
          }
        }
        appTransform(fun +: args)(Seq.empty[H.Atom])
      }
      case S.If(_cond: S.Tree, thenE: S.Tree, elseE: S.Tree) => {
        val thenC = Symbol.fresh("thenC")
        val elseC = Symbol.fresh("elseC")

        H.LetC(Seq(H.Cnt(thenC, Seq.empty[Symbol], tail(thenE, c)),
                   H.Cnt(elseC, Seq.empty[Symbol], tail(elseE, c))),
               cond(_cond, thenC, elseC))
      }
      // as is
      case S.Let(bdgs: Seq[(S.Name, S.Tree)], body: S.Tree) => // stack overflow?
        bdgs.foldRight(tail(body, c)) // modified
                      ((b, t) => nonTail(b._2)
                                        ((a: H.Atom) => H.LetP(b._1, L3ValuePrimitive.Id, Seq(a), t)))
      case S.LetRec(functions: Seq[S.Fun], body: S.Tree) => {
        val funs = functions.map {
          case S.Fun(name: S.Name, args: Seq[S.Name], body: S.Tree) => {
            val ret = Symbol.fresh("ret")
            H.Fun(name, ret, args, tail(body, ret))
          }
        }
        H.LetF(funs, tail(body, c)) // modified
      }

      case S.Prim(prim: L3TestPrimitive, args: Seq[S.Tree]) =>
        tail(S.If(S.Prim(prim, args), // modified
                  S.Lit(CL3Literal.BooleanLit(true)),
                  S.Lit(CL3Literal.BooleanLit(false))),
             c)
      // not even modified
      //     (@p e₁ e₂ …) where p is not logical primitive
      //     (halt e)
      //     a
      case default =>
        nonTail(default)((a: H.Atom) => H.AppC(c, Seq(a)))
  }
  
  private def cond(tree: S.Tree, thenC: Symbol, elseC: Symbol): H.Tree = {
    given Position = tree.pos

    tree match {
      // base case
      /*
      case S.Lit(CL3Literal.BooleanLit(v)) =>
        H.AppC(if (v) thenC else elseC, Seq.empty[H.Atom])
      case S.Lit(_) =>
        H.AppC(thenC, Seq.empty[H.Atom])
      case S.Ident(name) =>
        cond(S.Prim(L3TestPrimitive.Eq,
                    Seq(name, S.Lit(CL3Literal.BooleanLit(false))),
                  elseC, thenC),
             thenC, elseC)
      case S.Let() =>
        ???
      case S.Let() =>
        ???
      */
      case S.Prim(prim: L3TestPrimitive, args: Seq[S.Tree]) =>{
        def primTransform(es: Seq[S.Tree])(as: Seq[H.Atom]): H.Tree = {
          es match {
            case Seq() =>
              H.If(prim, as, thenC, elseC)
            case e +: es => nonTail(e)
                                   ((a: H.Atom) => primTransform(es)(as :+ a))
          }
        }
        primTransform(args)(Seq.empty[H.Atom])
      }
      // optimization
      case S.If(_cond: S.Tree, S.Lit(CL3Literal.BooleanLit(vt)),
                               S.Lit(CL3Literal.BooleanLit(ve))) => {
        if (vt != ve) {
          if (vt) cond(_cond, thenC, elseC) else cond(_cond, elseC, thenC)
        }
        else { 
          _cond match {
            case S.Prim(L3ValuePrimitive.ByteRead |
                        L3ValuePrimitive.ByteWrite, _) =>
              nonTail(_cond)((a: H.Atom) => H.AppC(if (vt) thenC else elseC, Seq.empty[H.Atom]))
            case _ =>
              H.AppC(if (vt) thenC else elseC, Seq.empty[H.Atom])
          }
        }
      }
      // // (if (e₁ #f #f))
      // case S.If(_cond: S.Tree, S.Lit(CL3Literal.BooleanLit(false)),
      //                          S.Lit(CL3Literal.BooleanLit(false))) => 
      //   H.AppC(elseC, Seq.empty[H.Atom])
      // // (if (e₁ #f #t))
      // case S.If(_cond: S.Tree, S.Lit(CL3Literal.BooleanLit(false)),
      //                          S.Lit(CL3Literal.BooleanLit(true))) =>
      //   cond(_cond, elseC, thenC)
      // // (if (e₁ #t #f))
      // case S.If(_cond: S.Tree, S.Lit(CL3Literal.BooleanLit(true)),
      //                          S.Lit(CL3Literal.BooleanLit(false))) =>
      //   cond(_cond, thenC, elseC)
      // // (if (e₁ #t #t))
      // case S.If(_cond: S.Tree, S.Lit(CL3Literal.BooleanLit(true)),
      //                          S.Lit(CL3Literal.BooleanLit(true))) =>
      //   H.AppC(thenC, Seq.empty[H.Atom])
      // (if (e₁ e₂ #f))
      case S.If(_cond: S.Tree, thenE: S.Tree,
                               S.Lit(CL3Literal.BooleanLit(false))) => {
        val ac = Symbol.fresh("ac")
        H.LetC(Seq(H.Cnt(ac, Seq.empty[Symbol], cond(thenE, thenC, elseC))),
               cond(_cond, ac, elseC))
      }
      // (if (e₁ e₂ #t))
      case S.If(_cond: S.Tree, thenE: S.Tree,
                               S.Lit(CL3Literal.BooleanLit(true))) => {
        val ac = Symbol.fresh("ac")
        H.LetC(Seq(H.Cnt(ac, Seq.empty[Symbol], cond(thenE, thenC, elseC))),
               cond(_cond, ac, thenC))
      }
      // (if (e₁ #f e₃))
      case S.If(_cond: S.Tree, S.Lit(CL3Literal.BooleanLit(false)),
                               elseE: S.Tree) => {
        val ac = Symbol.fresh("ac")
        H.LetC(Seq(H.Cnt(ac, Seq.empty[Symbol], cond(elseE, thenC, elseC))),
               cond(_cond, elseC, ac))
      }
      // (if (e₁ #t e₃))
      case S.If(_cond: S.Tree, S.Lit(CL3Literal.BooleanLit(true)),
                               elseE: S.Tree) => {
        val ac = Symbol.fresh("ac")
        H.LetC(Seq(H.Cnt(ac, Seq.empty[Symbol], cond(elseE, thenC, elseC))),
               cond(_cond, thenC, ac))
      }
      // as is
      // if (e₁ e₂ e₃)
      case S.If(_cond: S.Tree, thenE: S.Tree, elseE: S.Tree) => {
        val ac = Symbol.fresh("ac")
        val bc = Symbol.fresh("bc")
        H.LetC(Seq(H.Cnt(ac, Seq.empty[Symbol], cond(thenE, thenC, elseC)),
                   H.Cnt(bc, Seq.empty[Symbol], cond(elseE, thenC, elseC))),
               cond(_cond, ac, bc))
      }
      case default => {
        nonTail(default)((a: H.Atom) => H.If(L3TestPrimitive.Eq, 
                                             Seq(a, CL3Literal.BooleanLit(false)),
                                             elseC, thenC))
      }
    }
  }

  private def nonTail(tree: S.Tree)(ctx: H.Atom => H.Tree): H.Tree = 
    given Position = tree.pos // ???
    
    tree match {
      case S.Let(bdgs: Seq[(S.Name, S.Tree)], body: S.Tree) =>
        bdgs.foldRight(nonTail(body)(ctx)) // ⟦e⟧ C, i.e., C[e]
                      ((b, t) => nonTail(b._2)
                                        ((a: H.Atom) => H.LetP(b._1, L3ValuePrimitive.Id, Seq(a), t)))
      case S.LetRec(functions: Seq[S.Fun], body: S.Tree) => {
        val funs = functions.map {
          case S.Fun(name: S.Name, args: Seq[S.Name], body: S.Tree) => {
            val ret = Symbol.fresh("ret")
            H.Fun(name, ret, args, tail(body, ret))
          }
        }
        H.LetF(funs, nonTail(body)(ctx))
      }
      case S.App(fun: S.Tree, args: Seq[S.Tree]) =>{
        //@annotation.tailrec
        def appTransform(es: Seq[S.Tree])(as: Seq[H.Atom]): H.Tree = {
          es match {
            case Seq() => {
              val ret = Symbol.fresh("ret")
              val v   = Symbol.fresh("v")
              H.LetC(Seq(H.Cnt(ret, Seq(v), ctx(v))),
                     H.AppF(as.head, ret, as.tail))
            }
            case e +: es => nonTail(e)
                                   ((a: H.Atom) => appTransform(es)(as :+ a))
          }
        }
        appTransform(fun +: args)(Seq.empty[H.Atom])
      }
      case S.If(_cond: S.Tree, thenE: S.Tree, elseE: S.Tree) => {
        val thenC = Symbol.fresh("thenC")
        val elseC = Symbol.fresh("elseC")
        val c = Symbol.fresh("c")
        val r = Symbol.fresh("r")

        H.LetC(Seq(H.Cnt(c, Seq(r), ctx(r)),
                   H.Cnt(thenC, Seq.empty[Symbol], tail(thenE, c)),
                   H.Cnt(elseC, Seq.empty[Symbol], tail(elseE, c))),
                   cond(_cond, thenC, elseC))
      }
      /*
      case S.If(_cond: S.Tree, thenE: S.Tree, elseE: S.Tree) => ???
        nonTail(S.If(S.Prim(L3TestPrimitive.Eq,
                              Seq(_cond, S.Lit(CL3Literal.BooleanLit(false)))),
                       elseE,
                       thenE))
               (ctx)
      */
      case S.Prim(prim: L3TestPrimitive, args: Seq[S.Tree]) =>
        nonTail(S.If(S.Prim(prim, args),
                     S.Lit(CL3Literal.BooleanLit(true)),
                     S.Lit(CL3Literal.BooleanLit(false))))
               (ctx)
      case S.Prim(prim: S.Primitive, args: Seq[S.Tree]) => {
        val n = Symbol.fresh("n")

        def primTransform(es: Seq[S.Tree])(as: Seq[H.Atom]): H.Tree = {
          es match {
            case Seq() =>
              H.LetP(n, prim.asInstanceOf[L3ValuePrimitive], as, ctx(n))
            case e +: es => nonTail(e)
                                   ((a: H.Atom) => primTransform(es)(as :+ a))
          }
        }
        primTransform(args)(Seq.empty[H.Atom])
      }
      case S.Halt(arg: S.Tree) =>
        nonTail(arg)((a: H.Atom) => H.Halt(a))
      case S.Ident(value) => 
        ctx(value)
      case S.Lit(value) => 
        ctx(value)
    }
}
