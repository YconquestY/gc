package l3

import scala.collection.mutable.{ Map => MMap }

import l3.{ HighCPSTreeModule as H }
import l3.{ LowCPSTreeModule  as L }
import l3.{ L3Primitive as L3 }
import l3.{ CPSValuePrimitive as CPSV }
import l3.{ CPSTestPrimitive  as CPST }
import CL3Literal._
import BlockTag._
//import l3.CPSTreeModule.Tree

object CPSValueRepresenter extends (H.Tree => L.Tree) {
  def apply(tree: H.Tree): L.Tree =
    h2lVal(tree)(using Map.empty[Symbol, Set[Symbol]],
                       emptySubst[Symbol])
  /**
    * 
    *
    * @param tree
    * @param f2fvs immutable map from "known" function to free variables
    * @param f2w   map from "known" function to worker
    * @return
    */
  private def h2lVal(tree: H.Tree)
                    (using f2fvs: Map[Symbol, Set[Symbol]], // immutable!
                           f2w  : Subst[Symbol]): L.Tree = { 
    def substitute(tree: L.Tree)(using mappings: Subst[Symbol]): L.Tree = {
      def rewriteAtom(a: L.Atom): L.Atom = a match {
        case a: L.Literal => a
        case a: L.Name    => mappings(a)
      }
      def rewriteCnt(c: L.Cnt) = c match {
        case L.Cnt(name, args, body) =>
          L.Cnt(name, args, substitute(body))
      }

      tree match {
        case L.LetP(name, prim, args, body) =>
          L.LetP(name, prim, args.map(rewriteAtom(_)), substitute(body))
        case L.LetC(cnts, body) =>
          L.LetC(cnts.map(rewriteCnt(_)), substitute(body))
        case L.LetF(funs, body) =>
          L.LetF(funs, substitute(body))
        case L.AppC(c, args) =>
          L.AppC(c, args.map(rewriteAtom(_)))
        case L.AppF(f, retC, args) =>
          L.AppF(rewriteAtom(f), retC, args.map(rewriteAtom(_)))
        case L.If(cond, args, thenC, elseC) =>
          L.If(cond, args.map(rewriteAtom(_)), thenC, elseC)
        case L.Halt(a) =>
          L.Halt(rewriteAtom(a))
      }
    }
    /*
    def extractEnv(env: Symbol, body: H.Tree)
                  (substitutions: Subst[Symbol], fvs: Seq[Symbol]): L.Tree =
      fvs match {
        case Seq() =>
          substitute(h2lVal(body))(using substitutions)
        case fv +: fvs =>
          tmpLetP(CPSV.BlockGet, Seq(env, substitutions.size), { // `substition` is initially "f -> env" with size 1.
            (v: L.Name) => extractEnv(env, body)(substitutions + (fv -> v), fvs)
          })
      }
    */
    def extractEnv(env: Symbol, w: Symbol, c: Symbol, args: Seq[Symbol], numfvs: Int)
                  (vs: Seq[Symbol]): L.Body =
      vs.size match {
        case i if i == numfvs =>
          L.AppF(w, c, args ++ vs)
        case i if i < numfvs =>
          tmpLetP(CPSV.BlockGet, Seq(env, vs.size + 1), { // `vs` is initially empty with size 0.
            (v: L.Name) => extractEnv(env, w, c, args, numfvs)(vs :+ v)
          })
      }

    tree match {
      case H.LetC(cnts: Seq[H.Cnt], body: H.Body) =>
        L.LetC(cnts.map {
            case H.Cnt(name, args, e) =>
              L.Cnt(name, args, h2lVal(e) )
        }, h2lVal(body) )
      case H.AppC(cnt: H.Name, args: Seq[H.Atom]) =>
        L.AppC(cnt, args.map(rewrite(_)))
      // function
      case H.LetF(funs: Seq[H.Fun], body: H.Body) => {
        val _f2fvs: MMap[Symbol, Set[Symbol]] = MMap.from(f2fvs) // mutable copy!
        fvFuns(funs)(using _f2fvs) // enlarge `_f2fvs`; do not care about return value
        val __f2fvs = Map.from(_f2fvs); // convert back to immutable `Map`
        
        val ws: Map[Symbol, Symbol] = funs.map(_.name -> Symbol.fresh("w")).toMap;
        val (workers, wrappers, mappings) = funs.map {
          case H.Fun(f: Symbol, retC: H.Name, args: Seq[H.Name], e: H.Body) => {
            // worker
            val fvs: Seq[Symbol]   = __f2fvs(f).toSeq
            val us : Seq[Symbol]   = fvs.map(_ => Symbol.fresh("u"))
            val sub: Subst[Symbol] = (fvs zip us).foldLeft(emptySubst)((acc, kv) => acc + kv)
            val worker = L.Fun(ws(f), retC, args ++ us, substitute(h2lVal(e)
                                                                         (using __f2fvs, f2w ++ ws))
                                                                  (using sub))
            // wrapper
            val s = Symbol.fresh("s")
            val c = Symbol.fresh("c")
            val env = Symbol.fresh("env")
            val _args = args.map((_) => Symbol.fresh("args"))
            val wrapper = L.Fun(s, c, env +: _args, extractEnv(env, ws(f), c, _args, fvs.size)
                                                              (Seq.empty[Symbol]))
            (worker, wrapper, f -> (s, fvs))
          }
        }.unzip3
        /*
        val (closedFuns, mappings) = funs.map {
          case H.Fun(f: Symbol, retC: H.Name, args: Seq[H.Name], e: H.Body) => {
            val w   = Symbol.fresh("w")
            val env = Symbol.fresh("env")
            val fvs = ((fv(e) - f) diff args.toSet).toSeq

            (L.Fun(w, retC, env +: args, extractEnv(env, e)(subst(f, env), fvs)),
             f -> (w, fvs))
          }
        }.unzip
        */
        var WIP: L.Body = h2lVal(body)(using __f2fvs, f2w ++ ws)
        for ((f, (s, fvs)) <- mappings) {
          for ((fv, index) <- fvs.zipWithIndex) {
            WIP = L.LetP(Symbol.fresh("t"), CPSV.BlockSet, Seq(f, index + 1, fv), WIP)
          }
          WIP = L.LetP(Symbol.fresh("t"), CPSV.BlockSet, Seq(f, 0, s), WIP)
        }
        for ((f, (_, fvs)) <- mappings) {
          WIP = L.LetP(f, CPSV.BlockAlloc, Seq(BlockTag.Function, fvs.size + 1), WIP)
        }

        L.LetF(workers ++ wrappers, WIP)
      }
      case H.AppF(fun: H.Name, retC: H.Name, args: Seq[H.Atom]) => {
        val _w = f2w.get(fun)
        _w match {
          case None => {
            val f = Symbol.fresh("f")
            val rewrittenFun = rewrite(fun)
            L.LetP(f, CPSV.BlockGet, Seq(rewrittenFun, 0),
                   L.AppF(f, retC, rewrittenFun +: args.map(rewrite(_))))
          }
          case Some(w) =>
            L.AppF(w, retC, args.map(rewrite(_)) ++ f2fvs(fun))
        }
      }
      // arithmetics
      // +
      case H.LetP(n: H.Name, L3.IntAdd, Seq(x: H.Atom, y: H.Atom), b: H.Body) =>
        tmpLetP(CPSV.XOr, Seq(rewrite(x), 1), {
          _x => L.LetP(n, CPSV.Add, Seq(_x, rewrite(y)), h2lVal(b) )
        })
      // TODO: change (- 1) to (XOR 1) to clear LSB
      // -
      case H.LetP(n: H.Name, L3.IntSub, Seq(x: H.Atom, y: H.Atom), b: H.Body) =>
        tmpLetP(CPSV.Sub, Seq(rewrite(x), rewrite(y)), {
          _x => L.LetP(n, CPSV.Add, Seq(_x, 1), h2lVal(b) )
        })
      // ×
      case H.LetP(n: H.Name, L3.IntMul, Seq(x: H.Atom, y: H.Atom), b: H.Body) => {
        tmpLetP(CPSV.Sub, Seq(rewrite(x), 1), {
          _x => tmpLetP(CPSV.ShiftRight, Seq(rewrite(y), 1), {
            _y => tmpLetP(CPSV.Mul, Seq(_x, _y), {
              z => L.LetP(n, CPSV.Add, Seq(z, 1), h2lVal(b) )
            })
          })
        })
      }
      // ÷
      // ⟦n ÷ m⟧ = 2 × ( (⟦n⟧ - 1) / (⟦m⟧ - 1) ) + 1
      case H.LetP(n: H.Name, L3.IntDiv, Seq(x: H.Atom, y: H.Atom), b: H.Body) => {
        tmpLetP(CPSV.Sub, Seq(rewrite(x), 1), {
          _x => tmpLetP(CPSV.Sub, Seq(rewrite(y), 1), {
            _y => tmpLetP(CPSV.Div, Seq(_x, _y), {
              z => tmpLetP(CPSV.Mul, Seq(z, 2), {
                z2 => L.LetP(n, CPSV.Add, Seq(z2, 1), h2lVal(b) )
              })
            })
          })
        })
      }
      // %
      // ⟦n MOD m⟧ = (⟦n⟧ - 1 MOD ⟦m⟧ - 1) + 1
      case H.LetP(n: H.Name, L3.IntMod, Seq(x: H.Atom, y: H.Atom), b: H.Body) => {
        tmpLetP(CPSV.Sub, Seq(rewrite(x), 1), {
          _x => tmpLetP(CPSV.Sub, Seq(rewrite(y), 1), {
            _y => tmpLetP(CPSV.Mod, Seq(_x, _y), {
              z => L.LetP(n, CPSV.Add, Seq(z, 1), h2lVal(b) )
            })
          })
        })
      }
      // <<
      case H.LetP(n: H.Name, L3.IntShiftLeft, Seq(x: H.Atom, y: H.Atom), b: H.Body) => {
        tmpLetP(CPSV.Sub, Seq(rewrite(x), 1), {
          _x => tmpLetP(CPSV.ShiftRight, Seq(rewrite(y), 1), { // Right-shifting ⟦y⟧ already clears LSB.
            _y => tmpLetP(CPSV.ShiftLeft, Seq(_x, _y), {
              z => L.LetP(n, CPSV.Or, Seq(z, 1), h2lVal(b) ) // equivalent to adding 1
            })
          })
        })
      }
      // >>
      case H.LetP(n: H.Name, L3.IntShiftRight, Seq(x: H.Atom, y: H.Atom), b: H.Body) => {
        tmpLetP(CPSV.ShiftRight, Seq(rewrite(y), 1), {
          _y => tmpLetP(CPSV.ShiftRight, Seq(rewrite(x), _y), {
            z => L.LetP(n, CPSV.Or, Seq(z, 1), h2lVal(b) )
          })
        })
      }
      // &
      case H.LetP(n: H.Name, L3.IntBitwiseAnd, Seq(x: H.Atom, y: H.Atom), b: H.Body) => {
        L.LetP(n, CPSV.And, Seq(rewrite(x), rewrite(y)), h2lVal(b) )
      }
      // |
      case H.LetP(n: H.Name, L3.IntBitwiseOr, Seq(x: H.Atom, y: H.Atom), b: H.Body) => {
        L.LetP(n, CPSV.Or, Seq(rewrite(x), rewrite(y)), h2lVal(b) )
      }
      // ^
      case H.LetP(n: H.Name, L3.IntBitwiseXOr, Seq(x: H.Atom, y: H.Atom), b: H.Body) => {
        tmpLetP(CPSV.XOr, Seq(rewrite(x), rewrite(y)), {
          z => L.LetP(n, CPSV.Or, Seq(z, 1), h2lVal(b) )
        })
      }
      // id
      case H.LetP(n: H.Name, L3.Id, Seq(a: H.Atom), b: H.Body) => {
        L.LetP(n, CPSV.Id, Seq(rewrite(a)), h2lVal(b))
      }
      // Comparison
      // <
      case H.If(L3.IntLt, args: Seq[H.Atom], thenC: H.Name, elseC: H.Name) =>
        L.If(CPST.Lt, args.map(rewrite(_)), thenC, elseC)
      // ≤
      case H.If(L3.IntLe, args: Seq[H.Atom], thenC: H.Name, elseC: H.Name) =>
        L.If(CPST.Le, args.map(rewrite(_)), thenC, elseC)
      // =
      case H.If(L3.Eq, args: Seq[H.Atom], thenC: H.Name, elseC: H.Name) =>
        L.If(CPST.Eq, args.map(rewrite(_)), thenC, elseC)
      // Type check
      // int?
      case H.If(L3.IntP, Seq(a: H.Atom), thenC: H.Name, elseC: H.Name) =>
        tmpLetP(CPSV.And, Seq(rewrite(a), 1), {
          t1 => L.If(CPST.Eq, Seq(t1, 1), thenC, elseC)
        })
      // block?
      case H.If(L3.BlockP, Seq(a: H.Atom), thenC: H.Name, elseC: H.Name) =>
        tmpLetP(CPSV.And, Seq(rewrite(a), 0x3), {
          t1 => L.If(CPST.Eq, Seq(t1, 0), thenC, elseC)
        })
      // char?
      case H.If(L3.CharP, Seq(a: H.Atom), thenC: H.Name, elseC: H.Name) =>
        tmpLetP(CPSV.And, Seq(rewrite(a), 0x7), {
          t1 => L.If(CPST.Eq, Seq(t1, 6), thenC, elseC)
        })
      // bool?
      case H.If(L3.BoolP, Seq(a: H.Atom), thenC: H.Name, elseC: H.Name) => 
        tmpLetP(CPSV.And, Seq(rewrite(a), 0xF), {
          t1 => L.If(CPST.Eq, Seq(t1, 10), thenC, elseC)
        })
      // unit?
      case H.If(L3.UnitP, Seq(a: H.Atom), thenC: H.Name, elseC: H.Name) =>
        tmpLetP(CPSV.And, Seq(rewrite(a), 0xF), {
          t1 => L.If(CPST.Eq, Seq(t1, 2), thenC, elseC)
        })
      // block operations
      // block-alloc
      case H.LetP(n: H.Name, L3.BlockAlloc, Seq(a1, a2), body: H.Body) => {
        tmpLetP(CPSV.ShiftRight, Seq(rewrite(a1), 1), {
          t1 => tmpLetP(CPSV.ShiftRight, Seq(rewrite(a2), 1), {
            t2 => L.LetP(n, CPSV.BlockAlloc, Seq(t1, t2), h2lVal(body))
          })
        })
      }
      // block-tag
      case H.LetP(n: H.Name, L3.BlockTag, Seq(a: H.Atom), body: H.Body) =>
        tmpLetP(CPSV.BlockTag, Seq(rewrite(a)), {
          t1 => tmpLetP(CPSV.ShiftLeft, Seq(t1, 1), {
            t2 => L.LetP(n, CPSV.Or, Seq(t2, 1), h2lVal(body))
          })
        })
      // block-length
      case H.LetP(n: H.Name, L3.BlockLength, Seq(a: H.Atom), body: H.Body) =>
        tmpLetP(CPSV.BlockLength, Seq(rewrite(a)), {
          t1 => tmpLetP(CPSV.ShiftLeft, Seq(t1, 1), {
            t2 => L.LetP(n, CPSV.Or, Seq(t2, 1), h2lVal(body))
          })
        })
      // block-get
      case H.LetP(n: H.Name, L3.BlockGet, Seq(b: H.Atom, i: H.Atom), body: H.Body) =>
        tmpLetP(CPSV.ShiftRight, Seq(rewrite(i), 1), {
          _i => L.LetP(n, CPSV.BlockGet, Seq(rewrite(b), _i), h2lVal(body)) // need to untag returned element?
        })
      // block-set!
      case H.LetP(n: H.Name, L3.BlockSet, Seq(b: H.Atom, i: H.Atom, v: H.Atom), body: H.Body) =>
        tmpLetP(CPSV.ShiftRight, Seq(rewrite(i), 1), {
          _i => L.LetP(n, CPSV.BlockSet, Seq(rewrite(b), _i, rewrite(v)), h2lVal(body))
        })
      // I/O
      // byte-read
      case H.LetP(n: H.Name, L3.ByteRead, Seq(), body: H.Body) =>
        tmpLetP(CPSV.ByteRead, Seq.empty[L.Atom], {
          t1 => tmpLetP(CPSV.ShiftLeft, Seq(t1, 1), {
            t2 => L.LetP(n, CPSV.Or, Seq(t2, 1), h2lVal(body))
          })
        })
      // byte-write
      case H.LetP(n: H.Name, L3.ByteWrite, Seq(a: H.Atom), body: H.Body) =>
        tmpLetP(CPSV.ShiftRight, Seq(rewrite(a), 1), {
          t1 => L.LetP(n, CPSV.ByteWrite, Seq(t1), h2lVal(body))
        })
      // integer and character conversion
      // char->int
      case H.LetP(n: H.Name, L3.CharToInt, Seq(a: H.Atom), body: H.Body) =>
        L.LetP(n, CPSV.ShiftRight, Seq(rewrite(a), 2), h2lVal(body))
      // int->char
      case H.LetP(n: H.Name, L3.IntToChar, Seq(a: H.Atom), body: H.Body) =>
        tmpLetP(CPSV.ShiftLeft, Seq(rewrite(a), 2), {
          t1 => L.LetP(n, CPSV.Or, Seq(t1, 0x2), h2lVal(body))
        })
      // halt
      case H.Halt(arg: H.Atom) => 
        tmpLetP(CPSV.ShiftRight, Seq(rewrite(arg), 1), {
          _a => L.Halt(_a)
        })
    }
  }
  
  private def rewrite(a: H.Atom): L.Atom = a match {
    case n: H.Name     => n
    case IntLit(v)     => (v.toInt << 1) | 1 // equivalent to `v * 2 + 1`
    case CharLit(c)    => (c.toInt << 3) | 6 // 0b110
    case BooleanLit(b) => if (b) 0x1A else 0xA
    case UnitLit       => 0x2
  }

  private def tmpLetP(p: L.ValuePrimitive, args: Seq[L.Atom],
                      body: L.Name => L.Tree): L.Tree = {
    val tmp = Symbol.fresh("t")
    L.LetP(tmp, p, args, body(tmp))
  }
  /**
    * 
    *
    * @param funs  function definitions, possibly (mutually) recursive
    * @param f2fvs mutable map from "known" function to free variables
    * @return
    */
  private def fvFuns(funs: Seq[H.Fun])
                    (using f2fvs: MMap[Symbol, Set[Symbol]]): Map[Symbol, Set[Symbol]] = {
    // must use immutable map
    var previous: Map[Symbol, Set[Symbol]] = Map()
    var current : Map[Symbol, Set[Symbol]] = funs.map(fun => (fun.name -> Set())).toMap
    while (current != previous) {
      previous = current
      for (H.Fun(f, _, as, e) <- funs) {
        current = current.updated(f, fv(e)(using f2fvs ++ previous) - f diff as.toSet)
      }
    }
    f2fvs ++= current // update `f2fvs`
    current           // return free variables of target functions
  }
  /**
    * 
    *
    * @param tree
    * @param f2fvs map from "knwon" function to free variables
    *      It does not matter if the map is mutable or not, but for simplicity,
    *      a mutable version is adopted.
    * @param f2w map from "known" function to worker
    * @return
    */
  private def fv(tree: H.Tree)(using f2fvs: MMap[Symbol, Set[Symbol]]): Set[Symbol] = tree match {
    case H.LetP(n: H.Name, _, args: Seq[H.Atom], b: H.Body) =>
      (fv(b) excl n) union args.flatMap(fvAtom(_)).toSet
    case H.LetC(cnts: Seq[H.Cnt], body: H.Body) =>
      fv(body) union cnts.flatMap {
        case H.Cnt(_, args: Seq[H.Name], e: H.Body) =>
          fv(e) diff args.toSet
      }.toSet
    case H.LetF(funs: Seq[H.Fun], body: H.Body) => {
      val _f2fvs: MMap[Symbol, Set[Symbol]] = f2fvs.clone() // use copy to avoid modifying the original one
      val funfvs: Map[Symbol, Set[Symbol]]  = fvFuns(funs)(using _f2fvs) // enlarge `_f2fvs`
      val allfvs: Set[Symbol] = funfvs.values.flatten.toSet
      fv(body)(using _f2fvs) union allfvs diff funs.map(_.name).toSet
    }
    case H.AppC(_, args: Seq[H.Atom]) =>
      args.flatMap(fvAtom(_)).toSet
    case H.AppF(fun: H.Atom, _, args: Seq[H.Atom]) => {
      val fvs = f2fvs.get(fun.asInstanceOf[Symbol]) // correct?
      fvs match {
        case None =>
          fvAtom(fun) union args.flatMap(fvAtom(_)).toSet
        case Some(fvs) =>
          /* Suppose `f` invokes `g`. If `g` is "known", then `g` itself is not
           * a free variable of `f`. Instead, the free variables of `g` are
           * added to `f`'s.
           * 
           * If there is a value corresponding to the key `fun` in `f2fvs`,
           * then `fun` is "known".
           */
          fvs union args.flatMap(fvAtom(_)).toSet
      }
    }
    case H.If(_, args: Seq[H.Atom], _, _) =>
      args.flatMap(fvAtom(_)).toSet
    case H.Halt(a: H.Atom) =>
      fvAtom(a)
  }

  private def fvAtom(a: H.Atom): Set[Symbol] = a match {
    case n: H.Name => Set(n)
    case l: H.Literal => Set.empty
  }
}
