package l3
import l3.{ LowCPSTreeModule  as L }
import l3.{ FlatCPSTreeModule as F }
import l3.{ CPSValuePrimitive as CPSV }
//import l3.CPSTreeModule.Tree

object CPSHoister extends (L.Program => F.Program) {
  def apply(tree: L.Program): F.Program = 
    l2fHoist(tree)
  
  private def l2fHoist(tree: L.Program): F.Program = tree match {
    case L.LetP(name: L.Name, prim: CPSV, args: Seq[L.Atom], e: L.Body) => {
      l2fHoist(e).match {
        case F.LetF(fs, _e) =>
          F.LetF(fs, F.LetP(name, prim, args, _e))
      }
    }
    case L.LetC(cnts: Seq[L.Cnt], e: L.Body) =>
      l2fHoist(e).match {
        case F.LetF(fs, _e) => {
          val (_fsis: Seq[Seq[F.Fun]], _eis: Seq[F.Body]) = cnts.map {
            case L.Cnt(c: L.Name, ns: Seq[L.Name], ei: L.Body) =>
              l2fHoist(ei) match {
                case F.LetF(fsi, _ei) =>
                  (fsi, _ei)
              }
          }.unzip
          val fsis: Seq[F.Fun] = _fsis.fold(Seq.empty[F.Fun])(_ ++ _)
          val _cnts = (_eis zip cnts).map {
            case (_ei: F.Body, L.Cnt(c: L.Name, ns: Seq[L.Name], _)) =>
              F.Cnt(c, ns, _ei)
          }
          F.LetF(fsis ++ fs, F.LetC(_cnts, _e))
        }
      }
    case L.LetF(funs: Seq[L.Fun], e: L.Body) =>
      l2fHoist(e).match {
        case F.LetF(fs, _e) => {
          val (_fsis: Seq[Seq[F.Fun]], _eis: Seq[F.Body]) = funs.map {
            case L.Fun(f: L.Name, _, ns: Seq[L.Name], ei: L.Body) =>
              l2fHoist(ei) match {
                case F.LetF(fsi, _ei) =>
                  (fsi, _ei)
              }
          }.unzip
          val fsis: Seq[F.Fun] = _fsis.fold(Seq.empty[F.Fun])(_ ++ _)
          val _funs = (_eis zip funs).map {
            case (_ei: F.Body, L.Fun(f: L.Name, retC: L.Name, ns: Seq[L.Name], _)) =>
              F.Fun(f, retC, ns, _ei)
          }
          F.LetF(_funs ++ fsis ++ fs, _e)
        }
      }
    case e: L.Body =>
      F.LetF(Seq.empty[F.Fun], rewrite(e))
  }

  private def rewrite(e: L.Body): F.Body = e match {
    case L.AppF(fun, retC, args)        => F.AppF(fun, retC, args)
    case L.AppC(cnt, args)              => F.AppC(cnt, args)
    case L.If(cond, args, thenC, elseC) => F.If(cond, args, thenC, elseC)
    case L.Halt(arg)                    => F.Halt(arg)
    // `LetX` cases are handled in `l2fHoist`.
  }
}
