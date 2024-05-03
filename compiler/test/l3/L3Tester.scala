package l3

import java.io.{ ByteArrayOutputStream, PrintStream }
import java.nio.file.Path

import scala.util.Using.{resource => using}

import SymbolicCL3TreeModule.Tree

object L3Tester {
  def compile[T](backEnd: Tree => Either[String, T])
             (inFileNames: Seq[String]): Either[String, T] = {
    val basePath = Path.of(System.getProperty("user.dir"))
    Right(inFileNames)
      .flatMap(L3FileReader.readFilesExpandingModules(basePath, _))
      .flatMap(p => L3Parser.parse(p._1, p._2))
      .flatMap(CL3NameAnalyzer)
      .flatMap(backEnd)
  }

  def compileAndRun(backEnd: Tree => TerminalPhaseResult)
                   (inFileNames: Seq[String]): Either[String, String] = {
    def outputCapturingBackend(t: Tree): Either[String, String] = {
      val outBS = new ByteArrayOutputStream()
      using(new PrintStream(outBS)) { outPS =>
        val out0 = System.out
        try {
          System.setOut(outPS)
          backEnd(t)
            .flatMap(_ => Right(outBS.toString("UTF-8")))
        } finally {
          System.setOut(out0)
        }
      }
    }

    compile(outputCapturingBackend)(inFileNames)
  }

  val backEnd1 = (
    CL3Interpreter
  )

  val backEnd2 = (
    CL3ToCPSTranslator
      andThen HighCPSInterpreter
  )

  val backEnd3 = (
    CL3ToCPSTranslator
      andThen CPSValueRepresenter
      andThen CPSHoister
      andThen FlatCPSInterpreter
  )

  val backEnd4 = (
    CL3ToCPSTranslator
      andThen HighCPSOptimizer
      andThen CPSValueRepresenter
      andThen CPSHoister
      andThen FlatCPSOptimizer
      andThen FlatCPSInterpreter
  )

  val backEnd5 = (
    CL3ToCPSTranslator
      andThen CPSValueRepresenter
      andThen CPSHoister
      andThen CPSConstantNamer
      andThen CPSRegisterAllocator
      andThen CPSToASMTranslator
      andThen ASMLabelResolver
      andThen ASMInterpreter
  )

  val backEnd6 = (
    CL3ToCPSTranslator
      andThen HighCPSOptimizer
      andThen CPSValueRepresenter
      andThen CPSHoister
      andThen FlatCPSOptimizer
      andThen CPSConstantNamer
      andThen CPSRegisterAllocator
      andThen CPSToASMTranslator
      andThen ASMLabelResolver
      andThen ASMInterpreter
  )
}
