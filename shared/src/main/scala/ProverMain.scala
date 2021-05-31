import ljt.LJTTheoremProver
import naturalDeduction.Sequent
import naturalDeduction.parser.FormulaParser.parseSequent
import naturalDeduction.pretty.FormulaPrettyPrinter

import scala.io.StdIn

object ProverMain extends App {
  FormulaPrettyPrinter.includeAllParens = false
  while (true) {
    val line = StdIn.readLine("sequent> ")
    val sequent = parseSequent(line)
    val Some(derivation) = LJTTheoremProver.prove(sequent)
    println(derivation)
  }

}
