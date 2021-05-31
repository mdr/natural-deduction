package naturalDeduction

import naturalDeduction.parser.FormulaParser
import upickle.default._
import naturalDeduction.pretty.FormulaPrettyPrinter

sealed trait Formula {

  import Formula._

  def variables: Set[String] = this match {
    case Bottom => Set.empty
    case PropositionalVariable(name) =>Set(name)
    case Negation(formula) =>formula.variables
    case Conjunction(conjunct1, conjunct2) => conjunct1.variables ++ conjunct2.variables
    case Disjunction(disjunct1, disjunct2) => disjunct1.variables ++ disjunct2.variables
    case Implication(antecedent, consequent) => antecedent.variables ++ consequent.variables
    case Equivalence(formula1, formula2) => formula1.variables ++ formula2.variables
  }

  def replaceEquivalences: Formula = this match {
    case Bottom | PropositionalVariable(_) => this
    case Equivalence(formula1, formula2) =>
      val replacedFormula1 = formula1.replaceEquivalences
      val replacedFormula2 = formula2.replaceEquivalences
      (replacedFormula1 → replacedFormula2) ∧ (replacedFormula2 → replacedFormula1)
    case Conjunction(conjunct1, conjunct2) => conjunct1.replaceEquivalences ∧ conjunct2.replaceEquivalences
    case Disjunction(disjunct1, disjunct2) => disjunct1.replaceEquivalences ∨ disjunct2.replaceEquivalences
    case Implication(antecedent, consequent) => antecedent.replaceEquivalences → consequent.replaceEquivalences
    case Negation(formula) => formula.replaceEquivalences.not
  }

  def ∧(that: Formula): Conjunction = Conjunction(this, that)

  def ∨(that: Formula): Disjunction = Disjunction(this, that)

  def →(that: Formula): Implication = Implication(this, that)

  def ↔(that: Formula): Equivalence = Equivalence(this, that)

  def not: Negation = Negation(this)

  override def toString: String = FormulaPrettyPrinter.prettyPrint(this)

}

object Formula {

  implicit val formulaReadWrite: ReadWriter[Formula] =
    readwriter[String].bimap[Formula](_.toString, FormulaParser.parseFormula)

  val ⊥ : Bottom.type = Bottom

  case object Bottom extends Formula {
  }


  case class PropositionalVariable(name: String) extends Formula {
  }

  case class Negation(formula: Formula) extends Formula {
  }

  case class Conjunction(conjunct1: Formula, conjunct2: Formula) extends Formula {
  }

  case class Disjunction(disjunct1: Formula, disjunct2: Formula) extends Formula {
  }

  case class Implication(antecedent: Formula, consequent: Formula) extends Formula {
  }

  case class Equivalence(formula1: Formula, formula2: Formula) extends Formula {
    def forwardsImplication: Formula = formula1 → formula2

    def backwardsImplication: Formula = formula2 → formula1
  }


}

sealed trait EquivalenceDirection

object EquivalenceDirection {
  case object Forwards extends EquivalenceDirection
  case object Backwards extends EquivalenceDirection
}