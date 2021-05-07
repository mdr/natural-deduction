package naturalDeduction.html.modal

import naturalDeduction.Derivation._
import naturalDeduction.Formula._
import naturalDeduction._
import naturalDeduction.html.modal.ConjunctionIntroForwardsModalState.conjunctionIntroductionDerivation
import naturalDeduction.html.{DerivationIndex, State}
import naturalDeduction.parser.FormulaParser

sealed trait ModalState {
  val formulaText: String

  def withFormulaText(newValue: String): ModalState

  val formula: Formula =
    FormulaParser.tryParseFormula(formulaText).toOption getOrElse PropositionalVariable("?")

  def title: String

  def swapConjuncts: ModalState = this

  def canComplete: Boolean = FormulaParser.tryParseFormula(formulaText).isRight

  def complete(state: State): State
}

object ConjunctionElimBackwardsModalState {


}

case class ConjunctionElimBackwardsModalState(derivationIndex: DerivationIndex,
                                              path: DerivationPath,
                                              conclusion: Formula,
                                              conjunctToPick: ChildIndex = 0,
                                              formulaText: String = "") extends ModalState {
  def title: String = "∧-Elimination Backwards"

  def withFormulaText(newText: String): ModalState = copy(formulaText = newText)

  override def swapConjuncts: ModalState = copy(conjunctToPick = if (conjunctToPick == 0) 1 else 0)

  override def complete(state: State): State =
    state.transformDerivation(derivationIndex, _.set(path, newDerivation))

  def newDerivation: Derivation = conjunctionEliminationDerivation(conclusion, formula, conjunctToPick)

  private def conjunctionEliminationDerivation(conjunct1: Formula, conjunct2: Formula, conjunctToPick: Int): Derivation =
    conjunctToPick match {
      case 0 => LeftConjunctionElimination(Axiom(Conjunction(conjunct1, conjunct2)))
      case 1 => RightConjunctionElimination(Axiom(Conjunction(conjunct2, conjunct1)))
    }
}

case class ImplicationElimBackwardsModalState(derivationIndex: DerivationIndex,
                                              path: DerivationPath,
                                              consequent: Formula,
                                              formulaText: String = "") extends ModalState {
  def title: String = "→-Elimination Backwards"

  def withFormulaText(newText: String): ModalState = copy(formulaText = newText)

  override def swapConjuncts: ModalState = this

  override def complete(state: State): State =
    state.transformDerivation(derivationIndex, _.set(path, newDerivation))

  def newDerivation: ImplicationElimination = ImplicationElimination(formula, consequent)
}

case class NegationElimBackwardsModalState(derivationIndex: DerivationIndex,
                                           path: DerivationPath,
                                           formulaText: String = "") extends ModalState {
  def title: String = "¬-Elimination Backwards"

  def withFormulaText(newText: String): ModalState = copy(formulaText = newText)

  override def complete(state: State): State =
    state.transformDerivation(derivationIndex, _.set(path, newDerivation))

  def newDerivation: NegationElimination = NegationElimination(formula.axiom, formula.not.axiom)
}

object ConjunctionIntroForwardsModalState {

  def conjunctionIntroductionDerivation(conjunct1Derivation: Derivation, conjunct2: Formula, conjunctToPick: Int): Derivation =
    conjunctToPick match {
      case 0 => ConjunctionIntroduction(conjunct1Derivation, conjunct2.axiom)
      case 1 => ConjunctionIntroduction(conjunct2.axiom, conjunct1Derivation)
    }

}

case class ConjunctionIntroForwardsModalState(derivationIndex: DerivationIndex,
                                              existingConjunct: Formula,
                                              conjunctToPick: ChildIndex = 0,
                                              formulaText: String = "") extends ModalState {
  def title: String = "∧-Introduction Forwards"

  def withFormulaText(newText: String): ModalState = copy(formulaText = newText)

  override def swapConjuncts: ModalState = copy(conjunctToPick = if (conjunctToPick == 0) 1 else 0)

  override def complete(state: State): State = {
    val newConjunct = FormulaParser.parseFormula(formulaText)
    state.transformDerivation(derivationIndex, existingConjunctDerivation =>
      conjunctionIntroductionDerivation(existingConjunctDerivation, newConjunct, conjunctToPick))
  }
}

case class ImplicationIntroForwardsModalState(derivationIndex: DerivationIndex,
                                              consequent: Formula,
                                              formulaText: String = "") extends ModalState {
  def title: String = "→-Introduction Forwards"

  def withFormulaText(newText: String): ModalState = copy(formulaText = newText)

  override def complete(state: State): State = {
    val antecedent = FormulaParser.parseFormula(formulaText)
    state.transformDerivation(derivationIndex, consequentDerivation =>
      ImplicationIntroduction(antecedent, Some(consequentDerivation.nextFreshLabel), consequentDerivation))
  }

}

case class ImplicationElimForwardsFromAntecedentModalState(derivationIndex: DerivationIndex,
                                                           antecedent: Formula,
                                                           formulaText: String = "") extends ModalState {
  def title: String = "→-Elimination Forwards"

  def withFormulaText(newText: String): ModalState = copy(formulaText = newText)

  override def complete(state: State): State = {
    val consequent = FormulaParser.parseFormula(formulaText)
    state.transformDerivation(derivationIndex, antecedentDerivation =>
      ImplicationElimination(antecedentDerivation, (antecedent → consequent).axiom))
  }

}