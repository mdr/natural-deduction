package naturalDeduction.html.modal

import naturalDeduction.Derivation._
import naturalDeduction.Formula.{Conjunction, Implication}
import naturalDeduction._
import naturalDeduction.html.State
import naturalDeduction.html.modal.ConjunctionElimBackwardsModalState.conjunctionEliminationDerivation
import naturalDeduction.parser.FormulaParser

sealed trait ModalState {
  def withModalFormula(newValue: String): ModalState

  def title: String

  def swapConjuncts: ModalState = this

  def canComplete: Boolean

  def complete(state: State): State
}

object ConjunctionElimBackwardsModalState {

  def conjunctionEliminationDerivation(conjunct1: Formula, conjunct2: Formula, conjunctToPick: Int): Derivation =
    conjunctToPick match {
      case 0 => LeftConjunctionElimination(Axiom(Conjunction(conjunct1, conjunct2)))
      case 1 => RightConjunctionElimination(Axiom(Conjunction(conjunct2, conjunct1)))
    }

}

case class ConjunctionElimBackwardsModalState(derivationIndex: Int,
                                              path: DerivationPath,
                                              conclusion: Formula,
                                              conjunctToPick: Int = 0,
                                              formulaText: String = "") extends ModalState {
  def title: String = "∧-Elimination Backwards"

  def withModalFormula(newText: String): ModalState = copy(formulaText = newText)

  override def swapConjuncts: ModalState = copy(conjunctToPick = if (conjunctToPick == 0) 1 else 0)

  override def canComplete: Boolean = FormulaParser.tryParseFormula(formulaText).isRight

  override def complete(state: State): State = {
    val newFormula = FormulaParser.parseFormula(formulaText)
    val newDerivation: Derivation = conjunctionEliminationDerivation(conclusion, newFormula, conjunctToPick)
    state.transformDerivation(derivationIndex, _.set(path, newDerivation))
  }
}

case class ImplicationElimBackwardsModalState(derivationIndex: Int,
                                              path: DerivationPath,
                                              consequent: Formula,
                                              formulaText: String = "") extends ModalState {
  def title: String = "→-Elimination Backwards"

  def withModalFormula(newText: String): ModalState = copy(formulaText = newText)

  override def swapConjuncts: ModalState = this

  override def canComplete: Boolean = FormulaParser.tryParseFormula(formulaText).isRight

  override def complete(state: State): State = {
    val newFormula = FormulaParser.parseFormula(formulaText)
    val newDerivation = ImplicationElimination(newFormula, consequent)
    state.transformDerivation(derivationIndex, _.set(path, newDerivation))
  }
}


case class ImplicationElimForwardsFromAntecedentModalState(derivationIndex: Int,
                                                           antecedent: Formula,
                                                           formulaText: String = "") extends ModalState {
  def title: String = "→-Elimination Forwards"

  def withModalFormula(newText: String): ModalState = copy(formulaText = newText)

  override def canComplete: Boolean = FormulaParser.tryParseFormula(formulaText).isRight

  override def complete(state: State): State = {
    val consequent = FormulaParser.parseFormula(formulaText)
    val newDerivation = ImplicationElimination(antecedent, consequent)
    state.setDerivation(derivationIndex, newDerivation)
  }
}
