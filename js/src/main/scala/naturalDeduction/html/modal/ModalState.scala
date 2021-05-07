package naturalDeduction.html.modal

import naturalDeduction.Derivation._
import naturalDeduction.Formula._
import naturalDeduction._
import naturalDeduction.html.{DerivationIndex, State}
import naturalDeduction.html.modal.ConjunctionElimBackwardsModalState.conjunctionEliminationDerivation
import naturalDeduction.html.modal.ConjunctionIntroForwardsModalState.conjunctionIntroductionDerivation
import naturalDeduction.parser.FormulaParser

sealed trait ModalState {
  val formulaText: String

  def withModalFormula(newValue: String): ModalState

  val formula: Formula =
    FormulaParser.tryParseFormula(formulaText).toOption.getOrElse(PropositionalVariable("?"))

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

case class ConjunctionElimBackwardsModalState(derivationIndex: DerivationIndex,
                                              path: DerivationPath,
                                              conclusion: Formula,
                                              conjunctToPick: ChildIndex = 0,
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

  def withModalFormula(newText: String): ModalState = copy(formulaText = newText)

  override def swapConjuncts: ModalState = copy(conjunctToPick = if (conjunctToPick == 0) 1 else 0)

  override def canComplete: Boolean = FormulaParser.tryParseFormula(formulaText).isRight

  override def complete(state: State): State =
    state.transformDerivation(derivationIndex, existingConjunctDerivation => {
      val newConjunct = FormulaParser.parseFormula(formulaText)
      conjunctionIntroductionDerivation(existingConjunctDerivation, newConjunct, conjunctToPick)
    })
}

case class ImplicationElimBackwardsModalState(derivationIndex: DerivationIndex,
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


case class ImplicationElimForwardsFromAntecedentModalState(derivationIndex: DerivationIndex,
                                                           antecedent: Formula,
                                                           formulaText: String = "") extends ModalState {
  def title: String = "→-Elimination Forwards"

  def withModalFormula(newText: String): ModalState = copy(formulaText = newText)

  override def canComplete: Boolean = FormulaParser.tryParseFormula(formulaText).isRight

  override def complete(state: State): State =
    state.transformDerivation(derivationIndex, antecedentDerivation => {
      val consequent = FormulaParser.parseFormula(formulaText)
      ImplicationElimination(antecedentDerivation, (antecedent → consequent).axiom)
    })

}

case class NegationElimBackwardsModalState(derivationIndex: DerivationIndex,
                                           path: DerivationPath,
                                           formulaText: String = "") extends ModalState {
  def title: String = "¬-Elimination Backwards"

  def withModalFormula(newText: String): ModalState = copy(formulaText = newText)

  override def canComplete: Boolean = FormulaParser.tryParseFormula(formulaText).isRight

  override def complete(state: State): State =
    state.transformDerivation(derivationIndex, _.set(path, {
      val formula = FormulaParser.parseFormula(formulaText)
      NegationElimination(formula.axiom, formula.not.axiom)
    }))

}

case class ImplicationIntroForwardsModalState(derivationIndex: DerivationIndex,
                                              consequent: Formula,
                                              formulaText: String = "") extends ModalState {
  def title: String = "→-Introduction Forwards"

  def withModalFormula(newText: String): ModalState = copy(formulaText = newText)

  override def canComplete: Boolean = FormulaParser.tryParseFormula(formulaText).isRight

  override def complete(state: State): State =
    state.transformDerivation(derivationIndex, consequentDerivation => {
      val antecedent = FormulaParser.parseFormula(formulaText)
      ImplicationIntroduction(antecedent, Some(consequentDerivation.nextFreshLabel), consequentDerivation)
    })

}
