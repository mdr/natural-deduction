package naturalDeduction.html.modal

import naturalDeduction.Derivation._
import naturalDeduction.Formula._
import naturalDeduction.Labels.twoFreshLabels
import naturalDeduction._
import naturalDeduction.html.modal.ConjunctionIntroForwardsModalState.conjunctionIntroductionDerivation
import naturalDeduction.html.modal.DisjunctionElimForwardsFromDisjunctionModalState.disjunctionElimDerivation
import naturalDeduction.html.modal.DisjunctionIntroForwardsModalState.disjunctionIntroDerivation
import naturalDeduction.html.{DerivationIndex, State}
import naturalDeduction.parser.FormulaParser

sealed trait ModalState {
  val formulaText: String

  def withFormulaText(newValue: String): ModalState

  def withFormulaText2(newValue: String): ModalState = this

  val formula: Formula =
    FormulaParser.tryParseFormula(formulaText).toOption getOrElse PropositionalVariable("?")

  def title: String

  def swapConjuncts: ModalState = this

  def swapDisjuncts: ModalState = this

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

  def conjunctionIntroductionDerivation(conjunct1Derivation: Derivation, conjunct2: Formula, conjunctToPick: ChildIndex): Derivation =
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

case class NegationIntroForwardsModalState(derivationIndex: DerivationIndex,
                                           formulaText: String = "") extends ModalState {
  def title: String = "¬-Introduction Forwards"

  def withFormulaText(newText: String): ModalState = copy(formulaText = newText)

  override def complete(state: State): State = {
    val formula = FormulaParser.parseFormula(formulaText)
    state.transformDerivation(derivationIndex, bottomDerivation =>
      NegationIntroduction(formula, bottomDerivation.nextFreshLabel, bottomDerivation))
  }

}

case class ReductioForwardsModalState(derivationIndex: DerivationIndex,
                                      formulaText: String = "") extends ModalState {
  def title: String = "Reductio Forwards"

  def withFormulaText(newText: String): ModalState = copy(formulaText = newText)

  override def complete(state: State): State = {
    val formula = FormulaParser.parseFormula(formulaText)
    state.transformDerivation(derivationIndex, bottomDerivation =>
      ReductioAdAbsurdum(formula, bottomDerivation.nextFreshLabel, bottomDerivation))
  }

}


object DisjunctionIntroForwardsModalState {

  def disjunctionIntroDerivation(disjunct1Derivation: Derivation, disjunct2: Formula, disjunctToPick: ChildIndex): Derivation =
    disjunctToPick match {
      case 0 => LeftDisjunctionIntroduction(disjunct1Derivation, disjunct2)
      case 1 => RightDisjunctionIntroduction(disjunct2, disjunct1Derivation)
    }

}

case class DisjunctionIntroForwardsModalState(derivationIndex: DerivationIndex,
                                              existingDisjunct: Formula,
                                              disjunctToPick: ChildIndex = 0,
                                              formulaText: String = "") extends ModalState {
  def title: String = "∨-Introduction Forwards"

  def withFormulaText(newText: String): ModalState = copy(formulaText = newText)

  override def swapDisjuncts: ModalState = copy(disjunctToPick = if (disjunctToPick == 0) 1 else 0)

  override def complete(state: State): State = {
    val newDisjunct = FormulaParser.parseFormula(formulaText)

    state.transformDerivation(derivationIndex, existingDisjunctDerivation =>
      disjunctionIntroDerivation(existingDisjunctDerivation, newDisjunct, disjunctToPick))
  }
}

object DisjunctionElimForwardsFromDisjunctionModalState {

  def disjunctionElimDerivation(conclusion: Formula, disjunctionDerivation: Derivation): DisjunctionElimination = {
    val (label1, label2) = twoFreshLabels(disjunctionDerivation.labels)
    DisjunctionElimination(disjunctionDerivation, Some(label1), conclusion.axiom, Some(label2), conclusion.axiom)
  }
}

case class DisjunctionElimForwardsFromDisjunctionModalState(derivationIndex: DerivationIndex,
                                                            disjunction: Disjunction,
                                                            formulaText: String = "") extends ModalState {

  def title: String = "∨-Elimination Forwards"

  def withFormulaText(newText: String): ModalState = copy(formulaText = newText)

  override def complete(state: State): State = {
    val conclusion = FormulaParser.parseFormula(formulaText)
    state.transformDerivation(derivationIndex, disjunctionDerivation =>
      disjunctionElimDerivation(conclusion, disjunctionDerivation)
    )
  }

}

object DisjunctionElimBackwardsModalState {

  def disjunctionElimination(disjunct1: Formula, disjunct2: Formula, conclusion: Formula, wholeDerivation: Derivation): DisjunctionElimination = {
    val (label1, label2) = twoFreshLabels(wholeDerivation.labels)
    DisjunctionElimination((disjunct1 ∨ disjunct2).axiom, Some(label1), conclusion.axiom, Some(label2), conclusion.axiom)
  }

}

case class DisjunctionElimBackwardsModalState(derivationIndex: DerivationIndex,
                                              path: DerivationPath,
                                              wholeDerivation: Derivation,
                                              conclusion: Formula,
                                              formulaText: String = "",
                                              formulaText2: String = "",
                                             ) extends ModalState {
  val formula2: Formula =
    FormulaParser.tryParseFormula(formulaText2).toOption getOrElse PropositionalVariable("?")

  import DisjunctionElimBackwardsModalState._

  override def canComplete: Boolean = super.canComplete && FormulaParser.tryParseFormula(formulaText2).isRight

  def title: String = "∨-Elimination Backwards"

  def withFormulaText(newText: String): ModalState = copy(formulaText = newText)

  override def withFormulaText2(newText: String): ModalState = copy(formulaText2 = newText)

  override def complete(state: State): State = {
    val disjunct1 = FormulaParser.parseFormula(formulaText)
    val disjunct2 = FormulaParser.parseFormula(formulaText2)
    state.transformDerivation(derivationIndex, _.set(path, disjunctionElimination(disjunct1, disjunct2, conclusion, wholeDerivation)))
  }

}