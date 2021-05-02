package naturalDeduction.html

import naturalDeduction.Derivation.Axiom
import naturalDeduction.html.modal.{ConjunctionElimBackwardsModalState, ConjunctionIntroForwardsModalState, ImplicationElimBackwardsModalState, ImplicationElimForwardsFromAntecedentModalState, ImplicationIntroForwardsModalState, ModalState}
import naturalDeduction.parser.FormulaParser
import naturalDeduction.{Derivation, DerivationPath, Formula}

case class State(
                  newFormulaText: String = "",
                  derivations: Seq[Derivation] = Seq.empty,
                  modalState: Option[ModalState] = None,
                  undoRedo: UndoRedo[Seq[Derivation]] = UndoRedo()
                ) {

  lazy val formulaToDerivationIndices: Map[Formula, Seq[DerivationIndex]] =
    derivations.zipWithIndex.groupMap(_._1.formula)(_._2)

  def deleteDerivation(derivationIndex: DerivationIndex): State =
    withUndo(
      copy(
        derivations = derivations.patch(derivationIndex, Seq.empty, 1)))

  def duplicateDerivation(derivationIndex: DerivationIndex): State =
    withUndo(
      copy(
        derivations = derivations.patch(derivationIndex, Seq(derivations(derivationIndex), derivations(derivationIndex)), 1)))

  def withUndo(newState: State): State = newState.copy(undoRedo = undoRedo.push(derivations))

  def undo: State = {
    val (newDerivations, newUndoRedo) = undoRedo.undo(derivations)
    copy(derivations = newDerivations, undoRedo = newUndoRedo)
  }

  def redo: State = {
    val (newDerivations, newUndoRedo) = undoRedo.redo(derivations)
    copy(derivations = newDerivations, undoRedo = newUndoRedo)
  }

  def newFormulaIsValid: Boolean = FormulaParser.tryParseFormula(newFormulaText).isRight

  def acceptNewFormulaAsNewDerivation: State =
    withUndo(
      copy(
        derivations = derivations :+ Axiom(FormulaParser.parseFormula(newFormulaText)),
        newFormulaText = ""))

  def getDerivation(i: DerivationIndex): Derivation = derivations(i)

  def setDerivation(i: DerivationIndex, derivation: Derivation): State =
    copy(derivations = derivations.patch(i, Seq(derivation), 1))

  def transformDerivation(i: DerivationIndex, f: Derivation => Derivation): State =
    withUndo(setDerivation(i, f(getDerivation(i))))

  // Modal

  def closeModal: State = copy(modalState = None)

  def updateModalState(f: ModalState => ModalState): State = copy(modalState = modalState map f)

  def swapConjuncts: State = updateModalState(_.swapConjuncts)

  def withModalFormula(newValue: String): State = updateModalState(_.withModalFormula(newValue))

  def showConjunctionElimBackwardsModal(derivationIndex: DerivationIndex, path: DerivationPath): State = {
    val conclusion = derivationFormula(derivationIndex, path)
    copy(modalState = Some(ConjunctionElimBackwardsModalState(derivationIndex, path, conclusion)))
  }

  def showConjunctionIntroForwardsModal(derivationIndex: DerivationIndex): State = {
    val conclusion = derivationFormula(derivationIndex)
    copy(modalState = Some(ConjunctionIntroForwardsModalState(derivationIndex, conclusion)))
  }

  def showImplicationElimBackwardsModal(derivationIndex: DerivationIndex, path: DerivationPath): State = {
    val consequent = derivationFormula(derivationIndex, path)
    copy(modalState = Some(ImplicationElimBackwardsModalState(derivationIndex, path, consequent)))
  }

  def showImplicationIntroForwardsModal(derivationIndex: DerivationIndex): State = {
    val conclusion = derivationFormula(derivationIndex)
    copy(modalState = Some(ImplicationIntroForwardsModalState(derivationIndex, conclusion)))
  }

  def showImplicationElimForwardsFromAntecedentModal(derivationIndex: DerivationIndex): State = {
    val antecedent = derivationFormula(derivationIndex)
    copy(modalState = Some(ImplicationElimForwardsFromAntecedentModalState(derivationIndex, antecedent)))
  }

  private def derivationFormula(derivationIndex: DerivationIndex, path: DerivationPath = DerivationPath.empty): Formula =
    getDerivation(derivationIndex).get(path).formula


}
