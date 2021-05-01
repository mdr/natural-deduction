package naturalDeduction.html

import naturalDeduction.Derivation.Axiom
import naturalDeduction.parser.FormulaParser
import naturalDeduction.{Derivation, DerivationPath, Formula}

case class State(
                  newFormulaText: String = "",
                  derivations: Seq[Derivation] = Seq.empty,
                  modalState: Option[ModalState] = None,
                  undoRedo: UndoRedo[Seq[Derivation]] = UndoRedo()
                ) {

  lazy val formulaToDerivationIndices: Map[Formula, Seq[Int]] =
    derivations.zipWithIndex.groupMap(_._1.formula)(_._2)

  def deleteDerivation(derivationIndex: Int): State =
    withUndo(
      copy(
        derivations = derivations.patch(derivationIndex, Seq.empty, 1)))

  def duplicateDerivation(derivationIndex: Int): State =
    withUndo(
      copy(
        derivations = derivations.patch(derivationIndex, Seq(derivations(derivationIndex), derivations(derivationIndex)), 1)))

  def updateModalState(f: ModalState => ModalState): State = copy(modalState = modalState map f)

  def swapConjuncts: State = updateModalState(_.swapConjuncts)

  def withModalFormula(newValue: String): State = updateModalState(_.withModalFormula(newValue))

  def withUndo(newState: State): State = newState.copy(undoRedo = undoRedo.push(derivations))

  def undo: State = {
    val (newDerivations, newUndoRedo) = undoRedo.undo(derivations)
    copy(derivations = newDerivations, undoRedo = newUndoRedo)
  }

  def redo: State = {
    val (newDerivations, newUndoRedo) = undoRedo.redo(derivations)
    copy(derivations = newDerivations, undoRedo = newUndoRedo)
  }

  def acceptNewFormulaAsNewDerivation: State =
    withUndo(
      copy(
        derivations = derivations :+ Axiom(FormulaParser.parseFormula(newFormulaText)),
        newFormulaText = ""))

  def newFormulaIsValid: Boolean = FormulaParser.tryParseFormula(newFormulaText).isRight

  def getDerivation(i: Int): Derivation = derivations(i)

  private def setDerivation(i: Int, derivation: Derivation): State = copy(derivations = derivations.patch(i, Seq(derivation), 1))

  def transformDerivation(i: Int, f: Derivation => Derivation): State = withUndo(setDerivation(i, f(getDerivation(i))))

  def closeModal: State = copy(modalState = None)

  def showConjunctionElimBackwardsModalState(derivationIndex: Int, path: DerivationPath): State = {
    val conclusion = getDerivation(derivationIndex).get(path).formula
    copy(modalState = Some(ConjunctionElimBackwardsModalState(derivationIndex, path, conclusion)))
  }

  def showImplicationElimBackwardsModalState(derivationIndex: Int, path: DerivationPath): State = {
    val consequent = getDerivation(derivationIndex).get(path).formula
    copy(modalState = Some(ImplicationElimBackwardsModalState(derivationIndex, path, consequent)))
  }

}
