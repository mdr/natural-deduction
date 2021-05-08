package naturalDeduction.html

import ljt.LJTTheoremProver
import naturalDeduction.Derivation.{Axiom, RichFormula}
import naturalDeduction.Formula.Disjunction
import naturalDeduction.html.modal._
import naturalDeduction.parser.FormulaParser
import naturalDeduction.{ChildIndex, Derivation, DerivationPath, EquivalenceDirection, Formula}

case class State(
                  newFormulaText: String = "",
                  derivations: Seq[Derivation] = Seq.empty,
                  modalState: Option[ModalState] = None,
                  undoRedo: UndoRedo[Seq[Derivation]] = UndoRedo()
                ) {

  lazy val formulaToDerivationIndices: Map[Formula, Seq[DerivationIndex]] =
    derivations.zipWithIndex.groupMap(_._1.conclusion)(_._2)

  def deleteDerivation(derivationIndex: DerivationIndex): State =
    withUndo(
      copy(
        derivations = derivations.patch(derivationIndex, Seq.empty, 1)))

  def duplicateDerivation(derivationIndex: DerivationIndex): State = {
    val derivation = derivations(derivationIndex)
    withUndo(
      copy(
        derivations = derivations.patch(derivationIndex, Seq(derivation, derivation), 1)))
  }

  def extractSubderivation(derivationIndex: DerivationIndex, path: DerivationPath): State = {
    val derivation = derivations(derivationIndex)
    val subDerivation = derivation.get(path)
    withUndo(
      copy(
        derivations = derivations.patch(derivationIndex, Seq(derivation, subDerivation), 1)))
  }

  def withUndo(newState: State): State = newState.copy(undoRedo = undoRedo.push(derivations))

  def undo: State =
    if (undoRedo.canUndo) {
      val (newDerivations, newUndoRedo) = undoRedo.undo(derivations)
      copy(derivations = newDerivations, undoRedo = newUndoRedo)
    } else {
      this
    }

  def redo: State =
    if (undoRedo.canRedo) {
      val (newDerivations, newUndoRedo) = undoRedo.redo(derivations)
      copy(derivations = newDerivations, undoRedo = newUndoRedo)
    } else {
      this
    }

  def newFormulaIsValid: Boolean =
    FormulaParser.tryParseFormula(newFormulaText).isRight ||
      FormulaParser.tryParseSequent(newFormulaText).isRight

  def acceptNewFormulaAsNewDerivation: State = {
    val newDerivation: Derivation =
      FormulaParser.tryParseSequent(newFormulaText) match {
        case Right(sequent) => LJTTheoremProver.prove(sequent) getOrElse sequent.conclusion.axiom
        case Left(_) => Axiom(FormulaParser.parseFormula(newFormulaText))
      }
    acceptNewDerivation(newDerivation)
  }

  private def acceptNewDerivation(derivation: Derivation): State =
    withUndo(copy(derivations = derivations :+ derivation, newFormulaText = ""))

  // Manipulate derivation

  def getDerivation(i: DerivationIndex): Derivation = derivations(i)

  def setDerivation(i: DerivationIndex, derivation: Derivation): State =
    copy(derivations = derivations.patch(i, Seq(derivation), 1))

  def transformDerivation(i: DerivationIndex, f: Derivation => Derivation): State =
    withUndo(setDerivation(i, f(getDerivation(i))))

  def conjunctionIntroBackwards(derivationIndex: DerivationIndex, path: DerivationPath): State =
    transformDerivation(derivationIndex, _.transform(path, _.conjunctionIntroBackwards))

  def implicationIntroBackwards(derivationIndex: DerivationIndex, path: DerivationPath): State =
    transformDerivation(derivationIndex, implicationIntroBackwards(path))

  private def implicationIntroBackwards(path: DerivationPath)(derivation: Derivation): Derivation =
    derivation.transform(path, _.implicationIntroBackwards(derivation.nextFreshLabel))

  def equivalenceIntroBackwards(derivationIndex: DerivationIndex, path: DerivationPath): State =
    transformDerivation(derivationIndex, _.transform(path, _.equivalenceIntroBackwards))

  def equivalenceElimBackwards(derivationIndex: DerivationIndex, path: DerivationPath, direction: EquivalenceDirection): State =
    transformDerivation(derivationIndex, _.transform(path, _.equivalenceElimBackwards(direction)))

  def negationIntroBackwards(derivationIndex: DerivationIndex, path: DerivationPath): State =
    transformDerivation(derivationIndex, negationIntroBackwards(path))

  private def negationIntroBackwards(path: DerivationPath)(derivation: Derivation): Derivation =
    derivation.transform(path, _.negationIntroBackwards(derivation.nextFreshLabel))

  def reductioBackwards(derivationIndex: DerivationIndex, path: DerivationPath): State =
    transformDerivation(derivationIndex, reductioBackwards(path))

  private def reductioBackwards(path: DerivationPath)(derivation: Derivation): Derivation =
    derivation.transform(path, _.reductioBackwards(derivation.nextFreshLabel))

  def disjunctionIntroBackwards(derivationIndex: DerivationIndex, path: DerivationPath, childIndex: ChildIndex): State =
    transformDerivation(derivationIndex, _.transform(path, _.disjunctionIntroBackwards(childIndex)))

  def conjunctionElimForwards(derivationIndex: DerivationIndex, child: ChildIndex): State =
    transformDerivation(derivationIndex, _.conjunctionElimForwards(child))

  def implicationElimForwardsFromImplication(derivationIndex: DerivationIndex): State =
    transformDerivation(derivationIndex, _.implicationElimForwardsFromImplication)

  def inlineDerivation(derivationIndex: DerivationIndex, path: DerivationPath, derivationIndexToInline: DerivationIndex): State =
    transformDerivation(derivationIndex, _.set(path, derivations(derivationIndexToInline)))

  // Modal

  def confirmModal: State = modalState.map(_.complete(this)).getOrElse(this).closeModal

  private def closeModal: State = copy(modalState = None)

  def updateModalState(f: ModalState => ModalState): State = copy(modalState = modalState map f)

  def swapConjuncts: State = updateModalState(_.swapConjuncts)

  def swapDisjuncts: State = updateModalState(_.swapDisjuncts)

  def withModalFormula(newValue: String): State = updateModalState(_.withFormulaText(newValue))

  def showConjunctionElimBackwardsModal(derivationIndex: DerivationIndex, path: DerivationPath): State = {
    val conclusion = getDerivationConclusion(derivationIndex, path)
    copy(modalState = Some(ConjunctionElimBackwardsModalState(derivationIndex, path, conclusion)))
  }

  def showConjunctionIntroForwardsModal(derivationIndex: DerivationIndex): State = {
    val conclusion = getDerivationConclusion(derivationIndex)
    copy(modalState = Some(ConjunctionIntroForwardsModalState(derivationIndex, conclusion)))
  }

  def showNegationElimBackwardsModal(derivationIndex: DerivationIndex, path: DerivationPath): State = {
    copy(modalState = Some(NegationElimBackwardsModalState(derivationIndex, path)))
  }

  def showImplicationElimBackwardsModal(derivationIndex: DerivationIndex, path: DerivationPath): State = {
    val consequent = getDerivationConclusion(derivationIndex, path)
    copy(modalState = Some(ImplicationElimBackwardsModalState(derivationIndex, path, consequent)))
  }

  def showImplicationIntroForwardsModal(derivationIndex: DerivationIndex): State = {
    val conclusion = getDerivationConclusion(derivationIndex)
    copy(modalState = Some(ImplicationIntroForwardsModalState(derivationIndex, conclusion)))
  }

  def showImplicationElimForwardsFromAntecedentModal(derivationIndex: DerivationIndex): State = {
    val antecedent = getDerivationConclusion(derivationIndex)
    copy(modalState = Some(ImplicationElimForwardsFromAntecedentModalState(derivationIndex, antecedent)))
  }

  def showNegationIntroForwardsModal(derivationIndex: DerivationIndex): State =
    copy(modalState = Some(NegationIntroForwardsModalState(derivationIndex)))

  def showReductioForwardsModal(derivationIndex: DerivationIndex): State =
    copy(modalState = Some(ReductioForwardsModalState(derivationIndex)))

  def showDisjunctionIntroForwardsModal(derivationIndex: DerivationIndex): State = {
    val existingDisjunct = getDerivationConclusion(derivationIndex)
    copy(modalState = Some(DisjunctionIntroForwardsModalState(derivationIndex, existingDisjunct)))
  }

  def showDisjunctionElimForwardsFromDisjunctionModal(derivationIndex: DerivationIndex): State = {
    val disjunction = getDerivationConclusion(derivationIndex).asInstanceOf[Disjunction]
    copy(modalState = Some(DisjunctionElimForwardsFromDisjunctionModalState(derivationIndex, disjunction)))
  }

  private def getDerivationConclusion(derivationIndex: DerivationIndex, path: DerivationPath = DerivationPath.empty): Formula =
    getDerivation(derivationIndex).get(path).conclusion

}
