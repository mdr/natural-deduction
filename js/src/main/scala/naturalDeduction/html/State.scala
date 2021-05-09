package naturalDeduction.html

import ljt.LJTTheoremProver
import naturalDeduction.Derivation.RichFormula
import naturalDeduction.Formula.Disjunction
import naturalDeduction.html.modal._
import naturalDeduction.parser.FormulaParser
import naturalDeduction._

case class State(
                  newFormulaText: String = "",
                  derivationSections: Seq[DerivationSection] = Seq.empty,
                  modalState: Option[ModalState] = None,
                  undoRedo: UndoRedo[Seq[DerivationSection]] = UndoRedo()
                ) {

  lazy val formulaToDerivationIndices: Map[Formula, Seq[DerivationIndex]] =
    derivationSections.zipWithIndex.groupMap(_._1.conclusion)(_._2)

  def autoProve(derivationIndex: DerivationIndex): State =
    withUndo(
      transformDerivationSection(derivationIndex, autoProve))

  private def autoProve(section: DerivationSection): DerivationSection = {
    val newDerivation =
      for {
        goal <- section.goal
        derivation <- LJTTheoremProver.prove(goal)
      } yield derivation
    section.withDerivation(newDerivation getOrElse section.derivation)
  }

  def deleteDerivation(derivationIndex: DerivationIndex): State =
    withUndo(
      copy(
        derivationSections = derivationSections.patch(derivationIndex, Seq.empty, 1)))

  def duplicateDerivation(derivationIndex: DerivationIndex): State = {
    val section = derivationSections(derivationIndex)
    withUndo(
      copy(
        derivationSections = derivationSections.patch(derivationIndex, Seq(section, section), 1)))
  }

  def extractSubderivation(derivationIndex: DerivationIndex, path: DerivationPath): State = {
    val section = derivationSections(derivationIndex)
    val derivation = section.derivation
    val subDerivation = derivation.get(path)
    val newSection = DerivationSection(subDerivation)
    withUndo(
      copy(
        derivationSections = derivationSections.patch(derivationIndex, Seq(section, newSection), 1)))
  }

  def withUndo(newState: State): State = newState.copy(undoRedo = undoRedo.push(derivationSections))

  def undo: State =
    if (undoRedo.canUndo) {
      val (newDerivationSections, newUndoRedo) = undoRedo.undo(derivationSections)
      copy(derivationSections = newDerivationSections, undoRedo = newUndoRedo)
    } else {
      this
    }

  def redo: State =
    if (undoRedo.canRedo) {
      val (newDerivationSections, newUndoRedo) = undoRedo.redo(derivationSections)
      copy(derivationSections = newDerivationSections, undoRedo = newUndoRedo)
    } else {
      this
    }

  def newFormulaIsValid: Boolean =
    FormulaParser.tryParseFormula(newFormulaText).isRight ||
      FormulaParser.tryParseSequent(newFormulaText).isRight

  def acceptNewFormulaAsNewDerivation: State = {
    val newDerivation: DerivationSection =
      FormulaParser.tryParseSequent(newFormulaText) match {
        case Right(sequent) =>
          LJTTheoremProver.prove(sequent) getOrElse sequent.conclusion.axiom
          DerivationSection(sequent.conclusion.axiom, Some(sequent))
        case Left(_) =>
          val formula = FormulaParser.parseFormula(newFormulaText)
          val derivation = formula.axiom
          DerivationSection(derivation)
      }
    acceptNewDerivation(newDerivation)
  }

  private def acceptNewDerivation(section: DerivationSection): State =
    withUndo(addDerivationSection(section).copy(newFormulaText = ""))

  // Manipulate derivation

  def getDerivationSection(i: DerivationIndex): DerivationSection = derivationSections(i)

  def setDerivationSection(i: DerivationIndex, section: DerivationSection): State =
    copy(derivationSections = derivationSections.patch(i, Seq(section), 1))

  def transformDerivationSection(i: DerivationIndex, f: DerivationSection => DerivationSection): State =
    withUndo(setDerivationSection(i, f(getDerivationSection(i))))

  def addDerivationSection(section: DerivationSection): State =
    copy(derivationSections = derivationSections :+ section)

  def getDerivation(i: DerivationIndex): Derivation = derivationSections(i).derivation

  def transformDerivation(i: DerivationIndex, f: Derivation => Derivation): State =
    transformDerivationSection(i, _.transformDerivation(f))

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
    transformDerivation(derivationIndex, _.set(path, derivationSections(derivationIndexToInline).derivation))

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
