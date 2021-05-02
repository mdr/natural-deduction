package naturalDeduction.html

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation._
import naturalDeduction.Formula.{Conjunction, Implication}
import naturalDeduction._
import naturalDeduction.html.modal.{Modal, ModalProps}

import scala.scalajs.js.Dynamic.global

object App {

  class Backend($: BackendScope[Unit, State]) {

    def render(state: State): VdomTag =
      <.div(^.`class` := "container-fluid p-0",
        <.div(^.className := "d-flex flex-column flex-md-row align-items-center py-3 mb-3 bg-white border-bottom box-shadow",
          <.div(^.`class` := "container",
            <.div(^.className := "d-flex flex-column flex-md-row align-items-center ",
              <.h5(^.className := "my-0 mr-md-auto font-weight-normal", "Natural Deduction"),
            ),
          ),
        ),
        Modal.component(ModalProps(state.modalState, onChangeModalFormula, onSwapConjuncts, onConfirmModal)),
        <.div(^.`class` := "container",
          <.p(),
          MainButtonBar.component(MainButtonBarProps(state.undoRedo, onUndoClicked, onRedoClicked)),
          <.p(),
          Help.component(),
          <.p().when(state.derivations.nonEmpty),
          state.derivations
            .zipWithIndex.map { case (derivation, index) => derivationCard(derivation, index, state.formulaToDerivationIndices) }
            .mkTagMod(<.br()),
          <.br().when(state.derivations.nonEmpty),
          <.form(
            ^.`class` := "form-row align-items-center",
            ^.onSubmit ==> handleStartNewDerivation,
            <.div(
              ^.`class` := "col-auto",
              <.input(
                ^.`class` := "form-control mb-2",
                ^.`type` := "text",
                ^.placeholder := "Formula...",
                ^.onChange ==> onChangeNewDerivationFormula, ^.value := state.newFormulaText)
            ),
            <.div(
              ^.`class` := "col-auto",
              <.button(
                ^.`class` := "btn btn-secondary mb-2",
                ^.`type` := "submit",
                "Start New Derivation",
                ^.disabled := !state.newFormulaIsValid
              ),
            ),
          ),
        )
      )

    // modState helper to handle URL hash sync

    final def modState(mod: State => State): Callback =
      $.modState(mod) >> syncUrlHash

    private def syncUrlHash: Callback =
      $.modState(state => {
        UrlHashSync.writeToHash(state.derivations)
        state
      })

    // Modal handlers

    private def onChangeModalFormula(newFormula: String): Callback =
      modState(_.withModalFormula(newFormula))

    private def onConfirmModal: Callback =
      Callback {
        global.$("#interactionModal").modal("hide")
      } >>
        modState(oldState =>
          oldState.modalState.map(_.complete(oldState)).getOrElse(oldState).closeModal
        )

    private val showModal: Callback = Callback {
      global.$("#interactionModal").modal()
    }

    private def onSwapConjuncts: Callback = modState(_.swapConjuncts)

    // App button handlers
    private def onUndoClicked: Callback = modState(_.undo)

    private def onRedoClicked: Callback = modState(_.redo)

    // New derivation handlers

    private def onChangeNewDerivationFormula(e: ReactEventFromInput): Callback = {
      val newValue = e.target.value
      modState(_.copy(newFormulaText = newValue))
    }

    private def handleStartNewDerivation(e: ReactEventFromInput): Callback =
      e.preventDefaultCB >> modState(_.acceptNewFormulaAsNewDerivation)

    // Derivation card button handlers

    private def onDeleteDerivation(derivationIndex: DerivationIndex): Callback =
      modState(_.deleteDerivation(derivationIndex))

    private def onDuplicateDerivation(derivationIndex: DerivationIndex): Callback =
      modState(_.duplicateDerivation(derivationIndex))

    private def onExtractSubderivation(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.extractSubderivation(derivationIndex, path))

    // Derivation menu handlers

    private def onRemoveDerivation(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, _.convertToAxiom)))

    private def onConjunctionIntroBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, conjunctionIntroBackwards)))

    private def conjunctionIntroBackwards(derivation: Derivation): Derivation = {
      val conjunction = derivation.formula.asInstanceOf[Conjunction]
      ConjunctionIntroduction(Axiom(conjunction.conjunct1), Axiom(conjunction.conjunct2))
    }

    private def onImplicationIntroBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, implicationIntroBackwards(path)))

    private def implicationIntroBackwards(path: DerivationPath)(derivation: Derivation): Derivation =
      derivation.transform(path, implicationIntroBackwards(derivation.nextFreshLabel))

    private def implicationIntroBackwards(nextFreshLabel: Label)(derivation: Derivation): Derivation = {
      val implication = derivation.formula.asInstanceOf[Implication]
      ImplicationIntroduction(implication.antecedent, nextFreshLabel, Axiom(implication.consequent))
    }

    private def onConjunctionElimForwards(derivationIndex: DerivationIndex)(child: ChildIndex): Callback =
      modState(_.transformDerivation(derivationIndex, conjunctionElimForwards(child)))

    private def conjunctionElimForwards(child: ChildIndex)(derivation: Derivation): Derivation =
      child match {
        case 0 => LeftConjunctionElimination(derivation)
        case 1 => RightConjunctionElimination(derivation)
      }

    private def onImplicationElimForwardsFromImplication(derivationIndex: DerivationIndex): Callback =
      modState(_.transformDerivation(derivationIndex, implicationElimForwardsFromImplication))

    private def implicationElimForwardsFromImplication(derivation: Derivation): ImplicationElimination = {
      val implication = derivation.formula.asInstanceOf[Implication]
      ImplicationElimination(Axiom(implication.antecedent), derivation)
    }

    private def onInlineDerivation(derivationIndex: DerivationIndex)(path: DerivationPath, derivationIndexToInline: DerivationIndex): Callback =
      modState(oldState =>
        oldState.transformDerivation(derivationIndex, _.set(path, oldState.derivations(derivationIndexToInline)))
      )

    private def onBetaReduce(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, _.betaReduce.get)))

    private def onDischargeAssumption(derivationIndex: DerivationIndex)(path: DerivationPath, label: Label): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, _.dischargeAxiom(label))))

    private def onUndischargeAssumption(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, _.undischargeAxiom)))

    private def onConjunctionElimBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.showConjunctionElimBackwardsModal(derivationIndex, path)) >> showModal

    private def onConjunctionIntroForwards(derivationIndex: DerivationIndex): Callback =
      modState(_.showConjunctionIntroForwardsModal(derivationIndex)) >> showModal

    private def onImplicationElimBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.showImplicationElimBackwardsModal(derivationIndex, path)) >> showModal

    private def onImplicationElimForwardsFromAntecedent(derivationIndex: DerivationIndex): Callback =
      modState(_.showImplicationElimForwardsFromAntecedentModal(derivationIndex)) >> showModal

    private def onImplicationIntroForwards(derivationIndex: DerivationIndex): Callback =
      modState(_.showImplicationIntroForwardsModal(derivationIndex)) >> showModal

    private def derivationCard(derivation: Derivation,
                               derivationIndex: DerivationIndex,
                               formulaToDerivationIndices: Map[Formula, Seq[DerivationIndex]]): VdomNode =
      <.div(^.`class` := "card", ^.key := derivationIndex.toString,
        <.div(^.`class` := "card-header",
          s"${derivationIndex + 1}. ${derivation.sequent}",
          CardButtonBar.component(
            CardButtonBarProps(
              onDuplicateDerivation = onDuplicateDerivation(derivationIndex),
              onDeleteDerivation = onDeleteDerivation(derivationIndex))),
        ),
        <.div(^.`class` := "card-body",
          DerivationComponent.component(
            DerivationProps(derivation,
              Some(ManipulationInfo(
                onRemoveDerivation(derivationIndex),
                onConjunctionIntroBackwards(derivationIndex),
                onConjunctionElimForwards(derivationIndex),
                onConjunctionElimBackwards(derivationIndex),
                onConjunctionIntroForwards(derivationIndex),
                onImplicationIntroBackwards(derivationIndex),
                onImplicationIntroForwards(derivationIndex),
                onImplicationElimBackwards(derivationIndex),
                onImplicationElimForwardsFromAntecedent(derivationIndex),
                onImplicationElimForwardsFromImplication(derivationIndex),
                onInlineDerivation(derivationIndex),
                onDischargeAssumption(derivationIndex),
                onUndischargeAssumption(derivationIndex),
                onBetaReduce(derivationIndex),
                onExtractSubderivation(derivationIndex),
                derivationIndex,
                formulaToDerivationIndices,
              ))))
        )
      )

  }

  val app = ScalaComponent.builder[Unit]("App")
    .initialState(State(derivations = UrlHashSync.readFromHash))
    .renderBackend[Backend]
    .build

}
