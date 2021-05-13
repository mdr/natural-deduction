package naturalDeduction.html

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction._
import naturalDeduction.html.modal.Modal
import naturalDeduction.pretty.FormulaPrettyPrinter
import signatures.Mousetrap

import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.JSConverters._

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
        Modal.Props(state.modalState, onChangeModalFormula, onChangeModalFormula2, onSwapConjuncts, onSwapDisjuncts, onConfirmModal).make,
        <.div(^.`class` := "container",
          <.p(),
          MainButtonBar.Props(state.undoRedo, onUndo, onRedo, onToggleParens).make,
          <.p(),
          Help.component(),
          <.p().when(state.derivationSections.nonEmpty),
          state.derivationSections
            .zipWithIndex.map { case (section, index) => makeDerivationCard(state, section, index) }
            .mkTagMod(<.br()),
          <.br().when(state.derivationSections.nonEmpty),
          <.form(
            ^.`class` := "form-row align-items-center",
            ^.onSubmit ==> onStartNewDerivation,
            <.div(
              ^.`class` := "col-6",
              <.input(
                ^.`class` := "form-control mb-2",
                ^.`type` := "text",
                ^.placeholder := "Formula or sequent...",
                ^.onChange ==> ReactUtils.getTargetValueThen(onChangeNewDerivationFormula), ^.value := state.newFormulaText)
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

    private def makeDerivationCard(state: State, section: DerivationSection, index: DerivationIndex): VdomNode =
      DerivationCard.component(DerivationCard.Props(
        section,
        index,
        manipulationInfo(index, state.formulaToDerivationIndices),
        onAutoProve(index),
        onDuplicateDerivation(index),
        onDeleteDerivation(index)))

    /**
     * Modify state and handle URL hash sync
     */
    def modState(mod: State => State): Callback =
      $.modState(mod, syncUrlHash)

    private def syncUrlHash: Callback =
      $.state.map(state => UrlHashSync.writeToHash(state.derivationSections))

    // Modal handlers

    private def onChangeModalFormula(newFormula: String): Callback =
      modState(_.withModalFormula(newFormula))

    private def onChangeModalFormula2(newFormula: String): Callback =
      modState(_.withModalFormula2(newFormula))

    private def onConfirmModal: Callback = hideModal >> modState(_.confirmModal)

    private val showModal: Callback = Callback {
      global.$(s"#${Modal.Id}").modal()
    }

    private val hideModal: Callback = Callback {
      global.$(s"#${Modal.Id}").modal("hide")
    }

    private def onSwapConjuncts: Callback = modState(_.swapConjuncts)

    private def onSwapDisjuncts: Callback = modState(_.swapDisjuncts)

    // Undo/redo

    def onUndo: Callback = modState(_.undo)

    def onRedo: Callback = modState(_.redo)

    // New derivation handlers

    private def onChangeNewDerivationFormula(newFormulaText: String): Callback =
      modState(_.copy(newFormulaText = newFormulaText))

    private def onStartNewDerivation(e: ReactEventFromInput): Callback =
      e.preventDefaultCB >> modState(_.acceptNewFormulaAsNewDerivation)

    // Derivation card button handlers

    private def onAutoProve(derivationIndex: DerivationIndex): Callback =
      modState(_.autoProve(derivationIndex))

    private def onDeleteDerivation(derivationIndex: DerivationIndex): Callback =
      modState(_.deleteDerivation(derivationIndex))

    private def onDuplicateDerivation(derivationIndex: DerivationIndex): Callback =
      modState(_.duplicateDerivation(derivationIndex))

    // Derivation menu handlers

    private def onConjunctionIntroBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.conjunctionIntroBackwards(derivationIndex, path))

    private def onConjunctionElimBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.showConjunctionElimBackwardsModal(derivationIndex, path)) >> showModal

    private def onImplicationIntroBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.implicationIntroBackwards(derivationIndex, path))

    private def onImplicationElimBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.showImplicationElimBackwardsModal(derivationIndex, path)) >> showModal

    private def onEquivalenceIntroBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.equivalenceIntroBackwards(derivationIndex, path))

    private def onEquivalenceElimBackwards(derivationIndex: DerivationIndex)(path: DerivationPath, direction: EquivalenceDirection): Callback =
      modState(_.equivalenceElimBackwards(derivationIndex, path, direction))

    private def onNegationIntroBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.negationIntroBackwards(derivationIndex, path))

    private def onNegationElimBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.showNegationElimBackwardsModal(derivationIndex, path)) >> showModal

    private def onReductioBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.reductioBackwards(derivationIndex, path))

    private def onDisjunctionIntroBackwards(derivationIndex: DerivationIndex)(path: DerivationPath, childIndex: ChildIndex): Callback =
      modState(_.disjunctionIntroBackwards(derivationIndex, path, childIndex))

    private def onDisjunctionElimBackwards(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.showDisjunctionElimBackwardsModal(derivationIndex, path)) >> showModal

    private def onConjunctionIntroForwards(derivationIndex: DerivationIndex): Callback =
      modState(_.showConjunctionIntroForwardsModal(derivationIndex)) >> showModal

    private def onConjunctionElimForwards(derivationIndex: DerivationIndex)(child: ChildIndex): Callback =
      modState(_.conjunctionElimForwards(derivationIndex, child))

    private def onImplicationIntroForwards(derivationIndex: DerivationIndex): Callback =
      modState(_.showImplicationIntroForwardsModal(derivationIndex)) >> showModal

    private def onImplicationElimForwardsFromImplication(derivationIndex: DerivationIndex): Callback =
      modState(_.implicationElimForwardsFromImplication(derivationIndex))

    private def onImplicationElimForwardsFromAntecedent(derivationIndex: DerivationIndex): Callback =
      modState(_.showImplicationElimForwardsFromAntecedentModal(derivationIndex)) >> showModal

    private def onEquivalenceIntroForwards(derivationIndex: DerivationIndex)(direction: EquivalenceDirection): Callback =
      modState(_.transformDerivation(derivationIndex, _.equivalenceIntroForwards(direction)))

    private def onEquivalenceElimForwards(derivationIndex: DerivationIndex)(direction: EquivalenceDirection): Callback =
      modState(_.transformDerivation(derivationIndex, _.equivalenceElimForwards(direction)))

    private def onNegationIntroForwards(derivationIndex: DerivationIndex): Callback =
      modState(_.showNegationIntroForwardsModal(derivationIndex)) >> showModal

    private def onNegationElimForwardsFromPositive(derivationIndex: DerivationIndex): Callback =
      modState(_.transformDerivation(derivationIndex, _.negationElimForwardsFromPositive))

    private def onNegationElimForwardsFromNegative(derivationIndex: DerivationIndex): Callback =
      modState(_.transformDerivation(derivationIndex, _.negationElimForwardsFromNegative))

    private def onReductioForwards(derivationIndex: DerivationIndex): Callback =
      modState(_.showReductioForwardsModal(derivationIndex)) >> showModal

    private def onDisjunctionIntroForwards(derivationIndex: DerivationIndex): Callback =
      modState(_.showDisjunctionIntroForwardsModal(derivationIndex)) >> showModal

    private def onDisjunctionElimForwardsFromDisjunction(derivationIndex: DerivationIndex): Callback =
      modState(_.showDisjunctionElimForwardsFromDisjunctionModal(derivationIndex)) >> showModal

    private def onInlineDerivation(derivationIndex: DerivationIndex)(path: DerivationPath, derivationIndexToInline: DerivationIndex): Callback =
      modState(_.inlineDerivation(derivationIndex, path, derivationIndexToInline))

    private def onRemoveDerivation(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, _.convertToAxiom)))

    private def onBetaReduce(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, _.betaReduce.get)))

    private def onDischargeAssumption(derivationIndex: DerivationIndex)(path: DerivationPath, label: Label): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, _.dischargeAxiom(label))))

    private def onUndischargeAssumption(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, _.undischargeAxiom)))

    private def onExtractSubderivation(derivationIndex: DerivationIndex)(path: DerivationPath): Callback =
      modState(_.extractSubderivation(derivationIndex, path))

    private def manipulationInfo(derivationIndex: DerivationIndex,
                                 formulaToDerivationIndices: Map[Formula, Seq[DerivationIndex]]): ManipulationInfo =
      ManipulationInfo(
        onConjunctionIntroBackwards(derivationIndex),
        onConjunctionElimBackwards(derivationIndex),
        onImplicationIntroBackwards(derivationIndex),
        onImplicationElimBackwards(derivationIndex),
        onEquivalenceIntroBackwards(derivationIndex),
        onEquivalenceElimBackwards(derivationIndex),
        onNegationIntroBackwards(derivationIndex),
        onNegationElimBackwards(derivationIndex),
        onReductioBackwards(derivationIndex),
        onDisjunctionIntroBackwards(derivationIndex),
        onDisjunctionElimBackwards(derivationIndex),
        onConjunctionIntroForwards(derivationIndex),
        onConjunctionElimForwards(derivationIndex),
        onImplicationIntroForwards(derivationIndex),
        onImplicationElimForwardsFromAntecedent(derivationIndex),
        onImplicationElimForwardsFromImplication(derivationIndex),
        onEquivalenceIntroForwards(derivationIndex),
        onEquivalenceElimForwards(derivationIndex),
        onNegationIntroForwards(derivationIndex),
        onNegationElimForwardsFromPositive(derivationIndex),
        onNegationElimForwardsFromNegative(derivationIndex),
        onReductioForwards(derivationIndex),
        onDisjunctionIntroForwards(derivationIndex),
        onDisjunctionElimForwardsFromDisjunction(derivationIndex),
        onRemoveDerivation(derivationIndex),
        onInlineDerivation(derivationIndex),
        onDischargeAssumption(derivationIndex),
        onUndischargeAssumption(derivationIndex),
        onBetaReduce(derivationIndex),
        onExtractSubderivation(derivationIndex),
        derivationIndex,
        formulaToDerivationIndices)

    def componentDidMount: Callback = Callback {
      Mousetrap.bind(Seq("command+z", "ctrl+z").toJSArray, _ => onUndo.runNow())
      Mousetrap.bind(Seq("command+shift+z", "ctrl+y").toJSArray, _ => onRedo.runNow())
      Mousetrap.bind(Seq("command+b", "ctrl+b").toJSArray, _ => onToggleParens.runNow())
    }

    private def onToggleParens: Callback =
      Callback {
        FormulaPrettyPrinter.toggleIncludeAllParens()
      } >> $.forceUpdate

  }

  //noinspection TypeAnnotation
  val app = ScalaComponent.builder[Unit]("App")
    .initialState(State(derivationSections = UrlHashSync.readFromHash))
    .renderBackend[Backend]
    .componentDidMount(_.backend.componentDidMount)
    .build

}
