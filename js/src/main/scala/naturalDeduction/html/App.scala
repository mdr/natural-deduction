package naturalDeduction.html

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import naturalDeduction.Derivation._
import naturalDeduction.Formula.{Conjunction, Implication, PropositionalVariable}
import naturalDeduction.html.ConjunctionElimBackwardsModalState.conjunctionEliminationDerivation
import naturalDeduction.parser.FormulaParser
import naturalDeduction._

import scala.scalajs.js.Dynamic.global

object App {

  class Backend($: BackendScope[Unit, State]) {

    private def onConfirmModal: Callback =
      Callback {
        global.$("#interactionModal").modal("hide")
      } >>
        modState(oldState =>
          oldState.modalState.map(_.complete(oldState)).getOrElse(oldState).closeModal
        )

    private def onSwapConjuncts: Callback =
      modState(_.swapConjuncts)

    private def onChangeModalFormula(e: ReactEventFromInput): Callback = {
      val newValue = e.target.value
      modState(_.withModalFormula(newValue))
    }

    private def onChangeNewFormula(e: ReactEventFromInput): Callback = {
      val newValue = e.target.value
      modState(_.copy(newFormulaText = newValue))
    }

    private def handleSubmitNewFormula(e: ReactEventFromInput) =
      e.preventDefaultCB >> modState(_.acceptNewFormulaAsNewDerivation)

    def render(state: State): VdomTag =
      <.div(^.`class` := "container-fluid p-0",
        <.div(^.className := "d-flex flex-column flex-md-row align-items-center py-3 mb-3 bg-white border-bottom box-shadow",
          <.div(^.`class` := "container",
            <.div(^.className := "d-flex flex-column flex-md-row align-items-center ",
              <.h5(^.className := "my-0 mr-md-auto font-weight-normal", "Natural Deduction"),
            ),
          ),
        ),
        <.div(^.className := "modal fade", ^.id := "interactionModal", ^.tabIndex := -1, ^.role := "dialog", VdomAttr("aria-labelledby") := "interactionModalLabel", VdomAttr("aria-hidden") := "true",
          <.div(^.className := "modal-dialog", ^.role := "document",
            <.div(^.className := "modal-content",
              <.div(^.className := "modal-header",
                <.h5(^.className := "modal-title", ^.id := "interactionModalLabel", state.modalState.map(_.title).getOrElse("<>").asInstanceOf[String]),
                <.button(^.`type` := "button", ^.className := "close", VdomAttr("data-dismiss") := "modal", VdomAttr("aria-label") := "Close",
                  <.span(VdomAttr("aria-hidden") := "true", "×")
                )
              ),
              <.div(^.className := "modal-body", state.modalState.map {
                case ConjunctionElimBackwardsModalState(_, _, conclusion, conjunctToPick, formulaText) =>
                  val newFormulaOpt = FormulaParser.tryParseFormula(formulaText).toOption
                  val newFormula = newFormulaOpt.getOrElse(PropositionalVariable("?"))
                  val derivation = conjunctionEliminationDerivation(conclusion, newFormula, conjunctToPick)
                  <.div(
                    <.div(^.`class` := "d-flex justify-content-center",
                      DerivationComponent.component(DerivationProps(derivation))
                    ),
                    <.br(),
                    <.div(^.className := "form-row align-items-center",
                      <.div(^.className := "col-9",
                        <.label(^.className := "sr-only", ^.`for` := "inlineFormInput", "Name"),
                        <.input(^.`class` := "form-control mb-2", ^.`type` := "text", ^.placeholder := "Other conjunct...", ^.onChange ==> onChangeModalFormula, ^.value := formulaText),
                      ),
                      <.div(^.className := "col",
                        <.button(^.`class` := "btn btn-outline-secondary mb-2", ^.`type` := "button", ^.onClick --> onSwapConjuncts,
                          <.span(<.i(^.className := "fas fa-exchange-alt"), " Swap"),
                        ),
                      )
                    ),
                  )
                case ImplicationElimBackwardsModalState(_, _, consequent, formulaText) =>
                  val newFormulaOpt = FormulaParser.tryParseFormula(formulaText).toOption
                  val newFormula = newFormulaOpt.getOrElse(PropositionalVariable("?"))
                  val derivation = ImplicationElimination(newFormula, consequent)
                  <.div(
                    <.div(^.`class` := "d-flex justify-content-center",
                      DerivationComponent.component(html.DerivationProps(derivation))
                    ),
                    <.br(),
                    <.div(^.className := "form-row align-items-center",
                      <.div(^.className := "col-12",
                        <.label(^.className := "sr-only", ^.`for` := "inlineFormInput", "Name"),
                        <.input(^.`class` := "form-control mb-2", ^.`type` := "text", ^.placeholder := "Antecedent...", ^.onChange ==> onChangeModalFormula, ^.value := formulaText),
                      ),
                    ),
                  )
              }),
              <.div(^.className := "modal-footer",
                <.button(^.`type` := "button", ^.className := "btn btn-secondary", VdomAttr("data-dismiss") := "modal", "Close"),
                <.button(^.`type` := "button", ^.className := "btn btn-primary", "Apply", ^.disabled := !state.modalState.exists(_.canComplete), ^.onClick --> onConfirmModal)
              )
            )
          )
        ),
        <.div(^.`class` := "container",
          <.p(),
          MainButtonBar.component(MainButtonBarProps(state.undoRedo, onUndoClicked, onRedoClicked)),
          <.p(),
          Help.component(),
          <.p().when(state.derivations.nonEmpty),
          state.derivations.zipWithIndex.map { case (derivation, index) => derivationCard(derivation, index, state.formulaToDerivationIndices) }.mkTagMod(<.br()),
          <.br(),
          <.form(^.`class` := "form-row align-items-center",
            ^.onSubmit ==> handleSubmitNewFormula,
            <.div(^.`class` := "col-auto",
              <.input(^.`class` := "form-control mb-2", ^.`type` := "text", ^.placeholder := "Formula...", ^.onChange ==> onChangeNewFormula, ^.value := state.newFormulaText)),
            <.div(^.`class` := "col-auto",
              <.button(^.`type` := "submit", ^.`class` := "btn btn-secondary mb-2", "Start New Derivation", ^.disabled := !state.newFormulaIsValid),
            ),
          ),
        )
      )

    private def onDeleteDerivation(derivationIndex: Int): Callback =
      modState(_.deleteDerivation(derivationIndex))

    private def onDuplicateDerivation(derivationIndex: Int): Callback =
      modState(_.duplicateDerivation(derivationIndex))

    private def onRemoveDerivation(derivationIndex: Int)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, convertToAxiom)))

    private def convertToAxiom(derivation: Derivation): Axiom = Axiom(derivation.formula)

    private def onConjunctionIntroBackwards(derivationIndex: Int)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, conjunctionIntroBackwards)))

    private def conjunctionIntroBackwards(derivation: Derivation): Derivation = {
      val conjunction = derivation.formula.asInstanceOf[Conjunction]
      ConjunctionIntroduction(Axiom(conjunction.conjunct1), Axiom(conjunction.conjunct2))
    }

    private def onImplicationIntroBackwards(derivationIndex: Int)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, implicationIntroBackwards(path)))

    private def implicationIntroBackwards(path: DerivationPath)(derivation: Derivation): Derivation =
      derivation.transform(path, implicationIntroBackwards(derivation.nextFreshLabel))

    private def implicationIntroBackwards(nextFreshLabel: String)(derivation: Derivation): Derivation = {
      val implication = derivation.formula.asInstanceOf[Implication]
      ImplicationIntroduction(implication.antecedent, nextFreshLabel, Axiom(implication.consequent))
    }

    private def onConjunctionElimForwards(derivationIndex: Int)(path: DerivationPath, child: Int): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, conjunctionElimForwards(child))))

    private def onUndoClicked: Callback = modState(_.undo)

    private def onRedoClicked: Callback = modState(_.redo)

    private def syncUrlHash: Callback =
      $.modState(state => {
        UrlHashSync.writeToHash(state.derivations)
        state
      })

    final def modState(mod: State => State): Callback =
      $.modState(mod) >> syncUrlHash

    private def conjunctionElimForwards(child: Int)(derivation: Derivation): Derivation =
      child match {
        case 0 => LeftConjunctionElimination(derivation)
        case 1 => RightConjunctionElimination(derivation)
      }

    private def onInlineDerivation(derivationIndex: Int)(path: DerivationPath, derivationIndexToInline: Int): Callback =
      modState(oldState =>
        oldState.transformDerivation(derivationIndex, _.set(path, oldState.derivations(derivationIndexToInline)))
      )

    private def onDischargeAssumption(derivationIndex: Int)(path: DerivationPath, label: String): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, _.dischargeAxiom(label))))

    private def onUndischargeAssumption(derivationIndex: Int)(path: DerivationPath): Callback =
      modState(_.transformDerivation(derivationIndex, _.transform(path, _.undischargeAxiom)))

    private def onConjunctionElimBackwards(derivationIndex: Int)(path: DerivationPath): Callback =
      modState(_.showConjunctionElimBackwardsModalState(derivationIndex, path)) >>
        Callback {
          global.$("#interactionModal").modal()
        }

    private def onImplicationElimBackwards(derivationIndex: Int)(path: DerivationPath): Callback =
      modState(_.showImplicationElimBackwardsModalState(derivationIndex, path)) >>
        Callback {
          global.$("#interactionModal").modal()
        }

    private def derivationCard(derivation: Derivation, derivationIndex: Int, formulaToDerivationIndices: Map[Formula, Seq[Int]]): VdomNode =
      <.div(^.`class` := "card",
        <.div(^.`class` := "card-header",
          s"${derivationIndex + 1}. ${derivation.sequent}",
          CardButtonBar.component(
            CardButtonBarProps(
              onDuplicateDerivation = onDuplicateDerivation(derivationIndex),
              onDeleteDerivation = onDeleteDerivation(derivationIndex))),
        ),
        <.div(^.`class` := "card-body",
          DerivationComponent.component(
            html.DerivationProps(derivation,
              Some(ManipulationInfo(
                onRemoveDerivation(derivationIndex),
                onConjunctionIntroBackwards(derivationIndex),
                onConjunctionElimForwards(derivationIndex),
                onConjunctionElimBackwards(derivationIndex),
                onImplicationIntroBackwards(derivationIndex),
                onImplicationElimBackwards(derivationIndex),
                onInlineDerivation(derivationIndex),
                onDischargeAssumption(derivationIndex),
                onUndischargeAssumption(derivationIndex),
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
