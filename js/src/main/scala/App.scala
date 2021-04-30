import ExampleDerivations._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation._
import naturalDeduction.Formula.{Conjunction, Implication, PropositionalVariable}
import naturalDeduction.parser.FormulaParser
import naturalDeduction.{Derivation, DerivationComponent, DerivationPath, DerivationProps, Formula, ManipulationInfo}

import scala.scalajs.js.Dynamic.global

sealed trait ModalState {
  def withModalFormula(newValue: String): ModalState

  def title: String

  def withConjunctToPick(conjunctToPick: Int): ModalState = this

  def swapConjuncts: ModalState = this

  def canComplete: Boolean

}

case class ConjunctionElimBackwardsModalState(derivationIndex: Int,
                                              path: DerivationPath,
                                              conclusion: Formula,
                                              conjunctToPick: Int,
                                              formulaText: String = "") extends ModalState {
  def title: String = "∧-Elimination Backwards"

  def withModalFormula(newText: String): ConjunctionElimBackwardsModalState = copy(formulaText = newText)

  override def withConjunctToPick(conjunctToPick: Int): ModalState = copy(conjunctToPick = conjunctToPick)

  override def swapConjuncts: ModalState = copy(conjunctToPick = if (conjunctToPick == 0) 1 else 0)

  override def canComplete: Boolean = FormulaParser.tryParseFormula(formulaText).isRight
}

case class UndoRedo[T](undoStack: List[T] = List.empty, redoStack: List[T] = List.empty) {
  def canUndo: Boolean = undoStack.nonEmpty

  def canRedo: Boolean = redoStack.nonEmpty

  def push(state: T): UndoRedo[T] = copy(undoStack = state :: undoStack, redoStack = List.empty)

  def undo(currentState: T): (T, UndoRedo[T]) = {
    val previousState :: restOfUndoStack = undoStack
    (previousState, UndoRedo(restOfUndoStack, currentState :: redoStack))
  }

  def redo(currentState: T): (T, UndoRedo[T]) = {
    val previousState :: restOfRedoStack = redoStack
    (previousState, UndoRedo(currentState :: undoStack, restOfRedoStack))
  }

}

object App {

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

    def updateConjunctToPick(conjunctToPick: Int): State = updateModalState(_.withConjunctToPick(conjunctToPick))

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
      copy(modalState = Some(ConjunctionElimBackwardsModalState(derivationIndex, path, conclusion, 0, "")))
    }

  }

  class Backend($: BackendScope[Unit, State]) {

    private def onConfirmModal: Callback =
      Callback {
        global.$("#interactionModal").modal("hide")
      } >>
        $.modState { oldState =>
          oldState.modalState match {
            case Some(ConjunctionElimBackwardsModalState(derivationIndex, path, conclusion, conjunctToPick, newFormulaText)) =>
              val newFormula = FormulaParser.parseFormula(newFormulaText)
              val newDerivation: Derivation = conjunctionEliminationDerivation(conclusion, newFormula, conjunctToPick)
              oldState.transformDerivation(derivationIndex, _.set(path, newDerivation)).closeModal
            case _ => oldState
          }
        }

    private def conjunctionEliminationDerivation(conjunct1: Formula, conjunct2: Formula, conjunctToPick: Int): Derivation =
      conjunctToPick match {
        case 0 => LeftConjunctionElimination(Axiom(Conjunction(conjunct1, conjunct2)))
        case 1 => RightConjunctionElimination(Axiom(Conjunction(conjunct2, conjunct1)))
      }

    private def onSwapConjuncts: Callback =
      $.modState(_.swapConjuncts)

    private def onChangeModalFormula(e: ReactEventFromInput): Callback = {
      val newValue = e.target.value
      $.modState(_.withModalFormula(newValue))
    }

    private def onChangeNewFormula(e: ReactEventFromInput): Callback = {
      val newValue = e.target.value
      $.modState(_.copy(newFormulaText = newValue))
    }

    private def handleSubmitNewFormula(e: ReactEventFromInput) =
      e.preventDefaultCB >> $.modState(_.acceptNewFormulaAsNewDerivation)

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
          <.p("An implementation of natural deduction proofs as described in ", <.em("Mathematical Logic"), " by Ian Chiswell and Wilfrid Hodges."),
          <.div(^.`class` := "btn-group", ^.role := "group",
            <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", <.i(^.className := "fas fa-undo"), ^.onClick --> onUndoClicked, ^.disabled := !state.undoRedo.canUndo),
            <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", <.i(^.className := "fas fa-redo"), ^.onClick --> onRedoClicked, ^.disabled := !state.undoRedo.canRedo),
          ),
          <.p(),
          state.derivations.zipWithIndex.map { case (derivation, index) => derivationCard(derivation, index, state.formulaToDerivationIndices) }.mkTagMod(<.br()),
          <.br(),
          <.form(^.`class` := "form-row align-items-center",
            ^.onSubmit ==> handleSubmitNewFormula,
            <.div(^.`class` := "col-auto",
              <.input(^.`class` := "form-control mb-2", ^.`type` := "text", ^.placeholder := "Add formula...", ^.onChange ==> onChangeNewFormula, ^.value := state.newFormulaText)),
            <.div(^.`class` := "col-auto",
              <.button(^.`type` := "submit", ^.`class` := "btn btn-secondary mb-2", "Start New Derivation", ^.disabled := !state.newFormulaIsValid),
            ),
          ),
        )
      )

    private def onRemoveDerivation(derivationIndex: Int)(path: DerivationPath): Callback =
      $.modState(_.transformDerivation(derivationIndex, _.transform(path, convertToAxiom)))

    private def convertToAxiom(derivation: Derivation): Axiom = Axiom(derivation.formula)

    private def onConjunctionIntroBackwards(derivationIndex: Int)(path: DerivationPath): Callback =
      $.modState(_.transformDerivation(derivationIndex, _.transform(path, conjunctionIntroBackwards)))

    private def conjunctionIntroBackwards(derivation: Derivation): Derivation = {
      val conjunction = derivation.formula.asInstanceOf[Conjunction]
      ConjunctionIntroduction(Axiom(conjunction.conjunct1), Axiom(conjunction.conjunct2))
    }

    private def onImplicationIntroBackwards(derivationIndex: Int)(path: DerivationPath): Callback =
      $.modState(_.transformDerivation(derivationIndex, implicationIntroBackwards(path)))

    private def implicationIntroBackwards(path: DerivationPath)(derivation: Derivation): Derivation =
      derivation.transform(path, implicationIntroBackwards(derivation.nextFreshLabel))

    private def implicationIntroBackwards(nextFreshLabel: String)(derivation: Derivation): Derivation = {
      val implication = derivation.formula.asInstanceOf[Implication]
      ImplicationIntroduction(implication.antecedent, nextFreshLabel, Axiom(implication.consequent))
    }

    private def onConjunctionElimForwards(derivationIndex: Int)(path: DerivationPath, child: Int): Callback =
      $.modState(_.transformDerivation(derivationIndex, _.transform(path, conjunctionElimForwards(child))))

    private def onUndoClicked: Callback =
      $.modState(_.undo)

    private def onRedoClicked: Callback =
      $.modState(_.redo)

    private def conjunctionElimForwards(child: Int)(derivation: Derivation): Derivation =
      child match {
        case 0 => LeftConjunctionElimination(derivation)
        case 1 => RightConjunctionElimination(derivation)
      }


    private def onInlineDerivation(derivationIndex: Int)(path: DerivationPath, derivationIndexToInline: Int): Callback =
      $.modState(oldState =>
        oldState.transformDerivation(derivationIndex, _.set(path, oldState.derivations(derivationIndexToInline)))
      )

    private def onConjunctionElimBackwards(derivationIndex: Int)(path: DerivationPath): Callback =
      $.modState(_.showConjunctionElimBackwardsModalState(derivationIndex, path)) >>
        Callback {
          global.$("#interactionModal").modal()
        }

    private def derivationCard(derivation: Derivation, derivationIndex: Int, formulaToDerivationIndices: Map[Formula, Seq[Int]]): VdomNode =
      <.div(^.`class` := "card",
        <.div(^.`class` := "card-header",
          s"${derivationIndex + 1}. ${derivation.sequent}",
          <.div(^.`class` := "btn-group float-right", ^.role := "group",
            <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", <.i(^.className := "fas fa-clone"), ^.onClick --> onDuplicateDerivation(derivationIndex)),
            <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", <.i(^.className := "fas fa-trash"), ^.onClick --> onDeleteDerivation(derivationIndex)),
          ),
        ),

        <.div(^.`class` := "card-body",
          DerivationComponent.component(
            DerivationProps(derivation,
              Some(ManipulationInfo(
                onRemoveDerivation(derivationIndex),
                onConjunctionIntroBackwards(derivationIndex),
                onConjunctionElimForwards(derivationIndex),
                onConjunctionElimBackwards(derivationIndex),
                onImplicationIntroBackwards(derivationIndex),
                onInlineDerivation(derivationIndex),
                derivationIndex,
                formulaToDerivationIndices,
              ))))
        )
      )

    private def onDeleteDerivation(derivationIndex: Int): Callback =
      $.modState(_.deleteDerivation(derivationIndex))

    private def onDuplicateDerivation(derivationIndex: Int): Callback =
      $.modState(_.duplicateDerivation(derivationIndex))

  }

  val app = ScalaComponent.builder[Unit]("App")
    //    .initialState(State(derivations = Seq(φ.axiom conjunctionIntro ψ.axiom)))
    //        .initialState(State(derivations = Seq(derivation1, derivation2, derivation3)))
    .initialState(State(derivations = Seq.empty))
    .renderBackend[Backend]
    .build

}
