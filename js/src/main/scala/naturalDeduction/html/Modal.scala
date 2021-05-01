package naturalDeduction.html

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation.ImplicationElimination
import naturalDeduction.Formula.PropositionalVariable
import naturalDeduction.html
import naturalDeduction.html.ConjunctionElimBackwardsModalState.conjunctionEliminationDerivation
import naturalDeduction.html.ReactUtils.getTargetValueThen
import naturalDeduction.parser.FormulaParser

case class ModalProps(
                       modalState: Option[ModalState],
                       onChangeModalFormula: String => Callback,
                       onSwapConjuncts: Callback,
                       onConfirmModal: Callback,
                     )

object Modal {

  val component = ScalaComponent.builder[ModalProps]
    .render_P(render)
    .build

  private def render(props: ModalProps): VdomNode = {
    import props._
    val onChangeModalFormula = getTargetValueThen(props.onChangeModalFormula)
    <.div(^.className := "modal fade", ^.id := "interactionModal", ^.tabIndex := -1, ^.role := "dialog", VdomAttr("aria-labelledby") := "interactionModalLabel", VdomAttr("aria-hidden") := "true",
      <.div(^.className := "modal-dialog", ^.role := "document",
        <.div(^.className := "modal-content",
          <.div(^.className := "modal-header",
            <.h5(^.className := "modal-title", ^.id := "interactionModalLabel", modalState.map(_.title).getOrElse("<>").asInstanceOf[String]),
            <.button(^.`type` := "button", ^.className := "close", VdomAttr("data-dismiss") := "modal", VdomAttr("aria-label") := "Close",
              <.span(VdomAttr("aria-hidden") := "true", "×")
            )
          ),
          <.div(^.className := "modal-body", modalState.map {
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
            <.button(^.`type` := "button", ^.className := "btn btn-primary", "Apply", ^.disabled := !modalState.exists(_.canComplete), ^.onClick --> onConfirmModal)
          )
        )
      )
    )
  }

}
