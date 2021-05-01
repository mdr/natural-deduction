package naturalDeduction.html.modal

import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.Formula.PropositionalVariable
import naturalDeduction.html.ReactUtils.getTargetValueThen
import naturalDeduction.html.modal.ConjunctionElimBackwardsModalState.conjunctionEliminationDerivation
import naturalDeduction.html.{DerivationComponent, DerivationProps}
import naturalDeduction.parser.FormulaParser

object ConjunctionElimBackwardsModalBody {

  case class Props(
                    modalState: ConjunctionElimBackwardsModalState,
                    onChangeModalFormula: String => Callback,
                    onSwapConjuncts: Callback,
                  )


  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  private def render(props: Props): VdomNode = {
    import props._
    val onChangeModalFormula = getTargetValueThen(props.onChangeModalFormula)
    val ConjunctionElimBackwardsModalState(_, _, conclusion, conjunctToPick, formulaText) = modalState
    val newFormula = FormulaParser.tryParseFormula(formulaText).toOption.getOrElse(PropositionalVariable("?"))
    val derivation = conjunctionEliminationDerivation(conclusion, newFormula, conjunctToPick)
    <.div(
      <.div(^.`class` := "d-flex justify-content-center",
        DerivationComponent.component(DerivationProps(derivation))
      ),
      <.br(),
      <.div(^.className := "form-row align-items-center",
        <.div(^.className := "col-9",
          <.label(^.className := "sr-only", ^.`for` := "inlineFormInput", "Name"),
          <.input(
            ^.`class` := "form-control mb-2",
            ^.`type` := "text",
            ^.placeholder := "Other conjunct...",
            ^.onChange ==> onChangeModalFormula,
            ^.value := formulaText
          ),
        ),
        <.div(^.className := "col",
          <.button(^.`class` := "btn btn-outline-secondary mb-2", ^.`type` := "button", ^.onClick --> onSwapConjuncts,
            <.span(<.i(^.className := "fas fa-exchange-alt"), " Swap"),
          ),
        )
      ),
    )
  }

}
