package naturalDeduction.html

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation.ImplicationElimination
import naturalDeduction.Formula.PropositionalVariable
import naturalDeduction.html.ReactUtils.getTargetValueThen
import naturalDeduction.parser.FormulaParser

object ImplicationElimBackwardsModalBody {

  case class Props(
                    modalState: ImplicationElimBackwardsModalState,
                    onChangeModalFormula: String => Callback,
                  )

  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  private def render(props: Props): VdomNode = {
    import props._
    val ImplicationElimBackwardsModalState(_, _, consequent, formulaText) = modalState
    val onChangeModalFormula = getTargetValueThen(props.onChangeModalFormula)
    val newFormulaOpt = FormulaParser.tryParseFormula(formulaText).toOption
    val newFormula = newFormulaOpt.getOrElse(PropositionalVariable("?"))
    val derivation = ImplicationElimination(newFormula, consequent)
    <.div(
      <.div(^.`class` := "d-flex justify-content-center",
        DerivationComponent.component(DerivationProps(derivation))
      ),
      <.br(),
      <.div(^.className := "form-row align-items-center",
        <.div(^.className := "col-12",
          <.label(^.className := "sr-only", ^.`for` := "inlineFormInput", "Name"),
          <.input(^.`class` := "form-control mb-2", ^.`type` := "text", ^.placeholder := "Antecedent...", ^.onChange ==> onChangeModalFormula, ^.value := formulaText),
        ),
      ),
    )
  }

}
