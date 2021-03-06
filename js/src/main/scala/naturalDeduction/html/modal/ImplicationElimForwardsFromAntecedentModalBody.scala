package naturalDeduction.html.modal

import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.Derivation.ImplicationElimination
import naturalDeduction.html.DerivationComponent

object ImplicationElimForwardsFromAntecedentModalBody {

  case class Props(modalState: ImplicationElimForwardsFromAntecedentModalState,
                   onChangeModalFormula: String => Callback) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  private def render(props: Props): VdomNode = {
    import props._
    val ImplicationElimForwardsFromAntecedentModalState(_, antecedent, _) = modalState
    val consequent = modalState.formula
    val derivation = ImplicationElimination(antecedent, consequent)
    <.div(
      <.div(^.`class` := "d-flex justify-content-center",
        DerivationComponent.Props(derivation).make
      ),
      <.br(),
      FormulaFormRow.Props(modalState.formulaText, onChangeModalFormula, "Consequent").make,
    )
  }

}
