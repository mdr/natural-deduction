package naturalDeduction.html.modal

import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.Derivation.RichFormula
import naturalDeduction.html.DerivationComponent
import naturalDeduction.html.modal.DisjunctionElimForwardsFromDisjunctionModalState.disjunctionElimDerivation

object DisjunctionElimForwardsFromDisjunctionModalBody {

  case class Props(modalState: DisjunctionElimForwardsFromDisjunctionModalState,
                   onChangeModalFormula: String => Callback) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  private def render(props: Props): VdomNode = {
    import props._
    val DisjunctionElimForwardsFromDisjunctionModalState(_, disjunction, _) = modalState
    val derivation = disjunctionElimDerivation(modalState.formula, disjunction.axiom)
    <.div(
      <.div(^.`class` := "d-flex justify-content-center",
        DerivationComponent.Props(derivation).make
      ),
      <.br(),
      FormulaFormRow.Props(modalState.formulaText, onChangeModalFormula).make,
    )
  }

}
