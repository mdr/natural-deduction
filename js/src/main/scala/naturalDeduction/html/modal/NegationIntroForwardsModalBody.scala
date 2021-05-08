package naturalDeduction.html.modal

import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.Derivation.{NegationIntroduction, RichFormula}
import naturalDeduction.Formula.⊥
import naturalDeduction.html.DerivationComponent

object NegationIntroForwardsModalBody {

  case class Props(
                    modalState: NegationIntroForwardsModalState,
                    onChangeModalFormula: String => Callback,
                  ) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  private def render(props: Props): VdomNode = {
    import props._
    val derivation = NegationIntroduction(modalState.formula, ⊥.axiom)
    <.div(
      <.div(^.`class` := "d-flex justify-content-center",
        DerivationComponent.Props(derivation).make
      ),
      <.br(),
      FormulaFormRow.Props(modalState.formulaText, onChangeModalFormula).make,
    )
  }

}
