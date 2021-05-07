package naturalDeduction.html.modal

import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.html.DerivationComponent

object NegationElimBackwardsModalBody {

  case class Props(
                    modalState: NegationElimBackwardsModalState,
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
    <.div(
      <.div(^.`class` := "d-flex justify-content-center",
        DerivationComponent.Props(modalState.newDerivation).make
      ),
      <.br(),
      FormulaFormRow.Props(modalState.formulaText, onChangeModalFormula).make,
    )
  }

}
