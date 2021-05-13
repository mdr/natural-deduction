package naturalDeduction.html.modal

import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.html.DerivationComponent

object DisjunctionElimBackwardsModalBody {

  case class Props(modalState: DisjunctionElimBackwardsModalState,
                   onChangeModalFormula: String => Callback,
                   onChangeModalFormula2: String => Callback,
                  ) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build


  private def render(props: Props): VdomNode = {
    import props._
    val DisjunctionElimBackwardsModalState(_, _, conclusion, _, _) = modalState
    val derivation = DisjunctionElimBackwardsModalState.disjunctionElimination(modalState.formula, modalState.formula2, conclusion)
    <.div(
      <.div(^.`class` := "d-flex justify-content-center",
        DerivationComponent.Props(derivation).make
      ),
      <.br(),
      FormulaFormRow.Props(modalState.formulaText, onChangeModalFormula, "Left disjunct").make,
      FormulaFormRow.Props(modalState.formulaText2, onChangeModalFormula2, "Right disjunct", autofocus = false).make,
    )
  }

}
