package naturalDeduction.html.modal

import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.Derivation.ImplicationElimination
import naturalDeduction.html.ReactUtils.getTargetValueThen
import naturalDeduction.html.{DerivationComponent, DerivationProps}

object ImplicationElimBackwardsModalBody {

  case class Props(
                    modalState: ImplicationElimBackwardsModalState,
                    onChangeModalFormula: String => Callback,
                  )

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  private def render(props: Props): VdomNode = {
    import props._
    val ImplicationElimBackwardsModalState(_, _, consequent, formulaText) = modalState
    val onChangeModalFormula = getTargetValueThen(props.onChangeModalFormula)
    val derivation = ImplicationElimination(modalState.formula, consequent)
    <.div(
      <.div(^.`class` := "d-flex justify-content-center",
        DerivationComponent.component(DerivationProps(derivation))
      ),
      <.br(),
      <.div(^.className := "form-row align-items-center",
        <.div(^.className := "col-12",
          <.label(^.className := "sr-only", ^.`for` := "inlineFormInput", "Name"),
          <.input(
            ^.`class` := "form-control mb-2",
            ^.`type` := "text",
            ^.placeholder := "Antecedent...",
            ^.onChange ==> onChangeModalFormula,
            ^.value := formulaText
          ),
        ),
      ),
    )
  }

}
