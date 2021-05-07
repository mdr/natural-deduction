package naturalDeduction.html.modal

import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.Derivation.ImplicationIntroduction
import naturalDeduction.html.DerivationComponent
import naturalDeduction.html.ReactUtils.getTargetValueThen

object ImplicationIntroForwardsModalBody {

  case class Props(
                    modalState: ImplicationIntroForwardsModalState,
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
    val ImplicationIntroForwardsModalState(_, consequent, formulaText) = modalState
    val onChangeModalFormula = getTargetValueThen(props.onChangeModalFormula)
    val antecedent = modalState.formula
    val derivation = ImplicationIntroduction(antecedent, consequent)
    <.div(
      <.div(^.`class` := "d-flex justify-content-center",
        DerivationComponent.Props(derivation).make
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
