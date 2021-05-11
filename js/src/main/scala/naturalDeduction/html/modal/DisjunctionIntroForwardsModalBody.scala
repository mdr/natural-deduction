package naturalDeduction.html.modal

import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.Derivation.RichFormula
import naturalDeduction.html.DerivationComponent
import naturalDeduction.html.ReactUtils.getTargetValueThen
import naturalDeduction.html.modal.DisjunctionIntroForwardsModalState.disjunctionIntroDerivation

object DisjunctionIntroForwardsModalBody {

  case class Props(modalState: DisjunctionIntroForwardsModalState,
                   onChangeModalFormula: String => Callback,
                   onSwapDisjuncts: Callback) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  private def render(props: Props): VdomNode = {
    import props._
    val DisjunctionIntroForwardsModalState(_, existingDisjunct, disjunctToPick, formulaText) = modalState
    val onChangeModalFormula = getTargetValueThen(props.onChangeModalFormula)
    val derivation = disjunctionIntroDerivation(existingDisjunct.axiom, modalState.formula, disjunctToPick)
    <.div(
      <.div(^.`class` := "d-flex justify-content-center",
        DerivationComponent.Props(derivation).make
      ),
      <.br(),
      <.div(^.className := "form-row align-items-center",
        <.div(^.className := "col-9",
          <.label(^.className := "sr-only", ^.`for` := "inlineFormInput", "Name"),
          <.input(
            ^.`class` := "form-control mb-2 focus-on-modal-shown",
            ^.`type` := "text",
            ^.placeholder := "Other disjunct...",
            ^.onChange ==> onChangeModalFormula,
            ^.value := formulaText
          ),
        ),
        <.div(^.className := "col",
          <.button(^.`class` := "btn btn-outline-secondary mb-2", ^.`type` := "button", ^.onClick --> onSwapDisjuncts,
            <.span(<.i(^.className := "fas fa-exchange-alt"), " Swap"),
          ),
        )
      ),
    )
  }

}
