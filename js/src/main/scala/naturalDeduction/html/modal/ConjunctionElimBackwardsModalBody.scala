package naturalDeduction.html.modal

import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.html.DerivationComponent
import naturalDeduction.html.ReactUtils.getTargetValueThen

object ConjunctionElimBackwardsModalBody {

  case class Props(
                    modalState: ConjunctionElimBackwardsModalState,
                    onChangeModalFormula: String => Callback,
                    onSwapConjuncts: Callback,
                  ) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  private def render(props: Props): VdomNode = {
    import props._
    val onChangeModalFormula = getTargetValueThen(props.onChangeModalFormula)
    <.div(
      <.div(^.`class` := "d-flex justify-content-center",
        DerivationComponent.Props(modalState.newDerivation).make
      ),
      <.br(),
      <.div(^.className := "form-row align-items-center",
        <.div(^.className := "col-9",
          <.label(^.className := "sr-only", ^.`for` := "inlineFormInput", "Name"),
          <.input(
            ^.`class` := "form-control mb-2 focus-on-modal-shown",
            ^.`type` := "text",
            ^.placeholder := "Other conjunct...",
            ^.onChange ==> onChangeModalFormula,
            ^.value := modalState.formulaText
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
