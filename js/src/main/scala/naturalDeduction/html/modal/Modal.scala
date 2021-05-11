package naturalDeduction.html.modal

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.Lifecycle
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._

import scala.scalajs.js.Dynamic.global

object Modal {

  case class Props(
                    modalState: Option[ModalState],
                    onChangeModalFormula: String => Callback,
                    onSwapConjuncts: Callback,
                    onSwapDisjuncts: Callback,
                    onConfirmModal: Callback,
                  ) {
    def make: VdomNode = component(this)
  }

  val Id = "interactionModal"

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .componentDidMount(setUpFocusTrigger)
    .build

  private def setUpFocusTrigger(scope: Lifecycle.ComponentDidMount[_, _, _]): Callback =
    Callback {
      val $ = global.$
      $(s"#$Id").on("shown.bs.modal", () => {
        $(".focus-on-modal-shown").trigger("focus")
      })
    }

  private def render(props: Props): VdomNode = {
    import props._
    <.div(
      ^.className := "modal fade",
      ^.id := Id,
      ^.tabIndex := -1,
      ^.role := "dialog",
      VdomAttr("aria-labelledby") := "interactionModalLabel", VdomAttr("aria-hidden") := "true",
      <.div(^.className := "modal-dialog", ^.role := "document",
        <.div(^.className := "modal-content",
          modalHeader(modalState),
          modalBody(props),
          modalFooter(modalState.exists(_.canComplete), onConfirmModal)
        )
      )
    )
  }

  private def modalHeader(modalState: Option[ModalState]): VdomNode = {
    val title = modalState.map(_.title).getOrElse("<>")
    <.div(^.className := "modal-header",
      <.h5(^.className := "modal-title", ^.id := "interactionModalLabel", title),
      <.button(^.`type` := "button", ^.className := "close", VdomAttr("data-dismiss") := "modal", VdomAttr("aria-label") := "Close",
        <.span(VdomAttr("aria-hidden") := "true", "×")
      )
    )
  }

  private def onSubmit(props: Props)(e: ReactEventFromInput): Callback =
    e.preventDefaultCB >>
      (if (props.modalState.exists(_.canComplete)) props.onConfirmModal else Callback.empty)

  private def modalBody(props: Props): VdomNode = {
    import props._
    <.form(^.className := "modal-body",
      ^.onSubmit ==> onSubmit(props),
      modalState.map {
        case state: ConjunctionElimBackwardsModalState =>
          ConjunctionElimBackwardsModalBody.Props(state, onChangeModalFormula, onSwapConjuncts).make
        case state: ConjunctionIntroForwardsModalState =>
          ConjunctionIntroForwardsModalBody.Props(state, onChangeModalFormula, onSwapConjuncts).make
        case state: DisjunctionIntroForwardsModalState =>
          DisjunctionIntroForwardsModalBody.Props(state, onChangeModalFormula, onSwapDisjuncts).make
        case state: ImplicationElimBackwardsModalState =>
          ImplicationElimBackwardsModalBody.Props(state, onChangeModalFormula).make
        case state: ImplicationElimForwardsFromAntecedentModalState =>
          ImplicationElimForwardsFromAntecedentModalBody.Props(state, onChangeModalFormula).make
        case state: ImplicationIntroForwardsModalState =>
          ImplicationIntroForwardsModalBody.Props(state, onChangeModalFormula).make
        case state: NegationElimBackwardsModalState =>
          NegationElimBackwardsModalBody.Props(state, onChangeModalFormula).make
        case state: NegationIntroForwardsModalState =>
          NegationIntroForwardsModalBody.Props(state, onChangeModalFormula).make
        case state: ReductioForwardsModalState =>
          ReductioForwardsModalBody.Props(state, onChangeModalFormula).make
        case state: DisjunctionElimForwardsFromDisjunctionModalState =>
          DisjunctionElimForwardsFromDisjunctionModalBody.Props(state, onChangeModalFormula).make
      })
  }

  private def modalFooter(canConfirm: Boolean, onConfirmModal: Callback): VdomNode = {
    <.div(^.className := "modal-footer",
      <.button(
        ^.`type` := "button",
        ^.className := "btn btn-secondary",
        VdomAttr("data-dismiss") := "modal",
        "Close"
      ),
      <.button(
        ^.`type` := "button",
        ^.className := "btn btn-primary", "Apply",
        ^.disabled := !canConfirm,
        ^.onClick --> onConfirmModal
      )
    )
  }
}
