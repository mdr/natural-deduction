package naturalDeduction.html.modal

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._

object Modal {

  case class Props(
                         modalState: Option[ModalState],
                         onChangeModalFormula: String => Callback,
                         onSwapConjuncts: Callback,
                         onConfirmModal: Callback,
                       ){
    def make: VdomNode = component(this)
  }

  val Id = "interactionModal"

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

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

  private def modalBody(props: Props): VdomNode = {
    import props._
    <.div(^.className := "modal-body", modalState.map {
      case state: ConjunctionElimBackwardsModalState =>
        ConjunctionElimBackwardsModalBody.Props(state, onChangeModalFormula, onSwapConjuncts).make
      case state: ConjunctionIntroForwardsModalState =>
        ConjunctionIntroForwardsModalBody.Props(state, onChangeModalFormula, onSwapConjuncts).make
      case state: ImplicationElimBackwardsModalState =>
        ImplicationElimBackwardsModalBody.Props(state, onChangeModalFormula).make
      case state: ImplicationElimForwardsFromAntecedentModalState =>
        ImplicationElimForwardsFromAntecedentModalBody.Props(state, onChangeModalFormula).make
      case state: ImplicationIntroForwardsModalState =>
        ImplicationIntroForwardsModalBody.Props(state, onChangeModalFormula).make
      case state: NegationElimBackwardsModalState =>
        NegationElimBackwardsModalBody.Props(state, onChangeModalFormula).make
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
