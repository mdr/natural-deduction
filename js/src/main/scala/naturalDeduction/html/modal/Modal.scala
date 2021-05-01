package naturalDeduction.html.modal

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._

case class ModalProps(
                       modalState: Option[ModalState],
                       onChangeModalFormula: String => Callback,
                       onSwapConjuncts: Callback,
                       onConfirmModal: Callback,
                     )

object Modal {

  val component = ScalaComponent.builder[ModalProps]
    .render_P(render)
    .build

  private def render(props: ModalProps): VdomNode = {
    import props._
    <.div(
      ^.className := "modal fade",
      ^.id := "interactionModal",
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

  private def modalBody(props: ModalProps) = {
    import props._
    <.div(^.className := "modal-body", modalState.map {
      case state: ConjunctionElimBackwardsModalState =>
        ConjunctionElimBackwardsModalBody.component(
          ConjunctionElimBackwardsModalBody.Props(state, onChangeModalFormula, onSwapConjuncts))
      case state: ImplicationElimBackwardsModalState =>
        ImplicationElimBackwardsModalBody.component(
          ImplicationElimBackwardsModalBody.Props(state, onChangeModalFormula))
      case state: ImplicationElimForwardsFromAntecedentModalState =>
        ImplicationElimForwardsFromAntecedentModalBody.component(
          ImplicationElimForwardsFromAntecedentModalBody.Props(state, onChangeModalFormula))
    })
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
