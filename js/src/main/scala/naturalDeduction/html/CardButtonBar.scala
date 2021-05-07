package naturalDeduction.html

import japgolly.scalajs.react.vdom.all.VdomTag
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.html.TooltipUtils.activateTooltip

object CardButtonBar {

  case class Props(onDuplicateDerivation: Callback,
                   onDeleteDerivation: Callback) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  private def render(props: Props): VdomTag =
    <.div(^.`class` := "btn-group float-right", ^.role := "group",
      DuplicateButton(props),
      DeleteButton(props),
    )

  private val DuplicateButton =
    ScalaComponent.builder[Props]("DuplicateButton")
      .render_P(props =>
        <.button(
          ^.`class` := "btn btn-outline-secondary",
          ^.`type` := "button",
          <.i(^.className := "fas fa-clone", ^.title := "Duplicate"),
          ^.onClick --> props.onDuplicateDerivation,
          ^.title := "Duplicate",
        )
      )
      .componentDidMount(activateTooltip)
      .build

  private val DeleteButton =
    ScalaComponent.builder[Props]("DeleteButton")
      .render_P(props =>
        <.button(
          ^.`class` := "btn btn-outline-secondary",
          ^.`type` := "button",
          <.i(^.className := "fas fa-trash", ^.title := "Delete"),
          ^.onClick --> props.onDeleteDerivation,
          ^.title := "Delete",
        )
      )
      .componentDidMount(activateTooltip)
      .build

}
