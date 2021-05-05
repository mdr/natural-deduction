package naturalDeduction.html

import japgolly.scalajs.react.vdom.all.VdomTag
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.html.TooltipUtils.activateTooltip

case class CardButtonBarProps(
                               onDuplicateDerivation: Callback,
                               onDeleteDerivation: Callback,
                             )

object CardButtonBar {

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[CardButtonBarProps]
    .render_P(render)
    .build

  private def render(props: CardButtonBarProps): VdomTag =
    <.div(^.`class` := "btn-group float-right", ^.role := "group",
      DuplicateButton(props),
      DeleteButton(props),
    )

  private val DuplicateButton =
    ScalaComponent.builder[CardButtonBarProps]("DuplicateButton")
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
    ScalaComponent.builder[CardButtonBarProps]("DeleteButton")
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
