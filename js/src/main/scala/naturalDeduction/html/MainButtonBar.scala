package naturalDeduction.html

import japgolly.scalajs.react.component.builder.Lifecycle
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.all.VdomTag
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.html.LeftRuleLabel.render
import naturalDeduction.html.TooltipUtils.activateTooltip

import scala.scalajs.js.Dynamic.global

case class MainButtonBarProps(
                               undoRedo: UndoRedo[_],
                               onUndoClicked: Callback,
                               onRedoClicked: Callback,
                             )

object MainButtonBar {

  val component = ScalaComponent.builder[MainButtonBarProps]
    .render_P(render)
    .build

  private def render(props: MainButtonBarProps): VdomTag =
    <.div(^.`class` := "btn-group", ^.role := "group",
      UndoButton(props),
      RedoButton(props),
      HelpButton(),
    )

  private val UndoButton =
    ScalaComponent.builder[MainButtonBarProps]("UndoButton")
      .render_P(props =>
        <.button(
          ^.`class` := "btn btn-outline-secondary",
          ^.`type` := "button",
          <.i(^.className := "fas fa-undo", ^.title := "Undo"),
          ^.onClick --> props.onUndoClicked,
          ^.disabled := !props.undoRedo.canUndo,
          CustomAttributes.dataToggle := "tooltip",
          ^.title := "Undo",
        ))
      .componentDidMount(activateTooltip)
      .build


  private val RedoButton =
    ScalaComponent.builder[MainButtonBarProps]("RedoButton")
      .render_P(props =>
        <.button(
          ^.`class` := "btn btn-outline-secondary",
          ^.`type` := "button",
          <.i(^.className := "fas fa-redo", ^.title := "Redo"),
          ^.onClick --> props.onRedoClicked,
          ^.disabled := !props.undoRedo.canRedo,
          ^.title := "Redo",
        ))
      .componentDidMount(activateTooltip)
      .build

  private val HelpButton =
    ScalaComponent.builder[Unit]("HelpButton")
      .renderStatic(
        <.button(
          ^.`class` := "btn btn-outline-secondary",
          ^.`type` := "button",
          <.i(^.className := "fas fa-question-circle", ^.title := "Help"),
          CustomAttributes.dataToggle := "collapse",
          CustomAttributes.dataTarget := "#help",
          ^.title := "Help",
        )
      )
      .componentDidMount(activateTooltip)
      .build

}
