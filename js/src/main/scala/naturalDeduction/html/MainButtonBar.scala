package naturalDeduction.html

import japgolly.scalajs.react.vdom.all.VdomTag
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.html.TooltipUtils.activateTooltip


object MainButtonBar {

  case class Props(undoRedo: UndoRedo[_],
                   onUndoClicked: Callback,
                   onRedoClicked: Callback) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  private def render(props: Props): VdomTag =
    <.div(^.`class` := "btn-group", ^.role := "group",
      UndoButton(props),
      RedoButton(props),
      HelpButton(),
    )

  private val UndoButton =
    ScalaComponent.builder[Props]("UndoButton")
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
    ScalaComponent.builder[Props]("RedoButton")
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
