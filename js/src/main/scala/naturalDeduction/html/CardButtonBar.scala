package naturalDeduction.html

import japgolly.scalajs.react.vdom.all.VdomTag
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.html.TooltipUtils.activateTooltip

object CardButtonBar {

  case class Props(canAutoProve: Boolean,
                   onAutoProve: Callback,
                   onDuplicateDerivation: Callback,
                   onDeleteDerivation: Callback,
                   onShowKotlin: Callback,
                   onShowLatex: Callback,
                  ) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  private def render(props: Props): VdomTag =
    <.div(^.`class` := "btn-group", ^.role := "group",
      AutoProveButtonButton(props).when(props.canAutoProve),
      DuplicateButton(props),
      DeleteButton(props),
      MenuButton(props)
    )

  private val AutoProveButtonButton =
    ScalaComponent.builder[Props]("AutoProveButton")
      .render_P(props =>
        <.button(
          ^.`class` := "btn btn-outline-secondary",
          ^.`type` := "button",
          <.i(^.className := "fas fa-magic", ^.title := "Automatically prove"),
          ^.onClick --> props.onAutoProve,
          ^.title := "Automatically prove",
        )
      )
      .componentDidMount(activateTooltip)
      .build

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

  private val MenuButton =
    ScalaComponent.builder[Props]("MenuButton")
      .render_P(props =>
        <.div(^.className := "btn-group", ^.role := "group",
          <.button(^.id := "btnGroupDrop1", ^.`type` := "button", ^.className := "btn btn-outline-secondary", VdomAttr("data-toggle") := "dropdown", VdomAttr("aria-haspopup") := "true", VdomAttr("aria-expanded") := "false",
            <.i(^.className := "fas fa-ellipsis-v", ^.title := "Menu")),
          <.div(^.className := "dropdown-menu", VdomAttr("aria-labelledby") := "btnGroupDrop1",
            <.div(^.className := "dropdown-item", ^.href := "#", "Extract Kotlin...", ^.onClick --> props.onShowKotlin),
            <.div(^.className := "dropdown-item", ^.href := "#", "Extract Latex...", ^.onClick --> props.onShowLatex),
          )
        )
      )
      .componentDidMount(activateTooltip)
      .build

}
