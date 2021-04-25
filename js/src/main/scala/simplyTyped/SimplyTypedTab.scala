package simplyTyped

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object SimplyTypedTab {

  case class State(input: String = "")

  val simplyTypedTab =
    ScalaComponent.builder[Unit]("SimplyTypedTab")
      .initialState(State())
      .renderBackend[Backend]
      .build

  class Backend($: BackendScope[Unit, State]) {

    val dataToggle = VdomAttr("data-toggle")
    val dataTarget = VdomAttr("data-target")

    def onChange(e: ReactEventFromInput) = {
      val newValue = e.target.value
      $.modState(_.copy(input = newValue))
    }

    def render(state: State) = {
      <.div(^.`class` := "tab-pane fade show active", ^.id := "simply",
        <.div(^.`class` := "btn-group", ^.role := "group",
          <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", dataToggle := "collapse", dataTarget := "#instructions-table", "Show/hide instructions"),
          <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", dataToggle := "collapse", dataTarget := "#bindings-table", "Show/hide bindings"),
        ))
    }

  }

}
