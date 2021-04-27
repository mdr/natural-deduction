import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object App {
  val dataToggle = VdomAttr("data-toggle")

  case class State()

  class Backend($: BackendScope[Unit, State]) {

    def render(state: State) = {
      <.div(^.`class` := "container-fluid p-0",
        <.div(^.className := "d-flex flex-column flex-md-row align-items-center py-3 mb-3 bg-white border-bottom box-shadow",
          <.div(^.`class` := "container",
            <.div(^.className := "d-flex flex-column flex-md-row align-items-center ",
              <.h5(^.className := "my-0 mr-md-auto font-weight-normal", "Natural Deduction"),
            ),
          ),
        ),
        <.div(^.`class` := "container",
          <.p(),
          <.p("An implementation of natural deduction proofs as described in ", <.em("Mathematical Logic"), " by Ian Chiswell and Wilfrid Hodges."),
          proof))
    }

  }


  val proof = <.div(^.`class` := "rule-column",
    <.div(^.`class` := "rule-column-top",
      <.div(^.`class` := "rule-row",
        <.div("φ"),
        <.div(^.`class` := "rule-column",
          <.div(^.`class` := "rule-column-top", "ψ ∧ φ"),
          <.div(^.`class` := "rule-column-line"),
          <.div(^.`class` := "rule-column-bottom", "ψ")))),
    <.div(^.`class` := "rule-column-line"),
    <.div(^.`class` := "rule-column-bottom", "φ ∧ ψ"))

  val app = ScalaComponent.builder[Unit]("App")
    .initialState(State())
    .renderBackend[Backend]
    .build

}
