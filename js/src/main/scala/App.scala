import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import simplyTyped.SimplyTypedTab

object App {
  val dataToggle = VdomAttr("data-toggle")

  case class State(tab: Int)

  class Backend($: BackendScope[Unit, State]) {
    private def onSimplePressed: Callback = $.modState(_.copy(tab = 1))

    private def onDependentPressed: Callback = $.modState(_.copy(tab = 0))

    def render(state: State) = {
      <.div(^.`class` := "container-fluid p-0",
        <.div(^.className := "d-flex flex-column flex-md-row align-items-center py-3 mb-3 bg-white border-bottom box-shadow",
          <.div(^.`class` := "container",
            <.div(^.className := "d-flex flex-column flex-md-row align-items-center ",
              <.h5(^.className := "my-0 mr-md-auto font-weight-normal", "Natural Deduction"),
              <.nav(^.className := "my-2 my-md-0 mr-md-3",
                <.a(^.className := s"p-2 text-dark${if (state.tab == 0) " font-weight-bold" else ""}", ^.href := "#", ^.onClick --> onDependentPressed)(
                  "Dependently-typed"
                ),
                <.a(^.className := s"p-2 text-dark${if (state.tab == 1) " font-weight-bold" else ""}", ^.href := "#", ^.onClick --> onSimplePressed)(
                  "Simply-typed")
              ),
            ),
          ),
        ),
        <.div(^.`class` := "container",
          <.p(),
          <.p("A Scala port of ", <.a(^.href := "https://www.andres-loeh.de/LambdaPi/", <.em("A Tutorial Implementation of a Dependently Typed Lambda Calculus")), " by Andres Löh, Conor McBride and Wouter Swierstra. Code can be found on ", <.a(^.href := "https://github.com/mdr/dependently-typed-lambda-calculus", "Github"), "."),
          SimplyTypedTab.simplyTypedTab().when(state.tab == 1)))
    }

  }

  val app = ScalaComponent.builder[Unit]("App")
    .initialState(State(0))
    .renderBackend[Backend]
    .build

}
