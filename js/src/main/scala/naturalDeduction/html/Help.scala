package naturalDeduction.html

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._

object Help {
  val component =
    ScalaComponent.static("Help")(
      <.div(^.className := "card collapse", ^.id := "help",
        <.div(^.className := "card-body",
          <.p(),
          <.p("An implementation of natural deduction proofs as described in ", <.em("Mathematical Logic"), " by Ian Chiswell and Wilfrid Hodges."),
          <.p(),
          <.p("Formulae can be entered using ASCII alternatives:"),
          <.table(^.`class` := "table table-bordered",
            <.tbody(
              <.tr(
                <.td("∧"),
                <.td(<.code("/\\"), " or ", <.code("&&")),
              ),
              <.tr(
                <.td("∨"),
                <.td(<.code("\\/"), " or ", <.code("||")),
              ),
              <.tr(
                <.td("→"),
                <.td(<.code("->")),
              ),
              <.tr(
                <.td("↔"),
                <.td(<.code("<->")),
              ),
              <.tr(
                <.td("θ φ ψ χ"),
                <.td(<.code("theta"), ", ", <.code("phi"), ", ", <.code("psi"), ", ", <.code("chi")),
              ),
            ),
          ),
        ),
      )
    )
}
