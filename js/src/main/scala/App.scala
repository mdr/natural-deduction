import App.derivation
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.{Derivation, DerivationHtmlRenderer}
import naturalDeduction.Derivation._
import naturalDeduction.Formula.PropositionalVariable

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
          derivationCard(derivation),
          <.br(),
          derivationCard(derivation2)))
    }

  }

  def derivationCard(derivation: Derivation) =
    <.div(^.`class` := "card",
      <.div(^.`class` := "card-header", derivation.sequent.toString),
      <.div(^.`class` := "card-body",
        DerivationHtmlRenderer.renderDerivation(derivation)))

  private val φ = PropositionalVariable("φ")
  private val ψ = PropositionalVariable("ψ")
  private val χ = PropositionalVariable("χ")

  val derivation: Derivation =
    ImplicationIntroduction(φ.not.not, "❷",
      ReductioAdAbsurdum(φ, "❶",
        NegationElimination(
          Axiom(φ.not, "❶"),
          Axiom(φ.not.not, "❷")
        )))

  val derivation2 =
    ImplicationIntroduction(ψ → χ, "❸",
      ImplicationIntroduction(φ → ψ, "❷",
        ImplicationIntroduction(φ, "❶",
          ImplicationElimination(
            ImplicationElimination(Axiom(φ, "❶"), Axiom(φ → ψ, "❷")),
            Axiom(ψ → χ, "❸")))))


  val app = ScalaComponent.builder[Unit]("App")
    .initialState(State())
    .renderBackend[Backend]
    .build

}
