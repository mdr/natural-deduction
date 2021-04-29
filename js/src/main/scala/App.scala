import ExampleDerivations.{derivation1, derivation2}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation._
import naturalDeduction.parser.FormulaParser
import naturalDeduction.{Derivation, DerivationHtmlRenderer}

object App {

  case class State(newFormulaText: String = "", derivations: Seq[Derivation] = Seq.empty) {
    def acceptNewFormulaAsNewDerivation: State =
      copy(
        derivations = derivations :+ Axiom(FormulaParser.parseFormula(newFormulaText)),
        newFormulaText = "")

    def newFormulaIsValid: Boolean = FormulaParser.tryParseFormula(newFormulaText).isRight
  }

  class Backend($: BackendScope[Unit, State]) {
    private def onChange(e: ReactEventFromInput) = {
      val newValue = e.target.value
      $.modState(_.copy(newFormulaText = newValue))
    }

    private def handleSubmit(e: ReactEventFromInput) =
      e.preventDefaultCB >> $.modState(_.acceptNewFormulaAsNewDerivation)

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
          state.derivations.map(derivationCard).mkTagMod(<.br()),
          <.br(),
          <.form(^.`class` := "form-row align-items-center",
            ^.onSubmit ==> handleSubmit,
            <.div(^.`class` := "col-auto",
              <.input(^.`class` := "form-control mb-2", ^.`type` := "text", ^.placeholder := "Add formula...", ^.onChange ==> onChange, ^.value := state.newFormulaText)),
            <.div(^.`class` := "col-auto",
              <.button(^.`type` := "submit", ^.`class` := "btn btn-secondary mb-2", "Start New Derivation", ^.disabled := !state.newFormulaIsValid),
            ),
          ),
        )
      )
    }

  }

  def derivationCard(derivation: Derivation) =
    <.div(^.`class` := "card",
      <.div(^.`class` := "card-header", derivation.sequent.toString),
      <.div(^.`class` := "card-body",
        DerivationHtmlRenderer.renderDerivation(derivation)))

  val app = ScalaComponent.builder[Unit]("App")
    .initialState(State(derivations = Seq(derivation1, derivation2)))
    .renderBackend[Backend]
    .build

}
