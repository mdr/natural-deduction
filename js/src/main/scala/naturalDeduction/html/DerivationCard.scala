package naturalDeduction.html

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.DerivationSection

object DerivationCard {

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]("DerivationCard")
    .render_P(render)
    .build

  case class Props(derivationSection: DerivationSection,
                   derivationIndex: DerivationIndex,
                   manipulationInfo: ManipulationInfo,
                   onAutoProve: Callback,
                   onDuplicateDerivation: Callback,
                   onDeleteDerivation: Callback,
                   onShowKotlin: Callback,
                   onShowLatex: Callback,
                  )

  def render(props: Props): VdomNode = {
    import props._

    <.div(^.className := "card", ^.key := derivationIndex,
      <.div(^.className := s"card-header${if (derivationSection.derivationProvesGoal) " proved-header" else if (derivationSection.goal.isDefined) " unproved-header" else ""}",
        <.div(^.className := "container",
          <.div(^.className := "row align-items-center",
            <.span(^.className := "mr-auto",
              s"${derivationIndex + 1}. ",
              derivationSection.goal match {
                case None => derivationSection.derivation.sequent.toString
                case Some(sequent) if derivationSection.derivationProvesGoal => sequent.toString
                case Some(sequent) => s"Goal: $sequent"
              },
              <.span(
                " ",
                <.i(^.className := "fas fa-check-circle")).when(derivationSection.derivationProvesGoal),
            ),
            CardButtonBar.Props(derivationSection.goal.isDefined, onAutoProve, onDuplicateDerivation, onDeleteDerivation, onShowKotlin, onShowLatex).make,
          ),
        )
      ),
      <.div(^.className := "card-body",
        DerivationComponent.Props(derivationSection.derivation, Some(manipulationInfo)).make
      )
    )
  }
}
