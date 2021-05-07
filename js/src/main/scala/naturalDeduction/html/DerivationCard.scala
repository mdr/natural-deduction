package naturalDeduction.html

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.Derivation

object DerivationCard {

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]("DerivationCard")
    .render_P(render)
    .build

  case class Props(derivation: Derivation,
                   derivationIndex: DerivationIndex,
                   manipulationInfo: ManipulationInfo,
                   onDuplicateDerivation: Callback,
                   onDeleteDerivation: Callback)

  def render(props: Props): VdomNode = {
    import props._
    <.div(^.`class` := "card", ^.key := derivationIndex,
      <.div(^.`class` := "card-header",
        s"${derivationIndex + 1}. ${derivation.sequent}",
        CardButtonBar.Props(onDuplicateDerivation, onDeleteDerivation).make,
      ),
      <.div(^.`class` := "card-body",
        DerivationComponent.Props(derivation, Some(manipulationInfo)).make
      )
    )
  }
}
