package naturalDeduction.html

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation
import naturalDeduction.pretty.LatexRenderer

object LatexModal {

  val Id = "latexModal"

  case class Props(derivation: Option[Derivation] = None) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component =
    ScalaComponent.builder[Props]("LatexModal")
      .render_P(render)
      .build

  def render(props: Props): VdomTag =
    <.div(^.className := "modal fade", ^.id := Id, ^.role := "dialog",
      <.div(^.className := "modal-dialog modal-lg", ^.role := "document",
        <.div(^.className := "modal-content",
          <.div(^.className := "modal-header",
            <.h5(^.className := "modal-title",
              props.derivation.whenDefined(derivation =>
                derivation.sequent.toString)),
            <.button(^.`type` := "button", ^.className := "close", VdomAttr("data-dismiss") := "modal", VdomAttr("aria-label") := "Close",
              <.span(VdomAttr("aria-hidden") := "true", "×")
            )
          ),
          <.div(^.className := "modal-body",
            <.pre(
              props.derivation.whenDefined(derivation => LatexRenderer.render(derivation)))
          ),
          <.div(^.className := "modal-footer",
            <.button(^.`type` := "button", ^.className := "btn btn-secondary", VdomAttr("data-dismiss") := "modal", "Close")
          )
        ),
      )

    )

}
