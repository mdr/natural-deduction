package naturalDeduction.html.modal

import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.html.ReactUtils.getTargetValueThen

object FormulaFormRow {

  case class Props(formulaText: String,
                   onChangeFormulaText: String => Callback,
                   placeholder: String = "Formula",
                   autofocus: Boolean = true) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]("FormulaFormRow")
    .render_P(render)
    .build

  private def render(props: Props): VdomNode =
    <.div(^.className := "form-row align-items-center",
      <.div(^.className := "col-12",
        <.label(^.className := "sr-only", ^.`for` := "inlineFormInput", "Name"),
        <.input(
          ^.`class` := s"form-control mb-2${if (props.autofocus) " focus-on-modal-shown" else ""}",
          ^.`type` := "text",
          ^.placeholder := s"${props.placeholder}...",
          ^.onChange ==> getTargetValueThen(props.onChangeFormulaText),
          ^.value := props.formulaText
        ),
      ),
    )

}