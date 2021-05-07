package naturalDeduction.html

import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.{Formula, Label}

import scala.scalajs.js.Dynamic.global

object LeftRuleLabel {

  case class Props(label: Label, labelToFormula: Map[Label, Formula]) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component =
    ScalaComponent.builder[Props]("LeftRuleLabel")
      .render_P(render)
      .componentDidMount(scope => Callback {
        global.$(scope.getDOMNode.asElement()).tooltip()
      })
      .build

  def render(props: Props): VdomTag =
    <.div(
      ^.`class` := "rule-bottom-left-label",
      ^.title := tooltipContents(props),
      props.labelToFormula.keys.mkString(""))

  private def tooltipContents(props: Props): String =
    props.labelToFormula
      .map { case (label, formula) => s"$label: $formula" }
      .toSeq
      .sorted
      .mkString(", ")

}