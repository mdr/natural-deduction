package naturalDeduction.html

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import naturalDeduction.Formula

import scala.scalajs.js.Dynamic.global

case class LeftRuleLabelProps(label: String, labelToFormula: Map[String, Formula])

object LeftRuleLabel {

  def render(props: LeftRuleLabelProps): VdomTag =
    <.div(
      ^.`class` := "rule-bottom-left-label",
      ^.title := props.labelToFormula.map { case (label, formula) => s"$label: $formula" }.mkString(", "),
      CustomAttributes.dataToggle := "tooltip",
      props.labelToFormula.keys.mkString(""))

  val component =
    ScalaComponent.builder[LeftRuleLabelProps]("LeftRuleLabel")
      .render_P(render)
      .componentDidMount(scope => Callback {
        global.$(scope.getDOMNode.asElement()).tooltip()
      })
      .build

}