package naturalDeduction.html

import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Formula
import naturalDeduction.html.TooltipUtils.activateTooltip

import scala.scalajs.js.Dynamic.global

case class LeftRuleLabelProps(label: String, labelToFormula: Map[String, Formula])

object LeftRuleLabel {

  val component =
    ScalaComponent.builder[LeftRuleLabelProps]("LeftRuleLabel")
      .render_P(render)
      .componentDidMount(scope => Callback {
        global.$(scope.getDOMNode.asElement()).tooltip()
      })
      .build

  def render(props: LeftRuleLabelProps): VdomTag =
    <.div(
      ^.`class` := "rule-bottom-left-label",
      ^.title := tooltipContents(props),
      props.labelToFormula.keys.mkString(""))

  private def tooltipContents(props: LeftRuleLabelProps): String =
    props.labelToFormula
      .map { case (label, formula) => s"$label: $formula" }
      .toSeq
      .sorted
      .mkString(", ")

}