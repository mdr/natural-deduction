package naturalDeduction

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactDOM, ScalaComponent}

import scala.scalajs.js.Dynamic.global

case class LeftRuleLabelProps(label: String, labelToFormula: Map[String, Formula])

object LeftRuleLabel {

  class Backend($: BackendScope[LeftRuleLabelProps, Unit]) {
    def render(props: LeftRuleLabelProps) =
      <.div(
        ^.`class` := "rule-bottom-left-label",
        ^.title := props.labelToFormula.map { case (label, formula) => s"$label: $formula" }.mkString(", "),
        CustomAttributes.dataToggle := "tooltip",
        props.labelToFormula.keys.mkString(""))
  }

  val component =
    ScalaComponent.builder[LeftRuleLabelProps]("LeftRuleLabel")
      .renderBackend[Backend]
      .componentDidMount(scope => Callback {
        global.$(scope.getDOMNode.asElement()).tooltip()
      })
      .build

}