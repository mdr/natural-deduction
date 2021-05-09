package naturalDeduction.html

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation._
import naturalDeduction._

object DerivationComponent {

  case class Props(derivation: Derivation,
                   manipulationInfo: Option[ManipulationInfo] = None) {
    def make: VdomNode = component(this)
  }

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]("DerivationComponent")
    .render_P(render)
    .build

  private def render(props: Props): VdomNode =
    <.div(^.className := "derivation",
      new DerivationHtmlRenderer(props).renderDerivation(props.derivation))
}


class DerivationHtmlRenderer(props: DerivationComponent.Props) {

  def renderDerivation(derivation: Derivation,
                       bindings: Map[Label, Formula] = Map.empty,
                       path: DerivationPath = DerivationPath.empty): VdomNode = derivation match {
    case Axiom(formula, label) =>
      val isDischarged = bindings.keySet.intersect(label.toSet).nonEmpty
      <.div(^.className := "rule-axiom", // without this, dropdown menu for derivations that are just axioms don't position correctly...
        CustomAttributes.dataDerivationPath := path.toString,
        props.manipulationInfo match {
          case None => formula.toString
          case Some(manipulationInfo) =>
            ManipulatableFormula.Props(derivation, path, manipulationInfo, label, isDischarged, bindings).make
        }
      )
    case ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      val leftRendered = renderDerivation(leftDerivation, bindings, path.choose(0))
      val rightRendered = renderDerivation(rightDerivation, bindings, path.choose(1))
      rule(derivation, Seq(leftRendered, rightRendered), "∧I", None, path)
    case LeftConjunctionElimination(conjunctionDerivation) =>
      val conjunctionRendered = renderDerivation(conjunctionDerivation, bindings, path.choose(0))
      rule(derivation, Seq(conjunctionRendered), "∧E", None, path)
    case RightConjunctionElimination(conjunctionDerivation) =>
      val conjunctionRendered = renderDerivation(conjunctionDerivation, bindings, path.choose(0))
      rule(derivation, Seq(conjunctionRendered), "∧E", None, path)
    case ImplicationIntroduction(_, label, consequentDerivation) =>
      val consequentRendered = renderDerivation(consequentDerivation, bindings ++ derivation.bindingsForChild(0), path.choose(0))
      rule(derivation, Seq(consequentRendered), "→I", label, path)
    case ImplicationElimination(antecedentDerivation, implicationDerivation) =>
      val antecedentRendered = renderDerivation(antecedentDerivation, bindings, path.choose(0))
      val implicationRendered = renderDerivation(implicationDerivation, bindings, path.choose(1))
      rule(derivation, Seq(antecedentRendered, implicationRendered), "→E", None, path)
    case EquivalenceIntroduction(forwardsDerivation, backwardsDerivation) =>
      val forwardRendered = renderDerivation(forwardsDerivation, bindings, path.choose(0))
      val backwardsRendered = renderDerivation(backwardsDerivation, bindings, path.choose(1))
      rule(derivation, Seq(forwardRendered, backwardsRendered), "↔I", None, path)
    case ForwardsEquivalenceElimination(equivalenceDerivation) =>
      val equivalenceRendered = renderDerivation(equivalenceDerivation, bindings, path.choose(0))
      rule(derivation, Seq(equivalenceRendered), "↔E", None, path)
    case BackwardsEquivalenceElimination(equivalenceDerivation) =>
      val equivalenceRendered = renderDerivation(equivalenceDerivation, bindings, path.choose(0))
      rule(derivation, Seq(equivalenceRendered), "↔E", None, path)
    case NegationIntroduction(_, label, bottomDerivation) =>
      val bottomRendered = renderDerivation(bottomDerivation, bindings ++ derivation.bindingsForChild(0), path.choose(0))
      rule(derivation, Seq(bottomRendered), "¬I", label, path)
    case NegationElimination(positiveDerivation, negativeDerivation) =>
      val positiveRendered = renderDerivation(positiveDerivation, bindings, path.choose(0))
      val negativeRendered = renderDerivation(negativeDerivation, bindings, path.choose(1))
      rule(derivation, Seq(positiveRendered, negativeRendered), "¬E", None, path)
    case ReductioAdAbsurdum(_, label, bottomDerivation) =>
      val bottomRendered = renderDerivation(bottomDerivation, bindings ++ derivation.bindingsForChild(0), path.choose(0))
      rule(derivation, Seq(bottomRendered), "RAA", label, path)
    case LeftDisjunctionIntroduction(leftDerivation, _) =>
      val leftRendered = renderDerivation(leftDerivation, bindings, path.choose(0))
      rule(derivation, Seq(leftRendered), "∨I", None, path)
    case RightDisjunctionIntroduction(_, rightDerivation) =>
      val rightRendered = renderDerivation(rightDerivation, bindings, path.choose(0))
      rule(derivation, Seq(rightRendered), "∨I", None, path)
    case DisjunctionElimination(disjunctionDerivation, leftLabel, leftDerivation, rightLabel, rightDerivation) =>
      val disjunctionRendered = renderDerivation(disjunctionDerivation, bindings, path.choose(0))
      val leftRendered = renderDerivation(leftDerivation, bindings ++ derivation.bindingsForChild(1), path.choose(1))
      val rightRendered = renderDerivation(rightDerivation, bindings ++ derivation.bindingsForChild(2), path.choose(2))
      val compositeLabel = (leftLabel.toSeq ++ rightLabel).mkString(" ") match {
        case "" => None;
        case s => Some(s)
      }
      rule(derivation, Seq(disjunctionRendered, leftRendered, rightRendered), "∨E", compositeLabel, path)
  }

  private def rule(parent: Derivation,
                   children: Seq[TagMod],
                   rightLabel: String,
                   leftLabel: Option[Label] = None,
                   path: DerivationPath): VdomNode = {
    <.div(^.`class` := "rule",
      <.div(^.`class` := "rule-top",
        <.div(^.`class` := "rule-top-left-label").when(leftLabel.isDefined),
        <.div(^.`class` := "rule-top-main",
          children.map(child =>
            <.div(^.`class` := "rule-top-main-part", child)
          ).mkTagMod(<.div(^.`class` := "rule-top-main-spacer"))
        ),
        <.div(^.`class` := "rule-top-right-label")
      ),
      <.div(^.`class` := "rule-bottom",
        leftLabel.whenDefined(label =>
          LeftRuleLabel.Props(label, getDischargableFormula(parent)).make),
        <.div(^.`class` := "rule-bottom-main",
          <.div(^.`class` := "rule-conclusion",
            CustomAttributes.dataDerivationPath := path.toString,
            props.manipulationInfo match {
              case None =>
                <.span(parent.conclusion.toString)
              case Some(manipulationInfo) =>
                ManipulatableFormula.Props(parent, path, manipulationInfo).make
            }
          )
        ),
        <.div(^.`class` := "rule-bottom-right-label", s"($rightLabel)")
      )
    )
  }

  private def getDischargableFormula(derivation: Derivation): Map[Label, Formula] =
    derivation.children.indices.map(derivation.bindingsForChild).fold(Map.empty)(_ ++ _)

}
