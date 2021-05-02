package naturalDeduction.html

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation._
import naturalDeduction._

case class ManipulationInfo(onRemoveDerivation: DerivationPath => Callback,
                            onConjunctionIntroBackwards: DerivationPath => Callback,
                            onConjunctionElimForwards: ChildIndex => Callback,
                            onConjunctionElimBackwards: DerivationPath => Callback,
                            onConjunctionIntroForwards: Callback,
                            onImplicationIntroBackwards: DerivationPath => Callback,
                            onImplicationIntroForwards: Callback,
                            onImplicationElimBackwards: DerivationPath => Callback,
                            onImplicationElimForwardsFromAntecedent: Callback,
                            onImplicationElimForwardsFromImplication: Callback,
                            onInlineDerivation: (DerivationPath, DerivationIndex) => Callback,
                            onDischargeAssumption: (DerivationPath, String) => Callback,
                            onUndischargeAssumption: DerivationPath => Callback,
                            onBetaReduce: DerivationPath => Callback,
                            onExtractSubderivation: DerivationPath => Callback,
                            derivationIndex: DerivationIndex,
                            formulaToDerivationIndices: Map[Formula, Seq[DerivationIndex]])

case class DerivationProps(derivation: Derivation,
                           manipulationInfo: Option[ManipulationInfo] = None)

object DerivationComponent {

  val component = ScalaComponent.builder[DerivationProps]("DerivationComponent")
    .render_P(props => {
      val derivationRenderer = new DerivationHtmlRenderer(props)
      derivationRenderer.renderDerivation(props.derivation)
    })
    .build

}


class DerivationHtmlRenderer(props: DerivationProps) {

  def renderDerivation(derivation: Derivation,
                       bindings: Map[Label, Formula] = Map.empty,
                       path: DerivationPath = DerivationPath.empty): VdomNode = derivation match {
    case Axiom(formula, label) =>
      val isDischarged = bindings.keySet.intersect(label.toSet).nonEmpty
      props.manipulationInfo match {
        case None => formula.toString
        case Some(manipulationInfo) =>
          <.div(^.className := "rule-axiom", // without this, dropdown menu for derivations that are just axioms don't position correctly...
            ManipulatableFormula.component(ManipulatableFormula.Props(derivation, path, manipulationInfo, label, isDischarged, bindings))
          )
      }
    case ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      val leftRendered = renderDerivation(leftDerivation, bindings, path.choose(0))
      val rightRendered = renderDerivation(rightDerivation, bindings, path.choose(1))
      rule2(derivation, leftRendered, rightRendered, "∧I", path = path)
    case LeftConjunctionElimination(conjunctionDerivation) =>
      val conjunctionRendered = renderDerivation(conjunctionDerivation, bindings, path.choose(0))
      rule1(derivation, conjunctionRendered, "∧E", path = path)
    case RightConjunctionElimination(conjunctionDerivation) =>
      val conjunctionRendered = renderDerivation(conjunctionDerivation, bindings, path.choose(0))
      rule1(derivation, conjunctionRendered, "∧E", path = path)
    case ImplicationIntroduction(_, label, consequentDerivation) =>
      val consequentRendered = renderDerivation(consequentDerivation, bindings ++ derivation.bindingsForChild(0), path.choose(0))
      rule1(derivation, consequentRendered, "→I", label, path = path)
    case ImplicationElimination(antecedentDerivation, implicationDerivation) =>
      val antecedentRendered = renderDerivation(antecedentDerivation, bindings, path.choose(0))
      val implicationRendered = renderDerivation(implicationDerivation, bindings, path.choose(1))
      rule2(derivation, antecedentRendered, implicationRendered, "→E", path = path)
    case EquivalenceIntroduction(forwardsDerivation, backwardsDerivation) =>
      val forwardRendered = renderDerivation(forwardsDerivation, bindings, path.choose(0))
      val backwardsRendered = renderDerivation(backwardsDerivation, bindings, path.choose(1))
      rule2(derivation, forwardRendered, backwardsRendered, "↔I", path = path)
    case ForwardsEquivalenceElimination(equivalenceDerivation) =>
      val equivalenceRendered = renderDerivation(equivalenceDerivation, bindings, path.choose(0))
      rule1(derivation, equivalenceRendered, "↔E", path = path)
    case BackwardsEquivalenceElimination(equivalenceDerivation) =>
      val equivalenceRendered = renderDerivation(equivalenceDerivation, bindings, path.choose(0))
      rule1(derivation, equivalenceRendered, "↔E", path = path)
    case NegationIntroduction(_, label, bottomDerivation) =>
      val bottomRendered = renderDerivation(bottomDerivation, derivation.bindingsForChild(0), path.choose(0))
      rule1(derivation, bottomRendered, "¬I", label, path = path)
    case NegationElimination(positiveDerivation, negativeDerivation) =>
      val positiveRendered = renderDerivation(positiveDerivation, bindings, path.choose(0))
      val negativeRendered = renderDerivation(negativeDerivation, bindings, path.choose(1))
      rule2(derivation, positiveRendered, negativeRendered, "¬E", path = path)
    case ReductioAdAbsurdum(_, label, bottomDerivation) =>
      val bottomRendered = renderDerivation(bottomDerivation, derivation.bindingsForChild(0), path.choose(0))
      rule1(derivation, bottomRendered, "RAA", label, path = path)
    case LeftDisjunctionIntroduction(leftDerivation, _) =>
      val leftRendered = renderDerivation(leftDerivation, bindings, path.choose(0))
      rule1(derivation, leftRendered, "∨I", path = path)
    case RightDisjunctionIntroduction(_, rightDerivation) =>
      val rightRendered = renderDerivation(rightDerivation, bindings, path.choose(0))
      rule1(derivation, rightRendered, "∨I", path = path)
    case DisjunctionElimination(disjunctionDerivation, leftLabel, leftDerivation, rightLabel, rightDerivation) =>
      val disjunctionRendered = renderDerivation(disjunctionDerivation, bindings, path.choose(0))
      val leftRendered = renderDerivation(leftDerivation, derivation.bindingsForChild(1), path.choose(1))
      val rightRendered = renderDerivation(rightDerivation, derivation.bindingsForChild(2), path.choose(2))
      val compositiveLabel = (leftLabel.toSeq ++ rightLabel).mkString(" ") match {
        case "" => None;
        case s => Some(s)
      }
      rule3(derivation, disjunctionRendered, leftRendered, rightRendered, "∨E", compositiveLabel, path = path)
  }

  private def rule1(parent: Derivation,
                    child: TagMod,
                    rightLabel: Label,
                    leftLabel: Option[Label] = None,
                    path: DerivationPath): VdomNode =
    rule(parent, Seq(child), rightLabel, leftLabel, path)

  private def rule2(parent: Derivation,
                    child1: VdomNode,
                    child2: VdomNode,
                    rightLabel: Label,
                    leftLabel: Option[Label] = None,
                    path: DerivationPath): VdomNode =
    rule(parent, Seq(child1, child2), rightLabel, leftLabel, path)

  private def rule3(parent: Derivation,
                    child1: VdomNode,
                    child2: VdomNode,
                    child3: VdomNode,
                    rightLabel: Label,
                    leftLabel: Option[Label] = None,
                    path: DerivationPath): VdomNode =
    rule(parent, Seq(child1, child2, child3), rightLabel, leftLabel, path)

  private def rule(parent: Derivation,
                   children: Seq[TagMod],
                   rightLabel: Label,
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
          LeftRuleLabel.component(LeftRuleLabelProps(label, getDischargableFormula(parent)))),
        <.div(^.`class` := "rule-bottom-main",
          props.manipulationInfo match {
            case None =>
              <.span(parent.formula.toString)
            case Some(manipulationInfo) =>
              ManipulatableFormula.component(ManipulatableFormula.Props(parent, path, manipulationInfo))
          }
        ),
        <.div(^.`class` := "rule-bottom-right-label", rightLabel)
      )
    )
  }

  private def getDischargableFormula(derivation: Derivation): Map[Label, Formula] =
    derivation.children.indices.map(derivation.bindingsForChild).fold(Map.empty)(_ ++ _)

}
