package naturalDeduction

import japgolly.scalajs.react.vdom.TagMod
import naturalDeduction.Derivation._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Formula.Conjunction

case class DerivationManipulationCallbacks(
                                            onRemoveDerivation: DerivationPath => Callback,
                                            onConjunctionIntroBackwards: DerivationPath => Callback,
                                            onConjunctionElimForwards: (DerivationPath, Int) => Callback,
                                            onConjunctionElimBackwards: DerivationPath => Callback
                                          )

case class DerivationProps(derivation: Derivation, callbacks: Option[DerivationManipulationCallbacks] = None)

object DerivationComponent {

  val component = ScalaComponent.builder[DerivationProps]("DerivationComponent")
    .render_P(props => {
      val derivationRenderer = new DerivationHtmlRenderer(props)
      derivationRenderer.renderDerivation(props.derivation, Set.empty, DerivationPath.empty)
    })
    .build

}


class DerivationHtmlRenderer(props: DerivationProps) {

  def renderDerivation(derivation: Derivation, labels: Set[String] = Set.empty, path: DerivationPath): VdomNode = derivation match {
    case Axiom(formula, label) =>
      val isDischarged = labels.intersect(label.toSet).nonEmpty
      props.callbacks match {
        case None => formula.toString
        case Some(callbacks) =>
          if (isDischarged) <.span(<.span(^.cls := "discharged-axiom", formula.toString), <.sup(label.get)) else renderManipulatableFormula(derivation, path, callbacks)
      }
    case ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      rule2(derivation, renderDerivation(leftDerivation, labels, path.choose(0)), renderDerivation(rightDerivation, labels, path.choose(1)), "∧I", path = path)
    case LeftConjunctionElimination(conjunctionDerivation) =>
      rule1(derivation, renderDerivation(conjunctionDerivation, labels, path.choose(0)), "∧E", path = path)
    case RightConjunctionElimination(conjunctionDerivation) =>
      rule1(derivation, renderDerivation(conjunctionDerivation, labels, path.choose(0)), "∧E", path = path)
    case ImplicationIntroduction(_, label, consequentDerivation) =>
      rule1(derivation, renderDerivation(consequentDerivation, labels ++ label, path.choose(0)), "→I", label, path = path)
    case ImplicationElimination(antecedentDerivation, implicationDerivation) =>
      rule2(derivation, renderDerivation(antecedentDerivation, labels, path.choose(0)), renderDerivation(implicationDerivation, labels, path.choose(1)), "→E", path = path)
    case EquivalenceIntroduction(forwardsDerivation, backwardsDerivation) =>
      rule2(derivation, renderDerivation(forwardsDerivation, labels, path.choose(0)), renderDerivation(backwardsDerivation, labels, path.choose(1)), "↔I", path = path)
    case ForwardsEquivalenceElimination(equivalenceDerivation) =>
      rule1(derivation, renderDerivation(equivalenceDerivation, labels, path.choose(0)), "↔E", path = path)
    case BackwardsEquivalenceElimination(equivalenceDerivation) =>
      rule1(derivation, renderDerivation(equivalenceDerivation, labels, path.choose(0)), "↔E", path = path)
    case NegationIntroduction(_, label, bottomDerivation) =>
      rule1(derivation, renderDerivation(bottomDerivation, labels ++ label, path.choose(0)), "¬I", label, path = path)
    case NegationElimination(positiveDerivation, negativeDerivation) =>
      rule2(derivation, renderDerivation(positiveDerivation, labels, path.choose(0)), renderDerivation(negativeDerivation, labels, path.choose(1)), "¬E", path = path)
    case ReductioAdAbsurdum(_, label, bottomDerivation) =>
      rule1(derivation, renderDerivation(bottomDerivation, labels ++ label, path.choose(0)), "RAA", label, path = path)
    case LeftDisjunctionIntroduction(leftDerivation, _) =>
      rule1(derivation, renderDerivation(leftDerivation, labels, path.choose(0)), "∨I", path = path)
    case RightDisjunctionIntroduction(_, rightDerivation) =>
      rule1(derivation, renderDerivation(rightDerivation, labels, path.choose(0)), "∨I", path = path)
    case DisjunctionElimination(disjunctionDerivation, leftLabel, leftDerivation, rightLabel, rightDerivation) =>
      rule3(
        derivation,
        renderDerivation(disjunctionDerivation, labels, path.choose(0)),
        renderDerivation(leftDerivation, labels ++ leftLabel, path.choose(1)),
        renderDerivation(rightDerivation, labels ++ rightLabel, path.choose(2)),
        "∨E",
        (leftLabel.toSeq ++ rightLabel).mkString(" ") match { case "" => None; case s => Some(s) },
        path = path)
  }

  private def rule1(parent: Derivation, child: TagMod, rightLabel: String, leftLabel: Option[String] = None, path: DerivationPath): VdomNode =
    rule(parent, Seq(child), rightLabel, leftLabel, path)

  private def rule2(parent: Derivation, child1: VdomNode, child2: VdomNode, rightLabel: String, leftLabel: Option[String] = None, path: DerivationPath): VdomNode =
    rule(parent, Seq(child1, child2), rightLabel, leftLabel, path)

  private def rule3(parent: Derivation, child1: VdomNode, child2: VdomNode, child3: VdomNode, rightLabel: String, leftLabel: Option[String] = None, path: DerivationPath): VdomNode =
    rule(parent, Seq(child1, child2, child3), rightLabel, leftLabel, path)

  def intersperse[A](a: Seq[A], b: Seq[A]): Seq[A] = a match {
    case Seq(first, rest@_*) => first +: intersperse(b, rest)
    case _ => b
  }

  private def renderManipulatableFormula(derivation: Derivation, path: DerivationPath, callbacks: DerivationManipulationCallbacks): VdomNode =
    <.a(^.`class` := "dropdown link-secondary",
      <.div(
        ^.`type` := "button",
        ^.id := "ruleActionMenuTrigger",
        CustomAttributes.dataToggle := "dropdown",
        CustomAttributes.ariaHasPopup := "true",
        CustomAttributes.ariaExpanded := "false",
        derivation.formula.toString),
      <.div(^.className := "dropdown-menu", CustomAttributes.ariaLabelledBy := "ruleActionMenuTrigger",
        <.h6(^.className := "dropdown-header", "Forwards rule"),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Elimination (pick left)", ^.onClick --> callbacks.onConjunctionElimForwards(path, 0))
          .when(canConjunctionElimForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Elimination (pick right)", ^.onClick --> callbacks.onConjunctionElimForwards(path, 1))
          .when(canConjunctionElimForwards(derivation, path)),
        <.div(^.`class` := "dropdown-divider"),
        <.h6(^.className := "dropdown-header", "Backwards rule"),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Introduction", ^.onClick --> callbacks.onConjunctionIntroBackwards(path))
          .when(canConjunctionIntroBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Elimination...", ^.onClick --> callbacks.onConjunctionElimBackwards(path))
          .when(derivation.isAxiom),
        <.div(^.`class` := "dropdown-divider"),
        <.h6(^.className := "dropdown-header", "Other"),
        <.div(^.className := "dropdown-item", ^.href := "#", "Remove sub-derivation", ^.onClick --> callbacks.onRemoveDerivation(path))
          .when(!derivation.isAxiom),
      )
    )

  private def canConjunctionElimForwards(derivation: Derivation, path: DerivationPath): Boolean =
    path == DerivationPath.empty && derivation.formula.isInstanceOf[Conjunction]

  private def canConjunctionIntroBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.formula.isInstanceOf[Conjunction]

  private def rule(parent: Derivation, children: Seq[TagMod], rightLabel: String, leftLabel: Option[String] = None, path: DerivationPath): VdomNode = {
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
          props.callbacks match {
            case None => <.span(parent.formula.toString)
            case Some(callbacks) =>
              renderManipulatableFormula(parent, path, callbacks)
          }
        ),
        <.div(^.`class` := "rule-bottom-right-label", rightLabel)
      )
    )
  }

  private def getDischargableFormula(derivation: Derivation): Map[String, Formula] = derivation match {
    case ImplicationIntroduction(antecedent, Some(label), _) => Map(label -> antecedent)
    case NegationIntroduction(statement, Some(label), _) => Map(label -> statement)
    case raa@ReductioAdAbsurdum(_, Some(label), _) => Map(label -> raa.negation)
    case de@DisjunctionElimination(_, leftLabel, _, rightLabel, _) =>
      leftLabel.map(label => Map(label -> de.disjunction.disjunct1)).getOrElse(Map.empty) ++
        rightLabel.map(label => Map(label -> de.disjunction.disjunct2)).getOrElse(Map.empty)
    case _ => Map.empty
  }

}
