package naturalDeduction.html

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation._
import naturalDeduction.Formula.{Conjunction, Implication}
import naturalDeduction.{Derivation, DerivationPath, Formula}

case class ManipulationInfo(
                             onRemoveDerivation: DerivationPath => Callback,
                             onConjunctionIntroBackwards: DerivationPath => Callback,
                             onConjunctionElimForwards: (DerivationPath, Int) => Callback,
                             onConjunctionElimBackwards: DerivationPath => Callback,
                             onImplicationIntroBackwards: DerivationPath => Callback,
                             onImplicationElimBackwards: DerivationPath => Callback,
                             onImplicationElimForwardsFromAntecedent: Callback,
                             onInlineDerivation: (DerivationPath, Int) => Callback,
                             onDischargeAssumption: (DerivationPath, String) => Callback,
                             onUndischargeAssumption: DerivationPath => Callback,
                             derivationIndex: Int,
                             formulaToDerivationIndices: Map[Formula, Seq[Int]],
                           )

case class DerivationProps(
                            derivation: Derivation,
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

  def renderDerivation(derivation: Derivation, bindings: Map[String, Formula] = Map.empty, path: DerivationPath = DerivationPath.empty): VdomNode = derivation match {
    case Axiom(formula, label) =>
      val isDischarged = bindings.keySet.intersect(label.toSet).nonEmpty
      props.manipulationInfo match {
        case None => formula.toString
        case Some(manipulationInfo) =>
          <.div(^.className := "rule-axiom", // without this, dropdown menu for derivations that are just axioms don't position correctly...
            if (isDischarged)
              renderManipulatableFormula(derivation, path, manipulationInfo, label)
            else
              renderManipulatableFormula(derivation, path, manipulationInfo)
          )
      }
    case ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      rule2(derivation, renderDerivation(leftDerivation, bindings, path.choose(0)), renderDerivation(rightDerivation, bindings, path.choose(1)), "∧I", path = path)
    case LeftConjunctionElimination(conjunctionDerivation) =>
      rule1(derivation, renderDerivation(conjunctionDerivation, bindings, path.choose(0)), "∧E", path = path)
    case RightConjunctionElimination(conjunctionDerivation) =>
      rule1(derivation, renderDerivation(conjunctionDerivation, bindings, path.choose(0)), "∧E", path = path)
    case ImplicationIntroduction(_, label, consequentDerivation) =>
      rule1(derivation, renderDerivation(consequentDerivation, bindings ++ derivation.bindingsForChild(0), path.choose(0)), "→I", label, path = path)
    case ImplicationElimination(antecedentDerivation, implicationDerivation) =>
      rule2(derivation, renderDerivation(antecedentDerivation, bindings, path.choose(0)), renderDerivation(implicationDerivation, bindings, path.choose(1)), "→E", path = path)
    case EquivalenceIntroduction(forwardsDerivation, backwardsDerivation) =>
      rule2(derivation, renderDerivation(forwardsDerivation, bindings, path.choose(0)), renderDerivation(backwardsDerivation, bindings, path.choose(1)), "↔I", path = path)
    case ForwardsEquivalenceElimination(equivalenceDerivation) =>
      rule1(derivation, renderDerivation(equivalenceDerivation, bindings, path.choose(0)), "↔E", path = path)
    case BackwardsEquivalenceElimination(equivalenceDerivation) =>
      rule1(derivation, renderDerivation(equivalenceDerivation, bindings, path.choose(0)), "↔E", path = path)
    case NegationIntroduction(_, label, bottomDerivation) =>
      rule1(derivation, renderDerivation(bottomDerivation, derivation.bindingsForChild(0), path.choose(0)), "¬I", label, path = path)
    case NegationElimination(positiveDerivation, negativeDerivation) =>
      rule2(derivation, renderDerivation(positiveDerivation, bindings, path.choose(0)), renderDerivation(negativeDerivation, bindings, path.choose(1)), "¬E", path = path)
    case ReductioAdAbsurdum(_, label, bottomDerivation) =>
      rule1(derivation, renderDerivation(bottomDerivation, derivation.bindingsForChild(0), path.choose(0)), "RAA", label, path = path)
    case LeftDisjunctionIntroduction(leftDerivation, _) =>
      rule1(derivation, renderDerivation(leftDerivation, bindings, path.choose(0)), "∨I", path = path)
    case RightDisjunctionIntroduction(_, rightDerivation) =>
      rule1(derivation, renderDerivation(rightDerivation, bindings, path.choose(0)), "∨I", path = path)
    case DisjunctionElimination(disjunctionDerivation, leftLabel, leftDerivation, rightLabel, rightDerivation) =>
      rule3(
        derivation,
        renderDerivation(disjunctionDerivation, bindings, path.choose(0)),
        renderDerivation(leftDerivation, derivation.bindingsForChild(1), path.choose(1)),
        renderDerivation(rightDerivation, derivation.bindingsForChild(2), path.choose(2)),
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

  private def renderManipulatableFormula(derivation: Derivation, path: DerivationPath, manipulationInfo: ManipulationInfo, dischargeLabel: Option[String] = None): VdomNode = {
    val inlineableDerivationIndices =
      manipulationInfo.formulaToDerivationIndices.getOrElse(derivation.formula, Seq.empty).filter(_ => derivation.isAxiom).filter(i => i != manipulationInfo.derivationIndex).sorted
    val dischargeableLabels = props.derivation.bindingsAtPath(path).groupMap(_._2)(_._1).getOrElse(derivation.formula, Seq.empty).filter(_ => derivation.isAxiom && !canUndischargeAxiom(derivation)).toSeq.sorted
    val forwardsRulesPossible = path.isRoot
    val backwardsRulesPossible = derivation.isAxiom
    val otherActionsPossible = !derivation.isAxiom || inlineableDerivationIndices.nonEmpty || dischargeableLabels.nonEmpty || canUndischargeAxiom(derivation)
    <.a(^.`class` := "dropdown link-secondary",
      <.div(
        ^.`type` := "button",
        ^.id := "ruleActionMenuTrigger",
        CustomAttributes.dataToggle := "dropdown",
        CustomAttributes.ariaHasPopup := "true",
        CustomAttributes.ariaExpanded := "false",
        dischargeLabel match {
          case Some(label) => <.span(<.span(^.cls := "discharged-axiom", derivation.formula.toString), <.sup(label))
          case None => derivation.formula.toString
        }),
      <.div(^.className := "dropdown-menu", CustomAttributes.ariaLabelledBy := "ruleActionMenuTrigger",
        <.h6(^.className := "dropdown-header", "↓ Apply rule forwards")
          .when(forwardsRulesPossible),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Elimination (pick left)", ^.onClick --> manipulationInfo.onConjunctionElimForwards(path, 0))
          .when(canConjunctionElimForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Elimination (pick right)", ^.onClick --> manipulationInfo.onConjunctionElimForwards(path, 1))
          .when(canConjunctionElimForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "→-Elimination (as antecedent)...", ^.onClick --> manipulationInfo.onImplicationElimForwardsFromAntecedent)
          .when(path.isRoot),

        <.div(^.`class` := "dropdown-divider")
          .when(forwardsRulesPossible && backwardsRulesPossible),

        <.h6(^.className := "dropdown-header", "↑ Apply rule backwards")
          .when(backwardsRulesPossible),
        <.div(^.className := "dropdown-item", ^.href := "#", "→-Introduction", ^.onClick --> manipulationInfo.onImplicationIntroBackwards(path))
          .when(canImplicationIntroBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "→-Elimination...", ^.onClick --> manipulationInfo.onImplicationElimBackwards(path))
          .when(derivation.isAxiom),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Introduction", ^.onClick --> manipulationInfo.onConjunctionIntroBackwards(path))
          .when(canConjunctionIntroBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Elimination...", ^.onClick --> manipulationInfo.onConjunctionElimBackwards(path))
          .when(derivation.isAxiom),

        <.div(^.`class` := "dropdown-divider")
          .when(otherActionsPossible && (forwardsRulesPossible || backwardsRulesPossible)),

        <.h6(^.className := "dropdown-header", "Other")
          .when(otherActionsPossible),
        <.div(^.className := "dropdown-item", ^.href := "#", "Remove subderivation", ^.onClick --> manipulationInfo.onRemoveDerivation(path))
          .when(!derivation.isAxiom),
        inlineableDerivationIndices.toVdomArray(i =>
          <.div(^.className := "dropdown-item", ^.href := "#", s"Inline derivation #${i + 1}", ^.onClick --> manipulationInfo.onInlineDerivation(path, i))
        ),
        dischargeableLabels.toVdomArray(label =>
          <.div(^.className := "dropdown-item", ^.href := "#", s"Discharge assumption $label", ^.onClick --> manipulationInfo.onDischargeAssumption(path, label))),
        <.div(^.className := "dropdown-item", ^.href := "#", s"Undischarge assumption", ^.onClick --> manipulationInfo.onUndischargeAssumption(path))
          .when(canUndischargeAxiom(derivation))
      )
    )
  }

  private def canUndischargeAxiom(derivation: Derivation): Boolean = PartialFunction.cond(derivation) {
    case Axiom(_, Some(_)) => true
  }

  private def canConjunctionElimForwards(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.formula.isInstanceOf[Conjunction]

  private def canConjunctionIntroBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.formula.isInstanceOf[Conjunction]

  private def canImplicationIntroBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.formula.isInstanceOf[Implication]

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
          props.manipulationInfo match {
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
