package naturalDeduction.html

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation.Axiom
import naturalDeduction.Formula.{Conjunction, Disjunction, Equivalence, Implication, Negation, ⊥}
import naturalDeduction.pretty.FormulaPrettyPrinter
import naturalDeduction.{Derivation, DerivationPath, EquivalenceDirection, Formula, Label}

import scala.PartialFunction.cond

object ManipulatableFormula {

  //noinspection TypeAnnotation
  val component = ScalaComponent.builder[Props]("ManipulatableFormula")
    .render_P(render)
    .build

  case class Props(derivation: Derivation,
                   path: DerivationPath,
                   manipulationInfo: ManipulationInfo,
                   label: Option[Label] = None,
                   isDischarged: Boolean = false,
                   bindings: Map[Label, Formula] = Map.empty) {
    def make: VdomNode = component(this)
  }

  def render(props: Props): VdomNode = {
    import props._
    import manipulationInfo._
    val inlineableDerivationIndices: Seq[DerivationIndex] =
      formulaToDerivationIndices
        .getOrElse(derivation.conclusion, Seq.empty)
        .filter(_ => derivation.isAxiom)
        .filter(_ != derivationIndex)
        .sorted
    val dischargeableLabels: Seq[Label] =
      bindings
        .groupMap(_._2)(_._1)
        .getOrElse(derivation.conclusion, Seq.empty)
        .filter(_ => derivation.isAxiom && label.isEmpty)
        .toSeq
        .sorted
    val canBetaReduce = derivation.isBetaRedex
    val forwardsRulesPossible = path.isRoot
    val backwardsRulesPossible = derivation.isAxiom
    val otherActionsPossible = !derivation.isAxiom || inlineableDerivationIndices.nonEmpty ||
      dischargeableLabels.nonEmpty || canUndischargeAxiom(derivation) || canBetaReduce || !path.isRoot
    <.a(^.`class` := "dropdown link-secondary",
      <.div(
        ^.className := "rule-formula",
        ^.`type` := "button",
        ^.id := "ruleActionMenuTrigger",
        CustomAttributes.dataToggle := "dropdown",
        CustomAttributes.ariaHasPopup := "true",
        CustomAttributes.ariaExpanded := "false",
        <.span(
          <.span(
            (^.cls := "discharged-axiom").when(isDischarged),
            FormulaPrettyPrinter.prettyPrint(derivation.conclusion),
          ),
          label.toTagMod(l => <.sup(l))
        ),
      ),
      <.div(^.className := "dropdown-menu", CustomAttributes.ariaLabelledBy := "ruleActionMenuTrigger",

        <.h6(^.className := "dropdown-header", "↑ Apply rule backwards")
          .when(backwardsRulesPossible),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Introduction", ^.onClick --> onConjunctionIntroBackwards(path))
          .when(canConjunctionIntroBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Elimination...", ^.onClick --> onConjunctionElimBackwards(path))
          .when(derivation.isAxiom),
        <.div(^.className := "dropdown-item", ^.href := "#", "→-Introduction", ^.onClick --> onImplicationIntroBackwards(path))
          .when(canImplicationIntroBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "→-Elimination...", ^.onClick --> onImplicationElimBackwards(path))
          .when(derivation.isAxiom),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Introduction", ^.onClick --> onEquivalenceIntroBackwards(path))
          .when(canEquivalenceIntroBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Elimination (pick forwards)", ^.onClick --> onEquivalenceElimBackwards(path, EquivalenceDirection.Forwards))
          .when(canEquivalenceElimBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Elimination (pick backwards)", ^.onClick --> onEquivalenceElimBackwards(path, EquivalenceDirection.Backwards))
          .when(canEquivalenceElimBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "¬-Introduction", ^.onClick --> onNegationIntroBackwards(path))
          .when(canNegationIntroBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "¬-Elimination...", ^.onClick --> onNegationElimBackwards(path))
          .when(canNegationElimBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "Reductio ad absurdum", ^.onClick --> onReductioBackwards(path))
          .when(derivation.isAxiom),
        <.div(^.className := "dropdown-item", ^.href := "#", "∨-Introduction (pick left)", ^.onClick --> onDisjunctionIntroBackwards(path, 0))
          .when(canDisjunctionIntroBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "∨-Introduction (pick right)", ^.onClick --> onDisjunctionIntroBackwards(path, 1))
          .when(canDisjunctionIntroBackwards(derivation)),

        <.div(^.`class` := "dropdown-divider")
          .when(backwardsRulesPossible && forwardsRulesPossible),

        <.h6(^.className := "dropdown-header", "↓ Apply rule forwards")
          .when(forwardsRulesPossible),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Introduction...", ^.onClick --> onConjunctionIntroForwards)
          .when(path.isRoot),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Elimination (pick left)", ^.onClick --> onConjunctionElimForwards(0))
          .when(canConjunctionElimForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "∧-Elimination (pick right)", ^.onClick --> onConjunctionElimForwards(1))
          .when(canConjunctionElimForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "→-Introduction...", ^.onClick --> onImplicationIntroForwards)
          .when(path.isRoot),
        <.div(^.className := "dropdown-item", ^.href := "#", "→-Elimination (as antecedent)...", ^.onClick --> onImplicationElimForwardsFromAntecedent)
          .when(path.isRoot),
        <.div(^.className := "dropdown-item", ^.href := "#", "→-Elimination (as implication)", ^.onClick --> onImplicationElimForwardsFromImplication)
          .when(canImplicationElimForwardsFromImplication(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Introduction (as forward implication)", ^.onClick --> onEquivalenceIntroForwards(EquivalenceDirection.Forwards))
          .when(canEquivalenceIntroForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Introduction (as backwards implication)", ^.onClick --> onEquivalenceIntroForwards(EquivalenceDirection.Backwards))
          .when(canEquivalenceIntroForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Elimination (pick forwards)", ^.onClick --> onEquivalenceElimForwards(EquivalenceDirection.Forwards))
          .when(canEquivalenceElimForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Elimination (pick backwards)", ^.onClick --> onEquivalenceElimForwards(EquivalenceDirection.Backwards))
          .when(canEquivalenceElimForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "¬Introduction...", ^.onClick --> onNegationIntroForwards)
          .when(canNegationIntroForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "¬Elimination (as positive)", ^.onClick --> onNegationElimForwardsFromPositive)
          .when(path.isRoot),
        <.div(^.className := "dropdown-item", ^.href := "#", "¬Elimination (as negative)", ^.onClick --> onNegationElimForwardsFromNegative)
          .when(canNegationElimForwardsFromNegative(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "Reductio ad absurdum...", ^.onClick --> onReductioForwards)
          .when(canReductioForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "∨-Introduction...", ^.onClick --> onDisjunctionIntroForwards)
          .when(path.isRoot),
        <.div(^.className := "dropdown-item", ^.href := "#", "∨-Elimination (as disjunction)...", ^.onClick --> onDisjunctionElimForwardsFromDisjunction)
          .when(canDisjunctionElimForwardsFromDisjunction(derivation, path)),
        <.div(^.`class` := "dropdown-divider")
          .when(otherActionsPossible && (forwardsRulesPossible || backwardsRulesPossible)),

        <.h6(^.className := "dropdown-header", "Other")
          .when(otherActionsPossible),
        <.div(^.className := "dropdown-item", ^.href := "#", "Remove subderivation", ^.onClick --> onRemoveDerivation(path))
          .when(!derivation.isAxiom),
        <.div(^.className := "dropdown-item", ^.href := "#", "Extract subderivation", ^.onClick --> onExtractSubderivation(path))
          .when(!path.isRoot),
        inlineableDerivationIndices.toVdomArray(i =>
          <.div(^.className := "dropdown-item", ^.key := i, ^.href := "#", s"Inline derivation #${i + 1}", ^.onClick --> onInlineDerivation(path, i))
        ),
        dischargeableLabels.toVdomArray(label =>
          <.div(^.className := "dropdown-item", ^.key := label, ^.href := "#", s"Discharge assumption $label", ^.onClick --> onDischargeAssumption(path, label))),
        <.div(^.className := "dropdown-item", ^.href := "#", s"Undischarge assumption", ^.onClick --> onUndischargeAssumption(path))
          .when(canUndischargeAxiom(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "β-reduce", ^.onClick --> onBetaReduce(path))
          .when(canBetaReduce),
      )
    )
  }

  private def canUndischargeAxiom(derivation: Derivation): Boolean = cond(derivation) {
    case Axiom(_, Some(_)) => true
  }

  private def canConjunctionIntroBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.conclusion.isInstanceOf[Conjunction]

  private def canEquivalenceIntroBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.conclusion.isInstanceOf[Equivalence]

  private def canEquivalenceElimBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.conclusion.isInstanceOf[Implication]

  private def canImplicationIntroBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.conclusion.isInstanceOf[Implication]

  private def canNegationIntroBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.conclusion.isInstanceOf[Negation]

  private def canNegationElimBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.conclusion == ⊥

  private def canDisjunctionIntroBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.conclusion.isInstanceOf[Disjunction]

  private def canConjunctionElimForwards(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.conclusion.isInstanceOf[Conjunction]

  private def canImplicationElimForwardsFromImplication(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.conclusion.isInstanceOf[Implication]

  private def canEquivalenceIntroForwards(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.conclusion.isInstanceOf[Implication]

  private def canEquivalenceElimForwards(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.conclusion.isInstanceOf[Equivalence]

  private def canNegationElimForwardsFromNegative(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.conclusion.isInstanceOf[Negation]

  private def canNegationIntroForwards(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.conclusion == ⊥

  private def canReductioForwards(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.conclusion == ⊥

  private def canDisjunctionElimForwardsFromDisjunction(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.conclusion.isInstanceOf[Disjunction]

}
