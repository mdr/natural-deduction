package naturalDeduction.html

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation.Axiom
import naturalDeduction.Formula.{Conjunction, Equivalence, Implication}
import naturalDeduction.{Derivation, DerivationPath, EquivalenceDirection, Formula, Label}

object ManipulatableFormula {

  val component = ScalaComponent.builder[Props]("ManipulatableFormula")
    .render_P(render)
    .build

  case class Props(derivation: Derivation,
                   path: DerivationPath,
                   manipulationInfo: ManipulationInfo,
                   label: Option[Label] = None,
                   isDischarged: Boolean = false,
                   bindings: Map[Label, Formula] = Map.empty)

  def render(props: Props): VdomNode = {
    import props._
    import manipulationInfo._
    val inlineableDerivationIndices: Seq[DerivationIndex] =
      formulaToDerivationIndices
        .getOrElse(derivation.conclusion, Seq.empty)
        .filter(_ => derivation.isAxiom)
        .filter(i => i != derivationIndex)
        .sorted
    val dischargeableLabels: Seq[String] =
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
            derivation.conclusion.toString
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
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Introduction (pick forwards)", ^.onClick --> onEquivalenceElimBackwards(path, EquivalenceDirection.Forwards))
          .when(canEquivalenceElimBackwards(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Introduction (pick backwards)", ^.onClick --> onEquivalenceElimBackwards(path, EquivalenceDirection.Backwards))
          .when(canEquivalenceElimBackwards(derivation)),

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
          .when(path.isRoot && derivation.conclusion.isInstanceOf[Implication]),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Introduction (as forward implication)", ^.onClick --> onEquivalenceIntroForwards(EquivalenceDirection.Forwards))
          .when(canEquivalenceIntroForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Introduction (as backwards implication)", ^.onClick --> onEquivalenceIntroForwards(EquivalenceDirection.Backwards))
          .when(canEquivalenceIntroForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Elimination (pick forwards)", ^.onClick --> onEquivalenceElimForwards(EquivalenceDirection.Forwards))
          .when(canEquivalenceElimForwards(derivation, path)),
        <.div(^.className := "dropdown-item", ^.href := "#", "↔-Elimination (pick backwards)", ^.onClick --> onEquivalenceElimForwards(EquivalenceDirection.Backwards))
          .when(canEquivalenceElimForwards(derivation, path)),

        <.div(^.`class` := "dropdown-divider")
          .when(otherActionsPossible && (forwardsRulesPossible || backwardsRulesPossible)),

        <.h6(^.className := "dropdown-header", "Other")
          .when(otherActionsPossible),
        <.div(^.className := "dropdown-item", ^.href := "#", "Remove subderivation", ^.onClick --> onRemoveDerivation(path))
          .when(!derivation.isAxiom),
        <.div(^.className := "dropdown-item", ^.href := "#", "Extract subderivation", ^.onClick --> onExtractSubderivation(path))
          .when(!path.isRoot),
        inlineableDerivationIndices.toVdomArray(i =>
          <.div(^.className := "dropdown-item", ^.href := "#", s"Inline derivation #${i + 1}", ^.onClick --> onInlineDerivation(path, i))
        ),
        dischargeableLabels.toVdomArray(label =>
          <.div(^.className := "dropdown-item", ^.href := "#", s"Discharge assumption $label", ^.onClick --> onDischargeAssumption(path, label))),
        <.div(^.className := "dropdown-item", ^.href := "#", s"Undischarge assumption", ^.onClick --> onUndischargeAssumption(path))
          .when(canUndischargeAxiom(derivation)),
        <.div(^.className := "dropdown-item", ^.href := "#", "β-reduce", ^.onClick --> onBetaReduce(path))
          .when(canBetaReduce),
      )
    )
  }

  private def canUndischargeAxiom(derivation: Derivation): Boolean = PartialFunction.cond(derivation) {
    case Axiom(_, Some(_)) => true
  }

  private def canConjunctionElimForwards(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.conclusion.isInstanceOf[Conjunction]

  private def canEquivalenceIntroForwards(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.conclusion.isInstanceOf[Implication]

  private def canEquivalenceElimForwards(derivation: Derivation, path: DerivationPath): Boolean =
    path.isRoot && derivation.conclusion.isInstanceOf[Equivalence]

  private def canConjunctionIntroBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.conclusion.isInstanceOf[Conjunction]

  private def canEquivalenceIntroBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.conclusion.isInstanceOf[Equivalence]

  private def canEquivalenceElimBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.conclusion.isInstanceOf[Implication]

  private def canImplicationIntroBackwards(derivation: Derivation): Boolean =
    derivation.isAxiom && derivation.conclusion.isInstanceOf[Implication]

}
