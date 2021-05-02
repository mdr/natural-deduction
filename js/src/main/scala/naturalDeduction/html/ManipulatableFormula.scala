package naturalDeduction.html

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation.Axiom
import naturalDeduction.Formula.{Conjunction, Implication}
import naturalDeduction.{Derivation, DerivationPath, Formula, Label}

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
        .getOrElse(derivation.formula, Seq.empty)
        .filter(_ => derivation.isAxiom)
        .filter(i => i != derivationIndex)
        .sorted
    val dischargeableLabels: Seq[String] =
      bindings
        .groupMap(_._2)(_._1)
        .getOrElse(derivation.formula, Seq.empty)
        .filter(_ => derivation.isAxiom && label.isEmpty)
        .toSeq
        .sorted
    val forwardsRulesPossible = path.isRoot
    val backwardsRulesPossible = derivation.isAxiom
    val otherActionsPossible = !derivation.isAxiom || inlineableDerivationIndices.nonEmpty ||
      dischargeableLabels.nonEmpty || canUndischargeAxiom(derivation)
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
            derivation.formula.toString
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
          .when(path.isRoot && derivation.formula.isInstanceOf[Implication]),

        <.div(^.`class` := "dropdown-divider")
          .when(otherActionsPossible && (forwardsRulesPossible || backwardsRulesPossible)),

        <.h6(^.className := "dropdown-header", "Other")
          .when(otherActionsPossible),
        <.div(^.className := "dropdown-item", ^.href := "#", "Remove subderivation", ^.onClick --> onRemoveDerivation(path))
          .when(!derivation.isAxiom),
        inlineableDerivationIndices.toVdomArray(i =>
          <.div(^.className := "dropdown-item", ^.href := "#", s"Inline derivation #${i + 1}", ^.onClick --> onInlineDerivation(path, i))
        ),
        dischargeableLabels.toVdomArray(label =>
          <.div(^.className := "dropdown-item", ^.href := "#", s"Discharge assumption $label", ^.onClick --> onDischargeAssumption(path, label))),
        <.div(^.className := "dropdown-item", ^.href := "#", s"Undischarge assumption", ^.onClick --> onUndischargeAssumption(path))
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

}
