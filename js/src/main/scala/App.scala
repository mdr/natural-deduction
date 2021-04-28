import App.derivation
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^._
import naturalDeduction.Derivation
import naturalDeduction.Derivation._
import naturalDeduction.Formula.PropositionalVariable

object App {
  val dataToggle = VdomAttr("data-toggle")

  case class State()

  class Backend($: BackendScope[Unit, State]) {

    def render(state: State) = {
      <.div(^.`class` := "container-fluid p-0",
        <.div(^.className := "d-flex flex-column flex-md-row align-items-center py-3 mb-3 bg-white border-bottom box-shadow",
          <.div(^.`class` := "container",
            <.div(^.className := "d-flex flex-column flex-md-row align-items-center ",
              <.h5(^.className := "my-0 mr-md-auto font-weight-normal", "Natural Deduction"),
            ),
          ),
        ),
        <.div(^.`class` := "container",
          <.p(),
          <.p("An implementation of natural deduction proofs as described in ", <.em("Mathematical Logic"), " by Ian Chiswell and Wilfrid Hodges."),
          derivationCard(derivation),
          <.br(),
          derivationCard(derivation2)))
    }

  }

  def derivationCard(derivation: Derivation) =
    <.div(^.`class` := "card",
      <.div(^.`class` := "card-header", derivation.sequent.toString),
      <.div(^.`class` := "card-body",
        renderDerivation(derivation)))

  def renderDerivation(derivation: Derivation, labels: Set[String] = Set.empty): TagMod = derivation match {
    case Axiom(formula, label) =>
      val isDischarged = labels.intersect(label.toSet).nonEmpty
      if (isDischarged) <.span(<.span(^.cls := "discharged-axiom", formula.toString), <.sup(label.get)) else formula.toString
    case ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      rule2(derivation, renderDerivation(leftDerivation, labels), renderDerivation(rightDerivation, labels), "∧I")
    case LeftConjunctionElimination(conjunctionDerivation) =>
      rule1(derivation, renderDerivation(conjunctionDerivation, labels), "∧E")
    case RightConjunctionElimination(conjunctionDerivation) =>
      rule1(derivation, renderDerivation(conjunctionDerivation, labels), "∧E")
    case ImplicationIntroduction(_, label, consequentDerivation) =>
      rule1(derivation, renderDerivation(consequentDerivation, labels ++ label), "→I", label)
    case ImplicationElimination(antecedentDerivation, implicationDerivation) =>
      rule2(derivation, renderDerivation(antecedentDerivation, labels), renderDerivation(implicationDerivation, labels), "→E")
    case EquivalenceIntroduction(forwardsDerivation, backwardsDerivation) =>
      rule2(derivation, renderDerivation(forwardsDerivation, labels), renderDerivation(backwardsDerivation, labels), "↔I")
    case ForwardsEquivalenceElimination(equivalenceDerivation) =>
      rule1(derivation, renderDerivation(equivalenceDerivation, labels), "↔E")
    case BackwardsEquivalenceElimination(equivalenceDerivation) =>
      rule1(derivation, renderDerivation(equivalenceDerivation, labels), "↔E")
    case NegationIntroduction(_, label, bottomDerivation) =>
      rule1(derivation, renderDerivation(bottomDerivation, labels ++ label), "¬I", label)
    case NegationElimination(positiveDerivation, negativeDerivation) =>
      rule2(derivation, renderDerivation(positiveDerivation, labels), renderDerivation(negativeDerivation, labels), "¬E")
    case ReductioAdAbsurdum(_, label, bottomDerivation) =>
      rule1(derivation, renderDerivation(bottomDerivation, labels ++ label), "RAA", label)
    case LeftDisjunctionIntroduction(leftDerivation, _) =>
      rule1(derivation, renderDerivation(leftDerivation, labels), "∨I")
    case RightDisjunctionIntroduction(_, rightDerivation) =>
      rule1(derivation, renderDerivation(rightDerivation, labels), "∨I")
    case DisjunctionElimination(disjunctionDerivation, leftLabel, leftDerivation, rightLabel, rightDerivation) =>
      rule3(
        derivation,
        renderDerivation(disjunctionDerivation, labels),
        renderDerivation(leftDerivation, labels ++ leftLabel),
        renderDerivation(rightDerivation, labels ++ rightLabel),
        "∨E",
        (leftLabel.toSeq ++ rightLabel).mkString(" ") match { case "" => None; case s => Some(s) })
  }

  private def rule1(parent: Derivation, child: TagMod, rightLabel: String, leftLabel: Option[String] = None): TagMod =
    rule(parent, Seq(child), rightLabel, leftLabel)

  private def rule2(parent: Derivation, child1: TagMod, child2: TagMod, rightLabel: String, leftLabel: Option[String] = None): TagMod =
    rule(parent, Seq(child1, child2), rightLabel, leftLabel)

  private def rule3(parent: Derivation, child1: TagMod, child2: TagMod, child3: TagMod, rightLabel: String, leftLabel: Option[String] = None): TagMod =
    rule(parent, Seq(child1, child2, child3), rightLabel, leftLabel)

  def intersperse[A](a: Seq[A], b: Seq[A]): Seq[A] = a match {
    case Seq(first, rest@_*) => first +: intersperse(b, rest)
    case _ => b
  }

  private def rule(parent: Derivation, children: Seq[TagMod], rightLabel: String, leftLabel: Option[String] = None): TagMod = {
    <.div(^.`class` := "rule",
      <.div(^.`class` := "rule-top",
        <.div(^.`class` := "rule-top-left-label").when(leftLabel.isDefined),
        <.div(^.`class` := "rule-top-main",
          children.map(child =>
            <.div(^.`class` := "rule-top-main-part", child))
            .mkTagMod(<.div(^.`class` := "rule-top-main-spacer"))),
        <.div(^.`class` := "rule-top-right-label")),
      <.div(^.`class` := "rule-bottom",
        leftLabel.whenDefined(l => <.div(^.`class` := "rule-bottom-left-label", l)),
        <.div(^.`class` := "rule-bottom-main", parent.formula.toString),
        <.div(^.`class` := "rule-bottom-right-label", rightLabel)))
  }

  private val φ = PropositionalVariable("φ")
  private val ψ = PropositionalVariable("ψ")
  private val χ = PropositionalVariable("χ")

  val derivation: Derivation =
    ImplicationIntroduction(φ.not.not, "❷",
      ReductioAdAbsurdum(φ, "❶",
        NegationElimination(
          Axiom(φ.not, "❶"),
          Axiom(φ.not.not, "❷")
        )))

  val derivation2 =
    ImplicationIntroduction(ψ → χ, "❸",
      ImplicationIntroduction(φ → ψ, "❷",
        ImplicationIntroduction(φ, "❶",
          ImplicationElimination(
            ImplicationElimination(Axiom(φ, "❶"), Axiom(φ → ψ, "❷")),
            Axiom(ψ → χ, "❸")))))


  val app = ScalaComponent.builder[Unit]("App")
    .initialState(State())
    .renderBackend[Backend]
    .build

}
