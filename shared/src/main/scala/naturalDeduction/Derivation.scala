package naturalDeduction

import naturalDeduction.Derivation._
import naturalDeduction.pretty.DerivationRenderer

import java.util.regex.{MatchResult, Pattern}
import Formula._

object Sequent {

  implicit class RichSet[T <: Formula](assumptions: Set[T]) {
    def ⊢(conclusion: Formula): Sequent = Sequent(assumptions.toSet, conclusion)
  }

}

case class Sequent(assumptions: Set[Formula], conclusion: Formula) {
  override def toString: String = s"${assumptions.mkString(", ")} ⊢ $conclusion"
}

case class Assumptions(anonymousAssumptions: Set[Formula] = Set.empty, labelledAssumptions: Map[String, Formula] = Map.empty) {

  def discharge(label: String): Assumptions = copy(labelledAssumptions = labelledAssumptions - label)

  def ++(that: Assumptions): Assumptions = {
    val commonLabels = this.labelledAssumptions.keySet intersect that.labelledAssumptions.keySet
    for (label <- commonLabels) {
      val thisFormula = this.labelledAssumptions(label)
      val thatFormula = that.labelledAssumptions(label)
      assert(thisFormula == thatFormula, s"Error combining assumptions, mismatch for label $label: $thisFormula vs $thatFormula")
    }

    Assumptions(this.anonymousAssumptions ++ that.anonymousAssumptions, this.labelledAssumptions ++ that.labelledAssumptions)
  }

  def allFormulae: Set[Formula] = anonymousAssumptions ++ labelledAssumptions.values.toSet

}

sealed trait Derivation {
  def formula: Formula

  def undischargedAssumptions: Assumptions

  def sequent: Sequent = Sequent(undischargedAssumptions.allFormulae, formula)

  override def toString: String = {
    val rendered = DerivationRenderer.renderDerivation(this).toStringNormal
    return rendered
    val withStrikethrough = "-(.+?)-".r.replaceAllIn(rendered, result => " " + unicodeStrikeThrough(result.group(1)) + " ")
    """(?m)(\s+)$""".r.replaceAllIn(withStrikethrough, "")
  }

  private def unicodeStrikeThrough(s: String): String = s.flatMap(c => s"$c\u0336")

  def implicationIntro(formula: Formula, label: Option[String] = None): ImplicationIntroduction =
    ImplicationIntroduction(formula, label, this)

  def implicationIntro(formula: Formula, label: String): ImplicationIntroduction =
    ImplicationIntroduction(formula, label, this)

  def implicationElim(that: Derivation): ImplicationElimination = ImplicationElimination(this, that)

  def conjunctionIntro(that: Derivation): ConjunctionIntroduction = ConjunctionIntroduction(this, that)

  def leftConjunctionElim: LeftConjunctionElimination = LeftConjunctionElimination(this)

  def rightConjunctionElim: RightConjunctionElimination = RightConjunctionElimination(this)

  def equivalenceIntro(that: Derivation): EquivalenceIntroduction = EquivalenceIntroduction(this, that)

  def forwardsEquivalenceElim: ForwardsEquivalenceElimination = ForwardsEquivalenceElimination(this)

  def backwardsEquivalenceElim: BackwardsEquivalenceElimination = BackwardsEquivalenceElimination(this)

  def negationIntro(formula: Formula, label: Option[String] = None): NegationIntroduction =
    NegationIntroduction(formula, label, this)

  def negationIntro(formula: Formula, label: String): NegationIntroduction =
    NegationIntroduction(formula, label, this)

  def negationElim(that: Derivation): NegationElimination = NegationElimination(this, that)

}

object Derivation {

  implicit class RichFormula(formula: Formula) {
    def axiom: Axiom = Axiom(formula)
  }

  object Axiom {
    def apply(formula: Formula, label: String): Axiom = Axiom(formula, Some(label))
  }

  case class Axiom(formula: Formula, label: Option[String] = None) extends Derivation {

    override val undischargedAssumptions: Assumptions = label match {
      case None => Assumptions(anonymousAssumptions = Set(formula))
      case Some(label) => Assumptions(labelledAssumptions = Map(label -> formula))
    }
  }

  case class ConjunctionIntroduction(leftDerivation: Derivation, rightDerivation: Derivation) extends Derivation {
    override def undischargedAssumptions: Assumptions = leftDerivation.undischargedAssumptions ++ rightDerivation.undischargedAssumptions

    override def formula: Formula = leftDerivation.formula ∧ rightDerivation.formula
  }

  case class LeftConjunctionElimination(conjunctionDerivation: Derivation) extends Derivation {
    assert(conjunctionDerivation.formula.isInstanceOf[Conjunction])
    val conjunction: Conjunction = conjunctionDerivation.formula.asInstanceOf[Conjunction]

    override def undischargedAssumptions: Assumptions = conjunctionDerivation.undischargedAssumptions

    override def formula: Formula = conjunction.conjunct1
  }

  case class RightConjunctionElimination(conjunctionDerivation: Derivation) extends Derivation {
    assert(conjunctionDerivation.formula.isInstanceOf[Conjunction])
    val conjunction: Conjunction = conjunctionDerivation.formula.asInstanceOf[Conjunction]

    override def undischargedAssumptions: Assumptions = conjunctionDerivation.undischargedAssumptions

    override def formula: Formula = conjunction.conjunct2
  }

  object ImplicationIntroduction {
    def apply(antecedent: Formula, consequentDerivation: Derivation): ImplicationIntroduction =
      ImplicationIntroduction(antecedent, None, consequentDerivation)

    def apply(antecedent: Formula, label: String, consequentDerivation: Derivation): ImplicationIntroduction =
      ImplicationIntroduction(antecedent, Some(label), consequentDerivation)
  }

  case class ImplicationIntroduction(antecedent: Formula, label: Option[String], consequentDerivation: Derivation) extends Derivation {
    for {
      label <- label
      assumption <- consequentDerivation.undischargedAssumptions.labelledAssumptions.get(label)
    } assert(assumption == antecedent, s"Expected assumption $assumption to equal $antecedent for label $label")

    override def formula: Formula = antecedent → consequentDerivation.formula

    override def undischargedAssumptions: Assumptions = label match {
      case Some(label) => consequentDerivation.undischargedAssumptions.discharge(label)
      case None => consequentDerivation.undischargedAssumptions
    }
  }

  case class ImplicationElimination(antecedentDerivation: Derivation, implicationDerivation: Derivation) extends Derivation {
    assert(implicationDerivation.formula.isInstanceOf[Implication])
    val implication: Implication = implicationDerivation.formula.asInstanceOf[Implication]
    assert(antecedentDerivation.formula == implication.antecedent)

    override def formula: Formula = implication.consequent

    override def undischargedAssumptions: Assumptions = antecedentDerivation.undischargedAssumptions ++ implicationDerivation.undischargedAssumptions
  }

  case class EquivalenceIntroduction(forwardsDerivation: Derivation, backwardsDerivation: Derivation) extends Derivation {
    assert(forwardsDerivation.formula.isInstanceOf[Implication])
    assert(backwardsDerivation.formula.isInstanceOf[Implication])
    val formula1: Formula = forwardsDerivation.formula.asInstanceOf[Implication].antecedent
    val formula2: Formula = forwardsDerivation.formula.asInstanceOf[Implication].consequent
    assert(backwardsDerivation.formula == (formula2 → formula1))

    override def formula: Formula = formula1 ↔ formula2

    override def undischargedAssumptions: Assumptions = forwardsDerivation.undischargedAssumptions ++ backwardsDerivation.undischargedAssumptions
  }

  case class ForwardsEquivalenceElimination(equivalenceDerivation: Derivation) extends Derivation {
    assert(equivalenceDerivation.formula.isInstanceOf[Equivalence])
    val equivalance: Equivalence = equivalenceDerivation.formula.asInstanceOf[Equivalence]

    override def undischargedAssumptions: Assumptions = equivalenceDerivation.undischargedAssumptions

    override def formula: Formula = equivalance.forwardsImplication
  }

  case class BackwardsEquivalenceElimination(equivalenceDerivation: Derivation) extends Derivation {
    assert(equivalenceDerivation.formula.isInstanceOf[Equivalence])
    val equivalance: Equivalence = equivalenceDerivation.formula.asInstanceOf[Equivalence]

    override def undischargedAssumptions: Assumptions = equivalenceDerivation.undischargedAssumptions

    override def formula: Formula = equivalance.backwardsImplication
  }

  object NegationIntroduction {
    def apply(statement: Formula, bottomDerivation: Derivation): NegationIntroduction =
      NegationIntroduction(statement, None, bottomDerivation)

    def apply(statement: Formula, label: String, bottomDerivation: Derivation): NegationIntroduction =
      NegationIntroduction(statement, Some(label), bottomDerivation)
  }

  case class NegationIntroduction(statement: Formula, label: Option[String], bottomDerivation: Derivation) extends Derivation {
    for {
      label <- label
      assumption <- bottomDerivation.undischargedAssumptions.labelledAssumptions.get(label)
    } assert(assumption == statement, s"Expected assumption $assumption to equal $statement for label $label")

    override def formula: Formula = Negation(statement)

    override def undischargedAssumptions: Assumptions = label match {
      case Some(label) => bottomDerivation.undischargedAssumptions.discharge(label)
      case None => bottomDerivation.undischargedAssumptions
    }
  }

  case class NegationElimination(positiveDerivation: Derivation, negativeDerivation: Derivation) extends Derivation {
    assert(negativeDerivation.formula.isInstanceOf[Negation])
    assert(negativeDerivation.formula.asInstanceOf[Negation].formula == positiveDerivation.formula)

    override def formula: Formula = ⊥

    override def undischargedAssumptions: Assumptions =
      positiveDerivation.undischargedAssumptions ++ negativeDerivation.undischargedAssumptions
  }

}
