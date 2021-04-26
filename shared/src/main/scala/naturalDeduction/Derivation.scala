package naturalDeduction

import naturalDeduction.pretty.DerivationRenderer

case class Sequent(assumptions: Set[Formula], conclusion: Formula) {
  override def toString: String = s"${assumptions.mkString(", ")} ⊢ $conclusion"
}

sealed trait Derivation {
  def formula: Formula

  def undischargedAssumptions: Set[Formula]

  def sequent: Sequent = Sequent(undischargedAssumptions, formula)

  override def toString: String = DerivationRenderer.renderDerivation(this).toStringNormal
}

object Derivation {

  case class Axiom(formula: Formula, isDischarged: Boolean = false) extends Derivation {
    override val undischargedAssumptions: Set[Formula] = if (isDischarged) Set.empty else Set(formula)
  }

  case class ConjunctionIntroduction(leftDerivation: Derivation, rightDerivation: Derivation) extends Derivation {
    override def undischargedAssumptions: Set[Formula] = leftDerivation.undischargedAssumptions ++ rightDerivation.undischargedAssumptions

    override def formula: Formula = leftDerivation.formula ∧ rightDerivation.formula
  }

  case class LeftConjunctionElimination(conjunctionDerivation: Derivation) extends Derivation {
    assert(conjunctionDerivation.formula.isInstanceOf[Formula.Conjunction])
    val conjunction: Formula.Conjunction = conjunctionDerivation.formula.asInstanceOf[Formula.Conjunction]
    override def undischargedAssumptions: Set[Formula] = conjunctionDerivation.undischargedAssumptions

    override def formula: Formula = conjunction.conjunct1
  }

  case class RightConjunctionElimination(conjunctionDerivation: Derivation) extends Derivation {
    assert(conjunctionDerivation.formula.isInstanceOf[Formula.Conjunction])
    val conjunction: Formula.Conjunction = conjunctionDerivation.formula.asInstanceOf[Formula.Conjunction]
    override def undischargedAssumptions: Set[Formula] = conjunctionDerivation.undischargedAssumptions

    override def formula: Formula = conjunction.conjunct2
  }

  case class ImplicationIntroduction(antecedent: Formula, consequentDerivation: Derivation) extends Derivation {
    override def formula: Formula = antecedent → consequentDerivation.formula

    override def undischargedAssumptions: Set[Formula] = consequentDerivation.undischargedAssumptions
  }

}
