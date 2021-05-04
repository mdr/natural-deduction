package ljt

import naturalDeduction.Derivation.{Axiom, BackwardsEquivalenceElimination, ConjunctionIntroduction, EquivalenceIntroduction, ForwardsEquivalenceElimination, LeftConjunctionElimination, RichFormula, RightConjunctionElimination}
import naturalDeduction.Formula.{Conjunction, Equivalence, Implication}
import naturalDeduction.{Derivation, Formula, Sequent}

/**
 * Fix proofs that have had prior expansion of <-> and ¬ to then prove the original sequent.
 */
object ProofFixer {

  object ExpandedEquivalence {
    def unapply(formula: Formula): Option[(Formula, Formula)] = PartialFunction.condOpt(formula) {
      case Conjunction(Implication(antecedent1, consequent1), Implication(consequent2, antecedent2))
        if antecedent1 == antecedent2 && consequent1 == consequent2 => antecedent1 -> consequent1
    }
  }

  private def fixConclusionIfNeeded(sequent: Sequent, derivation: Derivation): Derivation =
    sequent.conclusion match {
      case Equivalence(formula1, formula2) =>
        val ExpandedEquivalence(f1, f2) = derivation.conclusion
        assert(f1 == formula1)
        assert(f2 == formula2)
        derivation match {
          case ConjunctionIntroduction(leftDerivation, rightDerivation)
            if leftDerivation.conclusion == Implication(f1, f2) && rightDerivation.conclusion == Implication(f2, f1) =>
            EquivalenceIntroduction(leftDerivation, rightDerivation)
          case _ => EquivalenceIntroduction(LeftConjunctionElimination(derivation), RightConjunctionElimination(derivation))
        }
      case _ => derivation
    }

  def fix(sequent: Sequent, derivation: Derivation): Derivation =
    sequent.assumptions
      .collect { case e: Equivalence => e }
      .foldLeft(fixConclusionIfNeeded(sequent, derivation))(fixAssumption)

  private def fixAssumption(derivation: Derivation, equivalence: Equivalence): Derivation = {
    import equivalence.{formula1, formula2}
    derivation.everywhere {
      case LeftConjunctionElimination(Axiom(ExpandedEquivalence(`formula1`, `formula2`), None)) =>
        ForwardsEquivalenceElimination(Equivalence(formula1, formula2).axiom)
      case RightConjunctionElimination(Axiom(ExpandedEquivalence(`formula1`, `formula2`), None)) =>
        BackwardsEquivalenceElimination(Equivalence(formula1, formula2).axiom)
    }
    //    val assumptionToReplace = equivalence.forwardsImplication ∧ equivalence.backwardsImplication
    //    derivation.substitute(assumptionToReplace,
    //      ConjunctionIntroduction(
    //        ForwardsEquivalenceElimination(equivalence.axiom),
    //        BackwardsEquivalenceElimination(equivalence.axiom)))
  }
}
