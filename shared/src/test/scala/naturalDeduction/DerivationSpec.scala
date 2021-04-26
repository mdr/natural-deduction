package naturalDeduction

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Derivation._
import TestConstants._

class DerivationSpec extends AnyFlatSpec with Matchers {

  "Example 2.2.3" should "be provable" in {
    val derivation = ConjunctionIntroduction(ConjunctionIntroduction(Axiom(φ), Axiom(ψ)), Axiom(χ))
    derivation.formula shouldEqual ((φ ∧ ψ) ∧ χ)
    derivation.undischargedAssumptions shouldEqual Set(φ, ψ, χ)
  }

  "Example 2.3.2" should "be provable" in {
    val derivation = ConjunctionIntroduction(
      RightConjunctionElimination(Axiom(φ ∧ ψ)),
      LeftConjunctionElimination(Axiom(φ ∧ ψ)))
    derivation.formula shouldEqual (ψ ∧ φ)
    derivation.undischargedAssumptions shouldEqual Set(φ ∧ ψ)
  }

  "Example 2.3.3" should "be provable" in {
    val derivation = ConjunctionIntroduction(
      ConjunctionIntroduction(
        LeftConjunctionElimination(Axiom(φ ∧ (ψ ∧ χ))),
        LeftConjunctionElimination(RightConjunctionElimination(Axiom(φ ∧ (ψ ∧ χ))))),
      RightConjunctionElimination(RightConjunctionElimination(Axiom(φ ∧ (ψ ∧ χ)))))
    derivation.formula shouldEqual ((φ ∧ ψ) ∧ χ)
    derivation.undischargedAssumptions shouldEqual Set(φ ∧ (ψ ∧ χ))
  }

  "Example 2.4.3" should "be provable" in {
    val derivation =
      ImplicationIntroduction(φ,
        ImplicationIntroduction(ψ,
          ConjunctionIntroduction(
            Axiom(φ, isDischarged = true),
            Axiom(ψ, isDischarged = true))))
    derivation.formula shouldEqual (φ → (ψ → (φ ∧ ψ)))
    derivation.undischargedAssumptions shouldEqual Set.empty
  }

}
