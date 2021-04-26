package naturalDeduction

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Derivation._
import TestConstants._
import Sequent.RichSet

class DerivationSpec extends AnyFlatSpec with Matchers {

  val noAssumptions = Set.empty[Formula]

  "Exercise 2.2.2(a)" should "be provable" in {
    val derivation = φ.axiom conjunctionIntro (ψ.axiom conjunctionIntro χ.axiom)
    derivation.sequent shouldEqual (Set(φ, ψ, χ) ⊢ (φ ∧ (ψ ∧ χ)))
  }

  "Exercise 2.2.2(b)" should "be provable" in {
    val derivation = ψ.axiom conjunctionIntro φ.axiom
    derivation.sequent shouldEqual (Set(φ, ψ) ⊢ (ψ ∧ φ))
  }

  "Exercise 2.2.2(c)" should "be provable" in {
    val derivation = ((φ.axiom conjunctionIntro φ.axiom) conjunctionIntro φ.axiom)
    derivation.sequent shouldEqual (Set(φ) ⊢ ((φ ∧ φ) ∧ φ))
  }

  "Exercise 2.2.2(d)" should "be provable" in {
    val derivation =
      (φ.axiom conjunctionIntro ψ.axiom) conjunctionIntro (φ.axiom conjunctionIntro ψ.axiom)
    derivation.sequent shouldEqual (Set(φ, ψ) ⊢ ((φ ∧ ψ) ∧ (φ ∧ ψ)))
  }

  "Example 2.2.3" should "be provable" in {
    val derivation = (φ.axiom conjunctionIntro ψ.axiom) conjunctionIntro χ.axiom
    derivation.sequent shouldEqual (Set(φ, ψ, χ) ⊢ ((φ ∧ ψ) ∧ χ))
  }

  "Example 2.3.2" should "be provable" in {
    val derivation = Axiom(φ ∧ ψ).rightConjunctionElim conjunctionIntro Axiom(φ ∧ ψ).leftConjunctionElim
    derivation.sequent shouldEqual (Set(φ ∧ ψ) ⊢ (ψ ∧ φ))
  }

  "Example 2.3.3" should "be provable" in {
    val derivation = ConjunctionIntroduction(
      ConjunctionIntroduction(
        Axiom(φ ∧ (ψ ∧ χ)).leftConjunctionElim,
        Axiom(φ ∧ (ψ ∧ χ)).rightConjunctionElim.leftConjunctionElim),
      Axiom(φ ∧ (ψ ∧ χ)).rightConjunctionElim.rightConjunctionElim)
    derivation.sequent shouldEqual (Set(φ ∧ (ψ ∧ χ)) ⊢ ((φ ∧ ψ) ∧ χ))
  }

  "Example 2.4.3" should "be provable" in {
    val derivation =
      ImplicationIntroduction(φ, "②",
        ImplicationIntroduction(ψ, "①",
          ConjunctionIntroduction(
            Axiom(φ, label = Some("②")),
            Axiom(ψ, label = Some("①")))))
    derivation.sequent shouldEqual (noAssumptions ⊢ (φ → (ψ → (φ ∧ ψ))))
  }

  "Example 2.4.4" should "be provable" in {
    val derivation =
      ImplicationIntroduction(φ, "②",
        ImplicationIntroduction(φ, "①",
          Axiom(φ, label = Some("②"))))
    derivation.sequent shouldEqual (noAssumptions ⊢ (φ → (φ → φ)))

    val alternativeDerivation =
      ImplicationIntroduction(φ, "②",
        ImplicationIntroduction(φ, "①",
          Axiom(φ, label = Some("①"))))
    alternativeDerivation.sequent shouldEqual (noAssumptions ⊢ (φ → (φ → φ)))
  }

  "Example 2.4.5" should "be provable" in {
    val derivation =
      ImplicationIntroduction(φ, "①",
        ImplicationElimination(
          ImplicationElimination(
            Axiom(φ, label = Some("①")),
            Axiom(φ → ψ)),
          Axiom(ψ → χ))
      )
    derivation.sequent shouldEqual (Set(φ → ψ, ψ → χ) ⊢ (φ → χ))
    println(
      ImplicationIntroduction(φ, "1",
        ImplicationElimination(
          ImplicationElimination(
            Axiom(φ, label = Some("1")),
            Axiom(φ → ψ)),
          Axiom(ψ → χ))
      )
    )
    println(
      ImplicationElimination(
        ImplicationElimination(
          Axiom(φ, label = Some("1")),
          Axiom(φ → ψ)),
        Axiom(ψ → χ))
    )
  }

}
