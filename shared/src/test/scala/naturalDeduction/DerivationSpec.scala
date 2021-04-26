package naturalDeduction

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Derivation._
import TestConstants._
import Sequent.RichSet

class DerivationSpec extends AnyFlatSpec with Matchers {

  val Ø = Set.empty[Formula]

  "Exercise 2.2.2(a)" should "be provable" in {
    val derivation = φ.axiom conjunctionIntro (ψ.axiom conjunctionIntro χ.axiom)
    derivation.sequent shouldEqual (Set(φ, ψ, χ) ⊢ (φ ∧ (ψ ∧ χ)))
  }

  "Exercise 2.2.2(b)" should "be provable" in {
    val derivation = ψ.axiom conjunctionIntro φ.axiom
    derivation.sequent shouldEqual (Set(φ, ψ) ⊢ (ψ ∧ φ))
  }

  "Exercise 2.2.2(c)" should "be provable" in {
    val derivation = (φ.axiom conjunctionIntro φ.axiom) conjunctionIntro φ.axiom
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
    derivation.sequent shouldEqual (Ø ⊢ (φ → (ψ → (φ ∧ ψ))))
  }

  "Example 2.4.4" should "be provable" in {
    val derivation =
      ImplicationIntroduction(φ, "②",
        ImplicationIntroduction(φ, "①",
          Axiom(φ, label = Some("②"))))
    derivation.sequent shouldEqual (Ø ⊢ (φ → (φ → φ)))

    val alternativeDerivation =
      ImplicationIntroduction(φ, "②",
        ImplicationIntroduction(φ, "①",
          Axiom(φ, label = Some("①"))))
    alternativeDerivation.sequent shouldEqual (Ø ⊢ (φ → (φ → φ)))
  }

  "Example 2.4.5" should "be provable" in {
    val derivation =
      ImplicationIntroduction(φ, "①",
        ImplicationElimination(
          ImplicationElimination(
            Axiom(φ, "①"),
            Axiom(φ → ψ)),
          Axiom(ψ → χ))
      )
    derivation.sequent shouldEqual (Set(φ → ψ, ψ → χ) ⊢ (φ → χ))
  }

  "Exercise 2.4.2(a)" should "be provable" in {
    val derivation = ImplicationIntroduction(φ ∧ ψ, "①",
      Axiom(φ ∧ ψ, "①").rightConjunctionElim conjunctionIntro
        Axiom(φ ∧ ψ, "①").leftConjunctionElim)
    derivation.sequent shouldEqual (Ø ⊢ ((φ ∧ ψ) → (ψ ∧ φ)))
  }

  "Exercise 2.4.2(b)" should "be provable" in {
    val derivation =
      ImplicationIntroduction(ψ → χ, "❸",
        ImplicationIntroduction(φ → ψ, "❷",
          ImplicationIntroduction(φ, "❶",
            ImplicationElimination(
              ImplicationElimination(Axiom(φ, "❶"), Axiom(φ → ψ, "❷")),
              Axiom(ψ → χ, "❸")))))
    derivation.sequent shouldEqual (Ø ⊢ ((ψ → χ) → ((φ → ψ) → (φ → χ))))
  }

  "Exercise 2.4.3(a)" should "be provable" in {
    val derivation = Axiom(φ, "❶").implicationIntro(ψ).implicationIntro(φ, "❶")
    derivation.sequent shouldEqual (Ø ⊢ (φ → (ψ → φ)))
  }

  "Exercise 2.4.3(b)" should "be provable" in {
    val derivation = Axiom(φ).implicationIntro(ψ).implicationIntro(φ)
    derivation.sequent shouldEqual (Set(φ) ⊢ (φ → (ψ → φ)))
  }

  "Exercise 2.4.3(c)" should "be provable" in {
    val derivation = (Axiom(ψ, label = "❶") conjunctionIntro Axiom(φ ∧ ψ).leftConjunctionElim).implicationIntro(ψ, "❶")
    derivation.sequent shouldEqual (Set(φ ∧ ψ) ⊢ (ψ → (ψ ∧ φ)))
  }

  "Exercise 2.4.3(d)" should "be provable" in {
    val derivation = Axiom(φ, label = "❶").implicationIntro(φ, "❶")
    derivation.sequent shouldEqual (Ø ⊢ (φ → φ))
  }

  "Example 2.5.1" should "be provable" in {
    val derivation = Axiom(φ ↔ ψ).backwardsEquivalenceElim equivalenceIntro Axiom(φ ↔ ψ).forwardsEquivalenceElim
    derivation.sequent shouldEqual (Set(φ ↔ ψ) ⊢ (ψ ↔ φ))
  }

  "Example 2.6.1" should "be provable" in {
    val derivation =
      (Axiom(φ, "❷") negationElim Axiom(φ.not, "❶")).negationIntro(φ.not, "❶").implicationIntro(φ, "❷")
    derivation.sequent shouldEqual (Ø ⊢ (φ → φ.not.not))
  }

  "Example 2.6.3" should "be provable" in {
    val derivation =
      ImplicationIntroduction(φ.not.not, "❷",
        ReductioAdAbsurdum(φ, "❶",
          NegationElimination(
            Axiom(φ.not, "❶"),
            Axiom(φ.not.not, "❷")
          )))
    derivation.sequent shouldEqual (Ø ⊢ (φ.not.not → φ))
  }

  "Ex falso" should "happen" in {
    val derivation = (φ.axiom negationElim φ.not.axiom) reductio φ
    derivation.sequent shouldEqual (Set(φ, φ.not) ⊢ φ)
  }

  "Example 2.7.1 (2nd derivation)" should "be provable" in {
    val derivation =
      ReductioAdAbsurdum(φ ∨ φ.not, "❸",
        NegationElimination(
          ReductioAdAbsurdum(φ, "❶",
            NegationElimination(
              RightDisjunctionIntroduction(φ,
                Axiom(φ.not, "❶")
              ),
              Axiom((φ ∨ φ.not).not, "❸")
            )),
          NegationIntroduction(φ, "❷",
            NegationElimination(
              LeftDisjunctionIntroduction(
                Axiom(φ, "❷"),
                φ.not),
              Axiom((φ ∨ φ.not).not, "❸")))))
    derivation.sequent shouldEqual (Ø ⊢ (φ ∨ φ.not))
  }

}
