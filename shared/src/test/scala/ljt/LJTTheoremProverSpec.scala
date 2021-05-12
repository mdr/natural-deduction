package ljt

import naturalDeduction.Sequent
import naturalDeduction.Sequent._
import naturalDeduction.TestConstants._
import naturalDeduction.parser.FormulaParser
import naturalDeduction.parser.FormulaParser.parseSequent
import naturalDeduction.pretty.DerivationSerialiser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LJTTheoremProverSpec extends AnyFlatSpec with Matchers {

  "The theorem prover" should "be able to prove stuff" in {
    checkCanProve(φ ⊢ (φ ∧ φ))
    checkCanProve((φ, ψ, χ) ⊢ ((φ ∧ ψ) ∧ χ))
    checkCanProve((φ, ψ, χ) ⊢ (φ ∧ (ψ ∧ χ)))
    checkCanProve((φ, ψ) ⊢ (ψ ∧ φ))
    checkCanProve(φ ⊢ ((φ ∧ φ) ∧ φ))
    checkCanProve((φ, ψ) ⊢ ((φ ∧ φ) ∧ (φ ∧ φ)))
    checkCanProve((φ ∧ ψ) ⊢ (ψ ∧ φ))
    checkCanProve((φ ∧ (ψ ∧ χ)) ⊢ ((φ ∧ ψ) ∧ χ))
    checkCanProve((φ ∧ ψ) ⊢ (φ ∧ φ))
    checkCanProve(((φ ∧ ψ) ∧ χ) ⊢ (φ ∧ (ψ ∧ χ)))
    checkCanProve((φ, ψ ∧ χ) ⊢ (χ ∧ φ))
    checkCanProve((φ ∧ (ψ ∧ χ)) ⊢ ((χ ∧ φ) ∧ ψ))
    checkCanProve(Ø ⊢ (φ → (ψ → (φ ∧ ψ))))
    checkCanProve(Ø ⊢ (φ → (φ → φ)))
    checkCanProve((φ → ψ, ψ → χ) ⊢ (φ → χ))
    checkCanProve(Ø ⊢ ((φ ∧ ψ) → (ψ ∧ φ)))
    checkCanProve(Ø ⊢ ((ψ → χ) → ((φ → ψ) → (φ → χ))))
    checkCanProve(Ø ⊢ (φ → (ψ → φ)))
    checkCanProve(φ ⊢ (φ → (ψ → φ)))
    checkCanProve((φ ∧ ψ) ⊢ (ψ → (ψ ∧ φ)))
    checkCanProve(Ø ⊢ (φ → φ))
    checkCanProve("⊢ (φ → (ψ → ψ))") // Exercise 2.4.4(a)
    checkCanProve("⊢ ((φ → φ) ∧ (ψ → ψ))") // Exercise 2.4.4(b)
    checkCanProve("⊢ ((φ → (θ → ψ)) → (θ → (φ → ψ)))") // Exercise 2.4.4(c)
    checkCanProve("{φ → ψ, φ → χ} ⊢ (φ → (ψ ∧ χ))") // Exercise 2.4.4(d)
    checkCanProve("{φ → ψ, (φ ∧ ψ) → χ} ⊢ (φ → χ)") // Exercise 2.4.4(e)
    checkCanProve("{φ → (ψ → χ)} ⊢ ((φ ∧ ψ) → χ)") // Exercise 2.4.4(f)
    checkCanProve("⊢ ((φ → ψ) → ((ψ → θ) → (φ → θ)))") // Exercise 2.4.4(g)
    checkCanProve("⊢ ((φ → (ψ ∧ θ)) → ((φ → θ) ∧ (φ → ψ)))") // Exercise 2.4.4(h)
  }

  it should "be able to prove even more stuff" in {
    checkCanProve((φ ↔ ψ) ⊢ (ψ ↔ φ))
    checkCanProve("{φ, φ ↔ ψ} ⊢ ψ") // Exercise 2.5.1(a)
    checkCanProve("⊢ (φ ↔ φ)") // Exercise 2.5.1(b)
    checkCanProve("{φ ↔ ψ, ψ ↔ χ} ⊢ (φ ↔ χ)") // Exercise 2.5.1(c)
    checkCanProve("⊢ ((φ ↔ ψ) ↔ χ) ↔ (φ ↔ (ψ ↔ χ))") // Exercise 2.5.1(d) (as printed)
    checkCanProve("⊢ ((φ ↔ ψ) ↔ χ) → φ → (ψ ↔ χ)") // Exercise 2.5.1(d) (corrected)
    checkCanProve("{φ ↔ (ψ ↔ ψ)} ⊢ φ") // 2.5.1.(e)
  }

  it should "be able to prove negation stuff" in {
    checkCanProve(Ø ⊢ (φ → φ.not.not))
    checkCanProve("⊢ ((¬(¬φ)) → φ)") // Example 2.6.3
    checkCanProve("⊢ (¬(φ ∧ (¬φ)))") // Exercise 2.6.1(a)
    checkCanProve("⊢ ((¬(φ → ψ)) → (¬ψ))") // Exercise 2.6.1(b)
    checkCanProve("⊢ ((φ ∧ ψ) → (¬(φ → (¬ψ))))") // Exercise 2.6.1(c)
    checkCanProve("{((¬(φ ∧ ψ)) ∧ φ)} ⊢ (¬ψ)") // Exercise 2.6.1(d)
    checkCanProve("{(φ → ψ)} ⊢ ((¬ψ) → (¬φ))") // Exercise 2.6.1(e)
    checkCanProve("{(φ → ψ)} ⊢ (¬(φ ∧ (¬ψ)))") // Exercise 2.6.1(f)

    checkCanProve("{((¬ψ) → (¬φ))} ⊢ (φ → ψ)") // Exercise 2.6.2(a)
    checkCanProve("⊢ ((¬(φ → ψ)) → φ)")
    checkCanProve("⊢ (φ → ((¬φ) → ψ))")
    checkCanProve("{(¬(φ ↔ ψ))} ⊢ ((¬φ) ↔ ψ)")
  }

  it should "be able to prove disjunction stuff" in {
    checkCanProve("⊢ φ → φ ∨ ψ")
    checkCanProve("{(¬(φ ∨ ψ))} ⊢ ((¬φ) ∧ (¬ψ))")
    checkCanProve("⊢ ((φ → ψ) → ((¬φ) ∨ ψ))")
    checkCanProve("{(φ ∨ ψ), (φ → χ), (ψ → χ)} ⊢ χ")
    checkCanProve("{(φ ∨ ψ), (¬φ)} ⊢ ψ")
    checkCanProve("{((¬φ) ∧ (¬ψ))} ⊢ (¬(φ ∨ ψ))")
    checkCanProve("{(φ ∧ ψ)} ⊢ (¬((¬φ) ∨ (¬ψ)))")
  }

  it should "be able to prove this one weird little theorem" in {
  }

  // ❶ ❷ ❸ ❹ ❺ φ ψ χ Ø ⊢ ¬ ∨ ∧ → ↔ ⊥

  private def checkCanProve(s: String): Unit =
    checkCanProve(parseSequent(s))

  private def checkCanProve(sequent: Sequent): Unit = {
    val Some(derivation) = LJTTheoremProver.prove(sequent)
    derivation.proves(sequent) shouldBe true

    // Bonus serialisation test because I'm lazy:
    val derivationAgain = FormulaParser.parseDerivation(DerivationSerialiser.serialise(derivation))
    derivationAgain shouldBe derivation
  }

}
