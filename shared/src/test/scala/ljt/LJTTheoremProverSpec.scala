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
    checkCanProve(Ø ⊢ (φ → (ψ → ψ)))
    checkCanProve(Ø ⊢ ((φ → φ) ∧ (ψ → ψ)))
    checkCanProve(Ø ⊢ ((φ → (θ → ψ)) → (θ → (φ → ψ))))
    checkCanProve((φ → ψ, φ → χ) ⊢ (φ → (ψ ∧ χ)))
    checkCanProve((φ → ψ, (φ ∧ ψ) → χ) ⊢ (φ → χ))
    checkCanProve(φ → (ψ → χ) ⊢ ((φ ∧ ψ) → χ))
    checkCanProve(Ø ⊢ ((φ → ψ) → ((ψ → θ) → (φ → θ))))
    checkCanProve(Ø ⊢ ((φ → (ψ ∧ θ)) → ((φ → θ) ∧ (φ → ψ))))
  }

  it should "be able to prove even more stuff" in {
    checkCanProve((φ ↔ ψ) ⊢ (ψ ↔ φ))
    checkCanProve((φ, φ ↔ ψ) ⊢ ψ)
    checkCanProve(Ø ⊢ (φ ↔ φ))
    checkCanProve((φ ↔ ψ, ψ ↔ χ) ⊢ (φ ↔ χ))
    checkCanProve(Ø ⊢ (((φ ↔ ψ) ↔ χ) → (φ → (ψ ↔ χ)))) // 2.5.1.(d) (corrected)
    checkCanProve(φ ↔ (ψ ↔ ψ) ⊢ φ) // 2.5.1.(e)
  }

  it should "be able to prove negation stuff" in {
    checkCanProve(Ø ⊢ (φ → φ.not.not))
    checkCanProve("⊢ (¬(φ ∧ (¬φ)))")
    checkCanProve("⊢ ((¬(φ → ψ)) → (¬ψ))")
    checkCanProve("⊢ ((φ ∧ ψ) → (¬(φ → (¬ψ))))")
    checkCanProve("{((¬(φ ∧ ψ)) ∧ φ)} ⊢ (¬ψ)")
    checkCanProve("{(φ → ψ)} ⊢ ((¬ψ) → (¬φ))")
    checkCanProve("{(φ → ψ)} ⊢ (¬(φ ∧ (¬ψ)))")

//    checkCanProve("{((¬ψ) → (¬φ))} ⊢ (φ → ψ)") // Exercise 2.6.2(a)
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
//    checkCanProve("⊢ ((¬(¬φ)) → φ)") // Example 2.6.3
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
