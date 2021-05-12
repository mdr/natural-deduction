package ljt

import ljt.LJTDerivation.{Axiom, ImplicationRight}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import naturalDeduction.TestConstants._
class LJTDerivationSpec extends AnyFlatSpec with Matchers {

  "thing" should "work" in {
    val ljtDerivation = ImplicationRight(φ, φ, Multiset.empty, Axiom(φ, Multiset.empty))
    println(ljtDerivation.naturalDeductionDerivation)
  }

  // ❶ ❷ ❸ ❹ ❺ φ ψ χ Ø ⊢ ¬ ∨ ∧ → ↔ ⊥
}