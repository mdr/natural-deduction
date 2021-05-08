package naturalDeduction.parser

import naturalDeduction.TestConstants._
import naturalDeduction.parser.FormulaParser.{parseDerivation, parseFormula}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FormulaParserSpec extends AnyFlatSpec with Matchers {
  "Parsing" should "work" in {
    parseFormula("φ") shouldEqual φ
    parseFormula("φ ∧ ψ") shouldEqual (φ ∧ ψ)
    parseFormula("φ ∧ φ ∧ φ") shouldEqual ((φ ∧ φ) ∧ φ)
    parseFormula("φ ∧ φ ∨ φ ∧ φ") shouldEqual ((φ ∧ φ) ∨ (φ ∧ φ))
    parseFormula("φ && φ || φ /\\ φ") shouldEqual ((φ ∧ φ) ∨ (φ ∧ φ))
    parseFormula("φ ∧ φ → φ") shouldEqual ((φ ∧ φ) → φ)
    parseFormula("¬¬φ → φ") shouldEqual (φ.not.not → φ)
    parseFormula("!φ") shouldEqual φ.not
    parseFormula("~φ") shouldEqual φ.not
    parseFormula("φ → φ → φ") shouldEqual (φ → (φ → φ))
    parseFormula("(ψ → χ) → (φ → ψ) → φ → χ") shouldEqual ((ψ → χ) → ((φ → ψ) → (φ → χ)))
    parseFormula("φ ∧ φ → φ ↔ φ ∧ φ → φ") shouldEqual (((φ ∧ φ) → φ) ↔ ((φ ∧ φ) → φ))
    parseFormula("φ∧φ→φ↔φ∧φ→φ") shouldEqual (((φ ∧ φ) → φ) ↔ ((φ ∧ φ) → φ))
  }

  "Parsing derivations" should "work" in {
    parseDerivation("""→I(φ,"②",→I(ψ,"①",∧I(Ax(φ,"②"),Ax(ψ,"①"))))""")
  }

  // ❶ ❷ ❸ ❹ ❺ φ ψ χ Ø ⊢ ¬ ∨ ∧ → ↔ ⊥
}
