package naturalDeduction.parser

import naturalDeduction.Formula.{Conjunction, PropositionalVariable}
import naturalDeduction.TestConstants._
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class FormulaParserSpec extends AnyFlatSpec with Matchers {
  "Parsing" should "work" in {
    FormulaParser.parseFormula("φ") shouldEqual φ
    FormulaParser.parseFormula("φ ∧ ψ") shouldEqual (φ ∧ ψ)
    FormulaParser.parseFormula("φ ∧ φ ∧ φ") shouldEqual ((φ ∧ φ) ∧ φ)
    FormulaParser.parseFormula("φ ∧ φ ∨ φ ∧ φ") shouldEqual ((φ ∧ φ) ∨ (φ ∧ φ))
    FormulaParser.parseFormula("φ && φ || φ /\\ φ") shouldEqual ((φ ∧ φ) ∨ (φ ∧ φ))
    FormulaParser.parseFormula("φ ∧ φ → φ") shouldEqual ((φ ∧ φ) → φ)
    FormulaParser.parseFormula("¬¬φ → φ") shouldEqual (φ.not.not → φ)
    FormulaParser.parseFormula("!φ") shouldEqual (φ.not)
    FormulaParser.parseFormula("~φ") shouldEqual (φ.not)
    FormulaParser.parseFormula("φ → φ → φ") shouldEqual (φ → (φ → φ))
    FormulaParser.parseFormula("(ψ → χ) → (φ → ψ) → φ → χ") shouldEqual ((ψ → χ) → ((φ → ψ) → (φ → χ)))
    FormulaParser.parseFormula("φ ∧ φ → φ ↔ φ ∧ φ → φ") shouldEqual (((φ ∧ φ) → φ) ↔ ((φ ∧ φ) → φ))
    FormulaParser.parseFormula("φ∧φ→φ↔φ∧φ→φ") shouldEqual (((φ ∧ φ) → φ) ↔ ((φ ∧ φ) → φ))
  }
  // ❶ ❷ ❸ ❹ ❺ φ ψ χ Ø ⊢ ¬ ∨ ∧ → ↔ ⊥
}
