package naturalDeduction.pretty

import naturalDeduction.Derivation
import naturalDeduction.Derivation.{Axiom, ConjunctionIntroduction, ImplicationElimination, ImplicationIntroduction, RichFormula}
import naturalDeduction.TestConstants.{φ, χ, ψ}
import naturalDeduction.pretty.DerivationRenderer._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DerivationRendererSpec extends AnyFlatSpec with Matchers {

  implicit class RichDerivation(derivation: Derivation) {
    def shouldRenderAs(expected: String) {
      s"Rendering $expected" should "work" in {
        val actual = derivation.toString
        val passed = actual == expected
        if (!passed) {
          throw new TestFailedException(s"Expected:\n$expected\nbut was\n$actual", 0)
        }
      }
    }
  }

  Axiom(χ) shouldRenderAs "χ"

  (Axiom(φ ∧ ψ).leftConjunctionElim) shouldRenderAs
    """φ ∧ ψ
      |───── ∧E
      |  φ""".stripMargin

  Axiom(φ ∧ ψ ∧ χ).leftConjunctionElim.leftConjunctionElim shouldRenderAs
    """(φ ∧ ψ) ∧ χ
      |─────────── ∧E
      |   φ ∧ ψ
      |   ───── ∧E
      |     φ""".stripMargin

  (Axiom(φ ∧ ψ).rightConjunctionElim conjunctionIntro Axiom(φ ∧ ψ).leftConjunctionElim) shouldRenderAs
    """φ ∧ ψ    φ ∧ ψ
      |───── ∧E ───── ∧E
      |  ψ        φ
      |  ────────── ∧I
      |    ψ ∧ φ""".stripMargin

  ConjunctionIntroduction(
    ConjunctionIntroduction(
      Axiom(φ ∧ (ψ ∧ χ)).leftConjunctionElim,
      Axiom(φ ∧ (ψ ∧ χ)).rightConjunctionElim.leftConjunctionElim),
    Axiom(φ ∧ (ψ ∧ χ)).rightConjunctionElim.rightConjunctionElim) shouldRenderAs
    """            φ ∧ (ψ ∧ χ)
      |            ─────────── ∧E
      |φ ∧ (ψ ∧ χ)    ψ ∧ χ    φ ∧ (ψ ∧ χ)
      |─────────── ∧E ───── ∧E ─────────── ∧E
      |     φ           ψ         ψ ∧ χ
      |     ───────────── ∧I      ───── ∧E
      |         φ ∧ ψ               χ
      |         ───────────────────── ∧I
      |              (φ ∧ ψ) ∧ χ""".stripMargin

  ImplicationIntroduction(ψ, "1",
    ConjunctionIntroduction(
      Axiom(φ, label = Some("2")),
      Axiom(ψ, label = Some("1")))) shouldRenderAs
    """  2 φ 1 -ψ-
      |  ───────── ∧I
      |    φ ∧ ψ
      |1 ───────── →I
      |  ψ → φ ∧ ψ""".stripMargin

  ImplicationIntroduction(φ, "2",
    ImplicationIntroduction(ψ, "1",
      ConjunctionIntroduction(
        Axiom(φ, label = Some("2")),
        Axiom(ψ, label = Some("1"))))) shouldRenderAs
    """    2 -φ- 1 -ψ-
      |    ─────────── ∧I
      |       φ ∧ ψ
      |   1 ───────── →I
      |     ψ → φ ∧ ψ
      | 2 ───────────── →I
      |   φ → ψ → φ ∧ ψ""".stripMargin

    (φ.axiom implicationElim Axiom(φ → ψ)) implicationElim Axiom(ψ → χ) shouldRenderAs
  """φ φ → ψ
    |─────── →E
    |ψ ψ → χ
    |─────── →E
    |   χ""".stripMargin


}
