package naturalDeduction.pretty

import naturalDeduction.Derivation
import naturalDeduction.Derivation.{Axiom, ConjunctionIntroduction, LeftConjunctionElimination, RightConjunctionElimination}
import naturalDeduction.TestConstants.{φ, χ, ψ}
import naturalDeduction.pretty.DerivationRenderer._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DerivationRendererSpec extends AnyFlatSpec with Matchers {

  private def render(derivation: Derivation) = renderDerivation(derivation).toStringNormal

  "Rendering derivations" should "work" in {
    render(Axiom(χ)) shouldEqual "χ"

    render(LeftConjunctionElimination(Axiom(φ ∧ ψ))) shouldEqual
      """φ ∧ ψ
        |───── ∧E
        |  φ""".stripMargin

    render(LeftConjunctionElimination(LeftConjunctionElimination(Axiom(φ ∧ ψ ∧ χ)))) shouldEqual
      """(φ ∧ ψ) ∧ χ
        |─────────── ∧E
        |   φ ∧ ψ
        |   ───── ∧E
        |     φ""".stripMargin

    render(ConjunctionIntroduction(
      RightConjunctionElimination(Axiom(φ ∧ ψ)),
      LeftConjunctionElimination(Axiom(φ ∧ ψ)))) shouldEqual
      """φ ∧ ψ    φ ∧ ψ
        |───── ∧E ───── ∧E
        |  ψ        φ
        |  ────────── ∧I
        |    ψ ∧ φ""".stripMargin

    render(ConjunctionIntroduction(
      ConjunctionIntroduction(
        LeftConjunctionElimination(Axiom(φ ∧ (ψ ∧ χ))),
        LeftConjunctionElimination(RightConjunctionElimination(Axiom(φ ∧ (ψ ∧ χ))))),
      RightConjunctionElimination(RightConjunctionElimination(Axiom(φ ∧ (ψ ∧ χ)))))) shouldEqual
      """            φ ∧ (ψ ∧ χ)
        |            ─────────── ∧E
        |φ ∧ (ψ ∧ χ)    ψ ∧ χ    φ ∧ (ψ ∧ χ)
        |─────────── ∧E ───── ∧E ─────────── ∧E
        |     φ           ψ         ψ ∧ χ
        |     ───────────── ∧I      ───── ∧E
        |         φ ∧ ψ               χ
        |         ───────────────────── ∧I
        |              (φ ∧ ψ) ∧ χ""".stripMargin

  }

}
