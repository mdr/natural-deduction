package naturalDeduction

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Derivation._
import TestConstants._
import DerivationRenderer._

class DerivationRendererSpec extends AnyFlatSpec with Matchers {

  "Rendering derivations" should "work" in {
    println(renderDerivation(Axiom(χ)))
    println()
    println(renderDerivation(LeftConjunctionElimination(Axiom(φ ∧ ψ))))
    println()
    println(renderDerivation(LeftConjunctionElimination(LeftConjunctionElimination(Axiom(φ ∧ ψ ∧ χ)))))
    println()
    println(renderDerivation(ConjunctionIntroduction(
      RightConjunctionElimination(Axiom(φ ∧ ψ)),
      LeftConjunctionElimination(Axiom(φ ∧ ψ)))))
    println()
    println(renderDerivation(ConjunctionIntroduction(ConjunctionIntroduction(Axiom(φ), Axiom(ψ)), Axiom(χ))))
  }

}
