import naturalDeduction.Derivation
import naturalDeduction.Derivation._
import naturalDeduction.Formula.PropositionalVariable

object ExampleDerivations {

  val φ = PropositionalVariable("φ")
  val ψ = PropositionalVariable("ψ")
  val χ = PropositionalVariable("χ")

  val derivation1: Derivation =
    ImplicationIntroduction(φ.not.not, "❷",
      ReductioAdAbsurdum(φ, "❶",
        NegationElimination(
          Axiom(φ.not, "❶"),
          Axiom(φ.not.not, "❷")
        )))

  val derivation2: Derivation =
    ImplicationIntroduction(ψ → χ, "❸",
      ImplicationIntroduction(φ → ψ, "❷",
        ImplicationIntroduction(φ, "❶",
          ImplicationElimination(
            ImplicationElimination(Axiom(φ, "❶"), Axiom(φ → ψ, "❷")),
            Axiom(ψ → χ, "❸")))))

  val derivation3: Derivation =
    DisjunctionElimination(
      Axiom(φ ∨ ψ),
      Some("❶"),
      RightDisjunctionIntroduction(ψ, Axiom(φ, "❶")),
      Some("❷"),
      LeftDisjunctionIntroduction(Axiom(ψ, "❷"), φ))

}
