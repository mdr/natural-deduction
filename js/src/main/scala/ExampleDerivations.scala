import naturalDeduction.Derivation
import naturalDeduction.Derivation.{Axiom, ImplicationElimination, ImplicationIntroduction, NegationElimination, ReductioAdAbsurdum}
import naturalDeduction.Formula.PropositionalVariable

object ExampleDerivations {

  private val φ = PropositionalVariable("φ")
  private val ψ = PropositionalVariable("ψ")
  private val χ = PropositionalVariable("χ")

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

}
