package naturalDeduction

import naturalDeduction.pretty.FormulaPrettyPrinter

sealed trait Formula {

  import Formula._

  def ∧(that: Formula): Conjunction = Conjunction(this, that)

  def →(that: Formula): Implication = Implication(this, that)

  def ↔(that: Formula): Equivalence = Equivalence(this, that)

  override def toString: String = FormulaPrettyPrinter.prettyPrint(this)

}

object Formula {

  case object Bottom extends Formula {
  }

  case class PropositionalVariable(name: String) extends Formula {
  }

  case class Negation(formula: Formula) extends Formula {
  }

  case class Conjunction(conjunct1: Formula, conjunct2: Formula) extends Formula {
  }

  case class Disjunction(disjunct1: Formula, disjunct2: Formula) extends Formula {
  }

  case class Implication(antecedent: Formula, consequent: Formula) extends Formula {
  }

  case class Equivalence(formula1: Formula, formula2: Formula) extends Formula {
    def forwardsImplication: Formula = formula1 → formula2
    def backwardsImplication: Formula = formula2 → formula1
  }


}
