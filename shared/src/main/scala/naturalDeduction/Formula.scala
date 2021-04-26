package naturalDeduction

import naturalDeduction.pretty.FormulaPrettyPrinter

sealed trait Formula {

  import Formula._

  def ∧(that: Formula): Conjunction = Conjunction(this, that)

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

  case class Equivalence(antecedent: Formula, consequent: Formula) extends Formula {
  }


}
