package naturalDeduction

sealed trait Formula {

  import Formula._

  def ∧(that: Formula): Conjunction = Conjunction(this, that)
}

object Formula {

  case object Bottom extends Formula {
    override def toString: String = "⊥"
  }

  case class PropositionalVariable(name: String) extends Formula {
    override def toString: String = name
  }

  case class Negation(formula: Formula) extends Formula {
    override def toString: String = s"¬$formula"
  }

  case class Conjunction(conjunct1: Formula, conjunct2: Formula) extends Formula {
    override def toString: String = s"($conjunct1 ∧ $conjunct2)"
  }

  case class Disjunction(disjunct1: Formula, disjunct2: Formula) extends Formula {
    override def toString: String = s"($disjunct1 ∨ $disjunct2)"
  }

  case class Implication(antecedent: Formula, consequent: Formula) extends Formula {
    override def toString: String = s"($antecedent → $consequent)"
  }

  case class Equivalence(antecedent: Formula, consequent: Formula) extends Formula {
    override def toString: String = s"($antecedent ↔ $consequent)"
  }


}
