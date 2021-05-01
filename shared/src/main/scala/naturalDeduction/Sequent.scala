package naturalDeduction

object Sequent {

  implicit class RichSet[T <: Formula](assumptions: Set[T]) {
    def ⊢(conclusion: Formula): Sequent = Sequent(assumptions.toSet, conclusion)
  }

  implicit class RichFormula2(formula: Formula) {
    def ⊢(conclusion: Formula): Sequent = Sequent(Set(formula), conclusion)
  }

  implicit class RichPair(pair: (Formula, Formula)) {
    def ⊢(conclusion: Formula): Sequent = Sequent(Set(pair._1, pair._2), conclusion)
  }

  implicit class RichTriple(triple: (Formula, Formula, Formula)) {
    def ⊢(conclusion: Formula): Sequent = Sequent(Set(triple._1, triple._2, triple._3), conclusion)
  }

}

case class Sequent(assumptions: Set[Formula], conclusion: Formula) {
  override def toString: String =
    if (assumptions.isEmpty) s"⊢ $conclusion" else s"{${assumptions.mkString(", ")}} ⊢ $conclusion"
}
