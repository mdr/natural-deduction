package ljt

import naturalDeduction.{Derivation, Labels}

object Relabeller {

  def relabel(derivation: Derivation): Derivation =
    new RelabelContext().relabel(derivation)

  private class RelabelContext {
    private var labels = Labels.AllLabels

    def relabel(derivation: Derivation): Derivation = {
      if (labels.isEmpty) {
        return derivation
      }
      val derivation2 = derivation match {
        case Derivation.ImplicationIntroduction(_, Some(_), _) | Derivation.NegationIntroduction(_, Some(_), _) | Derivation.ReductioAdAbsurdum(_, Some(_), _) =>
          val newDerivation = derivation.relabel(0, labels.head)
          labels = labels.tail
          newDerivation
        case Derivation.DisjunctionElimination(_, Some(_), _, Some(_), _) =>
          val newDerivation = derivation.relabel(1, labels.head)
          labels = labels.tail
          val newDerivation2 = newDerivation.relabel(2, labels.head)
          labels = labels.tail
          newDerivation2
        case _ =>
          derivation
      }
      derivation2.children.indices.foldLeft(derivation2) { case (derivation, childChoice) =>
        derivation.transformChild(childChoice, relabel)
      }
    }

  }

}
