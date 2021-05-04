package ljt

import naturalDeduction.Derivation.{ConjunctionIntroduction, DisjunctionElimination, ImplicationElimination, ImplicationIntroduction, LeftConjunctionElimination, LeftDisjunctionIntroduction, ReductioAdAbsurdum, RichFormula, RightConjunctionElimination, RightDisjunctionIntroduction}
import naturalDeduction.Formula.{PropositionalVariable, ⊥}
import naturalDeduction.{Derivation, Formula, Sequent}
import naturalDeduction.Labels.freshLabel

object Multiset {

  def empty[T]: Multiset[T] = Multiset[T](Map.empty[T, Int])

  def apply[T](items: T*): Multiset[T] = items.foldLeft(empty[T])(_ + _)
}

case class Multiset[T](map: Map[T, Int] = Map.empty) {
  assert(map.values.forall(_ > 0))

  def contains(t: T): Boolean = map contains t

  def keySet: Set[T] = map.keySet

  def +(t: T): Multiset[T] = copy(map = map + (t -> (map.getOrElse(t, 0) + 1)))

  def -(t: T): Multiset[T] = map.get(t) match {
    case None => this
    case Some(1) => copy(map = map - t)
    case Some(n) => copy(map = map + (t -> (n - 1)))
  }
}

object LJTSequent {

  def apply(conclusion: Formula): LJTSequent = LJTSequent(Multiset.empty[Formula], conclusion)

  def apply(sequent: Sequent): LJTSequent = LJTSequent(Multiset(sequent.assumptions.toSeq: _*), sequent.conclusion)

}

case class LJTSequent(assumptions: Multiset[Formula], conclusion: Formula) {

}

sealed trait LJTDerivation {

  val sequent: LJTSequent

  def assumptions: Multiset[Formula] = sequent.assumptions

  def conclusion: Formula = sequent.conclusion

  def naturalDeductionDerivation: Derivation

}

object LJTDerivation {

  case class Axiom(formula: Formula, Γ: Multiset[Formula]) extends LJTDerivation {
    override val sequent: LJTSequent = LJTSequent(Γ + formula, formula)

    override def naturalDeductionDerivation: Derivation = formula.axiom
  }

  case class BottomLeft(Γ: Multiset[Formula], override val conclusion: Formula) extends LJTDerivation {
    override val sequent: LJTSequent = LJTSequent(Γ + ⊥, conclusion)

    override def naturalDeductionDerivation: Derivation = ReductioAdAbsurdum(conclusion, ⊥.axiom)
  }

  case class ConjunctionLeft(conjunct1: Formula,
                             conjunct2: Formula,
                             Γ: Multiset[Formula],
                             override val conclusion: Formula,
                             childDerivation: LJTDerivation
                            ) extends LJTDerivation {
    assert(childDerivation.conclusion == conclusion)
    assert(childDerivation.assumptions == Γ + conjunct1 + conjunct2)

    override val sequent: LJTSequent = LJTSequent(Γ + (conjunct1 ∧ conjunct2), conclusion)

    override def naturalDeductionDerivation: Derivation =
      childDerivation.naturalDeductionDerivation
        .substitute(conjunct1, LeftConjunctionElimination((conjunct1 ∧ conjunct2).axiom))
        .substitute(conjunct2, RightConjunctionElimination((conjunct1 ∧ conjunct2).axiom))
  }

  case class ConjunctionRight(conjunct1: Formula,
                              conjunct2: Formula,
                              Γ: Multiset[Formula],
                              childDerivation1: LJTDerivation,
                              childDerivation2: LJTDerivation
                             ) extends LJTDerivation {
    assert(childDerivation1.conclusion == conjunct1)
    assert(childDerivation2.conclusion == conjunct2)
    assert(childDerivation1.assumptions == Γ)
    assert(childDerivation2.assumptions == Γ)

    override val sequent: LJTSequent = LJTSequent(Γ, conjunct1 ∧ conjunct2)

    override def naturalDeductionDerivation: Derivation =
      ConjunctionIntroduction(
        childDerivation1.naturalDeductionDerivation,
        childDerivation2.naturalDeductionDerivation)
  }

  case class DisjunctionLeft(disjunct1: Formula,
                             disjunct2: Formula,
                             Γ: Multiset[Formula],
                             override val conclusion: Formula,
                             childDerivation1: LJTDerivation,
                             childDerivation2: LJTDerivation
                            ) extends LJTDerivation {
    assert(childDerivation1.conclusion == conclusion)
    assert(childDerivation2.conclusion == conclusion)
    assert(childDerivation1.assumptions == Γ + disjunct1)
    assert(childDerivation2.assumptions == Γ + disjunct2)

    override val sequent: LJTSequent = LJTSequent(Γ + (disjunct1 ∨ disjunct2), conclusion)

    override def naturalDeductionDerivation: Derivation = {
      val natDerivation1 = childDerivation1.naturalDeductionDerivation
      val natDerivation2 = childDerivation2.naturalDeductionDerivation
      val label1 = freshLabel(natDerivation1.labels ++ natDerivation2.labels)
      val label2 = freshLabel(natDerivation1.labels ++ natDerivation2.labels + label1)
      DisjunctionElimination(
        (disjunct1 ∨ disjunct2).axiom,
        Some(label1),
        natDerivation1.substitute(disjunct1, Derivation.Axiom(disjunct1, label1)),
        Some(label2),
        natDerivation2.substitute(disjunct2, Derivation.Axiom(disjunct2, label2)))
    }
  }

  case class DisjunctionRight1(
                                disjunct1: Formula,
                                disjunct2: Formula,
                                Γ: Multiset[Formula],
                                childDerivation: LJTDerivation
                              ) extends LJTDerivation {
    assert(childDerivation.conclusion == disjunct1)
    assert(childDerivation.assumptions == Γ)

    override val sequent: LJTSequent = LJTSequent(Γ, disjunct1 ∨ disjunct2)

    override def naturalDeductionDerivation: Derivation =
      LeftDisjunctionIntroduction(childDerivation.naturalDeductionDerivation, disjunct2)
  }

  case class DisjunctionRight2(
                                disjunct1: Formula,
                                disjunct2: Formula,
                                Γ: Multiset[Formula],
                                childDerivation: LJTDerivation
                              ) extends LJTDerivation {
    assert(childDerivation.conclusion == disjunct2)
    assert(childDerivation.assumptions == Γ)

    override val sequent: LJTSequent = LJTSequent(Γ, disjunct1 ∨ disjunct2)

    override def naturalDeductionDerivation: Derivation =
      RightDisjunctionIntroduction(disjunct1, childDerivation.naturalDeductionDerivation)
  }

  case class ImplicationRight(
                               antecedent: Formula,
                               consequent: Formula,
                               Γ: Multiset[Formula],
                               childDerivation: LJTDerivation
                             ) extends LJTDerivation {
    assert(childDerivation.conclusion == consequent)
    assert(childDerivation.assumptions == Γ + antecedent, s"Expected child assumptions to be ${Γ + antecedent}, but was ${childDerivation.assumptions}")

    override val sequent: LJTSequent = LJTSequent(Γ, antecedent → consequent)

    override def naturalDeductionDerivation: Derivation = {
      val natDerivation = childDerivation.naturalDeductionDerivation
      val label = freshLabel(natDerivation.labels)
      ImplicationIntroduction(antecedent, label,
        natDerivation.substitute(antecedent, Derivation.Axiom(antecedent, label)))
    }
  }

  case class ImplicationLeft1(antecedent: Formula,
                              consequent: Formula,
                              Γ: Multiset[Formula],
                              override val conclusion: Formula,
                              childDerivation: LJTDerivation) extends LJTDerivation {
    assert(childDerivation.conclusion == conclusion)
    assert(childDerivation.assumptions == Γ + antecedent + consequent)
    assert(antecedent.isInstanceOf[PropositionalVariable])

    override val sequent: LJTSequent = LJTSequent(Γ + antecedent + (antecedent → consequent), conclusion)

    override def naturalDeductionDerivation: Derivation =
      childDerivation.naturalDeductionDerivation.substitute(consequent, ImplicationElimination(antecedent, consequent))
  }

  case class ImplicationLeft2(c: Formula,
                              d: Formula,
                              b: Formula,
                              Γ: Multiset[Formula],
                              override val conclusion: Formula,
                              childDerivation: LJTDerivation) extends LJTDerivation {
    assert(childDerivation.conclusion == conclusion)
    assert(childDerivation.assumptions == Γ + (c → (d → b)))

    override val sequent: LJTSequent = LJTSequent(Γ + (c ∧ d → b), conclusion)

    override def naturalDeductionDerivation: Derivation = {
      val natDerivation = childDerivation.naturalDeductionDerivation
      val label1 = freshLabel(natDerivation.labels)
      val label2 = freshLabel(natDerivation.labels + label1)
      natDerivation.substitute(c → (d → b),
        ImplicationIntroduction(c, label1,
          ImplicationIntroduction(d, label2,
            ImplicationElimination(
              ConjunctionIntroduction(
                Derivation.Axiom(c, label1),
                Derivation.Axiom(d, label2)),
              (c ∧ d → b).axiom
            ))))
    }
  }

  case class ImplicationLeft3(
                               b: Formula,
                               c: Formula,
                               d: Formula,
                               Γ: Multiset[Formula],
                               override val conclusion: Formula,
                               childDerivation: LJTDerivation
                             ) extends LJTDerivation {
    assert(childDerivation.conclusion == conclusion)
    assert(childDerivation.assumptions == Γ + (c → b) + (d → b))

    override val sequent: LJTSequent = LJTSequent(Γ + ((c ∨ d) → b), conclusion)

    override def naturalDeductionDerivation: Derivation = {
      val natDerivation = childDerivation.naturalDeductionDerivation
      val label1 = freshLabel(natDerivation.labels)
      val label2 = freshLabel(natDerivation.labels + label1)
      natDerivation
        .substitute(c → b,
          ImplicationIntroduction(c, label1,
            ImplicationElimination(
              LeftDisjunctionIntroduction(Derivation.Axiom(c, label1), d),
              ((c ∨ d) → b).axiom)))
        .substitute(d → b,
          ImplicationIntroduction(d, label2,
            ImplicationElimination(
              RightDisjunctionIntroduction(c, Derivation.Axiom(d, label2)),
              ((c ∨ d) → b).axiom)))
    }
  }

  case class ImplicationLeft4(b: Formula,
                              c: Formula,
                              d: Formula,
                              Γ: Multiset[Formula],
                              override val conclusion: Formula,
                              childDerivation1: LJTDerivation,
                              childDerivation2: LJTDerivation) extends LJTDerivation {
    assert(childDerivation1.conclusion == (c → d), s"Expected childDerivation1 conclusion to be ${c → d}, but was ${childDerivation1.conclusion}")
    assert(childDerivation1.assumptions == Γ + (d → b))
    assert(childDerivation2.conclusion == conclusion)
    assert(childDerivation2.assumptions == Γ + b)

    override val sequent: LJTSequent = LJTSequent(Γ + ((c → d) → b), conclusion)

    override def naturalDeductionDerivation: Derivation = {
      val natDerivation1 = childDerivation1.naturalDeductionDerivation
      val natDerivation2 = childDerivation2.naturalDeductionDerivation
      val label = freshLabel(natDerivation1.labels ++ natDerivation2.labels)
      natDerivation2.substitute(b,
        ImplicationElimination(
          natDerivation1.substitute(d → b,
            ImplicationIntroduction(d, label,
              ImplicationElimination(
                ImplicationIntroduction(c,
                  Derivation.Axiom(d, label)),
                ((c → d) → b).axiom
              ))),
          ((c → d) → b).axiom))
    }
  }

}