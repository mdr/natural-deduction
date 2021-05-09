package ljt

import naturalDeduction.Derivation.{BackwardsEquivalenceElimination, ConjunctionIntroduction, DisjunctionElimination, EquivalenceIntroduction, ForwardsEquivalenceElimination, ImplicationElimination, ImplicationIntroduction, LeftConjunctionElimination, LeftDisjunctionIntroduction, NegationElimination, NegationIntroduction, ReductioAdAbsurdum, RichFormula, RightConjunctionElimination, RightDisjunctionIntroduction}
import naturalDeduction.Formula.{PropositionalVariable, ⊥}
import naturalDeduction.{Derivation, Formula, Sequent}
import naturalDeduction.Labels.{freshLabel, twoFreshLabels}

object LJTSequent {

  def apply(conclusion: Formula): LJTSequent = LJTSequent(Multiset.empty[Formula], conclusion)

  def apply(sequent: Sequent): LJTSequent = LJTSequent(Multiset(sequent.assumptions.toSeq: _*), sequent.conclusion)

}

case class LJTSequent(assumptions: Multiset[Formula], conclusion: Formula) {

  override def toString = s"${assumptions.keys.mkString(", ")} => $conclusion"

}

sealed trait LJTDerivation {

  val sequent: LJTSequent

  def assumptions: Multiset[Formula] = sequent.assumptions

  def conclusion: Formula = sequent.conclusion

  def naturalDeductionDerivation: Derivation = {
    val derivation = _naturalDeductionDerivation
    assert(derivation.conclusion == conclusion,
      s"ND derivation conclusion (${derivation.conclusion} did not equal LTJ conclusion ($conclusion) for $getClass")
    derivation
  }

  protected def _naturalDeductionDerivation: Derivation

}

object LJTDerivation {

  case class Axiom(formula: Formula, Γ: Multiset[Formula]) extends LJTDerivation {
    override val sequent: LJTSequent = LJTSequent(Γ + formula, formula)

    override def _naturalDeductionDerivation: Derivation = formula.axiom
  }

  case class BottomLeft(Γ: Multiset[Formula], override val conclusion: Formula) extends LJTDerivation {
    override val sequent: LJTSequent = LJTSequent(Γ + ⊥, conclusion)

    override def _naturalDeductionDerivation: Derivation = ReductioAdAbsurdum(conclusion, ⊥.axiom)
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

    override def _naturalDeductionDerivation: Derivation =
      childDerivation.naturalDeductionDerivation
        .substitute(conjunct1, LeftConjunctionElimination((conjunct1 ∧ conjunct2).axiom))
        .substitute(conjunct2, RightConjunctionElimination((conjunct1 ∧ conjunct2).axiom))
  }

  case class NegationLeft(formula: Formula,
                          Γ: Multiset[Formula],
                          override val conclusion: Formula,
                          childDerivation: LJTDerivation
                         ) extends LJTDerivation {
    assert(childDerivation.conclusion == conclusion)
    assert(childDerivation.assumptions == Γ + (formula → ⊥))

    override val sequent: LJTSequent = LJTSequent(Γ + formula.not, conclusion)

    override def _naturalDeductionDerivation: Derivation = {
      val natDerivation = childDerivation.naturalDeductionDerivation
      val label = freshLabel(natDerivation.labels)
      childDerivation.naturalDeductionDerivation
        .substitute(formula → ⊥,
          ImplicationIntroduction(formula, label,
            NegationElimination(
              Derivation.Axiom(formula, label),
              formula.not.axiom)))
    }
  }

  case class EquivalenceLeft(formula1: Formula,
                             formula2: Formula,
                             Γ: Multiset[Formula],
                             override val conclusion: Formula,
                             childDerivation: LJTDerivation
                            ) extends LJTDerivation {
    assert(childDerivation.conclusion == conclusion)
    assert(childDerivation.assumptions == (Γ + (formula1 → formula2) + (formula2 → formula1)))

    override val sequent: LJTSequent = LJTSequent(Γ + (formula1 ↔ formula2), conclusion)

    override def _naturalDeductionDerivation: Derivation =
      childDerivation.naturalDeductionDerivation
        .substitute(formula1 → formula2, ForwardsEquivalenceElimination((formula1 ↔ formula2).axiom))
        .substitute(formula2 → formula1, BackwardsEquivalenceElimination((formula1 ↔ formula2).axiom))
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

    override def _naturalDeductionDerivation: Derivation =
      ConjunctionIntroduction(
        childDerivation1.naturalDeductionDerivation,
        childDerivation2.naturalDeductionDerivation)
  }

  case class EquivalenceRight(formula1: Formula,
                              formula2: Formula,
                              Γ: Multiset[Formula],
                              childDerivation1: LJTDerivation,
                              childDerivation2: LJTDerivation
                             ) extends LJTDerivation {
    assert(childDerivation1.conclusion == (formula1 → formula2))
    assert(childDerivation2.conclusion == (formula2 → formula1))
    assert(childDerivation1.assumptions == Γ)
    assert(childDerivation2.assumptions == Γ)

    override val sequent: LJTSequent = LJTSequent(Γ, formula1 ↔ formula2)

    override def _naturalDeductionDerivation: Derivation =
      EquivalenceIntroduction(
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

    override def _naturalDeductionDerivation: Derivation = {
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

    override def _naturalDeductionDerivation: Derivation =
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

    override def _naturalDeductionDerivation: Derivation =
      RightDisjunctionIntroduction(disjunct1, childDerivation.naturalDeductionDerivation)
  }

  case class NegationRight(
                            formula: Formula,
                            Γ: Multiset[Formula],
                            childDerivation: LJTDerivation
                          ) extends LJTDerivation {
    assert(childDerivation.conclusion == (formula → ⊥))
    assert(childDerivation.assumptions == Γ)

    override val sequent: LJTSequent = LJTSequent(Γ, formula.not)

    override def _naturalDeductionDerivation: Derivation = {
      val natDerivation = childDerivation.naturalDeductionDerivation
      val label = freshLabel(natDerivation.labels)
      NegationIntroduction(formula, label,
        ImplicationElimination(
          Derivation.Axiom(formula, label),
          childDerivation.naturalDeductionDerivation))
    }
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

    override def _naturalDeductionDerivation: Derivation = {
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

    override def _naturalDeductionDerivation: Derivation =
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

    override def _naturalDeductionDerivation: Derivation = {
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

  case class ImplicationLeft2b(c: Formula,
                               d: Formula,
                               b: Formula,
                               Γ: Multiset[Formula],
                               override val conclusion: Formula,
                               childDerivation: LJTDerivation) extends LJTDerivation {
    assert(childDerivation.conclusion == conclusion)
    assert(childDerivation.assumptions == Γ + ((c → d) → ((d → c) → b)))

    override val sequent: LJTSequent = LJTSequent(Γ + ((c ↔ d) → b), conclusion)

    override def _naturalDeductionDerivation: Derivation = {
      val natDerivation = childDerivation.naturalDeductionDerivation
      val label1 = freshLabel(natDerivation.labels)
      val label2 = freshLabel(natDerivation.labels + label1)
      natDerivation.substitute((c → d) → ((d → c) → b),
        ImplicationIntroduction(c → d, label1,
          ImplicationIntroduction(d → c, label2,
            ImplicationElimination(
              EquivalenceIntroduction(
                Derivation.Axiom(c → d, label1),
                Derivation.Axiom(d → c, label2)),
              ((c ↔ d) → b).axiom
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

    override def _naturalDeductionDerivation: Derivation = {
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

    override def _naturalDeductionDerivation: Derivation = {
      val natDerivation1 = childDerivation1.naturalDeductionDerivation
      val natDerivation2 = childDerivation2.naturalDeductionDerivation
      val label = freshLabel(natDerivation1.labels ++ natDerivation2.labels)
      //      println("====")
      //      println(s"child1 = ${childDerivation1.sequent}")
      //      println(s"child2 = ${childDerivation2.sequent}")
      //      println(s"b = $b")
      //      println(s"c = $c")
      //      println(s"d = $d")
      //      println(s"natDerivation1: ${natDerivation1.sequent}")
      //      println(s"natDerivation2: ${natDerivation2.sequent}")
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

  case class ImplicationLeft4a(c: Formula,
                               b: Formula,
                               Γ: Multiset[Formula],
                               override val conclusion: Formula,
                               childDerivation: LJTDerivation) extends LJTDerivation {

    assert(childDerivation.conclusion == conclusion)
    assert(childDerivation.assumptions == Γ + ((c → ⊥) → b))

    override val sequent: LJTSequent = LJTSequent(Γ + (c.not → b), conclusion)

    override def _naturalDeductionDerivation: Derivation = {
      val natDerivation = childDerivation.naturalDeductionDerivation
      val (label1, label2) = twoFreshLabels(natDerivation.labels)
      natDerivation.substitute((c → ⊥) → b,
        ImplicationIntroduction(c → ⊥, label1,
          ImplicationElimination(
            NegationIntroduction(c, label2,
              ImplicationElimination(
                Derivation.Axiom(c, label2),
                Derivation.Axiom(c → ⊥, label1))),
            (c.not → b).axiom
          )))
    }
  }

}