package ljt

import ljt.LJTDerivation._
import naturalDeduction.Formula.{Conjunction, Disjunction, Implication, PropositionalVariable, ⊥}
import naturalDeduction.{Derivation, Formula, Sequent}

import scala.PartialFunction.condOpt

object LJTTheoremProver {

  def prove(sequent: Sequent): Option[Derivation] = {
    val expandedSequent = replaceEquivalences(sequent)
    prove(LJTSequent(expandedSequent)).map(derivation => ProofFixer.fix(sequent, derivation.naturalDeductionDerivation))
  }

  private def replaceEquivalences(sequent: Sequent): Sequent =
    Sequent(sequent.assumptions.map(_.replaceEquivalences), sequent.conclusion.replaceEquivalences)

  def prove(sequent: LJTSequent): Option[LJTDerivation] = {
    val proof = proveAxiom(sequent) orElse
      proveBottomLeft(sequent) orElse
      proveConjunctionRight(sequent) orElse
      proveDisjunctionRight1(sequent) orElse
      proveDisjunctionRight2(sequent) orElse
      proveImplicationRight(sequent) orElse
      proveConjunctionLeft(sequent) orElse
      proveDisjunctionLeft(sequent) orElse
      proveImplicationLeft1(sequent) orElse
      proveImplicationLeft2(sequent) orElse
      proveImplicationLeft3(sequent) orElse
      proveImplicationLeft4(sequent)
    proof.foreach(p => assert(p.sequent == sequent))
    proof
  }

  private def proveAxiom(sequent: LJTSequent): Option[LJTDerivation] =
    if (sequent.assumptions contains sequent.conclusion) {
      Some(Axiom(sequent.conclusion, sequent.assumptions - sequent.conclusion))
    } else
      None

  private def proveBottomLeft(sequent: LJTSequent): Option[LJTDerivation] =
    if (sequent.assumptions contains ⊥) {
      Some(BottomLeft(sequent.assumptions - ⊥, sequent.conclusion))
    } else
      None

  private def proveImplicationRight(sequent: LJTSequent): Option[LJTDerivation] =
    condOptFlatten(sequent.conclusion) {
      case Implication(antecedent, consequent) =>
        val childSequent = LJTSequent(sequent.assumptions + antecedent, consequent)
        prove(childSequent).map(childDerivation =>
          ImplicationRight(antecedent, consequent, sequent.assumptions, childDerivation))
    }

  private def proveConjunctionRight(sequent: LJTSequent): Option[LJTDerivation] =
    condOptFlatten(sequent.conclusion) {
      case Conjunction(conjunct1, conjunct2) =>
        val childSequent1 = LJTSequent(sequent.assumptions, conjunct1)
        val childSequent2 = LJTSequent(sequent.assumptions, conjunct2)
        for {
          childDerivation1 <- prove(childSequent1)
          childDerivation2 <- prove(childSequent2)
        } yield ConjunctionRight(conjunct1, conjunct2, sequent.assumptions, childDerivation1, childDerivation2)
    }

  private def proveDisjunctionRight1(sequent: LJTSequent): Option[LJTDerivation] =
    condOptFlatten(sequent.conclusion) {
      case Disjunction(disjunct1, disjunct2) =>
        val childSequent = LJTSequent(sequent.assumptions, disjunct1)
        for {
          childDerivation <- prove(childSequent)
        } yield DisjunctionRight1(disjunct1, disjunct2, sequent.assumptions, childDerivation)
    }

  private def proveDisjunctionRight2(sequent: LJTSequent): Option[LJTDerivation] =
    condOptFlatten(sequent.conclusion) {
      case Disjunction(disjunct1, disjunct2) =>
        val childSequent = LJTSequent(sequent.assumptions, disjunct2)
        for {
          childDerivation <- prove(childSequent)
        } yield DisjunctionRight2(disjunct1, disjunct2, sequent.assumptions, childDerivation)
    }

  private def proveConjunctionLeft(sequent: LJTSequent): Option[LJTDerivation] = {
    val matchingAssumptions = sequent.assumptions.keySet.collect { case conjunction: Conjunction => conjunction }.to(LazyList)

    def proveForAssumption(assumption: Conjunction): Option[LJTDerivation] = {
      val childSequent = LJTSequent(sequent.assumptions - assumption + assumption.conjunct1 + assumption.conjunct2, sequent.conclusion)
      for {
        childDerivation <- prove(childSequent)
      } yield ConjunctionLeft(assumption.conjunct1, assumption.conjunct2, sequent.assumptions - assumption, sequent.conclusion, childDerivation)
    }

    matchingAssumptions.flatMap(proveForAssumption).headOption
  }

  private def proveDisjunctionLeft(sequent: LJTSequent): Option[LJTDerivation] = {
    val matchingAssumptions = sequent.assumptions.keySet.collect { case disjunction: Disjunction => disjunction }.to(LazyList)

    def proveForAssumption(assumption: Disjunction): Option[LJTDerivation] = {
      val childSequent1 = LJTSequent(sequent.assumptions - assumption + assumption.disjunct1, sequent.conclusion)
      val childSequent2 = LJTSequent(sequent.assumptions - assumption + assumption.disjunct2, sequent.conclusion)
      for {
        childDerivation1 <- prove(childSequent1)
        childDerivation2 <- prove(childSequent2)
      } yield DisjunctionLeft(assumption.disjunct1, assumption.disjunct2, sequent.assumptions - assumption, sequent.conclusion, childDerivation1, childDerivation2)
    }

    matchingAssumptions.flatMap(proveForAssumption).headOption
  }

  private def proveImplicationLeft1(sequent: LJTSequent): Option[LJTDerivation] = {
    val matchingAssumptions =
      sequent.assumptions
        .keySet
        .collect { case implication: Implication if implication.antecedent.isInstanceOf[PropositionalVariable] => implication }
        .filter(implication => sequent.assumptions.keySet contains implication.antecedent)
        .to(LazyList)

    def proveForAssumption(assumption: Implication): Option[LJTDerivation] = {
      import assumption.{consequent, antecedent}
      val baseAssumptions = sequent.assumptions - (antecedent → consequent) - antecedent
      val childSequent = LJTSequent(baseAssumptions + antecedent + consequent, sequent.conclusion)
      for {
        childDerivation <- prove(childSequent)
      } yield ImplicationLeft1(antecedent, consequent, baseAssumptions, sequent.conclusion, childDerivation)

    }

    matchingAssumptions.flatMap(proveForAssumption).headOption
  }

  object ImplicationLeftAssumption2 {

    def unapply(formula: Formula): Option[(Formula, Formula, Formula)] = condOpt(formula) {
      case Implication(Conjunction(c, d), b) => (c, d, b)
    }

  }

  private def proveImplicationLeft2(sequent: LJTSequent): Option[LJTDerivation] = {
    val matchingAssumptions =
      sequent.assumptions
        .keySet
        .collect { case implication@ImplicationLeftAssumption2(_, _, _) => implication }
        .to(LazyList)
    def proveForAssumption(assumption: Formula): Option[LJTDerivation] = {
      val ImplicationLeftAssumption2(c, d, b) = assumption
      val baseAssumptions = sequent.assumptions - (c ∧ d → b)
      val childSequent = LJTSequent(baseAssumptions + (c → (d → b)), sequent.conclusion)
      for {
        childDerivation <- prove(childSequent)
      } yield ImplicationLeft2(c, d, b, baseAssumptions, sequent.conclusion, childDerivation)
    }

    matchingAssumptions.flatMap(proveForAssumption).headOption
  }

  object ImplicationLeftAssumption3 {

    def unapply(formula: Formula): Option[(Formula, Formula, Formula)] = condOpt(formula) {
      case Implication(Disjunction(c, d), b) => (c, d, b)
    }

  }

  private def proveImplicationLeft3(sequent: LJTSequent): Option[LJTDerivation] = {
    val matchingAssumptions =
      sequent.assumptions
        .keySet
        .collect { case implication@ImplicationLeftAssumption3(_, _, _) => implication }
        .to(LazyList)

    def proveForAssumption(assumption: Formula): Option[LJTDerivation] = {
      val ImplicationLeftAssumption3(c, d, b) = assumption
      val baseAssumptions = sequent.assumptions - ((c ∨ d) → b)
      val childSequent = LJTSequent(baseAssumptions + (c → b) + (d → b), sequent.conclusion)
      for {
        childDerivation <- prove(childSequent)
      } yield ImplicationLeft3(b, c, d, baseAssumptions, sequent.conclusion, childDerivation)
    }

    matchingAssumptions.flatMap(proveForAssumption).headOption
  }

  object ImplicationLeftAssumption4 {

    def unapply(formula: Formula): Option[(Formula, Formula, Formula)] = condOpt(formula) {
      case Implication(Implication(c, d), b) => (c, d, b)
    }

  }

  private def proveImplicationLeft4(sequent: LJTSequent): Option[LJTDerivation] = {
    val matchingAssumptions =
      sequent.assumptions
        .keySet
        .collect { case implication@ImplicationLeftAssumption4(_, _, _) => implication }
        .to(LazyList)
    def proveForAssumption(assumption: Formula): Option[LJTDerivation] = {
      val ImplicationLeftAssumption4(c, d, b) = assumption
      val baseAssumptions = sequent.assumptions - ((c → d) → b)
      val childSequent1 = LJTSequent(baseAssumptions + (d → b), c → d)
      val childSequent2 = LJTSequent(baseAssumptions + b, sequent.conclusion)
      for {
        childDerivation1 <- prove(childSequent1)
        childDerivation2 <- prove(childSequent2)
      } yield ImplicationLeft4(b, c, d, baseAssumptions, sequent.conclusion, childDerivation1, childDerivation2)
    }

    matchingAssumptions.flatMap(proveForAssumption).headOption
  }

  private def condOptFlatten[T, U](x: T)(pf: PartialFunction[T, Option[U]]): Option[U] = condOpt(x)(pf).flatten

}
