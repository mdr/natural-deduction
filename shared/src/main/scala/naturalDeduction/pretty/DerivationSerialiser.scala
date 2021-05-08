package naturalDeduction.pretty

import naturalDeduction.Derivation._
import naturalDeduction.{Derivation, Label}

object DerivationSerialiser {

  private def serialiseLabel(label: Label): String = s""""$label""""

  private def serialiseLabelWithComma(label: Option[Label]): String = label match {
    case Some(label) => s",${serialiseLabel(label)}"
    case None => ""
  }

  def serialise(derivations: Seq[Derivation]): String = derivations.map(serialise).mkString(",")

  def serialise(derivation: Derivation): String = derivation match {
    case Axiom(conclusion, label) =>
      s"Ax($conclusion${serialiseLabelWithComma(label)})"
    case ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      s"∧I(${serialise(leftDerivation)},${serialise(rightDerivation)})"
    case LeftConjunctionElimination(conjunctionDerivation) =>
      s"∧E1(${serialise(conjunctionDerivation)})"
    case RightConjunctionElimination(conjunctionDerivation) =>
      s"∧E2(${serialise(conjunctionDerivation)})"
    case ImplicationIntroduction(antecedent, label, consequentDerivation) =>
      s"→I($antecedent${serialiseLabelWithComma(label)},${serialise(consequentDerivation)})"
    case ImplicationElimination(antecedentDerivation, implicationDerivation) =>
      s"→E(${serialise(antecedentDerivation)},${serialise(implicationDerivation)})"
    case EquivalenceIntroduction(forwardsDerivation, backwardsDerivation) =>
      s"↔I(${serialise(forwardsDerivation)},${serialise(backwardsDerivation)})"
    case ForwardsEquivalenceElimination(equivalenceDerivation) =>
      s"↔E1(${serialise(equivalenceDerivation)})"
    case BackwardsEquivalenceElimination(equivalenceDerivation) =>
      s"↔E2(${serialise(equivalenceDerivation)})"
    case NegationIntroduction(statement, label, bottomDerivation) =>
      s"¬I($statement${serialiseLabelWithComma(label)},${serialise(bottomDerivation)})"
    case NegationElimination(positiveDerivation, negativeDerivation) =>
      s"¬E(${serialise(positiveDerivation)},${serialise(negativeDerivation)})"
    case ReductioAdAbsurdum(conclusion, label, bottomDerivation) =>
      s"RAA($conclusion${serialiseLabelWithComma(label)},${serialise(bottomDerivation)})"
    case LeftDisjunctionIntroduction(leftDerivation, right) =>
      s"∨I1(${serialise(leftDerivation)},$right)"
    case RightDisjunctionIntroduction(left, rightDerivation) =>
      s"∨I2($left,${serialise(rightDerivation)})"
    case DisjunctionElimination(disjunctionDerivation, leftLabel, leftDerivation, rightLabel, rightDerivation) =>
      s"∨E(${serialise(disjunctionDerivation)}${serialiseLabelWithComma(leftLabel)},${serialise(leftDerivation)}${serialiseLabelWithComma(rightLabel)},${serialise(rightDerivation)})"
  }

}
