package naturalDeduction.pretty

import naturalDeduction.Derivation
import naturalDeduction.Derivation._

object DerivationRenderer {

  def renderDerivation(derivation: Derivation, availableLabels: Set[String] = Set.empty): RaggedTextRegion = derivation match {
    case Axiom(formula, label) =>
      val isDischarged = availableLabels.intersect(label.toSet).nonEmpty
      val labelPrefix = label.map( _ + " ").getOrElse("")
      val renderedFormula = labelPrefix + (if (isDischarged) "[" + formula.toString + "]" else formula.toString)
      RaggedTextRegion(Seq(LineAndOffset(renderedFormula)))
    case ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      renderTwoChildren(derivation, renderDerivation(leftDerivation, availableLabels), renderDerivation(rightDerivation, availableLabels), "∧I")
    case LeftConjunctionElimination(conjunctionDerivation) =>
      renderSingleChild(derivation, renderDerivation(conjunctionDerivation, availableLabels), "∧E")
    case RightConjunctionElimination(conjunctionDerivation) =>
      renderSingleChild(derivation, renderDerivation(conjunctionDerivation, availableLabels), "∧E")
    case ImplicationIntroduction(_, label, consequentDerivation) =>
      renderSingleChild(derivation, renderDerivation(consequentDerivation, availableLabels ++ label), "→I", label)
    case ImplicationElimination(antecedentDerivation, implicationDerivation) =>
      renderTwoChildren(derivation, renderDerivation(antecedentDerivation, availableLabels), renderDerivation(implicationDerivation, availableLabels), "→E")
    case EquivalenceIntroduction(forwardsDerivation, backwardsDerivation) =>
      renderTwoChildren(derivation, renderDerivation(forwardsDerivation, availableLabels), renderDerivation(backwardsDerivation, availableLabels), "↔I")
    case ForwardsEquivalenceElimination(equivalenceDerivation) =>
      renderSingleChild(derivation, renderDerivation(equivalenceDerivation, availableLabels), "↔E")
    case BackwardsEquivalenceElimination(equivalenceDerivation) =>
      renderSingleChild(derivation, renderDerivation(equivalenceDerivation, availableLabels), "↔E")
    case NegationIntroduction(_, label, bottomDerivation) =>
      renderSingleChild(derivation, renderDerivation(bottomDerivation, availableLabels ++ label), "¬I", label)
    case NegationElimination(positiveDerivation, negativeDerivation) =>
      renderTwoChildren(derivation, renderDerivation(positiveDerivation, availableLabels), renderDerivation(negativeDerivation, availableLabels), "¬E")
    case ReductioAdAbsurdum(_, label, bottomDerivation) =>
      renderSingleChild(derivation, renderDerivation(bottomDerivation, availableLabels ++ label), "RAA", label)
    case LeftDisjunctionIntroduction(leftDerivation, _) =>
      renderSingleChild(derivation, renderDerivation(leftDerivation, availableLabels ), "∨I")
    case RightDisjunctionIntroduction(_, rightDerivation) =>
      renderSingleChild(derivation, renderDerivation(rightDerivation, availableLabels ), "∨I")
  }

  private def renderTwoChildren(parent: Derivation, child1Region: RaggedTextRegion, child2Region: RaggedTextRegion, ruleName: String): RaggedTextRegion = {
    val minHorizSpacing = 3
    val formulaString = parent.formula.toString
    val lengthOfChildLineWhenClosestPacked = {
      val childrenRegion = child1Region.pasteHorizontal(child2Region, minHorizSpacing)
      val firstChildLine = childrenRegion.lines.head
      firstChildLine.text.length
    }
    val ruleLineWidth = formulaString.length max lengthOfChildLineWhenClosestPacked
    val actualHorizontalSpacing = minHorizSpacing + 0.max(ruleLineWidth - lengthOfChildLineWhenClosestPacked)
    val childrenRegion = child1Region.pasteHorizontal(child2Region, actualHorizontalSpacing)
    val firstChildLine = childrenRegion.lines.head
    val (parentFormulaOffset, _) = calculateLeftRightPaddingForCentering(formulaString, ruleLineWidth)
    val parentRegion =
      RaggedTextRegion(Seq(
        LineAndOffset(formulaString, parentFormulaOffset),
        LineAndOffset("─" * ruleLineWidth + " " + ruleName),
      ))
    childrenRegion pasteVertical parentRegion.shiftHorizontal(firstChildLine.offset)
  }

  private def renderSingleChild(parent: Derivation, childRegion: RaggedTextRegion, ruleName: String, label: Option[String] = None): RaggedTextRegion = {
    val formulaString = parent.formula.toString
    val firstChildLine = childRegion.lines.head
    val firstChildLineOffset = firstChildLine.offset // beta
    val ruleLineWidth = formulaString.length max firstChildLine.text.length
    val (parentFormulaOffset, _) = calculateLeftRightPaddingForCentering(formulaString, ruleLineWidth)
    val labelPrefix = label.map(_ + " ").getOrElse("")
    val parentRegion =
      RaggedTextRegion(Seq(
        LineAndOffset(formulaString, parentFormulaOffset + labelPrefix.length),
        LineAndOffset(labelPrefix + "─" * ruleLineWidth + " " + ruleName),
      ))
    val (childFormulaOffset, _) = calculateLeftRightPaddingForCentering(firstChildLine.text, ruleLineWidth) // alpha
    childRegion.shiftHorizontal(0.max((childFormulaOffset + labelPrefix.length) - firstChildLineOffset)) pasteVertical
      parentRegion.shiftHorizontal(0.max(firstChildLineOffset - (childFormulaOffset + labelPrefix.length)))
  }

  private def calculateLeftRightPaddingForCentering(s: String, width: Int): (Int, Int) = {
    assert(s.length <= width, s"String '$s' is longer (${s.length}) than given width $width")
    val excess = width - s.length
    val equalPadding = excess / 2
    val bonusPadding = excess % 2
    (equalPadding, equalPadding + bonusPadding)
  }

}
