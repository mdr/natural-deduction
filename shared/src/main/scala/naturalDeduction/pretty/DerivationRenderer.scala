package naturalDeduction.pretty

import naturalDeduction.Derivation
import naturalDeduction.Derivation._

object DerivationRenderer {

  def renderDerivation(derivation: Derivation, labels: Set[String] = Set.empty): RaggedTextRegion = derivation match {
    case Axiom(formula, label) =>
      val isDischarged = labels.intersect(label.toSet).nonEmpty
      val labelPrefix = label.map(_ + " ").getOrElse("")
      val renderedFormula = labelPrefix + (if (isDischarged) "[" + formula.toString + "]" else formula.toString)
      RaggedTextRegion(Seq(LineAndOffset(renderedFormula)))
    case ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      renderTwoChildren(derivation, renderDerivation(leftDerivation, labels), renderDerivation(rightDerivation, labels), "∧I")
    case LeftConjunctionElimination(conjunctionDerivation) =>
      renderSingleChild(derivation, renderDerivation(conjunctionDerivation, labels), "∧E")
    case RightConjunctionElimination(conjunctionDerivation) =>
      renderSingleChild(derivation, renderDerivation(conjunctionDerivation, labels), "∧E")
    case ImplicationIntroduction(_, label, consequentDerivation) =>
      renderSingleChild(derivation, renderDerivation(consequentDerivation, labels ++ label), "→I", label)
    case ImplicationElimination(antecedentDerivation, implicationDerivation) =>
      renderTwoChildren(derivation, renderDerivation(antecedentDerivation, labels), renderDerivation(implicationDerivation, labels), "→E")
    case EquivalenceIntroduction(forwardsDerivation, backwardsDerivation) =>
      renderTwoChildren(derivation, renderDerivation(forwardsDerivation, labels), renderDerivation(backwardsDerivation, labels), "↔I")
    case ForwardsEquivalenceElimination(equivalenceDerivation) =>
      renderSingleChild(derivation, renderDerivation(equivalenceDerivation, labels), "↔E")
    case BackwardsEquivalenceElimination(equivalenceDerivation) =>
      renderSingleChild(derivation, renderDerivation(equivalenceDerivation, labels), "↔E")
    case NegationIntroduction(_, label, bottomDerivation) =>
      renderSingleChild(derivation, renderDerivation(bottomDerivation, labels ++ label), "¬I", label)
    case NegationElimination(positiveDerivation, negativeDerivation) =>
      renderTwoChildren(derivation, renderDerivation(positiveDerivation, labels), renderDerivation(negativeDerivation, labels), "¬E")
    case ReductioAdAbsurdum(_, label, bottomDerivation) =>
      renderSingleChild(derivation, renderDerivation(bottomDerivation, labels ++ label), "RAA", label)
    case LeftDisjunctionIntroduction(leftDerivation, _) =>
      renderSingleChild(derivation, renderDerivation(leftDerivation, labels), "∨I")
    case RightDisjunctionIntroduction(_, rightDerivation) =>
      renderSingleChild(derivation, renderDerivation(rightDerivation, labels), "∨I")
    case DisjunctionElimination(disjunctionDerivation, leftLabel, leftDerivation, rightLabel, rightDerivation) =>
      renderThreeChildren(
        derivation,
        renderDerivation(disjunctionDerivation, labels),
        renderDerivation(leftDerivation, labels ++ leftLabel),
        renderDerivation(rightDerivation, labels ++ rightLabel),
        "∨E",
        (leftLabel.toSeq ++ rightLabel).mkString(" ") match { case "" => None; case s => Some(s) })
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

  private def renderThreeChildren(parent: Derivation,
                                  child1Region: RaggedTextRegion,
                                  child2Region: RaggedTextRegion,
                                  child3Region: RaggedTextRegion,
                                  ruleName: String,
                                  label: Option[String] = None): RaggedTextRegion = {
    val minHorizSpacing = 3
    val formulaString = parent.formula.toString
    val lengthOfChildLineWhenClosestPacked = {
      val childrenRegion = child1Region
        .pasteHorizontal(child2Region, minHorizSpacing)
        .pasteHorizontal(child3Region, minHorizSpacing)
      val firstChildLine = childrenRegion.lines.head
      firstChildLine.text.length
    }
    val ruleLineWidth = formulaString.length max lengthOfChildLineWhenClosestPacked
    val excessSpace = 0.max(ruleLineWidth - lengthOfChildLineWhenClosestPacked)
    val childrenRegion = child1Region
      .pasteHorizontal(child2Region, minHorizSpacing + excessSpace / 2)
      .pasteHorizontal(child3Region, minHorizSpacing + excessSpace / 2)
    val firstChildLine = childrenRegion.lines.head
    val (parentFormulaOffset, _) = calculateLeftRightPaddingForCentering(formulaString, ruleLineWidth)
    val labelPrefix = label.map(_ + " ").getOrElse("")
    val parentRegion =
      RaggedTextRegion(Seq(
        LineAndOffset(formulaString, parentFormulaOffset + labelPrefix.length),
        LineAndOffset(labelPrefix + "─" * ruleLineWidth + " " + ruleName),
      ))
    childrenRegion.shiftHorizontal(labelPrefix.length) pasteVertical parentRegion.shiftHorizontal(firstChildLine.offset)
  }

  private def calculateLeftRightPaddingForCentering(s: String, width: Int): (Int, Int) = {
    assert(s.length <= width, s"String '$s' is longer (${s.length}) than given width $width")
    val excess = width - s.length
    val equalPadding = excess / 2
    val bonusPadding = excess % 2
    (equalPadding, equalPadding + bonusPadding)
  }

}
