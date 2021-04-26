package naturalDeduction.pretty

import naturalDeduction.Derivation
import naturalDeduction.Derivation.ImplicationIntroduction

object DerivationRenderer {

  def renderDerivation(derivation: Derivation): RaggedTextRegion = derivation match {
    case Derivation.Axiom(formula, isDischarged) =>
      val renderedFormula = if (isDischarged) "-" + formula.toString + "-" else formula.toString
      RaggedTextRegion(Seq(LineAndOffset(renderedFormula)))
    case Derivation.ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      renderTwoChildren(derivation, leftDerivation, rightDerivation, "∧I")
    case Derivation.LeftConjunctionElimination(conjunctionDerivation) =>
      renderSingleChild(derivation, conjunctionDerivation, "∧E")
    case Derivation.RightConjunctionElimination(conjunctionDerivation) =>
      renderSingleChild(derivation, conjunctionDerivation, "∧E")
    case ImplicationIntroduction(_, consequentDerivation) =>
      renderSingleChild(derivation, consequentDerivation, "→I")
  }

  private def renderTwoChildren(parent: Derivation, child1: Derivation, child2: Derivation, ruleLabel: String): RaggedTextRegion = {
    val formulaString = parent.formula.toString
    val child1Region = renderDerivation(child1)
    val child2Region = renderDerivation(child2)
    val lengthOfChildLineWhenClosestPacked = {
      val childrenRegion = child1Region.pasteHorizontal(child2Region, 1)
      val firstChildLine = childrenRegion.lines.head
      firstChildLine.text.length
    }
    val ruleLineWidth = formulaString.length max lengthOfChildLineWhenClosestPacked
    val actualHorizontalSpacing = 1 + 0.max(ruleLineWidth - lengthOfChildLineWhenClosestPacked)
    val childrenRegion = child1Region.pasteHorizontal(child2Region, actualHorizontalSpacing)
    val firstChildLine = childrenRegion.lines.head
    val (parentFormulaOffset, _) = calculateLeftRightPaddingForCentering(formulaString, ruleLineWidth)
    val parentRegion =
      RaggedTextRegion(Seq(
        LineAndOffset(formulaString, parentFormulaOffset),
        LineAndOffset("─" * ruleLineWidth + " " + ruleLabel),
      ))
    childrenRegion pasteVertical parentRegion.shiftHorizontal(firstChildLine.offset)
  }

  private def renderSingleChild(parent: Derivation, child: Derivation, ruleLabel: String): RaggedTextRegion = {
    val formulaString = parent.formula.toString
    val childRegion = renderDerivation(child)
    val firstChildLine = childRegion.lines.head
    val ruleLineWidth = formulaString.length max firstChildLine.text.length
    val (parentFormulaOffset, _) = calculateLeftRightPaddingForCentering(formulaString, ruleLineWidth)
    val parentRegion =
      RaggedTextRegion(Seq(
        LineAndOffset(formulaString, parentFormulaOffset),
        LineAndOffset("─" * ruleLineWidth + " " + ruleLabel),
      ))
    val (childFormulaOffset, _) = calculateLeftRightPaddingForCentering(firstChildLine.text, ruleLineWidth)
    childRegion.shiftHorizontal(childFormulaOffset) pasteVertical parentRegion.shiftHorizontal(firstChildLine.offset)
  }

  private def unicodeStrikeThrough(s: String): String = s.flatMap(_ + "\u0336")

  private def calculateLeftRightPaddingForCentering(s: String, width: Int): (Int, Int) = {
    assert(s.length <= width, s"String '$s' is longer (${s.length}) than given width $width")
    val excess = width - s.length
    val equalPadding = excess / 2
    val bonusPadding = excess % 2
    (equalPadding, equalPadding + bonusPadding)
  }

}
