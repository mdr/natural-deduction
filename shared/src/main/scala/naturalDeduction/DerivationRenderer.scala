package naturalDeduction

object DerivationRenderer {

  def renderDerivation(derivation: Derivation): RaggedTextRegion = derivation match {
    case Derivation.Axiom(formula, isDischarged) =>
      RaggedTextRegion(Seq(LineAndOffset(formula.toString)))
    case Derivation.ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      renderTwoChildren(derivation, leftDerivation, rightDerivation, "∧I")
    case Derivation.LeftConjunctionElimination(conjunctionDerivation) =>
      renderSingleChild(derivation, conjunctionDerivation, "∧E")
    case Derivation.RightConjunctionElimination(conjunctionDerivation) =>
      renderSingleChild(derivation, conjunctionDerivation, "∧E")

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
    val lineLength = formulaString.length max lengthOfChildLineWhenClosestPacked
    val actualHorizontalSpacing = 1 + 0.max(lineLength - lengthOfChildLineWhenClosestPacked)
    val childrenRegion = child1Region.pasteHorizontal(child2Region, actualHorizontalSpacing)
    val firstChildLine = childrenRegion.lines.head
    val parentRegion =
      RaggedTextRegion(Seq(
        LineAndOffset(center(parent.formula.toString, lineLength)),
        LineAndOffset("─" * lineLength + " " + ruleLabel),
      ))
    childrenRegion pasteVertical parentRegion.shift(firstChildLine.offset)
  }

  private def renderSingleChild(parent: Derivation, child: Derivation, ruleLabel: String): RaggedTextRegion = {
    val formulaString = parent.formula.toString
    val childRegion = renderDerivation(child)
    val firstChildLine = childRegion.lines.head
    val lineLength = formulaString.length max firstChildLine.text.length
    val parentRegion =
      RaggedTextRegion(Seq(
        LineAndOffset(center(parent.formula.toString, lineLength)),
        LineAndOffset("─" * lineLength + " " + ruleLabel),
      ))
    // TODO: if firstChildLine.text.length is less than lineLength, we'll need a different shift for parentRegion to
    //       centre the child formula above the rule line.
    childRegion pasteVertical parentRegion.shift(firstChildLine.offset)
  }

  private def center(s: String, width: Int): String = {
    val (left, right) = calculateLeftRightPaddingForCentering(s, width)
    " " * left + s + " " * right
  }

  private def calculateLeftRightPaddingForCentering(s: String, width: Int): (Int, Int) = {
    assert(s.length <= width)
    val excess = width - s.length
    val equalPadding = excess / 2
    val bonusPadding = excess % 2
    (equalPadding, equalPadding + bonusPadding)
  }

}
