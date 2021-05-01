package naturalDeduction

case class Assumptions(anonymousAssumptions: Set[Formula] = Set.empty,
                       labelledAssumptions: Map[Label, Formula] = Map.empty) {

  def discharge(label: Label): Assumptions = copy(labelledAssumptions = labelledAssumptions - label)

  def discharge(label: Option[Label]): Assumptions = label match {
    case None => this
    case Some(label) => discharge(label)
  }

  def ++(that: Assumptions): Assumptions = {
    val commonLabels = this.labelledAssumptions.keySet intersect that.labelledAssumptions.keySet
    for (label <- commonLabels) {
      val thisFormula = this.labelledAssumptions(label)
      val thatFormula = that.labelledAssumptions(label)
      assert(thisFormula == thatFormula, s"Error combining assumptions, mismatch for label $label: $thisFormula vs $thatFormula")
    }

    Assumptions(this.anonymousAssumptions ++ that.anonymousAssumptions, this.labelledAssumptions ++ that.labelledAssumptions)
  }

  def allFormulae: Set[Formula] = anonymousAssumptions ++ labelledAssumptions.values.toSet

}
