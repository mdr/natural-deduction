package naturalDeduction

object DerivationPath {
  val empty: DerivationPath = DerivationPath(Seq.empty)
}

case class DerivationPath(childChoices: Seq[ChildIndex]) {
  def isRoot: Boolean = childChoices.isEmpty

  def choose(choice: ChildIndex): DerivationPath = copy(childChoices = childChoices :+ choice)

  override def toString: String = childChoices.mkString
}