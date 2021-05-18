package naturalDeduction

case class DerivationSection(derivation: Derivation, goal: Option[Sequent] = None) {

  def hasGoal: Boolean = goal.isDefined

  def transformDerivation(f: Derivation => Derivation): DerivationSection = withDerivation(f(derivation))

  def withDerivation(derivation: Derivation): DerivationSection = copy(derivation = derivation)

  assert(goal.forall(_.conclusion == derivation.conclusion))

  def conclusion: Formula = derivation.conclusion

  def derivationProvesGoal: Boolean = goal.exists(derivation.proves)

}
