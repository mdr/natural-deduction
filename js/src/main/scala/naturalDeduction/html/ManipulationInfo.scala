package naturalDeduction.html

import japgolly.scalajs.react._
import naturalDeduction._

case class ManipulationInfo(/* Backwards rules */
                            onConjunctionIntroBackwards: DerivationPath => Callback,
                            onConjunctionElimBackwards: DerivationPath => Callback,
                            onImplicationIntroBackwards: DerivationPath => Callback,
                            onImplicationElimBackwards: DerivationPath => Callback,
                            onEquivalenceIntroBackwards: DerivationPath => Callback,
                            onEquivalenceElimBackwards: (DerivationPath, EquivalenceDirection) => Callback,
                            onNegationIntroBackwards: DerivationPath => Callback,
                            onNegationElimBackwards: DerivationPath => Callback,
                            onReductioBackwards: DerivationPath => Callback,
                            onDisjunctionIntroBackwards: (DerivationPath, ChildIndex) => Callback,
                            onDisjunctionElimBackwards: DerivationPath => Callback,

                            /* Forwards rules */
                            onConjunctionIntroForwards: Callback,
                            onConjunctionElimForwards: ChildIndex => Callback,
                            onImplicationIntroForwards: Callback,
                            onImplicationElimForwardsFromAntecedent: Callback,
                            onImplicationElimForwardsFromImplication: Callback,
                            onEquivalenceIntroForwards: EquivalenceDirection => Callback,
                            onEquivalenceElimForwards: EquivalenceDirection => Callback,
                            onNegationIntroForwards: Callback,
                            onNegationElimForwardsFromPositive: Callback,
                            onNegationElimForwardsFromNegative: Callback,
                            onReductioForwards: Callback,
                            onDisjunctionIntroForwards: Callback,
                            onDisjunctionElimForwardsFromDisjunction: Callback,

                            /* Other actions */
                            onRemoveDerivation: DerivationPath => Callback,
                            onInlineDerivation: (DerivationPath, DerivationIndex) => Callback,
                            onDischargeAssumption: (DerivationPath, Label) => Callback,
                            onUndischargeAssumption: DerivationPath => Callback,
                            onBetaReduce: DerivationPath => Callback,
                            onExtractSubderivation: DerivationPath => Callback,

                            derivationIndex: DerivationIndex,
                            formulaToDerivationIndices: Map[Formula, Seq[DerivationIndex]],
                            hasGoal: Boolean)
