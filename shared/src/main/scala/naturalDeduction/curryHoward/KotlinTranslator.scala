package naturalDeduction.curryHoward

import ljt.LJTTheoremProver
import naturalDeduction.Derivation._
import naturalDeduction.parser.FormulaParser.parseSequent
import naturalDeduction.pretty.FormulaPrettyPrinter
import naturalDeduction.{Assumptions, Derivation, Formula, Label}


object Main extends App {
  FormulaPrettyPrinter.includeAllParens = false
  val sequent = parseSequent("{φ ∧ ψ} ⊢ ¬(¬φ ∨ ¬ψ)")
  val Some(derivation) = LJTTheoremProver.prove(sequent)
  println(KotlinTranslator.toKotlin(derivation))

}

object KotlinTranslator {

  val Preamble: String =
    """sealed class Either<out A, out B> {
      |  inline fun <C> fold(ifLeft: (A) -> C, ifRight: (B) -> C): C = when (this) {
      |    is Right -> ifRight(value)
      |    is Left -> ifLeft(value)
      |  }
      |}
      |data class Left<out A> constructor(val value: A) : Either<A, Nothing>()
      |data class Right<out B> constructor(val value: B) : Either<Nothing, B>()
      |typealias Not<A> = (A) -> Nothing""".stripMargin

  val AllLabels: Seq[Label] =
    "abcdefghijklmnopqrstuvwxyz".map(_.toString)

  def freshLabel(existingLabels: Set[Label]): Label =
    (AllLabels diff existingLabels.toSeq).headOption.getOrElse("Out of labels!")

  def toKotlin(derivation: Derivation): String = {
    val Assumptions(anonymousAssumptions, labelledAssumptions) = derivation.undischargedAssumptions
    var usedLabels: Set[Label] = labelledAssumptions.keySet
    val undischargedAssumptions: Map[Label, Formula] = labelledAssumptions ++
      anonymousAssumptions.map(f => {
        val label = freshLabel(usedLabels)
        usedLabels += label
        label -> f
      }).toMap
    // fun <φ, ψ> `⊢ φ ∧ ψ → ψ ∧ φ`(): (`∧`<φ, ψ>) -> `∧`<ψ, φ> =
    val variables = derivation.conclusion.variables ++ derivation.undischargedAssumptions.allFormulae.flatMap(_.variables)
    val genericsSection = if (variables.isEmpty) "" else s"<${variables.mkString(", ")}> "
    val argumentsSection = undischargedAssumptions.map { case (label, formula) =>
      s"${escapeIfNeeded(label)}: ${toKotlinType(formula)}"
    }.mkString(", ")
    s"$Preamble\n\nfun $genericsSection`${derivation.sequent}`($argumentsSection): ${toKotlinType(derivation.conclusion)} =\n  ${Context(undischargedAssumptions).toKotlin(derivation)}"
  }

  private case class Context(undischargedAssumptions: Map[Label, Formula]) {

    def toKotlin(derivation: Derivation): String = derivation match {
      case Axiom(conclusion, label) =>
        label match {
          case Some(label) => escapeIfNeeded(label)
          case None => undischargedAssumptions.collectFirst {
            case (label, `conclusion`) => escapeIfNeeded(label)
          }.get
        }
      case ConjunctionIntroduction(leftDerivation, rightDerivation) =>
        s"(${toKotlin(leftDerivation)}) to (${toKotlin(rightDerivation)})"
      case LeftConjunctionElimination(conjunctionDerivation) =>
        s"(${toKotlin(conjunctionDerivation)}).first"
      case RightConjunctionElimination(conjunctionDerivation) =>
        s"(${toKotlin(conjunctionDerivation)}).second"
      case ImplicationIntroduction(antecedent, label, consequentDerivation) =>
        s"{ ${label.map(escapeIfNeeded).getOrElse("_")}: ${toKotlinType(antecedent)} -> ${toKotlin(consequentDerivation)} }"
      case ImplicationElimination(antecedentDerivation, implicationDerivation) =>
        s"(${toKotlin(implicationDerivation)})(${toKotlin(antecedentDerivation)})"
      case EquivalenceIntroduction(forwardsDerivation, backwardsDerivation) =>
        s"(${toKotlin(forwardsDerivation)}) to (${toKotlin(backwardsDerivation)})"
      case ForwardsEquivalenceElimination(equivalenceDerivation) =>
        s"(${toKotlin(equivalenceDerivation)}).first"
      case BackwardsEquivalenceElimination(equivalenceDerivation) =>
        s"(${toKotlin(equivalenceDerivation)}).second"
      case NegationIntroduction(statement, label, bottomDerivation) =>
        s"{ ${label.map(escapeIfNeeded).getOrElse("_")}: ${toKotlinType(statement)} -> ${toKotlin(bottomDerivation)} }"
      case NegationElimination(positiveDerivation, negativeDerivation) =>
        s"(${toKotlin(negativeDerivation)})(${toKotlin(positiveDerivation)})"
      case ReductioAdAbsurdum(conclusion, label, bottomDerivation) =>
        s"RAA(${toKotlin(bottomDerivation)})"
      case LeftDisjunctionIntroduction(leftDerivation, right) =>
        s"Left(${toKotlin(leftDerivation)})"
      case RightDisjunctionIntroduction(left, rightDerivation) =>
        s"Right(${toKotlin(rightDerivation)})"
      case elim@DisjunctionElimination(disjunctionDerivation, leftLabel, leftDerivation, rightLabel, rightDerivation) =>
        s"(${toKotlin(disjunctionDerivation)}).fold({ ${leftLabel.map(escapeIfNeeded).getOrElse("_")}: ${toKotlinType(elim.disjunction.disjunct1)} -> ${toKotlin(leftDerivation)} }, { ${rightLabel.map(escapeIfNeeded).getOrElse("_")}: ${toKotlinType(elim.disjunction.disjunct2)} -> ${toKotlin(rightDerivation)} })"
    }
  }

  private def escapeIfNeeded(label: Label): String = if (label.matches("^[a-z]+$")) label else s"`$label`"

  def toKotlinType(formula: Formula): String = formula match {
    case Formula.Bottom => "Nothing"
    case Formula.PropositionalVariable(name) => name
    case Formula.Negation(formula) => s"Not<${toKotlinType(formula)}>"
    case Formula.Conjunction(conjunct1, conjunct2) => s"Pair<${toKotlinType(conjunct1)}, ${toKotlinType(conjunct2)}>"
    case Formula.Disjunction(disjunct1, disjunct2) => s"Either<${toKotlinType(disjunct1)}, ${toKotlinType(disjunct2)}>"
    case Formula.Implication(antecedent, consequent) => s"(${toKotlinType(antecedent)}) -> ${toKotlinType(consequent)}"
    case equiv@Formula.Equivalence(_, _) => s"Pair<${toKotlinType(equiv.forwardsImplication)}, ${toKotlinType(equiv.backwardsImplication)}>"
  }

}