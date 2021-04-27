package naturalDeduction.pretty

import naturalDeduction.Formula
import naturalDeduction.Formula._

import scala.PartialFunction.cond

object FormulaPrettyPrinter {

  def prettyPrint(formula: Formula): String = formula match {
    case Bottom => "⊥"
    case PropositionalVariable(name) => name
    case Negation(subformula) =>
      val needsParen = cond(subformula) {
        case _: Conjunction | _: Disjunction | _: Implication | _: Equivalence => true
      }
      s"¬${prettyPrintWithParen(subformula, needsParen)}"
    case Conjunction(conjunct1, conjunct2) =>
      val needsParen1 = cond(conjunct1) {
        case _: Conjunction | _: Disjunction | _: Implication | _: Equivalence => true
      }
      val needsParen2 = cond(conjunct2) {
        case _: Conjunction | _: Disjunction | _: Implication | _: Equivalence => true
      }
      s"${prettyPrintWithParen(conjunct1, needsParen1)} ∧ ${prettyPrintWithParen(conjunct2, needsParen2)}"
    case Disjunction(disjunct1, disjunct2) =>
      val needsParen1 = cond(disjunct1) {
        case _: Conjunction | _: Disjunction | _: Implication | _: Equivalence => true
      }
      val needsParen2 = cond(disjunct2) {
        case _: Conjunction | _: Disjunction | _: Implication | _: Equivalence => true
      }
      s"${prettyPrintWithParen(disjunct1, needsParen1)} ∨ ${prettyPrintWithParen(disjunct2, needsParen2)}"
    case Implication(antecedent, consequent) =>
      val needsParen1 = cond(antecedent) {
        case _: Implication | _: Equivalence => true
      }
      val needsParen2 = cond(consequent) {
        case _: Equivalence => true
      }
      s"${prettyPrintWithParen(antecedent, needsParen1)} → ${prettyPrintWithParen(consequent, needsParen2)}"
    case Equivalence(formula1, formula2) =>
      val needsParen1 = cond(formula1) {
        case _: Implication | _: Equivalence => true
      }
      val needsParen2 = cond(formula2) {
        case _: Implication | _: Equivalence => true
      }
      s"${prettyPrintWithParen(formula1, needsParen1)} ↔ ${prettyPrintWithParen(formula2, needsParen2)}"

  }

  private def prettyPrintWithParen(formula: Formula, needsParen: Boolean): String = maybeParen(prettyPrint(formula), needsParen)

  private def maybeParen(s: String, needsParen: Boolean): String = if (needsParen) s"($s)" else s

}
