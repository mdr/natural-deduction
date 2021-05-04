package naturalDeduction.parser

import naturalDeduction.{Formula, Sequent}
import naturalDeduction.Formula._

import scala.util.parsing.combinator.RegexParsers

object FormulaParser extends RegexParsers {

  def parseFormula(s: String): Formula = tryParseFormula(s) match {
    case Left(error) => throw new RuntimeException(s"Parser error: $error")
    case Right(formula) => formula
  }

  def parseSequent(s: String): Sequent = tryParseSequent(s) match {
    case Left(error) => throw new RuntimeException(s"Parser error: $error")
    case Right(sequent) => sequent
  }

  def tryParseFormula(s: String): Either[String, Formula] = toEither(parseAll(formula, s))

  def tryParseSequent(s: String): Either[String, Sequent] = toEither(parseAll(sequent, s))

  private lazy val sequent: Parser[Sequent] = opt(assumptions) ~ (("⊢" | "|-") ~> FormulaParser.formula) ^^ {
    case formulae ~ conclusion => Sequent(formulae getOrElse Set.empty, conclusion)
  }

  private lazy val assumptions: Parser[Set[Formula]] =
    "{" ~> repsep[Formula](FormulaParser.formula, ",") <~ "}" ^^ (_.toSet)

  private lazy val formula: Parser[Formula] = rep1sep(formula1, "↔" | "<->" | "<=>" | "⇔" | "≡") ^^ (_.reduceRight[Formula](Equivalence))

  private lazy val formula1: Parser[Formula] = rep1sep(formula2, "→" | "->" | "⇒" | "=>" | "⊃") ^^ (_.reduceRight[Formula](Implication))

  private lazy val formula2: Parser[Formula] = rep1sep(formula3, "∨" | "\\/" | "||" | "|" | "or") ^^ (_.reduceLeft[Formula](Disjunction))

  private lazy val formula3: Parser[Formula] = rep1sep(formula4, "∧" | "/\\" | "&&" | "&" | "and") ^^ (_.reduceLeft[Formula](Conjunction))

  private lazy val formula4: Parser[Formula] = (("¬" | "~" | "!") ~> formula4) ^^ Negation | formula5

  private lazy val formula5: Parser[Formula] = {
    "theta" ^^^ PropositionalVariable("θ") |
      "phi" ^^^ PropositionalVariable("φ") |
      "psi" ^^^ PropositionalVariable("ψ") |
      "chi" ^^^ PropositionalVariable("χ") |
      ("⊥" | "_|_" | "bottom" | "false") ^^^ Bottom |
      variable |
      ("(" ~> formula <~ ")")
  }

  private lazy val variable: Parser[PropositionalVariable] = "" ~> // handle whitespace
    rep1(acceptIf(Character.isJavaIdentifierStart)("identifier expected but '" + _ + "' found"),
      elem("identifier part", Character.isJavaIdentifierPart(_: Char))) ^^ (chars => PropositionalVariable(chars.mkString))

  private def toEither[T](parseResult: ParseResult[T]): Either[String, T] =
    parseResult match {
      case NoSuccess(message, _) => Left(message)
      case Success(term, _) => Right(term)
    }

}
