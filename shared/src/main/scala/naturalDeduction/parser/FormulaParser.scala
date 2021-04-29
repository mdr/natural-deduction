package naturalDeduction.parser

import naturalDeduction.Formula
import naturalDeduction.Formula._

import scala.util.parsing.combinator.RegexParsers

object FormulaParser extends RegexParsers {

  def formula: Parser[Formula] = rep1sep(formula1, "↔" | "<->" | "<=>" | "⇔" | "≡") ^^ (_.reduceRight[Formula](Equivalence))

  private def formula1: Parser[Formula] = rep1sep(formula2, "→" | "->" | "⇒" | "=>" | "⊃") ^^ (_.reduceRight[Formula](Implication))

  private def formula2: Parser[Formula] = rep1sep(formula3, "∨" | "\\/" | "||" | "|") ^^ (_.reduceLeft[Formula](Disjunction))

  private def formula3: Parser[Formula] = rep1sep(formula4, "∧" | "/\\" | "&&" | "&") ^^ (_.reduceLeft[Formula](Conjunction))

  private def formula4: Parser[Formula] = (("¬" | "~" | "!") ~> formula4) ^^ Negation | formula5

  private def formula5: Parser[Formula] = variable | ("(" ~> formula <~ ")")

  private def variable: Parser[PropositionalVariable] = "" ~> // handle whitespace
    rep1(acceptIf(Character.isJavaIdentifierStart)("identifier expected but '" + _ + "' found"),
      elem("identifier part", Character.isJavaIdentifierPart(_: Char))) ^^ (chars => PropositionalVariable(chars.mkString))

  def parseFormula(s: String): Formula = tryParseFormula(s) match {
    case Left(error) => throw new RuntimeException(s"Parser error: $error")
    case Right(term) => term
  }

  def tryParseFormula(s: String): Either[String, Formula] = toEither(parseAll(formula, s))

  private def toEither[T](parseResult: ParseResult[T]): Either[String, T] =
    parseResult match {
      case NoSuccess(message, _) => Left(message)
      case Success(term, _) => Right(term)
    }

}
