package naturalDeduction.parser

import naturalDeduction.Derivation._
import naturalDeduction.{Derivation, DerivationSection, Formula, Label, Sequent}
import naturalDeduction.Formula._

import scala.util.parsing.combinator.RegexParsers

object FormulaParser extends RegexParsers {

  private def get[T](x: Either[String, T]): T = x match {
    case Left(error) => throw new RuntimeException(s"Parser error: $error")
    case Right(thing) => thing
  }

  def parseFormula(s: String): Formula = get(tryParseFormula(s))

  def parseSequent(s: String): Sequent = get(tryParseSequent(s))

  def parseDerivation(s: String): Derivation = get(tryParseDerivation(s))

  def parseDerivationSections(s: String): Seq[DerivationSection] = get(tryParseDerivationSections(s))

  def tryParseFormula(s: String): Either[String, Formula] = toEither(parseAll(formula, s))

  def tryParseSequent(s: String): Either[String, Sequent] = toEither(parseAll(sequent, s))

  def tryParseDerivation(s: String): Either[String, Derivation] = toEither(parseAll(derivation, s))

  def tryParseDerivationSections(s: String): Either[String, Seq[DerivationSection]] = toEither(parseAll(derivationSections, s))

  private lazy val sequent: Parser[Sequent] = opt(assumptions) ~ (("⊢" | "|-") ~> FormulaParser.formula) ^^ {
    case formulae ~ conclusion => Sequent(formulae getOrElse Set.empty, conclusion)
  }

  private lazy val assumptions: Parser[Set[Formula]] =
    "{" ~> repsep[Formula](FormulaParser.formula, ",") <~ "}" ^^ (_.toSet)

  private lazy val formula: Parser[Formula] = rep1sep(formula1, "↔" | "⇿" | "<->" | "<=>" | "⇔" | "≡") ^^ (_.reduceRight[Formula](Equivalence))

  private lazy val formula1: Parser[Formula] = rep1sep(formula2, "→" | "->" | "⇒" | "=>" | "⊃" | "⇾") ^^ (_.reduceRight[Formula](Implication))

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

  private lazy val label: Parser[Label] =
    ("\"" + """([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""" + "\"").r ^^ (s => s.substring(1, s.length - 1))

  private lazy val derivationSections: Parser[Seq[DerivationSection]] = rep1sep(derivationSection, ",")

  private lazy val derivationSection: Parser[DerivationSection] =
    "Section(" ~> derivation ~ opt("," ~> sequent) <~ ")" ^^ {
      case derivation ~ goal => DerivationSection(derivation, goal)
    }

  private lazy val derivation: Parser[Derivation] =
    axiom | conjunctionIntro | leftConjunctionElim | rightConjunctionElim | implicationIntro |
      implicationElim | equivalenceIntro | equivalenceElim1 | equivalenceElim2 | negationIntro |
      negationElim | reductio | disjunctionIntro1 | disjunctionIntro2 | disjunctionElim

  private lazy val commaLabel: Parser[Label] = "," ~> label

  private lazy val axiom: Parser[Axiom] =
    ("Ax" ~> "(" ~> formula) ~ opt(commaLabel) <~ ")" ^^ {
      case formula ~ label => Axiom(formula, label)
    }

  private lazy val conjunctionIntro: Parser[Derivation] =
    ("∧I" ~> "(" ~> derivation) ~ ("," ~> derivation) <~ ")" ^^ {
      case derivation1 ~ derivation2 => ConjunctionIntroduction(derivation1, derivation2)
    }

  private lazy val leftConjunctionElim: Parser[Derivation] =
    ("∧E1" ~> "(" ~> derivation) <~ ")" ^^ LeftConjunctionElimination.apply

  private lazy val rightConjunctionElim: Parser[Derivation] =
    ("∧E2" ~> "(" ~> derivation) <~ ")" ^^ RightConjunctionElimination.apply

  private lazy val implicationIntro: Parser[Derivation] =
    ("→I" ~> "(" ~> formula) ~ opt(commaLabel) ~ ("," ~> derivation) <~ ")" ^^ {
      case formula ~ label ~ derivation => ImplicationIntroduction(formula, label, derivation)
    }

  private lazy val implicationElim: Parser[Derivation] =
    ("→E" ~> "(" ~> derivation) ~ ("," ~> derivation) <~ ")" ^^ {
      case derivation1 ~ derivation2 => ImplicationElimination(derivation1, derivation2)
    }

  private lazy val equivalenceIntro: Parser[Derivation] =
    ("↔I" ~> "(" ~> derivation) ~ ("," ~> derivation) <~ ")" ^^ {
      case derivation1 ~ derivation2 => EquivalenceIntroduction(derivation1, derivation2)
    }

  private lazy val equivalenceElim1: Parser[Derivation] =
    ("↔E1" ~> "(" ~> derivation) <~ ")" ^^ ForwardsEquivalenceElimination.apply

  private lazy val equivalenceElim2: Parser[Derivation] =
    ("↔E2" ~> "(" ~> derivation) <~ ")" ^^ BackwardsEquivalenceElimination.apply

  private lazy val negationIntro: Parser[Derivation] =
    ("¬I" ~> "(" ~> formula) ~ opt(commaLabel) ~ ("," ~> derivation) <~ ")" ^^ {
      case formula ~ label ~ derivation => NegationIntroduction(formula, label, derivation)
    }

  private lazy val negationElim: Parser[Derivation] =
    ("¬E" ~> "(" ~> derivation) ~ ("," ~> derivation) <~ ")" ^^ {
      case derivation1 ~ derivation2 => NegationElimination(derivation1, derivation2)
    }

  private lazy val reductio: Parser[Derivation] =
    ("RAA" ~> "(" ~> formula) ~ opt(commaLabel) ~ ("," ~> derivation) <~ ")" ^^ {
      case formula ~ label ~ derivation => ReductioAdAbsurdum(formula, label, derivation)
    }

  private lazy val disjunctionIntro1: Parser[Derivation] =
    ("∨I1" ~> "(" ~> derivation) ~ ("," ~> formula) <~ ")" ^^ {
      case derivation ~ formula => LeftDisjunctionIntroduction(derivation, formula)
    }

  private lazy val disjunctionIntro2: Parser[Derivation] =
    ("∨I2" ~> "(" ~> formula) ~ ("," ~> derivation) <~ ")" ^^ {
      case formula ~ derivation => RightDisjunctionIntroduction(formula, derivation)
    }

  private lazy val disjunctionElim: Parser[Derivation] =
    ("∨E" ~> "(" ~> derivation) ~ opt(commaLabel) ~ ("," ~> derivation) ~ opt(commaLabel) ~ ("," ~> derivation) <~ ")" ^^ {
      case derivation1 ~ label1 ~ derivation2 ~ label2 ~ derivation3 =>
        DisjunctionElimination(derivation1, label1, derivation2, label2, derivation3)
    }

  private def toEither[T](parseResult: ParseResult[T]): Either[String, T] =
    parseResult match {
      case NoSuccess(message, _) => Left(message)
      case Success(term, _) => Right(term)
    }

}
