package naturalDeduction

import naturalDeduction.Derivation._
import naturalDeduction.Formula._
import naturalDeduction.Labels.freshLabel
import naturalDeduction.pretty.DerivationRenderer
import upickle.default.{macroRW, ReadWriter => RW}
import util.Utils.unicodeStrikeThrough

import scala.PartialFunction.{cond, condOpt}
import scala.annotation.tailrec

sealed trait Derivation {

  private object BetaRedex {
    def unapply(derivation: Derivation): Option[(Derivation, Option[Label], Derivation)] = condOpt(derivation) {
      case ImplicationElimination(antecedentDerivation, ImplicationIntroduction(_, label, consequentDerivation)) =>
        (antecedentDerivation, label, consequentDerivation)
    }
  }

  def isBetaRedex: Boolean = cond(this) { case BetaRedex(_, _, _) => true }

  def betaReduce: Option[Derivation] = condOpt(this) {
    case BetaRedex(antecedentDerivation, label, consequentDerivation) =>
      label match {
        case Some(label) => consequentDerivation.substitute(label, antecedentDerivation)
        case None => consequentDerivation
      }
  }

  def substitute(label: Label, replacement: Derivation): Derivation = this match {
    case Axiom(_, Some(`label`)) => replacement
    case _ =>
      children.indices.foldLeft(this) { case (derivation, childChoice) =>
        val childBindingLabels: Set[Label] = bindingsForChild(childChoice).keySet
        // if childBindingLabel is free in replacement, we need to first relabel with a label fresh both
        // in derivation and in replacement
        if (childBindingLabels contains label)
          derivation
        else {
          val isFreeInReplacement = (childBindingLabels intersect replacement.undischargedAssumptions.labelledAssumptions.keySet).nonEmpty
          if (isFreeInReplacement) {
            val newLabel = freshLabel(derivation.labels ++ replacement.labels)
            derivation.relabel(childChoice, newLabel).transformChild(childChoice, _.substitute(label, replacement))
          } else {
            derivation.transformChild(childChoice, _.substitute(label, replacement))
          }
        }
      }
  }

  def convertToAxiom: Axiom = Axiom(formula)

  def dischargeAxiom(label: Label): Derivation = this match {
    case axiom: Axiom => axiom.copy(label = Some(label))
  }

  def undischargeAxiom: Derivation = this match {
    case axiom: Axiom => axiom.copy(label = None)
  }

  def nextFreshLabel: Label = freshLabel(this.labels)

  def labels: Set[Label] = children.flatMap(_.labels).toSet

  def formula: Formula

  def undischargedAssumptions: Assumptions

  def sequent: Sequent = Sequent(undischargedAssumptions.allFormulae, formula)

  def children: Seq[Derivation]

  def getChild(i: ChildIndex): Derivation = children(i)

  def replaceChild(i: ChildIndex, newChild: Derivation): Derivation

  def transformChild(i: ChildIndex, f: Derivation => Derivation): Derivation = replaceChild(i, f(getChild(i)))

  def isAxiom: Boolean = this.isInstanceOf[Axiom]

  def relabel(childChoice: ChildIndex, newLabel: Label): Derivation = {
    val (oldLabel, formula) = bindingsForChild(childChoice).headOption.get
    setChildLabel(childChoice, newLabel).transformChild(childChoice, _.substitute(oldLabel, Axiom(formula, newLabel)))
  }

  private def setChildLabel(childChoice: ChildIndex, newLabel: Label): Derivation = this match {
    case d: ImplicationIntroduction => d.copy(label = Some(newLabel))
    case d: ReductioAdAbsurdum => d.copy(label = Some(newLabel))
    case d: NegationIntroduction => d.copy(label = Some(newLabel))
    case d: DisjunctionElimination if childChoice == 1 => d.copy(leftLabel = Some(newLabel))
    case d: DisjunctionElimination if childChoice == 2 => d.copy(rightLabel = Some(newLabel))
  }

  def bindingsForChild(childChoice: ChildIndex): Map[Label, Formula] = this match {
    case raa@ReductioAdAbsurdum(_, Some(label), _) => Map(label -> raa.negation)
    case NegationIntroduction(statement, Some(label), _) => Map(label -> statement)
    case ImplicationIntroduction(antecedent, Some(label), _) => Map(label -> antecedent)
    case de@DisjunctionElimination(_, Some(leftLabel), _, _, _) if childChoice == 1 => Map(leftLabel -> de.disjunction.disjunct1)
    case de@DisjunctionElimination(_, _, _, Some(rightLabel), _) if childChoice == 2 => Map(rightLabel -> de.disjunction.disjunct2)
    case _ => Map.empty
  }

  def bindingsAtPath(path: DerivationPath): Map[Label, Formula] = bindingsAtPath(path.childChoices)

  private def bindingsAtPath(childChoices: Seq[ChildIndex]): Map[String, Formula] =
    childChoices match {
      case Seq() => Map.empty
      case Seq(firstChoice, remainingChoices@_*) => bindingsForChild(firstChoice) ++ children(firstChoice).bindingsAtPath(remainingChoices)
    }

  @tailrec
  private def get(childChoices: Seq[ChildIndex]): Derivation = childChoices match {
    case Seq() => this
    case Seq(firstChoice, remainingChoices@_*) => children(firstChoice).get(remainingChoices)
  }

  def get(path: DerivationPath): Derivation = get(path.childChoices)

  def set(path: DerivationPath, replacement: Derivation): Derivation = set(path.childChoices, replacement)

  private def set(childChoices: Seq[ChildIndex], replacement: Derivation): Derivation = childChoices match {
    case Seq() => replacement
    case Seq(firstChoice, remainingChoices@_*) =>
      val newChild = children(firstChoice).set(remainingChoices, replacement)
      replaceChild(firstChoice, newChild)
  }

  def transform(path: DerivationPath, f: Derivation => Derivation): Derivation = set(path, f(get(path)))

  override def toString: String = {
    val rendered = DerivationRenderer.renderDerivation(this).toStringNormal
    val useStrikethrough = false
    if (useStrikethrough) {
      val withStrikethrough = "-(.+?)-".r.replaceAllIn(rendered, result => " " + unicodeStrikeThrough(result.group(1)) + " ")
      """(?m)(\s+)$""".r.replaceAllIn(withStrikethrough, "")
    } else {
      rendered
    }
  }

  def implicationIntro(formula: Formula, label: Option[Label] = None): ImplicationIntroduction =
    ImplicationIntroduction(formula, label, this)

  def implicationIntro(formula: Formula, label: Label): ImplicationIntroduction =
    ImplicationIntroduction(formula, label, this)

  def implicationElim(that: Derivation): ImplicationElimination = ImplicationElimination(this, that)

  def conjunctionIntro(that: Derivation): ConjunctionIntroduction = ConjunctionIntroduction(this, that)

  def leftConjunctionElim: LeftConjunctionElimination = LeftConjunctionElimination(this)

  def rightConjunctionElim: RightConjunctionElimination = RightConjunctionElimination(this)

  def equivalenceIntro(that: Derivation): EquivalenceIntroduction = EquivalenceIntroduction(this, that)

  def forwardsEquivalenceElim: ForwardsEquivalenceElimination = ForwardsEquivalenceElimination(this)

  def backwardsEquivalenceElim: BackwardsEquivalenceElimination = BackwardsEquivalenceElimination(this)

  def negationIntro(formula: Formula, label: Option[Label] = None): NegationIntroduction =
    NegationIntroduction(formula, label, this)

  def negationIntro(formula: Formula, label: Label): NegationIntroduction =
    NegationIntroduction(formula, label, this)

  def negationElim(that: Derivation): NegationElimination = NegationElimination(this, that)

  def reductio(conclusion: Formula, label: Label): ReductioAdAbsurdum =
    ReductioAdAbsurdum(conclusion, label, this)

  def reductio(conclusion: Formula): ReductioAdAbsurdum =
    ReductioAdAbsurdum(conclusion, this)

}

object Derivation {

  implicit val rw: RW[Derivation] =
    RW.merge(
      Axiom.rw,
      ConjunctionIntroduction.rw,
      LeftConjunctionElimination.rw,
      RightConjunctionElimination.rw,
      ImplicationIntroduction.rw,
      ImplicationElimination.rw,
      EquivalenceIntroduction.rw,
      ForwardsEquivalenceElimination.rw,
      BackwardsEquivalenceElimination.rw,
      NegationIntroduction.rw,
      NegationElimination.rw,
      ReductioAdAbsurdum.rw,
      LeftDisjunctionIntroduction.rw,
      RightDisjunctionIntroduction.rw,
      DisjunctionElimination.rw)

  implicit class RichFormula(formula: Formula) {
    def axiom: Axiom = Axiom(formula)
  }

  object Axiom {
    implicit val rw: RW[Axiom] = macroRW

    def apply(formula: Formula, label: Label): Axiom = Axiom(formula, Some(label))
  }

  case class Axiom(formula: Formula, label: Option[Label] = None) extends Derivation {

    override val undischargedAssumptions: Assumptions = label match {
      case None => Assumptions(anonymousAssumptions = Set(formula))
      case Some(label) => Assumptions(labelledAssumptions = Map(label -> formula))
    }

    override def children: Seq[Derivation] = Seq.empty

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation =
      throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")

    override def labels: Set[Label] = label.toSet
  }

  object ConjunctionIntroduction {
    implicit val rw: RW[ConjunctionIntroduction] = macroRW
  }

  case class ConjunctionIntroduction(leftDerivation: Derivation, rightDerivation: Derivation) extends Derivation {

    override def undischargedAssumptions: Assumptions =
      leftDerivation.undischargedAssumptions ++ rightDerivation.undischargedAssumptions

    override def formula: Formula = leftDerivation.formula ∧ rightDerivation.formula

    override def children: Seq[Derivation] = Seq(leftDerivation, rightDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(leftDerivation = newChild)
      case 1 => copy(rightDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }
  }

  object LeftConjunctionElimination {
    implicit val rw: RW[LeftConjunctionElimination] = macroRW
  }

  case class LeftConjunctionElimination(conjunctionDerivation: Derivation) extends Derivation {
    assert(conjunctionDerivation.formula.isInstanceOf[Conjunction])
    val conjunction: Conjunction = conjunctionDerivation.formula.asInstanceOf[Conjunction]

    override def undischargedAssumptions: Assumptions = conjunctionDerivation.undischargedAssumptions

    override def formula: Formula = conjunction.conjunct1

    override def children: Seq[Derivation] = Seq(conjunctionDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(conjunctionDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }
  }

  object RightConjunctionElimination {
    implicit val rw: RW[RightConjunctionElimination] = macroRW
  }

  case class RightConjunctionElimination(conjunctionDerivation: Derivation) extends Derivation {

    assert(conjunctionDerivation.formula.isInstanceOf[Conjunction])
    val conjunction: Conjunction = conjunctionDerivation.formula.asInstanceOf[Conjunction]

    override def undischargedAssumptions: Assumptions = conjunctionDerivation.undischargedAssumptions

    override def formula: Formula = conjunction.conjunct2

    override def children: Seq[Derivation] = Seq(conjunctionDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(conjunctionDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }
  }

  object ImplicationIntroduction {
    implicit val rw: RW[ImplicationIntroduction] = macroRW

    def apply(antecedent: Formula, consequent: Formula): ImplicationIntroduction =
      ImplicationIntroduction(antecedent, None, consequent.axiom)

    def apply(antecedent: Formula, consequentDerivation: Derivation): ImplicationIntroduction =
      ImplicationIntroduction(antecedent, None, consequentDerivation)

    def apply(antecedent: Formula, label: Label, consequentDerivation: Derivation): ImplicationIntroduction =
      ImplicationIntroduction(antecedent, Some(label), consequentDerivation)
  }

  case class ImplicationIntroduction(antecedent: Formula, label: Option[Label], consequentDerivation: Derivation) extends Derivation {

    for {
      label <- label
      assumption <- consequentDerivation.undischargedAssumptions.labelledAssumptions.get(label)
    } assert(assumption == antecedent, s"Expected assumption $assumption to equal $antecedent for label $label")

    override def formula: Formula = antecedent → consequentDerivation.formula

    override def undischargedAssumptions: Assumptions = consequentDerivation.undischargedAssumptions.discharge(label)

    override def children: Seq[Derivation] = Seq(consequentDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(consequentDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }

    override def labels: Set[Label] = label.toSet ++ children.flatMap(_.labels)

  }

  object ImplicationElimination {
    def apply(antecedent: Formula, consequent: Formula): ImplicationElimination =
      ImplicationElimination(antecedent.axiom, (antecedent → consequent).axiom)

    implicit val rw: RW[ImplicationElimination] = macroRW
  }

  case class ImplicationElimination(antecedentDerivation: Derivation, implicationDerivation: Derivation) extends Derivation {

    assert(implicationDerivation.formula.isInstanceOf[Implication], s"Expected implicationDerivation to prove an Implication, but instead it proved ${implicationDerivation.formula}")
    val implication: Implication = implicationDerivation.formula.asInstanceOf[Implication]
    assert(antecedentDerivation.formula == implication.antecedent, s"Mismatched antecedent formulae: ${antecedentDerivation.formula} vs ${implication.antecedent}")

    override def formula: Formula = implication.consequent

    override def undischargedAssumptions: Assumptions =
      antecedentDerivation.undischargedAssumptions ++ implicationDerivation.undischargedAssumptions

    override def children: Seq[Derivation] = Seq(antecedentDerivation, implicationDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(antecedentDerivation = newChild)
      case 1 => copy(implicationDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }
  }

  object EquivalenceIntroduction {
    implicit val rw: RW[EquivalenceIntroduction] = macroRW
  }

  case class EquivalenceIntroduction(forwardsDerivation: Derivation, backwardsDerivation: Derivation) extends Derivation {

    assert(forwardsDerivation.formula.isInstanceOf[Implication])
    assert(backwardsDerivation.formula.isInstanceOf[Implication])
    val formula1: Formula = forwardsDerivation.formula.asInstanceOf[Implication].antecedent
    val formula2: Formula = forwardsDerivation.formula.asInstanceOf[Implication].consequent
    assert(backwardsDerivation.formula == (formula2 → formula1))

    override def formula: Formula = formula1 ↔ formula2

    override def undischargedAssumptions: Assumptions =
      forwardsDerivation.undischargedAssumptions ++ backwardsDerivation.undischargedAssumptions

    override def children: Seq[Derivation] = Seq(forwardsDerivation, backwardsDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(forwardsDerivation = newChild)
      case 1 => copy(backwardsDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }
  }

  object ForwardsEquivalenceElimination {
    implicit val rw: RW[ForwardsEquivalenceElimination] = macroRW
  }

  case class ForwardsEquivalenceElimination(equivalenceDerivation: Derivation) extends Derivation {
    assert(equivalenceDerivation.formula.isInstanceOf[Equivalence])
    val equivalance: Equivalence = equivalenceDerivation.formula.asInstanceOf[Equivalence]

    override def undischargedAssumptions: Assumptions = equivalenceDerivation.undischargedAssumptions

    override def formula: Formula = equivalance.forwardsImplication

    override def children: Seq[Derivation] = Seq(equivalenceDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(equivalenceDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }
  }

  object BackwardsEquivalenceElimination {
    implicit val rw: RW[BackwardsEquivalenceElimination] = macroRW
  }

  case class BackwardsEquivalenceElimination(equivalenceDerivation: Derivation) extends Derivation {
    assert(equivalenceDerivation.formula.isInstanceOf[Equivalence])
    val equivalance: Equivalence = equivalenceDerivation.formula.asInstanceOf[Equivalence]

    override def undischargedAssumptions: Assumptions = equivalenceDerivation.undischargedAssumptions

    override def formula: Formula = equivalance.backwardsImplication

    override def children: Seq[Derivation] = Seq(equivalenceDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(equivalenceDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }
  }

  object NegationIntroduction {
    implicit val rw: RW[NegationIntroduction] = macroRW

    def apply(statement: Formula, bottomDerivation: Derivation): NegationIntroduction =
      NegationIntroduction(statement, None, bottomDerivation)

    def apply(statement: Formula, label: Label, bottomDerivation: Derivation): NegationIntroduction =
      NegationIntroduction(statement, Some(label), bottomDerivation)
  }

  case class NegationIntroduction(statement: Formula, label: Option[Label], bottomDerivation: Derivation) extends Derivation {
    for {
      label <- label
      assumption <- bottomDerivation.undischargedAssumptions.labelledAssumptions.get(label)
    } assert(assumption == statement, s"Expected assumption $assumption to equal $statement for label $label")

    override def formula: Formula = Negation(statement)

    override def undischargedAssumptions: Assumptions = bottomDerivation.undischargedAssumptions.discharge(label)

    override def children: Seq[Derivation] = Seq(bottomDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(bottomDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }

    override def labels: Set[Label] = label.toSet ++ children.flatMap(_.labels)
  }

  object NegationElimination {
    implicit val rw: RW[NegationElimination] = macroRW
  }

  case class NegationElimination(positiveDerivation: Derivation, negativeDerivation: Derivation) extends Derivation {
    assert(negativeDerivation.formula.isInstanceOf[Negation], s"Negative derivation must prove a negation, but was $negativeDerivation")
    assert(negativeDerivation.formula.asInstanceOf[Negation].formula == positiveDerivation.formula)

    override def formula: Formula = ⊥

    override def undischargedAssumptions: Assumptions =
      positiveDerivation.undischargedAssumptions ++ negativeDerivation.undischargedAssumptions

    override def children: Seq[Derivation] = Seq(positiveDerivation, negativeDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(positiveDerivation = newChild)
      case 1 => copy(negativeDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }
  }

  object ReductioAdAbsurdum {
    implicit val rw: RW[ReductioAdAbsurdum] = macroRW

    def apply(conclusion: Formula, bottomDerivation: Derivation): ReductioAdAbsurdum =
      ReductioAdAbsurdum(conclusion, None, bottomDerivation)

    def apply(conclusion: Formula, label: Label, bottomDerivation: Derivation): ReductioAdAbsurdum =
      ReductioAdAbsurdum(conclusion, Some(label), bottomDerivation)

  }

  case class ReductioAdAbsurdum(conclusion: Formula, label: Option[Label], bottomDerivation: Derivation) extends Derivation {
    val negation: Negation = conclusion.not
    for {
      label <- label
      assumption <- bottomDerivation.undischargedAssumptions.labelledAssumptions.get(label)
    } assert(assumption == negation, s"Expected assumption $assumption to equal $negation for label $label")

    override def formula: Formula = conclusion

    override def undischargedAssumptions: Assumptions = bottomDerivation.undischargedAssumptions.discharge(label)

    override def children: Seq[Derivation] = Seq(bottomDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(bottomDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }

    override def labels: Set[Label] = label.toSet ++ children.flatMap(_.labels)
  }

  object LeftDisjunctionIntroduction {
    implicit val rw: RW[LeftDisjunctionIntroduction] = macroRW
  }

  case class LeftDisjunctionIntroduction(leftDerivation: Derivation, right: Formula) extends Derivation {

    override def formula: Formula = leftDerivation.formula ∨ right

    override def undischargedAssumptions: Assumptions = leftDerivation.undischargedAssumptions

    override def children: Seq[Derivation] = Seq(leftDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(leftDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }
  }

  object RightDisjunctionIntroduction {
    implicit val rw: RW[RightDisjunctionIntroduction] = macroRW
  }

  case class RightDisjunctionIntroduction(left: Formula, rightDerivation: Derivation) extends Derivation {

    override def formula: Formula = left ∨ rightDerivation.formula

    override def undischargedAssumptions: Assumptions = rightDerivation.undischargedAssumptions

    override def children: Seq[Derivation] = Seq(rightDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(rightDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }
  }

  object DisjunctionElimination {
    implicit val rw: RW[DisjunctionElimination] = macroRW
  }

  case class DisjunctionElimination(disjunctionDerivation: Derivation,
                                    leftLabel: Option[String],
                                    leftDerivation: Derivation,
                                    rightLabel: Option[String],
                                    rightDerivation: Derivation) extends Derivation {

    assert(leftDerivation.formula == rightDerivation.formula)
    assert(disjunctionDerivation.formula.isInstanceOf[Disjunction])
    val disjunction: Disjunction = disjunctionDerivation.formula.asInstanceOf[Disjunction]
    for {
      label <- leftLabel
      assumption <- leftDerivation.undischargedAssumptions.labelledAssumptions.get(label)
    } assert(assumption == disjunction.disjunct1, s"Expected assumption $assumption to equal ${disjunction.disjunct1} for label $label")
    for {
      label <- rightLabel
      assumption <- rightDerivation.undischargedAssumptions.labelledAssumptions.get(label)
    } assert(assumption == disjunction.disjunct2, s"Expected assumption $assumption to equal ${disjunction.disjunct2} for label $label")

    override def formula: Formula = leftDerivation.formula

    override def undischargedAssumptions: Assumptions =
      disjunctionDerivation.undischargedAssumptions ++
        leftDerivation.undischargedAssumptions.discharge(leftLabel) ++
        rightDerivation.undischargedAssumptions.discharge(rightLabel)

    override def children: Seq[Derivation] = Seq(disjunctionDerivation, leftDerivation, rightDerivation)

    override def replaceChild(i: ChildIndex, newChild: Derivation): Derivation = i match {
      case 0 => copy(disjunctionDerivation = newChild)
      case 1 => copy(leftDerivation = newChild)
      case 2 => copy(rightDerivation = newChild)
      case _ => throw new IllegalArgumentException(s"Cannot replace child: illegal child index $i for $this")
    }

    override def labels: Set[Label] = leftLabel.toSet ++ rightLabel.toSet ++ children.flatMap(_.labels)

  }

}
