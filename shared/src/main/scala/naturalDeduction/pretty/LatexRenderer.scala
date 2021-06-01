package naturalDeduction.pretty

import naturalDeduction.{Derivation, Formula, Labels}

object LatexRenderer {

  def render(formula: Formula): String =
    FormulaPrettyPrinter.prettyPrint(formula)
      .replace("⊥", """\bot""")
      .replace("φ", """\phi""")
      .replace("ψ", """\psi""")
      .replace("χ", """\chi""")
      .replace("θ", """\theta""")
      .replace("¬", """\neg""")
      .replace("∨", """\lor""")
      .replace("∧", """\land""")
      .replace("→", """\rightarrow""")
      .replace("↔", """\leftrightarrow""")

  def render(derivation: Derivation): String = {
    val contents = render_(derivation)
    s"""\\documentclass[preview,border=4]{standalone}
       |\\usepackage{bussproofs}
       |\\usepackage{cancel}
       |\\begin{document}
       |\\begin{prooftree}
       |\\def\\ScoreOverhang{1pt}
       |\\def\\extraVskip{3pt}
       |\\def\\defaultHypSeparation{\\hskip .1in}
       |$contents
       |\\end{prooftree}
       |\\end{document}
       |""".stripMargin

  }

  private def render_(derivation: Derivation): String = derivation match {
    case Derivation.Axiom(conclusion, label) =>
      label match {
        case Some(label) =>
          s"""\\AxiomC{$${\\cancel{${render(conclusion)}}}^{\\,\\raisebox{\\depth}{\\textcircled{\\footnotesize ${renderLabel(label)}}}}$$}"""
        case None =>
          s"""\\AxiomC{$${${render(conclusion)}}$$}"""
      }
    case Derivation.ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      s"""${render_(leftDerivation)}
         |${render_(rightDerivation)}
         |\\RightLabel{\\small{($$\\land$$I)}}
         |\\BinaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.LeftConjunctionElimination(conjunctionDerivation) =>
      s"""${render_(conjunctionDerivation)}
         |\\RightLabel{\\small{($$\\land$$E)}}
         |\\UnaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.RightConjunctionElimination(conjunctionDerivation) =>
      s"""${render_(conjunctionDerivation)}
         |\\RightLabel{\\small{($$\\land$$E)}}
         |\\UnaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.ImplicationIntroduction(_, label, consequentDerivation) =>
      s"""${render_(consequentDerivation)}
         |\\RightLabel{\\small{($$\\rightarrow$$I)}}
         |\\LeftLabel{\\textcircled{\\scriptsize ${renderLabel(label.getOrElse(""))}}}
         |\\UnaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.ImplicationElimination(antecedentDerivation, implicationDerivation) =>
      s"""${render_(antecedentDerivation)}
         |${render_(implicationDerivation)}
         |\\RightLabel{\\small{($$\\rightarrow$$E)}}
         |\\BinaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.EquivalenceIntroduction(forwardsDerivation, backwardsDerivation) =>
      s"""${render_(forwardsDerivation)}
         |${render_(backwardsDerivation)}
         |\\RightLabel{\\small{($$\\leftrightarrow$$I)}}
         |\\BinaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.ForwardsEquivalenceElimination(equivalenceDerivation) =>
      s"""${render_(equivalenceDerivation)}
         |\\RightLabel{\\small{($$\\leftrightarrow$$E)}}
         |\\UnaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.BackwardsEquivalenceElimination(equivalenceDerivation) =>
      s"""${render_(equivalenceDerivation)}
         |\\RightLabel{\\small{($$\\leftrightarrow$$E)}}
         |\\UnaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.NegationIntroduction(_, label, bottomDerivation) =>
      s"""${render_(bottomDerivation)}
         |\\RightLabel{\\small{($$\\neg$$I)}}
         |\\LeftLabel{\\textcircled{\\scriptsize ${renderLabel(label.getOrElse(""))}}}
         |\\UnaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.NegationElimination(positiveDerivation, negativeDerivation) =>
      s"""${render_(positiveDerivation)}
         |${render_(negativeDerivation)}
         |\\RightLabel{\\small{($$\\neg$$E)}}
         |\\BinaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.ReductioAdAbsurdum(_, label, bottomDerivation) =>
      s"""${render_(bottomDerivation)}
         |\\RightLabel{\\small{(RAA)}}
         |\\LeftLabel{\\textcircled{\\scriptsize ${renderLabel(label.getOrElse(""))}}}
         |\\UnaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.LeftDisjunctionIntroduction(leftDerivation, _) =>
      s"""${render_(leftDerivation)}
         |\\RightLabel{\\small{($$\\lor$$I)}}
         |\\UnaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.RightDisjunctionIntroduction(_, rightDerivation) =>
      s"""${render_(rightDerivation)}
         |\\RightLabel{\\small{($$\\lor$$I)}}
         |\\UnaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

    case Derivation.DisjunctionElimination(disjunctionDerivation, leftLabel, leftDerivation, rightLabel, rightDerivation) =>
      s"""${render_(disjunctionDerivation)}
         |${render_(leftDerivation)}
         |${render_(rightDerivation)}
         |\\LeftLabel{\\textcircled{\\scriptsize ${renderLabel(leftLabel.getOrElse(""))}}\\textcircled{\\scriptsize ${renderLabel(rightLabel.getOrElse(""))}}}
         |\\RightLabel{\\small{($$\\lor$$E)}}
         |\\TrinaryInfC{$$${render(derivation.conclusion)}$$}""".stripMargin

  }

  def renderLabel(s: String): String = {
    Labels.AllLabels.indexOf(s) match {
      case -1 => s
      case n =>  (n + 1).toString
    }
  }

}
