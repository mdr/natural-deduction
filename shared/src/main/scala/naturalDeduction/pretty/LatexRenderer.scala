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
      .replace("¬", """\neg """)
      .replace("∨", """\lor""")
      .replace("∧", """\land""")
      .replace("→", """\rightarrow""")
      .replace("↔", """\leftrightarrow""")

  def render(derivation: Derivation): String = {
    val contents = render_(derivation)
    s"""\\documentclass[preview,border=4]{standalone}
       |\\usepackage{ebproof}
       |\\usepackage{cancel}
       |\\begin{document}
       |\\begin{prooftree}
       |$contents
       |\\end{prooftree}
       |\\end{document}
       |""".stripMargin

  }

  private def render_(derivation: Derivation): String = derivation match {
    case Derivation.Axiom(conclusion, label) =>
      label match {
        case Some(label) =>
          s"""\\hypo{{\\cancel{${render(conclusion)}}}^{\\,\\raisebox{\\depth}{\\textcircled{\\scriptsize ${renderLabel(label)}}}}}"""
        case None =>
          s"""\\hypo{{${render(conclusion)}}}"""
      }
    case Derivation.ConjunctionIntroduction(leftDerivation, rightDerivation) =>
      s"""${render_(leftDerivation)}
         |${render_(rightDerivation)}
         |\\infer2[\\scriptsize{($$\\land$$I)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.LeftConjunctionElimination(conjunctionDerivation) =>
      s"""${render_(conjunctionDerivation)}
         |\\infer1[\\scriptsize{($$\\land$$E)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.RightConjunctionElimination(conjunctionDerivation) =>
      s"""${render_(conjunctionDerivation)}
         |\\infer1[\\scriptsize{($$\\land$$E)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.ImplicationIntroduction(_, label, consequentDerivation) =>
      val leftLabel = label.fold("")(label => s"""[left label=\\textcircled{\\scriptsize ${renderLabel(label)}}]""")
      s"""${render_(consequentDerivation)}
         |\\infer${leftLabel}1[\\scriptsize{($$\\rightarrow$$I)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.ImplicationElimination(antecedentDerivation, implicationDerivation) =>
      s"""${render_(antecedentDerivation)}
         |${render_(implicationDerivation)}
         |\\infer2[\\scriptsize{($$\\rightarrow$$E)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.EquivalenceIntroduction(forwardsDerivation, backwardsDerivation) =>
      s"""${render_(forwardsDerivation)}
         |${render_(backwardsDerivation)}
         |\\infer2[\\scriptsize{($$\\leftrightarrow$$I)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.ForwardsEquivalenceElimination(equivalenceDerivation) =>
      s"""${render_(equivalenceDerivation)}
         |\\infer1[\\scriptsize{($$\\leftrightarrow$$E)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.BackwardsEquivalenceElimination(equivalenceDerivation) =>
      s"""${render_(equivalenceDerivation)}
         |\\infer1[\\scriptsize{($$\\leftrightarrow$$E)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.NegationIntroduction(_, label, bottomDerivation) =>
      val leftLabel = label.fold("")(label => s"""[left label=\\textcircled{\\scriptsize ${renderLabel(label)}}]""")
      s"""${render_(bottomDerivation)}
         |\\infer${leftLabel}1[\\scriptsize{($$\\neg$$I)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.NegationElimination(positiveDerivation, negativeDerivation) =>
      s"""${render_(positiveDerivation)}
         |${render_(negativeDerivation)}
         |\\infer2[\\scriptsize{($$\\neg$$E)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.ReductioAdAbsurdum(_, label, bottomDerivation) =>
      val leftLabel = label.fold("")(label => s"""[left label=\\textcircled{\\scriptsize ${renderLabel(label)}}]""")
      s"""${render_(bottomDerivation)}
         |\\infer${leftLabel}1[\\scriptsize{(RAA)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.LeftDisjunctionIntroduction(leftDerivation, _) =>
      s"""${render_(leftDerivation)}
         |\\infer1[\\scriptsize{($$\\lor$$I)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.RightDisjunctionIntroduction(_, rightDerivation) =>
      s"""${render_(rightDerivation)}
         |\\infer1[\\scriptsize{($$\\lor$$I)}]{${render(derivation.conclusion)}}""".stripMargin

    case Derivation.DisjunctionElimination(disjunctionDerivation, leftLabel, leftDerivation, rightLabel, rightDerivation) =>
      s"""${render_(disjunctionDerivation)}
         |${render_(leftDerivation)}
         |${render_(rightDerivation)}
         |\\infer[left label=\\textcircled{\\scriptsize ${renderLabel(leftLabel.getOrElse(""))}}\\textcircled{\\scriptsize ${renderLabel(rightLabel.getOrElse(""))}}]3[\\scriptsize{($$\\lor$$E)}]{${render(derivation.conclusion)}}""".stripMargin

  }

  private def renderLabel(s: String): String =
    Labels.AllLabels.indexOf(s) match {
      case -1 => s
      case n => (n + 1).toString
    }

}
