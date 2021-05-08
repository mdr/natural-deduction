package naturalDeduction.html

import naturalDeduction.Derivation
import naturalDeduction.parser.FormulaParser.parseDerivations
import naturalDeduction.pretty.DerivationSerialiser
import org.scalajs.dom.window

object UrlHashSync {

  def readFromHash: Seq[Derivation] = window.location.hash match {
    case "" => Seq.empty
    case s => parseDerivations(s.substring(1))
  }

  def writeToHash(derivations: Seq[Derivation]): Unit = window.location.hash = DerivationSerialiser.serialise(derivations)

}
