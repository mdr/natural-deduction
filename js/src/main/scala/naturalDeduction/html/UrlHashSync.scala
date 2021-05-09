package naturalDeduction.html

import naturalDeduction.DerivationSection
import naturalDeduction.parser.FormulaParser.tryParseDerivationSections
import naturalDeduction.pretty.DerivationSerialiser
import org.scalajs.dom.window

import scala.scalajs.js.URIUtils

object UrlHashSync {

  def readFromHash: Seq[DerivationSection] = window.location.hash match {
    case "" => Seq.empty
    case s =>
      val serialisedSections = URIUtils.decodeURIComponent(s.substring(1))
      tryParseDerivationSections(serialisedSections).getOrElse(Seq.empty)
  }

  def writeToHash(derivationSections: Seq[DerivationSection]): Unit =
    window.location.hash = DerivationSerialiser.serialise(derivationSections)

}
