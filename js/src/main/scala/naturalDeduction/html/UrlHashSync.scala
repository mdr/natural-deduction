package naturalDeduction.html

import naturalDeduction.Derivation
import org.scalajs.dom.window
import upickle.default.{read, write}

import scala.scalajs.js.URIUtils

object UrlHashSync {

  def readFromHash: Seq[Derivation] = window.location.hash match {
    case "" => Seq.empty
    case s => parseDerivations(s.substring(1))
  }

  private def parseDerivations(s: String): Seq[Derivation] = read[Seq[Derivation]](URIUtils.decodeURIComponent(s))

  def writeToHash(derivations: Seq[Derivation]): Unit = window.location.hash = write(derivations)

}
