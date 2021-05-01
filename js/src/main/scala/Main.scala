import naturalDeduction.html.App
import org.scalajs.dom.document

object Main {

  def main(args: Array[String]): Unit = {
    val target = document.getElementById("root")
    App.app().renderIntoDOM(target)
  }

}
