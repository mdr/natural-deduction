package naturalDeduction.html

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.builder.Lifecycle

import scala.scalajs.js.Dynamic.global

object TooltipUtils {

  def activateTooltip(scope: Lifecycle.ComponentDidMount[_, _, _]): Callback =
    Callback {
      if (false) {
        // Tooltips bugs galore...
        global.$(scope.getDOMNode.asElement()).tooltip()
      }
    }

}
