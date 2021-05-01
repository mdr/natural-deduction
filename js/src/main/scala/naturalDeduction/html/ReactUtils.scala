package naturalDeduction.html

import japgolly.scalajs.react.{Callback, ReactEventFromInput}

object ReactUtils {
  def getTargetValueThen(f: String => Callback): ReactEventFromInput => Callback = e => f(e.target.value)
}
