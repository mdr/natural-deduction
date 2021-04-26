package naturalDeduction

case class LineAndOffset(text: String, offset: Int = 0) {
  def rightExtent: Int = text.length + offset

  def shift(n: Int): LineAndOffset = copy(offset = offset + n)

}

/**
 * @param lines - ordered bottom first to top last
 */
case class RaggedTextRegion(lines: Seq[LineAndOffset]) {
  assert(lines.nonEmpty)

  def shift(n: Int): RaggedTextRegion = copy(lines = lines.map(_.shift(n)))

  lazy val width: Int = lines.map(_.rightExtent).max

  override def toString: String = toStringDebug
  
  def toStringNormal: String =
    lines
      .reverse
      .map { case line@LineAndOffset(text, offset) =>
        val rightPadding = width - line.rightExtent
        " " * offset + text + " " * rightPadding
      }
      .mkString(sep = "\n")

  def toStringDebug: String =
    lines
      .reverse
      .map { case line@LineAndOffset(text, offset) =>
        val rightPadding = width - line.rightExtent
        "║" + "·" * offset + text + "·" * rightPadding + "║"
      }
      .mkString(start = "╔" + "═" * width + "╗\n", sep = "\n", end = "\n╚" + "═" * width + "╝")

  def pasteVertical(that: RaggedTextRegion): RaggedTextRegion = RaggedTextRegion(that.lines ++ this.lines)

  def pasteHorizontal(otherRegion: RaggedTextRegion, spacing: Int): RaggedTextRegion = {
    val shift = this.lines.zip(otherRegion.lines).map {
      case (line1, line2) => width - line1.rightExtent + line2.offset
    }.min
    val newLines = zipLongest(this.lines, otherRegion.lines) .map {
      case (line1Opt, line2Opt) =>
        val line1 = line1Opt.getOrElse(LineAndOffset(" " * width))
        val line2 = line2Opt.getOrElse(LineAndOffset(""))
        val spaces = width - line1.rightExtent + line2.offset - shift + spacing
        val newText = line1.text + " " * spaces + line2.text
        LineAndOffset(newText, line1.offset)
    }
    RaggedTextRegion(newLines)
  }

  def zipLongest[X, Y](xs: Seq[X], ys: Seq[Y]): Seq[(Option[X], Option[Y])] =
    xs.map(Some(_)).padTo(ys.length, None) zip ys.map(Some(_)).padTo(xs.length, None)

}

