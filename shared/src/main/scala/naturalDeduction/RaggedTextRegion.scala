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

  override def toString: String =
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
    val newLines = this.lines.zip(otherRegion.lines).map {
      case (line1, line2) =>
        val spaces = width - line1.rightExtent + line2.offset - shift + spacing
        val newText = line1.text + " " * spaces + line2.text
        LineAndOffset(newText, line1.offset)
    }
    RaggedTextRegion(newLines)
  }

}

