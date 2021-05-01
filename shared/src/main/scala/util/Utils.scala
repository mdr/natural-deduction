package util

object Utils {

  def zipLongest[X, Y](xs: Seq[X], ys: Seq[Y]): Seq[(Option[X], Option[Y])] =
    xs.map(Some(_)).padTo(ys.length, None) zip ys.map(Some(_)).padTo(xs.length, None)

  def rtrim(s: String): String = s.replaceAll("\\s+$", "")

  def unicodeStrikeThrough(s: String): String = s.flatMap(c => s"$c\u0336")

}
