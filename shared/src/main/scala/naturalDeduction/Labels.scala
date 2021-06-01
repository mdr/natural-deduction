package naturalDeduction

object Labels {

  val AllLabels: Seq[Label] =
    "①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳㉑㉒㉓㉔㉕㉖㉗㉘㉙㉚㉛㉜㉝㉞㉟㊱㊲㊳㊴㊵㊶㊷㊸㊹㊺㊻㊼㊽㊾㊿abcdefghijklmnopqrstuvwxyzαβγδεζηθικλμνξοπρστυφχψω❶❷❸❹❺❻❼❽❾❿⓫⓬⓭⓮⓯⓰⓱⓲⓳⓴".map(_.toString)

  def freshLabel(existingLabels: Set[Label]): Label =
    (AllLabels diff existingLabels.toSeq).headOption.getOrElse("Out of labels!")

  def twoFreshLabels(existingLabels: Set[Label]): (Label, Label) = {
    val label1 = freshLabel(existingLabels)
    val label2 = freshLabel(existingLabels + label1)
    label1 -> label2
  }

}
