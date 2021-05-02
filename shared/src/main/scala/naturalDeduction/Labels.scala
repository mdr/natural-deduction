package naturalDeduction

object Labels {

  val ALL_LABELS: Seq[Label] =
    "①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳㉑㉒㉓㉔㉕㉖㉗㉘㉙㉚㉛㉜㉝㉞㉟㊱㊲㊳㊴㊵㊶㊷㊸㊹㊺㊻㊼㊽㊾㊿".map(_.toString)

  def freshLabel(existingLabels: Set[Label]): Label =
    (ALL_LABELS diff existingLabels.toSeq).headOption.getOrElse("Out of labels!")

}
