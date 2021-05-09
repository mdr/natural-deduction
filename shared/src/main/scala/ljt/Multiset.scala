package ljt

object Multiset {

  def empty[T]: Multiset[T] = Multiset[T](Map.empty[T, Int])

  def apply[T](items: T*): Multiset[T] = items.foldLeft(empty[T])(_ + _)
}

case class Multiset[T](map: Map[T, Int] = Map.empty) {
  assert(map.values.forall(_ > 0))

  def contains(t: T): Boolean = map contains t

  def keySet: Set[T] = map.keySet

  def keys: Seq[T] = map.toSeq.flatMap { case (t, n) => Seq.fill(n)(t) }

  def +(t: T): Multiset[T] = copy(map = map + (t -> (map.getOrElse(t, 0) + 1)))

  def -(t: T): Multiset[T] = map.get(t) match {
    case None => this
    case Some(1) => copy(map = map - t)
    case Some(n) => copy(map = map + (t -> (n - 1)))
  }
}
