package u07.utils

// A multiset datatype
trait MSet[A] extends (A => Int):
  def union(m: MSet[A]): MSet[A]
  def diff(m: MSet[A]): MSet[A]
  def disjoined(m: MSet[A]): Boolean
  def size: Int
  def matches(m: MSet[A]): Boolean
  def extract(m: MSet[A]): Option[MSet[A]]
  def asList: List[A]
  def asMap: Map[A,Int]
  def iterator: Iterator[A]
  def frequencyOf(el : A): Int


extension [A](self: Map[A, Int])
  def union(m: Map[A, Int]): Map[A, Int] =
    (self.keySet ++ m.keySet).foldLeft(Map.empty[A, Int])((acc, k) => acc + (k -> (m.getOrElse(k, 0) + self.getOrElse(k, 0))))

  def diff(m: Map[A, Int]): Map[A, Int] =
    (self.keySet).foldLeft(Map.empty[A, Int])((acc, k) => acc + (k -> (self.getOrElse(k, 0) - m.getOrElse(k, 0))))

  def disjoined(m: Map[A, Int]): Boolean =
    (self.keySet intersect m.keySet).isEmpty
// Functional-style helpers/implementation
object MSet:
  // Factories
  def apply[A](l: A*): MSet[A] =
    new MSetImpl(l.toList)
  def ofList[A](l: List[A]): MSet[A] =
    new MSetImpl(l)
  def ofMap[A](m: Map[A,Int]): MSet[A] =
    MSetImpl(m)

  // Hidden reference implementation
  private case class MSetImpl[A](asMap: Map[A,Int]) extends MSet[A]:

    def this(list: List[A]) =
      this(list.groupBy(a=>a).map{case (a,n) => (a, n.size)})

    override lazy val asList =
      asMap.toList.flatMap{case (a,n) => List.fill(n)(a)}

    override def apply(v1: A) =
      asMap.getOrElse(v1,0)
    override def union(m: MSet[A]) =
      new MSetImpl[A](asMap union m.asMap)
    override def diff(m: MSet[A]) = new MSetImpl[A](asMap diff m.asMap)
    override def disjoined(m: MSet[A]) = asMap disjoined  m.asMap
    override def size = asMap.foldLeft(0)((acc, e) => acc + e._2)
    override def matches(m: MSet[A]) = extract(m).isDefined
    override def extract(m: MSet[A]) =
      Some(this diff m) filter (_.size == size - m.size)
    override def iterator = asMap.keysIterator
    override def toString = asMap.map((k, v) => s"(${k}:${v})").reduce(_ + " | " + _)
    override def frequencyOf(el: A): Int = asMap.getOrElse(el, 0)

def timeOf(task: Runnable): Long =
  val start = System.nanoTime()
  task.run()
  val end = System.nanoTime()
  (end - start) / 1000000
@main def main() =
  println(timeOf(() => MSet.ofMap(Map(("A", 10), ("B", 20), ("C", 30)))))
  println(timeOf(() => MSet.ofMap(Map(("A", 10000000), ("B", 20000000), ("C", 3000000)))))
  println(MSet.ofMap(Map(("A", 10000000), ("B", 20000000), ("C", 3000000))).toString())