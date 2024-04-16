package scala.u06.utils

// A multiset datatype
trait MSet[+A] extends Iterable[A]:
  def union[B >: A](m: MSet[B]): MSet[B]
  def diff[B >: A](m: MSet[B]): MSet[A]
  def disjoined[B >: A](m: MSet[B]): Boolean
  def size: Int
  def matches[B >: A](m: MSet[B]): Boolean
  def extract[B >: A](m: MSet[B]): Option[MSet[A]]
  def asList: List[A]
  def asMap[B >: A]: Map[B,Int]
  def iterator: Iterator[A]


// Functional-style helpers/implementation
object MSet:
  // Factories
  def apply[A](l: A*): MSet[A] = new MSetImpl(l.toList)
  def ofList[A](l: List[A]): MSet[A] = new MSetImpl(l)
  def ofMap[A](m: Map[A,Int]): MSet[A] = MSetImpl(m)
  def apply[A](n:Int, a:A): MSet[A] = new MSetImpl(List.fill(n)(a))

  // Hidden reference implementation
  private case class MSetImpl[A](asm: Map[A,Int]) extends MSet[A]:
    def this(list: List[A]) = this:
      list.groupBy(a => a).map((a,n) => (a, n.size))
    override val asList =
      asMap.toList.flatMap((a,n) => List.fill(n)(a))

    def apply[B >: A](v1: A) = asMap.getOrElse(v1,0)
    override def union[B >: A](m: MSet[B]) =
      new MSetImpl(m.asList ++ this.asList)
    override def diff[B >: A](m: MSet[B]) = new MSetImpl[A](asList diff m.asList)
    override def disjoined[B >: A](m: MSet[B]) = (asList intersect m.asList).isEmpty
    override def size = asList.size
    override def matches[B >: A](m: MSet[B]) = extract(m).isDefined
    override def extract[B >: A](m: MSet[B]) =
      Some(this diff m) filter (_.size == size - m.size)
    override def iterator = asMap.keysIterator
    override def toString = s"{${asList.mkString("|")}}"

    override def asMap[B >: A]: Map[B, Int] =
      asList.groupBy(a => a).map((a,n) => (a, n.size))