package scala.u07.utils

import java.util.Collection
import scala.collection.immutable.List
import scala.math.Numeric

object List:
  extension (self: Iterable[Double])
    def mean: Double = self.sum/self.size

//override def takeWhile(p: A => Boolean): LazyList[A]

  extension [A](self: LazyList[A])
    def takeUntil(p: A => Boolean): List[A] = self match
      case head #:: tail if p.apply(head) => head :: tail.takeUntil(p)
      case head #:: tail => scala.collection.immutable.List(head)
      case _ => scala.collection.immutable.List()