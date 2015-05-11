/**
 * Created by besil on 09/05/15.
 */
object MyList {
  def reverse[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case h :: tail => reverse(tail) ::: List(h)
  }

  def length[T](l: List[T]) = {
    def count(n: Int, l: List[T]): Int = l match {
      case Nil => n
      case h :: tail => count(n+1, tail)
    }
    count(0, l)
  }

  def nth[T](k: Int, l: List[T]): T = {
    def next(k: Int, l: List[T]): T = k match {
      case 0 => l.head
      case _ => next(k-1, l.tail)
    }
    next(k, l)
  }

  def penultimate[T](l: List[T]): T = l match {
    case Nil => throw new NoSuchElementException
    case pe :: ul :: Nil => pe
    case h :: tail => penultimate(tail)
  }

  def last[T](l: List[T]): T = l match {
    case Nil => throw new NoSuchElementException
    case h :: Nil => h
    case h :: tail => last(tail)
  }
}
