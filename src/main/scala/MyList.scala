/**
 * Created by besil on 09/05/15.
 */
object MyList {
  def decode[T](l: List[(Int, T)]): List[T] = l match {
    case Nil => Nil
    case (k, v) :: tail => ( (for(i <- 1 to k) yield v).toList) ::: decode(tail)
  }

  def encodeModified[T](l: List[T]): List[Any] = {
    def myencode(l: List[(Int, T)], acc: List[Any]): List[Any] = l match {
      case Nil => acc
      case h :: tail => h match {
        case (1, n) => myencode(tail, acc :+ n)
        case (x, y) => myencode(tail, acc :+ (x, y) )
      }
    }
    myencode(encode(l), List[Any]())
  }

  def encode[T](l: List[T]): List[(Int, T)] = pack(l).map(t => (t.size, t(0)))

  def pack[T](l: List[T]): List[List[T]] = l match {
      case Nil => Nil
      case h :: tail => l.takeWhile(_==h) :: pack(l.dropWhile(_==h))
  }

  def compress[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case h :: tail => h :: compress( l.dropWhile( _ == h ) )
  }

  def flatten[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case (l: List[T]) :: (tail: List[T]) => flatten(l) ::: flatten(tail)
    case head :: tail => head :: flatten(tail)
  }

  def isPalindrome[T](l: List[T]) = reverse(l) == l

  def reverse[T](l: List[T]): List[T] = {
    def rev[T](res: List[T], curList: List[T]): List[T] = curList match {
      case Nil => res
      case h :: tail => rev(h :: res, tail)
    }
    rev(List(), l)
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
