import scala.util.Random

/**
 * Created by besil on 09/05/15.
 */
object MyList {
  def lotto(n: Int, max: Int): List[Int] = randomSelect(max - n, range(0, max))


  def randomSelect[T](n: Int, l: List[T]): List[T] = {
    val random = new Random()
    var res = l
    for (x <- range(0, n))
      res = removeAt(random.nextInt(res.size), res)._1
    res
  }

  def range(start: Int, end: Int): List[Int] = start to end toList

  def removeAt[T](i: Int, l: List[T]): (List[T], T) = {
    def myRemove(i: Int, l: List[T], acc: List[T]): (List[T], T) = i match {
      case 0 => (acc ::: l.tail, l.head)
      case n => myRemove(n - 1, l.tail, l.head :: acc)
    }
    myRemove(i, l, List())
  }

  def insertAt[T](el: T, pos: Int, l: List[T]): List[T] = {
    def myInsert(i: Int, l: List[T], acc: List[T]): List[T] = i match {
      case 0 => (acc :+ el) ::: l
      case n => myInsert(n - 1, l.tail, l.head :: acc)
    }
    myInsert(pos, l, List())
  }

  def rotate[T](r: Int, l: List[T]): List[T] = {
    def myRotate(k: Int, acc: List[T], list: List[T]): List[T] = k match {
      case 0 => if(r > 0) list ::: acc else acc:::list
      case x if x > 0 => myRotate(k-1, acc :+ list.head, list.tail)
      case x if x < 0 => myRotate(k+1, list.reverse.head :: acc, list.reverse.tail.reverse)
    }
    myRotate(r, List(), l)
  }

  //  def slice[T](s:Int, e:Int, l: List[T]): List[T] = (for( i <- s until e) yield l(i) ).toList
  def slice[T](s: Int, e: Int, l: List[T]): List[T] = {
    def isIn(n: Int) = n >= s && n < e
    def isOut(n: Int) = n >= e

    def myslice(k: Int, acc: List[T], l: List[T]): List[T] = (k, l) match {
      case (x, h :: tail) if isOut(k) => acc
      case (x, h :: tail) if isIn(k)  => myslice(x + 1, acc :+ h, tail)
      case (x, h :: tail)             => myslice(x + 1, acc, tail)
    }
    myslice(0, List(), l)
  }

  def split[T](n: Int, l: List[T]): (List[T], List[T]) = {
    def mysplit(k: Int, acc: List[T], l: List[T]): (List[T], List[T]) = (k, l) match {
      case (_, Nil) => (acc, Nil)
      case (0, list) => (acc, list)
      case (m, h :: tail) => mysplit(m - 1, acc :+ h, tail)
    }
    mysplit(n, List(), l)
  }

  def drop[T](n: Int, l: List[T]): List[T] = {
    def mydrop(k: Int, l: List[T]): List[T] = (k, l) match {
      case (_, Nil) => Nil
      case (x, h :: tail) if x > 1 => h :: mydrop(x - 1, tail)
      case (1, h :: tail) => mydrop(n, tail)
    }
    mydrop(n, l)
  }

  def duplicateN[T](n: Int, l: List[T]): List[T] = l match {
    case Nil => Nil
    case h :: tail => (for (i <- 1 to n) yield h).toList ::: duplicateN(n, tail)
  }

  def duplicate[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case h :: tail => h :: h :: duplicate(tail)
  }

  def encodeDirect[T](l: List[T]): List[(Int, T)] = l match {
    case Nil => Nil
    case t => {
      val (same, rest) = t.span(_ == t.head)
      (same.size, same.head) :: encodeDirect(rest)
    }
  }

  def decode[T](l: List[(Int, T)]): List[T] = l match {
    case Nil => Nil
    case (k, v) :: tail => (for (i <- 1 to k) yield v).toList ::: decode(tail)
  }

  def encodeModified[T](l: List[T]): List[Any] = {
    def myencode(l: List[(Int, T)], acc: List[Any]): List[Any] = l match {
      case Nil => acc
      case h :: tail => h match {
        case (1, n) => myencode(tail, acc :+ n)
        case (x, y) => myencode(tail, acc :+(x, y))
      }
    }
    myencode(encode(l), List[Any]())
  }

  def encode[T](l: List[T]): List[(Int, T)] = pack(l).map(t => (t.size, t.head))

  def pack[T](l: List[T]): List[List[T]] = l match {
    case Nil => Nil
    case h :: tail => l.takeWhile(_ == h) :: pack(l.dropWhile(_ == h))
  }

  def compress[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case h :: tail => h :: compress(l.dropWhile(_ == h))
  }

  def flatten[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case (l: List[T]) :: (tail) => flatten(l) ::: flatten(tail)
    case head :: tail => head :: flatten(tail)
  }

  def isPalindrome[T](l: List[T]) = reverse(l) == l

  def reverse[T](l: List[T]): List[T] = {
    def rev(res: List[T], curList: List[T]): List[T] = curList match {
      case Nil => res
      case h :: tail => rev(h :: res, tail)
    }
    rev(List(), l)
  }

  def length[T](l: List[T]) = {
    def count(n: Int, l: List[T]): Int = l match {
      case Nil => n
      case h :: tail => count(n + 1, tail)
    }
    count(0, l)
  }

  def nth[T](k: Int, l: List[T]): T = {
    def next(k: Int, l: List[T]): T = k match {
      case 0 => l.head
      case _ => next(k - 1, l.tail)
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
