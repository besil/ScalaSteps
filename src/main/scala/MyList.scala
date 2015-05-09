/**
 * Created by besil on 09/05/15.
 */
object MyList {
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
