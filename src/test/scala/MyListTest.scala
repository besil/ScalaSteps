import org.scalatest.FunSuite

/**
 * Created by besil on 09/05/15.
 */
class MyListTest extends FunSuite {
  val l = 1 to 5 toList

  test("Reverse") { assert( MyList.reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1) ) }

  test("Length") { assert(MyList.length(List(1, 1, 2, 3, 5, 8)) == 6)}

  test("Nth") { assert(MyList.nth(2, List(1, 1, 2, 3, 5, 8)) == 2) }

  test("Last") { assert( MyList.last(l) == 5 ) }

  test("Penultimate") { assert( MyList.penultimate(l) == 4 ) }

}
