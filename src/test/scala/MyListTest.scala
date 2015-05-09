import org.scalatest.FunSuite

/**
 * Created by besil on 09/05/15.
 */
class MyListTest extends FunSuite {
  test("Last") {
    val l = 1 to 5 toList

    assert( MyList.last(l) == 5 )
  }

  test("Penultimate") {
    val l = 1 to 5 toList

    assert( MyList.penultimate(l) == 4 )
  }

}
