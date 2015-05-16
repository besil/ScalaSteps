import org.scalatest.FunSuite

/**
 * Created by besil on 09/05/15.
 */
class MyListTest extends FunSuite {
  val l = 1 to 5 toList

  test("Decode") { assert(MyList.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) }

  test("EncodedModified") { assert(MyList.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))) }

  test("Encode") { assert(MyList.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))) }

  test("Pack") { assert(MyList.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))) }

  test("Compress") { assert(MyList.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e)) }

  test("Flatten") { assert(MyList.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))}

  test("Palindrome") { assert( MyList.isPalindrome(List(1, 2, 3, 2, 1)) == true ) }

  test("Reverse") { assert( MyList.reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1) ) }

  test("Length") { assert(MyList.length(List(1, 1, 2, 3, 5, 8)) == 6)}

  test("Nth") { assert(MyList.nth(2, List(1, 1, 2, 3, 5, 8)) == 2) }

  test("Last") { assert( MyList.last(l) == 5 ) }

  test("Penultimate") { assert( MyList.penultimate(l) == 4 ) }

}
