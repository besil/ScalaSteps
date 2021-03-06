import org.scalatest.FunSuite

/**
 * Created by besil on 09/05/15.
 */
class MyListTest extends FunSuite {
  val l = 1 to 5 toList

  test("lsort") {
    val actual: List[List[Symbol]] = MyList.lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    val expected: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))

    assert(actual == expected)
  }

  test("combinations") {
    val actual: List[List[Symbol]] = MyList.combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
    val expected = MyList.range(1, 20)

    assert(actual.size == expected.size)
  }

  test("randomPermute") {
    val actual = MyList.randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
    val expected = List('b, 'a, 'd, 'c, 'e, 'f)

    assert(actual.size == expected.size)
  }

  test("lotto") {
    val actual: List[Int] = MyList.lotto(6, 49)
    val expected = List(1, 2, 3, 4, 5, 6)

    assert(actual.size == expected.size)
  }

  test("randomSelect") {
    val actual: List[Symbol] = MyList.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    val expected = List('e, 'd, 'a)

    assert(actual.size == expected.size)
  }

  test("range") {
    val actual: List[Int] = MyList.range(4, 9)
    val expected = List(4, 5, 6, 7, 8, 9)

    assert(actual == expected)
  }

  test("InsertAt") {
    val actual = MyList.insertAt('new, 1, List('a, 'b, 'c, 'd))
    val expected = List('a, 'new, 'b, 'c, 'd)

    assert(actual == expected)
  }

  test("RemoveAt") {
    val actual: (List[Symbol], Symbol) = MyList.removeAt(1, List('a, 'b, 'c, 'd))
    val expected = (List('a, 'c, 'd),'b)

    assert( actual == expected )
  }

  test("Rotate") {
    assert( MyList.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c) )
    assert( MyList.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) )
  }

  test("Slice") {
    assert( MyList.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g) )
  }

  test("Split") {
    assert(MyList.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  test("Drop") {
    assert(MyList.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("DuplicateN") {
    assert(MyList.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  test("Duplicate") {
    assert(MyList.duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  test("Encode direct") {
    assert(MyList.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  test("Decode") { assert(MyList.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) }

  test("EncodedModified") { assert(MyList.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))) }

  test("Encode") { assert(MyList.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))) }

  test("Pack") { assert(MyList.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))) }

  test("Compress") { assert(MyList.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e)) }

  test("Flatten") { assert(MyList.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))}

  test("Palindrome") { assert( MyList.isPalindrome(List(1, 2, 3, 2, 1)) ) }

  test("Reverse") { assert( MyList.reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1) ) }

  test("Length") { assert(MyList.length(List(1, 1, 2, 3, 5, 8)) == 6)}

  test("Nth") { assert(MyList.nth(2, List(1, 1, 2, 3, 5, 8)) == 2) }

  test("Last") { assert( MyList.last(l) == 5 ) }

  test("Penultimate") { assert( MyList.penultimate(l) == 4 ) }

}
