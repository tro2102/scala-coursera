package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
   test("string take") {
     val message = "hello, world"
     assert(message.take(5) == "hello")
   }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val firstThree = sizeThreeList(1)
    val nextThree = sizeThreeList(2)
    val firstTen = consecutiveInts(1,10)

    def evenFun(i: Int) = i % 2 == 0
    def oddFun(i: Int) = i % 2 == 1
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains only intersecting members of each set") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect singleton 1")
      assert(!contains(s, 2), "Intersect singleton 2")

      val t = intersect(firstThree, nextThree)
      assert(!contains(t, 1), "Intersect 1")
      assert(contains(t, 2), "Intersect 2")
      assert(contains(t, 3), "Intersect 3")
      assert(!contains(t, 4), "Intersect 4")
    }
  }

  test("diff contains only members set1 not in set2") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff singleton 1")

      val t = diff(firstThree, nextThree)
      assert(contains(t, 1), "Intersect 1")
      assert(!contains(t, 2), "Intersect 2")
      assert(!contains(t, 3), "Intersect 3")
      assert(!contains(t, 4), "Intersect 4")
    }
  }

  test("filter returns only members that satisfy a predicate") {
    new TestSets {
      val evens = filter(firstTen, evenFun)

      assert(!contains(evens, 1), "Filter 1")
      assert(contains(evens, 2), "Filter 2")
      assert(!contains(evens, 3), "Filter 3")
      assert(contains(evens, 4), "Filter 4")
      assert(!contains(evens, 5), "Filter 5")
      assert(contains(evens, 6), "Filter 6")
      assert(!contains(evens, 7), "Filter 7")
      assert(contains(evens, 8), "Filter 8")
      assert(!contains(evens, 9), "Filter 9")
      assert(contains(evens, 10), "Filter 10")
      assert(!contains(evens, 12), "Filter 12")
    }
  }

  test("foreach returns true if all members of a set satisfy p") {
    new TestSets {
      val evens = filter(firstTen, evenFun)

      assert(forall(evens, evenFun), "Forach Even set 1")
      assert(!forall(firstTen, evenFun), "Forach  Even set 2")
    }
  }

  test("exists returns true if a single member of a set satisfies p") {
    new TestSets {
      val evens = filter(firstTen, evenFun)
      val odds = filter(firstTen, oddFun)

      assert(exists(evens, evenFun), "Exists Even set 1")
      assert(!exists(odds, evens), "Exists Even set 2")
      assert(exists(firstTen, i => i == 7), "Exists Even set 3")
      assert(!exists(firstTen, i => i == 11), "Exists Even set 4")
    }
  }

  test("map returns a new set with predicate applied") {
    new TestSets {
      val evens = filter(firstTen, evenFun)
      val odds = filter(firstTen, oddFun)

      val mappedEvens = map(evens, i => i * 2)
      assert(contains(mappedEvens, 4), "Mapped set 1")
      assert(!contains(mappedEvens, 3), "Mapped set 2")
      assert(contains(mappedEvens, 8), "Mapped set 3")
      assert(!contains(mappedEvens, 6), "Mapped set 6")
    }
  }
}
