import TrampolineExample.*
import org.scalatest.funsuite.AnyFunSuite

class TrampolineSpec extends AnyFunSuite {

  test("even function should return true for 0") {
    assert(even(0).run)
  }

  test("odd function should return false for 0") {
    assert(!odd(0).run)
  }

  test("even function should return false for 1") {
    assert(!even(1).run)
  }

  test("odd function should return true for 1") {
    assert(odd(1).run)
  }

  test("even function should handle large numbers") {
    assert(even(1000000).run)
  }

  test("odd function should handle large numbers") {
    assert(odd(1000001).run)
  }
}