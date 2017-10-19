package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait Visualization2Test extends FunSuite with Checkers {

  test("bilinear interpolation test") {
    val exp = Visualization2.bilinearInterpolation(0d, 0d, -1d, -43.682226182296105, -48.190592673570755, 32.38201774618433)
    assert(exp == -1d)
  }

}
