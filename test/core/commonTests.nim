import math, unittest, times
import core


suite "core/commonTests":

  test "isNan":
    assert isNaN(0'f32/0'f32)
    assert(not isNaN(1'i32))

  test "lerp":
    assert lerp(FloatT(1.5), 3.3, 0.2).isClose(FloatT(1.86))
    assert lerp(FloatT(-3.3), 10.5, 0.75).isClose(FloatT(7.05))

  test "isClose":
      assert 15'f32.isClose(15.00001'f32)
      assert 15'f64.isClose(15.000000001'f64)

  test "quadraticDelta & solveQuadratic":
    let
      a = 1.0
      b = -1.786737601482363
      c = 2.054360090947453e-8
      delta = quadraticDelta(a, b, c)
      (x1, x2) = solveQuadratic(a, b, c, delta)

    assert x1.isClose(1.786737589984535)
    assert x2.isClose(1.149782767465722e-08)

  test "sprintf":
    var s = sprintf("stuff: %f", 123.4)
    assert s == "stuff: 123.400000"


# vim: et:ts=2:sw=2:fdm=marker
