import common
import math, unittest, times


suite "core/commonTests":

  test "isNan":
    check:
      isNaN(0'f32/0'f32)
      not isNaN(0'f32)
      not isNaN(1'f32)
      not isNaN(-1'f32)
      not isNaN(Inf)
      not isNaN(-Inf)

      isNaN(0'f64/0'f64)
      not isNaN(0'f64)
      not isNaN(1'f64)
      not isNaN(-1'f64)
      not isNaN(Inf)
      not isNaN(-Inf)

  test "lerp":
    check:
      lerp(FloatT(1.5), 3.3, 0.2).isClose(FloatT(1.86))
      lerp(FloatT(-3.3), 10.5, 0.75).isClose(FloatT(7.05))

  test "isClose":
    check:
      15'f32.isClose(15.00001'f32)
      15'f64.isClose(15.000000001'f64)

  test "quadraticDelta & solveQuadratic":
    let
      a = 1.0
      b = -1.786737601482363
      c = 2.054360090947453e-8
      delta = quadraticDelta(a, b, c)
      (x1, x2) = solveQuadratic(a, b, c, delta)

    check:
      x1.isClose(1.786737589984535)
      x2.isClose(1.149782767465722e-08)

  test "sprintf":
    var s = sprintf("stuff: %f", 123.4)
    check s == "stuff: 123.400000"

  test "prevFloat & nextFloat (float32)":
    let infinity: float32 = Inf
    let negInfinity: float32 = NegInf
    let negativeZero: float32 = 1e-1000
    check:
      nextFloat(infinity) == infinity
      prevFloat(infinity) < infinity
      nextFloat(prevFloat(infinity)) == infinity
      nextFloat(0'f32) > 0
      nextFloat(negativeZero) > 0

      prevFloat(negInfinity) == negInfinity
      nextFloat(negInfinity) > negInfinity
      prevFloat(nextFloat(negInfinity)) == negInfinity
      prevFloat(0'f32) < 0
      prevFloat(negativeZero) < 0

  test "prevFloat & nextFloat (float64)":
    let infinity: float64 = Inf
    let negInfinity: float64 = NegInf
    let negativeZero: float64 = 1e-10000
    check:
      nextFloat(infinity) == infinity
      prevFloat(infinity) < infinity
      nextFloat(prevFloat(infinity)) == infinity
      nextFloat(0'f64) > 0
      nextFloat(negativeZero) > 0

      prevFloat(negInfinity) == negInfinity
      nextFloat(negInfinity) > negInfinity
      prevFloat(nextFloat(negInfinity)) == negInfinity
      prevFloat(0'f64) < 0
      prevFloat(negativeZero) < 0


# vim: et:ts=2:sw=2:fdm=marker
