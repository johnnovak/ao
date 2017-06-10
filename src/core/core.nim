import math


when defined(useFloat64):
  type FloatT* = float64
else:
  type FloatT* = float32

template f32[T: SomeNumber](x: T): float32 = float32(x)
template f64[T: SomeNumber](x: T): float64 = float64(x)

const
  NegZero* = -1e-1000

  ShadowEpsilon* = 0.0001

  InvPi*   = 0.31830988618379067154
  Inv2Pi*  = 0.15915494309189533577
  Inv4Pi*  = 0.07957747154594766788
  PiOver2* = 1.57079632679489661923
  PiOver4* = 0.78539816339744830961
  Sqrt2*   = 1.41421356237309504880

template isNaN*[T: SomeReal](x: T): bool =
  classify(x) == fcNan

proc isCloseFn[T: SomeReal](a, b: T, maxRelDiff: T): bool =
  let
    diff = abs(a - b)
    largest = max(abs(a), abs(b))
  result = diff <= largest * maxRelDiff

proc isClose*(a, b: float32, maxRelDiff: float32 = 1e-5): bool =
  isCloseFn(a, b, maxRelDiff)

proc isClose*(a, b: float64, maxRelDiff: float64 = 1e-10): bool =
  isCloseFn(a, b, maxRelDiff)

proc sgn*[T: SomeNumber](a: T): int {.inline.} =
  cast[int](T(0) < a) - cast[int](a < T(0))

proc modulo*[T: SomeReal](x: T): T {.inline.} =
  abs(x - floor(x))

proc lerp*(a, b, t: FloatT): FloatT {.inline.} =
  (1-t)*a + t*b

proc quadraticDelta*(a, b, c: float64): float64 {.inline.} =
  b*b - 4*a*c

proc solveQuadratic*(a, b, c, delta: float64): (float64, float64) {.inline.} =
  var
    t1 = (-b - sgn(b).f64 * sqrt(delta)) / 2*a
    t2 = c / (a*t1)
  result = (t1, t2)

proc printf*(format: cstring) {.header: "<stdio.h>", varargs.}

proc c_snprintf(buf: cstring, size: int, format: cstring)
               {.importc: "snprintf", header: "<stdio.h>", varargs.}

template sprintf*(format: cstring, args: varargs[untyped]): string =
  const MAXLEN = 4096
  var buf = newString(MAXLEN)
  c_snprintf(buf, MAXLEN, format, args)
  buf.setLen(buf.cstring.len)
  var s = newString(buf.len)
  s = buf
  s


when isMainModule:
  assert $NegZero == "-0.0"

  assert isNaN(0/0)

  assert lerp(FloatT(1.5), 3.3, 0.2).isClose(FloatT(1.86))
  assert lerp(FloatT(-3.3), 10.5, 0.75).isClose(FloatT(7.05))

  block: # isClose() tests
    assert 15'f32.isClose(15.00001'f32)
    assert 15'f64.isClose(15.000000001'f64)

  block: # sgn() tests
    assert sgn(1'i8) == 1
    assert sgn(1'i16) == 1
    assert sgn(1'i32) == 1
    assert sgn(1'i64) == 1
    assert sgn(-12342.8844'f32) == -1
    assert sgn(0'f32) == 0
    assert sgn(NegZero) == 0
    assert sgn(NegInf) == -1
    assert sgn(Inf) == 1
    assert sgn(NaN) == 0
    assert sgn(-12342.8844'f32) == -1

  block: # quadraticDelta() & solveQuadratic() tests
    let
      a = 1.0
      b = -1.786737601482363
      c = 2.054360090947453e-8
      delta = quadraticDelta(a, b, c)
      (x1, x2) = solveQuadratic(a, b, c, delta)

    assert x1.isClose(1.786737589984535)
    assert x2.isClose(1.149782767465722e-08)

  var s = sprintf("stuff: %f", 123.4)
  assert s == "stuff: 123.400000"


# vim: et:ts=2:sw=2:fdm=marker
