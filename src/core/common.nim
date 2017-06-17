import types
import math

export types.FloatT

template f32*[T: SomeNumber](x: T): float32 = float32(x)
template f64*[T: SomeNumber](x: T): float64 = float64(x)

const
  ShadowEpsilon* = 0.0001

  InvPi*   = 0.31830988618379067154
  Inv2Pi*  = 0.15915494309189533577
  Inv4Pi*  = 0.07957747154594766788
  PiOver2* = 1.57079632679489661923
  PiOver4* = 0.78539816339744830961
  Sqrt2*   = 1.41421356237309504880

template isNaN*[T: SomeReal](x: T): bool =
  classify(x) == fcNan

template isNaN*[T: SomeInteger](x: T): bool =
  false

proc isCloseFn[T: SomeReal](a, b: T, maxRelDiff: T): bool =
  let
    diff = abs(a - b)
    largest = max(abs(a), abs(b))
  result = diff <= largest * maxRelDiff

proc isClose*(a, b: float32, maxRelDiff: float32 = 1e-5): bool =
  isCloseFn(a, b, maxRelDiff)

proc isClose*(a, b: float64, maxRelDiff: float64 = 1e-10): bool =
  isCloseFn(a, b, maxRelDiff)

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

template notNil*[T](x: T): bool = not isNil(x)

# vim: et:ts=2:sw=2:fdm=marker
