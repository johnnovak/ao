import types
import fenv, math

export types.FloatT


const debug* = not defined(release)

const MachineEpsilonF32* = pow(2'f64, -24'f64)
const MachineEpsilonF64* = pow(2'f64, -53'f64)

when defined(useFloat64):
  const MachineEpsilon* = MachineEpsilonF64
else:
  const MachineEpsilon* = MachineEpsilonF32

template f32*[T: SomeNumber](x: T): float32 = float32(x)
template f64*[T: SomeNumber](x: T): float64 = float64(x)

template notNil*[T](x: T): bool = not isNil(x)

const
  InvPi*   = 0.31830988618379067154
  Inv2Pi*  = 0.15915494309189533577
  Inv4Pi*  = 0.07957747154594766788
  PiOver2* = 1.57079632679489661923
  PiOver4* = 0.78539816339744830961
  Sqrt2*   = 1.41421356237309504880

proc isNaN*(v: float32): bool {.inline.} =
  (cast[uint32](v) and 0x7fffffff'u32) > 0x7f800000'u32

proc isNaN*(v: float64): bool {.inline.} =
  (cast[uint64](v) and 0x7fffffffffffffff'u64) > 0x7ff0000000000000'u64

proc isNaN*[T: SomeInteger](x: T): bool {.inline.} =
  false

proc nextFloat*(f: float32): float32 {.inline.} =
  if isNaN(f) or f == Inf: return f
  var n = cast[uint32](f)
  if n == 0x80000000'u32: n = 0  # turn -0.0 to +0.0
  if f >= 0: n += 1
  else:      n -= 1
  result = cast[float32](n)

proc prevFloat*(f: float32): float32 {.inline.} =
  if isNaN(f) or f == NegInf: return f
  var n = cast[uint32](f)
  if n == 0: n = 0x80000000'u32  # turn +0.0 to -0.0
  if f > 0: n -= 1
  else:     n += 1
  result = cast[float32](n)

proc nextFloat*(f: float64): float64 {.inline.} =
  if isNaN(f) or f == Inf: return f
  var n = cast[uint64](f)
  if n == 0x8000000000000000'u64: n = 0  # turn -0.0 to +0.0
  if f >= 0: n += 1
  else:      n -= 1
  result = cast[float64](n)

proc prevFloat*(f: float64): float64 {.inline.} =
  if isNaN(f) or f == NegInf: return f
  var n = cast[uint64](f)
  if n == 0: n = 0x8000000000000000'u64  # turn +0.0 to -0.0
  if f > 0: n -= 1
  else:     n += 1
  result = cast[float64](n)

proc gamma*(n: int): FloatT =
  let nf = FloatT(n)
  (nf * MachineEpsilon) / (1 - nf * MachineEpsilon)

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


# vim: et:ts=2:sw=2:fdm=marker
