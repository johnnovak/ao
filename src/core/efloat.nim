import common
import math


type EFloat* = object
  v*: float32
  lo*, hi*: float32
  when debug:
    vPrecise*: float64

proc check(a: EFloat) {.inline.} =
  if not isNaN(a.lo) and not isNaN(a.hi):
    assert a.lo <= a.hi
  when debug:
    if not isNaN(a.v):
      assert a.lo <= a.vPrecise
      assert a.vPrecise <= a.hi

proc absError*(a: EFloat): float32 {.inline.} =
  result = a.hi - a.lo

when debug:
  proc relError*(a: EFloat): float32 =
    result = abs((a.vPrecise - a.v) / a.vPrecise)

proc efloat*(v: float32): EFloat {.inline.} =
  result.v = v
  when debug:
    result.vPrecise = v
  result.lo = v
  result.hi = v
  check(result)

proc efloat*(v, err: float32): EFloat {.inline.} =
  result.v = v
  when debug:
    result.vPrecise = v
  result.lo = prevFloat(v - err)
  result.hi = nextFloat(v + err)
  check(result)

proc `+`*(a: EFloat, b: EFloat): EFloat {.inline.} =
  result.v = a.v + b.v
  when debug:
    result.vPrecise = a.vPrecise + b.vPrecise
  result.lo = prevFloat(a.lo + b.lo)
  result.hi = nextFloat(a.hi + b.hi)
  check(result)

proc `-`*(a: EFloat, b: EFloat): EFloat {.inline.} =
  result.v = a.v - b.v
  when debug:
    result.vPrecise = a.vPrecise - b.vPrecise
  result.lo = prevFloat(a.lo - b.hi)
  result.hi = nextFloat(a.hi - b.lo)
  check(result)

proc `-`*(a: EFloat): EFloat {.inline.} =
  result.v = -a.v
  when debug:
    result.vPrecise = -a.vPrecise
  result.lo = -a.hi
  result.hi = -a.lo
  check(result)

proc `*`*(a: EFloat, b: EFloat): EFloat {.inline.} =
  result.v = a.v * b.v
  when debug:
    result.vPrecise = a.vPrecise * b.vPrecise
  let
    p0 = a.lo * b.lo
    p1 = a.lo * b.hi
    p2 = a.hi * b.lo
    p3 = a.hi * b.hi
  result.lo = prevFloat(min(p0, min(p1, min(p2, p3))))
  result.hi = nextFloat(max(p0, max(p1, max(p2, p3))))
  check(result)

proc `/`*(a: EFloat, b: EFloat): EFloat {.inline.} =
  result.v = a.v / b.v
  when debug:
    result.vPrecise = a.vPrecise / b.vPrecise
  if b.lo <= 0 and b.hi >= 0:
    result.lo = NegInf
    result.hi = Inf
  else:
    let
      p0 = a.lo / b.lo
      p1 = a.lo / b.hi
      p2 = a.hi / b.lo
      p3 = a.hi / b.hi
    result.lo = prevFloat(min(p0, min(p1, min(p2, p3))))
    result.hi = nextFloat(max(p0, max(p1, max(p2, p3))))
  check(result)

proc `+`*(e: EFloat, n: SomeNumber): EFloat {.inline.} = e + efloat(n)
proc `-`*(e: EFloat, n: SomeNumber): EFloat {.inline.} = e - efloat(n)
proc `*`*(e: EFloat, n: SomeNumber): EFloat {.inline.} = e * efloat(n)
proc `/`*(e: EFloat, n: SomeNumber): EFloat {.inline.} = e / efloat(n)

proc `+`*(n: SomeNumber, e: EFloat): EFloat {.inline.} = efloat(n) + e
proc `-`*(n: SomeNumber, e: EFloat): EFloat {.inline.} = efloat(n) - e
proc `*`*(n: SomeNumber, e: EFloat): EFloat {.inline.} = efloat(n) * e
proc `/`*(n: SomeNumber, e: EFloat): EFloat {.inline.} = efloat(n) / e

proc sqrt*(a: EFloat): EFloat {.inline.} =
  result.v = sqrt(a.v)
  when debug:
    result.vPrecise = sqrt(a.vPrecise)
  result.lo = prevFloat(sqrt(a.lo))
  result.hi = nextFloat(sqrt(a.hi))
  check(result)

proc abs*(a: EFloat): EFloat {.inline.} =
  if a.lo >= 0: result = a
  elif a.hi < 0:
    result.v = -a.v
    when debug:
      result.vPrecise = -a.vPrecise
    result.lo = -a.hi
    result.hi = -a.lo
  else:
    result.v = abs(a.v)
    when debug:
      result.vPrecise = abs(a.vPrecise)
    result.lo = 0
    result.hi = max(-a.lo, a.hi)
  check(result)


# vim: et:ts=2:sw=2:fdm=marker
