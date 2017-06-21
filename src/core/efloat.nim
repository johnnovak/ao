import common


type EFloat* = object
  v: float32
  lo, hi: float32

proc efloat*(v: float32): EFloat {.inline.} =
  result.v = v
  result.lo = v
  result.hi = v

proc `+`*(a: var EFloat, b: EFloat) {.inline.} =
  a.v += b.v
  a.lo = prevFloat(a.lo + b.lo)
  a.hi = nextFloat(a.hi + b.hi)

proc `-`*(a: var EFloat, b: EFloat) {.inline.} =
  a.v -= b.v
  a.lo = prevFloat(a.lo - b.hi)
  a.hi = nextFloat(a.hi - b.lo)


proc `*`*(a: var EFloat, b: EFloat) {.inline.} =
  a.v *= b.v
  let
    p0 = a.lo * b.lo
    p1 = a.lo * b.hi
    p2 = a.hi * b.lo
    p3 = a.hi * b.hi

  a.lo = prevFloat(min(p0, min(p1, min(p2, p3))))
  a.hi = nextFloat(max(p0, max(p1, max(p2, p3))))


proc `/`*(a: var EFloat, b: EFloat) {.inline.} =
  a.v /= b.v
  let
    p0 = a.lo / b.lo
    p1 = a.lo / b.hi
    p2 = a.hi / b.lo
    p3 = a.hi / b.hi

  a.lo = prevFloat(min(p0, min(p1, min(p2, p3))))
  a.hi = nextFloat(max(p0, max(p1, max(p2, p3))))

# vim: et:ts=2:sw=2:fdm=marker
