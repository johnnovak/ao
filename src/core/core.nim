import math


when defined(useFloat64):
  type FloatT* = float64
else:
  type FloatT* = float32

template isNaN*[T: float | float32 | float64](x: T): bool =
  classify(x) == fcNan

# vim: et:ts=2:sw=2:fdm=marker
