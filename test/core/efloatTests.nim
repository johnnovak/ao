import common, efloat
import math, unittest, random, times


randomize()

proc randomFloat(): float32 =
  let
    minExp = -6
    maxExp = 6
    logu = lerp(random(1.0), minExp.FloatT, maxExp.FloatT)
    sign = if random(1.0) < 0.5: -1.0 else: 1.0
  result = sign * pow(10, logu)


proc randomEFloat(): EFloat =
  let val = randomFloat()
  var err: float32 = 0

  case random(4):
  of 0: discard # no error
  of 1:
    let
      ulpError = random(1024).uint32
      offset = cast[float32](cast[uint32](val) + ulpError)
    err = abs(offset - val)
  of 2:
    let
      ulpError = random(1024 * 1024).uint32
      offset = cast[float32](cast[uint32](val) + ulpError)
    err = abs(offset - val)
  of 3:
    err = (4 * random(1.0)) * abs(val)
  else: discard

  result = efloat(val, err)


const NumIterations = 1_000_000

suite "core/efloatTests":

  test "basic tests":
    # All operations will trigger the bounds check assertions in debug mode
    let
      a = efloat(-3.12345)
      b = efloat(0.65433, 0.002)
      r1 = a * b
      r2 = a + b
      r3 = a - b
      r4 = a / b
      r5 = abs(a)
      r6 = sqrt(a)
      r7 = 1.2 * a
      r8 = a * 1.2
      r9 = 1.2 / a
      r10 = a / 1.2
      r11 = 1.2 + a
      r12 = a + 1.2
      r13 = 1.2 - a
      r14 = a - 1.2

    when debug:
      discard r1.relError
      discard r1.absError

  test "add":
    for i in 0..<NumIterations:
      let
        a = randomEFloat()
        b = randomEFloat()
        res = a + b

    for i in 0..<NumIterations:
      let
        a = randomEFloat()
        b = randomFloat()
        res = a + b

    for i in 0..<NumIterations:
      let
        a = randomFloat()
        b = randomEFloat()
        res = a + b

  test "sub":
    for i in 0..<NumIterations:
      let
        a = randomEFloat()
        b = randomEFloat()
        res = a - b

    for i in 0..<NumIterations:
      let
        a = randomEFloat()
        b = randomFloat()
        res = a - b

    for i in 0..<NumIterations:
      let
        a = randomFloat()
        b = randomEFloat()
        res = a - b

  test "neg":
    for i in 0..<NumIterations:
      let
        a = randomEFloat()
        res = -a

  test "mul":
    for i in 0..<NumIterations:
      let
        a = randomEFloat()
        b = randomEFloat()
        res = a * b

    for i in 0..<NumIterations:
      let
        a = randomEFloat()
        b = randomFloat()
        res = a * b

    for i in 0..<NumIterations:
      let
        a = randomFloat()
        b = randomEFloat()
        res = a * b

  test "div":
    for i in 0..<NumIterations:
      let
        a = randomEFloat()
        b = randomEFloat()
        res = a / b

    for i in 0..<NumIterations:
      let
        a = randomEFloat()
        b = randomFloat()
        res = a / b

    for i in 0..<NumIterations:
      let
        a = randomFloat()
        b = randomEFloat()
        res = a / b 

  test "sqrt":
    for i in 0..<NumIterations:
      let a = randomEFloat()
      if a.lo < 0 and a.hi > 0:
        continue
      let res = sqrt(a)

  test "abs":
    for i in 0..<NumIterations:
      let
        a = randomEFloat()
        res = abs(a)


# vim: et:ts=2:sw=2:fdm=marker
