import math

when defined(useFloat64):
  type FloatT* = float64
else:
  type FloatT* = float32

template isNaN*[T: float | float32 | float64](x: T): bool =
  classify(x) == fcNan

# {{{ Vec2

# TODO byref pragma?
type Vec2[T] = object
  x*, y*: T

type
  Vec2f* = Vec2[FloatT]
  Vec2i* = Vec2[int]

proc hasNaNs*(a: Vec2f): bool {.inline.} =
  isNaN(a.x) or isNaN(a.y)

proc vec2*[T](x, y: T): Vec2[T] {.inline.} =
  Vec2[T](x: x, y: y)

proc vec2i*(x, y: int): Vec2[int] {.inline.} =
  Vec2[int](x: x, y: y)

proc vec2f*(x, y: FloatT): Vec2[FloatT] {.inline.} =
  result = Vec2[FloatT](x: x, y: y)
  assert (not hasNaNs(result))

proc `[]`*[T](a: Vec2[T], i: int): T {.inline.} =
  assert (i >= 0 and i <= 1)
  if   i == 0: result = a.x
  elif i == 1: result = a.y

proc `[]=`*[T](a: var Vec2[T], i: int, v: T) {.inline.} =
  assert (i >= 0 and i <= 1)
  if   i == 0: a.x = v
  elif i == 1: a.y = v

proc `+`*[T](a, b: Vec2[T]): Vec2[T] {.inline.} =
  assert (not hasNaNs(b))
  Vec2[T](x: a.x + b.x,
          y: a.y + b.y)

proc `+=`*[T](a: var Vec2[T], b: Vec2[T]) {.inline.} =
  assert (not hasNaNs(b))
  a.x += b.x
  a.y += b.y

proc `-`*[T](a, b: Vec2[T]): Vec2[T] {.inline.} =
  assert (not hasNaNs(b))
  Vec2[T](x: a.x - b.x,
          y: a.y - b.y)

proc `-=`*[T](a: var Vec2[T], b: Vec2[T]) {.inline.} =
  assert (not hasNaNs(b))
  a.x -= b.x
  a.y -= b.y

proc `*`*[T,U](a: Vec2[T], s: U): Vec2[T] {.inline.} =
  assert (not isNaN(T(s)))
  Vec2[T](x: a.x * T(s),
          y: a.y * T(s))

proc `*`*[T,U](s: U, a: Vec2[T]): Vec2[T] {.inline.} =
  assert (not isNaN(T(s)))
  Vec2[T](x: a.x * T(s),
          y: a.y * T(s))

proc `*=`*[T,U](a: var Vec2[T], s: U) {.inline.} =
  assert (not isNaN(T(s)))
  a.x *= T(s)
  a.y *= T(s)

proc `/`*[T,U](a: Vec2[T], d: U): Vec2[T] {.inline.} =
  assert (not isNaN(T(d)))
  assert d != 0
  let s = 1 / float64(d)
  Vec2[T](x: a.x * s,
          y: a.y * s)

proc `/=`*[T,U](a: var Vec2[T], d: U) {.inline.} =
  assert (not isNaN(T(d)))
  assert d != 0
  let s = 1 / float64(d)
  a.x *= s
  a.y *= s

proc dot*[T](a, b: Vec2[T]): T {.inline.} =
  assert (not hasNaNs(a))
  assert (not hasNaNs(b))
  a.x * b.x + a.y * b.y

proc len*[T](a: Vec2[T]): T {.inline.} =
  sqrt(a.x * a.x + a.y * a.y)

proc len2*[T](a: Vec2[T]): T {.inline.} =
  let r = len(a)
  r * r

proc min*[T](a: Vec2[T]): T {.inline.} =
  min(a.x, a.y)

proc max*[T](a: Vec2[T]): T {.inline.} =
  max(a.x, a.y)

# }}}
# {{{ Vec3

# TODO byref pragma?
type Vec3[T] = object
  x*, y*, z*: T

type
  Vec3f* = Vec3[FloatT]
  Vec3i* = Vec3[int]

proc hasNaNs*(a: Vec3f): bool {.inline.} =
  isNaN(a.x) or isNaN(a.y) or isNaN(a.z)

proc vec3*[T](x, y, z: T): Vec3[T] {.inline.} =
  Vec3[T](x: x, y: y, z: z)

proc vec3i*(x, y, z: int): Vec3[int] {.inline.} =
  Vec3[int](x: x, y: y, z: z)

proc vec3f*(x, y, z: FloatT): Vec3[FloatT] {.inline.} =
  result = Vec3[FloatT](x: x, y: y, z: z)
  assert (not hasNaNs(result))

proc `[]`*[T](a: Vec3[T], i: int): T {.inline.} =
  assert (i >= 0 and i <= 2)
  if   i == 0: result = a.x
  elif i == 1: result = a.y
  elif i == 2: result = a.z

proc `[]=`*[T](a: var Vec3[T], i: int, v: T) {.inline.} =
  assert (i >= 0 and i <= 2)
  if   i == 0: a.x = v
  elif i == 1: a.y = v
  elif i == 2: a.z = v

proc `+`*[T](a, b: Vec3[T]): Vec3[T] {.inline.} =
  assert (not hasNaNs(b))
  Vec3[T](x: a.x + b.x,
          y: a.y + b.y,
          z: a.z + b.z)

proc `+=`*[T](a: var Vec3[T], b: Vec3[T]) {.inline.} =
  assert (not hasNaNs(b))
  a.x += b.x
  a.y += b.y
  a.z += b.z

proc `-`*[T](a, b: Vec3[T]): Vec3[T] {.inline.} =
  assert (not hasNaNs(b))
  Vec3[T](x: a.x - b.x,
          y: a.y - b.y,
          z: a.z - b.z)

proc `-=`*[T](a: var Vec3[T], b: Vec3[T]) {.inline.} =
  assert (not hasNaNs(b))
  a.x -= b.x
  a.y -= b.y
  a.z -= b.z

proc `*`*[T,U](a: Vec3[T], s: U): Vec3[T] {.inline.} =
  assert (not isNaN(T(s)))
  Vec3[T](x: a.x * T(s),
          y: a.y * T(s),
          z: a.z * T(s))

proc `*`*[T,U](s: U, a: Vec3[T]): Vec3[T] {.inline.} =
  assert (not isNaN(T(s)))
  Vec3[T](x: a.x * T(s),
          y: a.y * T(s),
          z: a.z * T(s))

proc `*=`*[T,U](a: var Vec3[T], s: U) {.inline.} =
  assert (not isNaN(T(s)))
  a.x *= T(s)
  a.y *= T(s)
  a.z *= T(s)

proc `/`*[T,U](a: Vec3[T], d: U): Vec3[T] {.inline.} =
  assert (not isNaN(T(d)))
  assert d != 0
  let s = 1 / float64(d)
  Vec3[T](x: a.x * s,
          y: a.y * s,
          z: a.z * s)

proc `/=`*[T,U](a: var Vec3[T], d: U) {.inline.} =
  assert (not isNaN(T(d)))
  assert d != 0
  let s = 1 / float64(d)
  a.x *= T(s)
  a.y *= T(s)
  a.z *= T(s)

proc dot*[T](a, b: Vec3[T]): T {.inline.} =
  assert (not hasNaNs(a))
  assert (not hasNaNs(b))
  a.x * b.x + a.y * b.y + a.z * b.z

proc cross*[T](a, b: Vec3[T]): Vec3[T] {.inline.} =
  assert (not hasNaNs(a))
  assert (not hasNaNs(b))
  Vec3[T](x: a.y * b.z - a.z * b.y,
          y: a.z * b.x - a.x * b.z,
          z: a.x * b.y - a.y * b.x)

proc len*(a: Vec3f): FloatT {.inline.} =
  sqrt(a.x * a.x + a.y * a.y + a.z * a.z)

proc len2*[T](a: Vec3[T]): T {.inline.} =
  let r = len(a)
  r * r

proc min*(a: Vec3f): FloatT {.inline.} =
  min(min(a.x, a.y), a.z)

proc max*(a: Vec3f): FloatT {.inline.} =
  max(max(a.x, a.y), a.z)

# }}}
# {{{ Mat4

# }}}
# {{{ Ray

# TODO byref pragma?
type Ray = object
  o: Vec3f
  d: Vec3f
  time: FloatT

# }}}
# {{{ AABB

# }}}

# {{{ Tests
when isMainModule:
  # 2D vectors
  block:
    var
      a = vec2f(1, 2)
      b = vec2f(3, 5)

    try:
      let c = vec2f(1, NaN)
      assert false
    except AssertionError:
      assert true

    assert a.x == 1
    assert a.y == 2
    assert a[0] == 1
    assert a[1] == 2
    assert a + b == vec2f(4, 7)
    assert a - b == vec2f(-2, -3)
    assert a * 2 == vec2f(2, 4)
    assert 2 * a == vec2f(2, 4)
    assert a / 2 == vec2f(0.5, 1)

    try:
      discard a / 0
      assert false
    except AssertionError:
      assert true

    assert a.dot(b) == 13
    assert len(a) == sqrt(FloatT(5))
    assert len2(a) == FloatT(5)
    assert min(a) == 1
    assert max(a) == 2

    a = vec2f(1, 2)
    a[0] = 3
    a[1] = 4
    assert a.x == 3
    assert a.y == 4

    a = vec2f(1, 2)
    a += b
    assert a == vec2f(4, 7)

    a = vec2f(1, 2)
    a -= b
    assert a == vec2f(-2, -3)

    a = vec2f(1, 2)
    a *= 2
    assert a == vec2f(2, 4)

    a = vec2f(1, 2)
    a /= 2
    assert a == vec2f(0.5, 1)

    try:
      a /= 0
      assert false
    except AssertionError:
      assert true

  # 3D vectors
  block:
    var
      a = vec3f(1, 2, 3)
      b = vec3f(3, 5, 7)

    try:
      let c = vec3f(1, 2, NaN)
      assert false
    except AssertionError:
      assert true

    assert a.x == 1
    assert a.y == 2
    assert a.z == 3
    assert a[0] == 1
    assert a[1] == 2
    assert a[2] == 3
    assert a + b == vec3f(4, 7, 10)
    assert a - b == vec3f(-2, -3, -4)
    assert a * 2 == vec3f(2, 4, 6)
    assert 2 * a == vec3f(2, 4, 6)
    assert a / 2 == vec3f(0.5, 1, 1.5)

    try:
      discard a / 0
      assert false
    except AssertionError:
      assert true

    assert a.dot(b) == 34
    # TODO assert a.cross(b) == 0
    assert len(a) == sqrt(FloatT(14))
    assert abs(len2(a) - FloatT(14)) < 0.00001  # TODO comparison helper
    assert min(a) == 1
    assert max(a) == 3

    a = vec3f(1, 2, 3)
    a[0] = 3
    a[1] = 4
    a[2] = 5
    assert a.x == 3
    assert a.y == 4
    assert a.z == 5

    a = vec3f(1, 2, 3)
    a += b
    assert a == vec3f(4, 7, 10)

    a = vec3f(1, 2, 3)
    a -= b
    assert a == vec3f(-2, -3, -4)

    a = vec3f(1, 2, 3)
    a *= 2
    assert a == vec3f(2, 4, 6)

    a = vec3f(1, 2, 3)
    a /= 2
    assert a == vec3f(0.5, 1, 1.5)

    try:
      a /= 0
      assert false
    except AssertionError:
      assert true

# }}}

# vim: et:ts=2:sw=2:fdm=marker
