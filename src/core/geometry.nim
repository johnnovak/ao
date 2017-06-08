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

proc vec2i*(x, y: int): Vec2i {.inline.} =
  Vec2[int](x: x, y: y)

proc vec2f*(x, y: FloatT): Vec2f {.inline.} =
  result = Vec2f(x: x, y: y)
  assert(not hasNaNs(result))

proc s*[T](a: Vec2[T]): T {.inline.} = a.x
proc t*[T](a: Vec2[T]): T {.inline.} = a.y

proc `s=`*[T](a: var Vec2[T], s: T) {.inline.} = a.x = s
proc `t=`*[T](a: var Vec2[T], t: T) {.inline.} = a.y = t

proc r*[T](a: Vec2[T]): T {.inline.} = a.x
proc g*[T](a: Vec2[T]): T {.inline.} = a.y

proc `r=`*[T](a: var Vec2[T], r: T) {.inline.} = a.x = r
proc `g=`*[T](a: var Vec2[T], g: T) {.inline.} = a.y = g

proc `[]`*[T](a: Vec2[T], i: int): T {.inline.} =
  assert(i >= 0 and i <= 1)
  if   i == 0: result = a.x
  elif i == 1: result = a.y

proc `[]=`*[T](a: var Vec2[T], i: int, v: T) {.inline.} =
  assert(i >= 0 and i <= 1)
  if   i == 0: a.x = v
  elif i == 1: a.y = v

proc `+`*[T](a, b: Vec2[T]): Vec2[T] {.inline.} =
  assert(not hasNaNs(b))
  Vec2[T](x: a.x + b.x,
          y: a.y + b.y)

proc `+=`*[T](a: var Vec2[T], b: Vec2[T]) {.inline.} =
  assert(not hasNaNs(b))
  a.x += b.x
  a.y += b.y

proc `-`*[T](a, b: Vec2[T]): Vec2[T] {.inline.} =
  assert(not hasNaNs(b))
  Vec2[T](x: a.x - b.x,
          y: a.y - b.y)

proc `-=`*[T](a: var Vec2[T], b: Vec2[T]) {.inline.} =
  assert(not hasNaNs(b))
  a.x -= b.x
  a.y -= b.y

proc `*`*[T,U](a: Vec2[T], s: U): Vec2[T] {.inline.} =
  assert(not isNaN(T(s)))
  Vec2[T](x: a.x * T(s),
          y: a.y * T(s))

proc `*`*[T,U](s: U, a: Vec2[T]): Vec2[T] {.inline.} =
  assert(not isNaN(T(s)))
  Vec2[T](x: a.x * T(s),
          y: a.y * T(s))

proc `*=`*[T,U](a: var Vec2[T], s: U) {.inline.} =
  assert(not isNaN(T(s)))
  a.x *= T(s)
  a.y *= T(s)

proc `/`*[T,U](a: Vec2[T], d: U): Vec2[T] {.inline.} =
  assert(not isNaN(T(d)))
  assert(d != 0)
  let s = 1 / float64(d)
  Vec2[T](x: a.x * s,
          y: a.y * s)

proc `/=`*[T,U](a: var Vec2[T], d: U) {.inline.} =
  assert(not isNaN(T(d)))
  assert(d != 0)
  let s = 1 / float64(d)
  a.x *= s
  a.y *= s

proc dot*[T](a, b: Vec2[T]): T {.inline.} =
  assert(not hasNaNs(a))
  assert(not hasNaNs(b))
  a.x * b.x + a.y * b.y

proc absDot*[T](a, b: Vec2[T]): T {.inline.} =
  abs(dot(a, b))

proc len*[T](a: Vec2[T]): T {.inline.} =
  sqrt(a.x * a.x + a.y * a.y)

proc len2*[T](a: Vec2[T]): T {.inline.} =
  let r = len(a)
  r * r

proc norm*[T](a: Vec2[T]): Vec2[T] {.inline.} =
  a / a.len

proc min*[T](a: Vec2[T]): T {.inline.} =
  min(a.x, a.y)

proc max*[T](a: Vec2[T]): T {.inline.} =
  max(a.x, a.y)

proc min*[T](a, b: Vec2[T]): Vec2[T] {.inline.} =
  Vec2[T](x: min(a.x, b.x),
          y: min(a.y, b.y))

proc max*[T](a, b: Vec2[T]): Vec2[T] {.inline.} =
  Vec2[T](x: max(a.x, b.x),
          y: max(a.y, b.y))

proc abs*[T](a: Vec2[T]): Vec2[T] {.inline.} =
  Vec2[T](x: abs(a.x),
          y: abs(a.y))

proc floor*[T](a: Vec2[T]): Vec2[T] {.inline.} =
  Vec2[T](x: floor(a.x),
          y: floor(a.y))

proc ceil*[T](a: Vec2[T]): Vec2[T] {.inline.} =
  Vec2[T](x: ceil(a.x),
          y: ceil(a.y))

proc clamp*[T](v: Vec2[T], a, b: T): Vec2[T] {.inline.} =
  Vec2[T](x: clamp(v.x, a, b),
          y: clamp(v.y, a, b))

proc lerp*[T](a, b: Vec2[T], t: T): Vec2[T] {.inline.} =
  Vec2[T](x: (1-t) * a.x + t * b.x,
          y: (1-t) * a.y + t * b.y)

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

proc vec3i*(x, y, z: int): Vec3i {.inline.} =
  Vec3i(x: x, y: y, z: z)

proc vec3f*(x, y, z: FloatT): Vec3f {.inline.} =
  result = Vec3f(x: x, y: y, z: z)
  assert(not hasNaNs(result))

proc s*[T](a: Vec3[T]): T {.inline.} = a.x
proc t*[T](a: Vec3[T]): T {.inline.} = a.y
proc u*[T](a: Vec3[T]): T {.inline.} = a.z

proc `s=`*[T](a: var Vec3[T], s: T) {.inline.} = a.x = s
proc `t=`*[T](a: var Vec3[T], t: T) {.inline.} = a.y = t
proc `u=`*[T](a: var Vec3[T], u: T) {.inline.} = a.z = u

proc r*[T](a: Vec3[T]): T {.inline.} = a.x
proc g*[T](a: Vec3[T]): T {.inline.} = a.y
proc b*[T](a: Vec3[T]): T {.inline.} = a.z

proc `r=`*[T](a: var Vec3[T], r: T) {.inline.} = a.x = r
proc `g=`*[T](a: var Vec3[T], g: T) {.inline.} = a.y = g
proc `b=`*[T](a: var Vec3[T], b: T) {.inline.} = a.z = b

proc `[]`*[T](a: Vec3[T], i: int): T {.inline.} =
  assert(i >= 0 and i <= 2)
  if   i == 0: result = a.x
  elif i == 1: result = a.y
  elif i == 2: result = a.z

proc `[]=`*[T](a: var Vec3[T], i: int, v: T) {.inline.} =
  assert(i >= 0 and i <= 2)
  if   i == 0: a.x = v
  elif i == 1: a.y = v
  elif i == 2: a.z = v

proc `+`*[T](a, b: Vec3[T]): Vec3[T] {.inline.} =
  assert(not hasNaNs(b))
  Vec3[T](x: a.x + b.x,
          y: a.y + b.y,
          z: a.z + b.z)

proc `+=`*[T](a: var Vec3[T], b: Vec3[T]) {.inline.} =
  assert(not hasNaNs(b))
  a.x += b.x
  a.y += b.y
  a.z += b.z

proc `-`*[T](a, b: Vec3[T]): Vec3[T] {.inline.} =
  assert(not hasNaNs(b))
  Vec3[T](x: a.x - b.x,
          y: a.y - b.y,
          z: a.z - b.z)

proc `-=`*[T](a: var Vec3[T], b: Vec3[T]) {.inline.} =
  assert(not hasNaNs(b))
  a.x -= b.x
  a.y -= b.y
  a.z -= b.z

proc `*`*[T,U](a: Vec3[T], s: U): Vec3[T] {.inline.} =
  assert(not isNaN(T(s)))
  Vec3[T](x: a.x * T(s),
          y: a.y * T(s),
          z: a.z * T(s))

proc `*`*[T,U](s: U, a: Vec3[T]): Vec3[T] {.inline.} =
  assert(not isNaN(T(s)))
  Vec3[T](x: a.x * T(s),
          y: a.y * T(s),
          z: a.z * T(s))

proc `*=`*[T,U](a: var Vec3[T], s: U) {.inline.} =
  assert(not isNaN(T(s)))
  a.x *= T(s)
  a.y *= T(s)
  a.z *= T(s)

proc `/`*[T,U](a: Vec3[T], d: U): Vec3[T] {.inline.} =
  assert(not isNaN(T(d)))
  assert(d != 0)
  let s = 1 / float64(d)
  Vec3[T](x: a.x * s,
          y: a.y * s,
          z: a.z * s)

proc `/=`*[T,U](a: var Vec3[T], d: U) {.inline.} =
  assert(not isNaN(T(d)))
  assert(d != 0)
  let s = 1 / float64(d)
  a.x *= T(s)
  a.y *= T(s)
  a.z *= T(s)

proc dot*[T](a, b: Vec3[T]): T {.inline.} =
  assert(not hasNaNs(a))
  assert(not hasNaNs(b))
  a.x * b.x + a.y * b.y + a.z * b.z

proc absDot*[T](a, b: Vec3[T]): T {.inline.} =
  abs(dot(a, b))

proc cross*[T](a, b: Vec3[T]): Vec3[T] {.inline.} =
  assert(not hasNaNs(a))
  assert(not hasNaNs(b))
  Vec3[T](x: a.y * b.z - a.z * b.y,
          y: a.z * b.x - a.x * b.z,
          z: a.x * b.y - a.y * b.x)

proc len*[T](a: Vec3[T]): FloatT {.inline.} =
  sqrt(a.x * a.x + a.y * a.y + a.z * a.z)

proc len2*[T](a: Vec3[T]): FloatT {.inline.} =
  let r = len(a)
  r * r

proc norm*[T](a: Vec3[T]): Vec3[T] {.inline.} =
  a / a.len

proc min*[T](a: Vec3[T]): FloatT {.inline.} =
  min(min(a.x, a.y), a.z)

proc max*[T](a: Vec3[T]): FloatT {.inline.} =
  max(max(a.x, a.y), a.z)

proc min*[T](a, b: Vec3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: min(a.x, b.x),
          y: min(a.y, b.y),
          z: min(a.z, b.z))

proc max*[T](a, b: Vec3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: max(a.x, b.x),
          y: max(a.y, b.y),
          z: max(a.z, b.z))

proc abs*[T](a: Vec3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: abs(a.x),
          y: abs(a.y),
          z: abs(a.z))

proc floor*[T](a: Vec3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: floor(a.x),
          y: floor(a.y),
          z: floor(a.z))

proc ceil*[T](a: Vec3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: ceil(a.x),
          y: ceil(a.y),
          z: ceil(a.z))

proc clamp*[T](v: Vec3[T], a, b: T): Vec3[T] {.inline.} =
  Vec3[T](x: clamp(v.x, a, b),
          y: clamp(v.y, a, b),
          z: clamp(v.z, a, b))

proc lerp*[T](a, b: Vec3[T], t: T): Vec3[T] {.inline.} =
  Vec3[T](x: (1-t) * a.x + t * b.x,
          y: (1-t) * a.y + t * b.y,
          z: (1-t) * a.z + t * b.z)

# }}}
# {{{ Bounds2

type Bounds2[T] = object
  pMin*: Vec2[T]
  pMax*: Vec2[T]

type
  Bounds2f = Bounds2[FloatT]
  Bounds2i = Bounds2[int]

proc bounds2f*(): Bounds2f {.inline.} =
  Bounds2f(pMin: vec2f(Inf,    Inf),
           pMax: vec2f(NegInf, NegInf))

proc bounds2f*(a, b: Vec2f): Bounds2f {.inline.} =
  bounds2f(vec2f(min(a.x, b.x), min(a.y, b.y)),
           vec2f(max(a.x, b.x), max(a.y, b.y)))

proc bounds2f*(p: Vec2f): Bounds2f {.inline.} =
  bounds2f(p, p)

proc bounds2i*(): Bounds2i {.inline.} =
  let maxInt = high(int)
  let minInt = low(int)
  Bounds2i(pMin: vec2i(maxInt, maxInt),
           pMax: vec2i(minInt, minInt))

proc bounds2i*(a, b: Vec2i): Bounds2i {.inline.} =
  bounds2i(vec2i(min(a.x, b.x), min(a.y, b.y)),
           vec2i(max(a.x, b.x), max(a.y, b.y)))

proc bounds2i*(p: Vec2i): Bounds2i {.inline.} =
  bounds2i(p, p)

proc union*[T](a: Bounds2[T], p: Vec2[T]): Bounds2[T] {.inline.} =
  Bounds2[T](pMin: vec2f(min(a.pMin.x, p.x),
                         min(a.pMin.y, p.y)),
             pMax: vec2f(max(a.pMax.x, p.x),
                         max(a.pMax.y, p.y)))

proc union*[T](a: Bounds2[T], b: Bounds2[T]): Bounds2[T] {.inline.} =
  Bounds2[T](pMin: vec2f(min(a.pMin.x, b.pMin.x),
                         min(a.pMin.y, b.pMin.y)),
             pMax: vec2f(max(a.pMax.x, b.pMax.x),
                         max(a.pMax.y, b.pMax.y)))

proc intersect*[T](a: Bounds2[T], b: Bounds2[T]): Bounds2[T] {.inline.} =
  Bounds2[T](pMin: vec2f(max(a.pMin.x, b.pMin.x),
                         max(a.pMin.y, b.pMin.y)),
             pMax: vec2f(min(a.pMax.x, b.pMax.x),
                         min(a.pMax.y, b.pMax.y)))

# }}}
# {{{ Bounds3

type Bounds3[T] = object
  pMin*: Vec3[T]
  pMax*: Vec3[T]

type
  Bounds3f = Bounds3[FloatT]
  Bounds3i = Bounds3[int]

proc bounds3f*(): Bounds3f {.inline.} =
  Bounds3f(pMin: vec3f(Inf,    Inf,    Inf),
           pMax: vec3f(NegInf, NegInf, NegInf))

proc bounds3f*(a, b: Vec3f): Bounds3f {.inline.} =
  bounds3f(vec3f(min(a.x, b.x), min(a.y, b.y), min(a.z, b.z)),
           vec3f(max(a.x, b.x), max(a.y, b.y), max(a.z, b.z)))

proc bounds3i*(): Bounds3i {.inline.} =
  let maxInt = high(int)
  let minInt = low(int)
  Bounds3i(pMin: vec3i(maxInt, maxInt, maxInt),
           pMax: vec3i(minInt, minInt, minInt))

proc bounds3i*(a, b: Vec3i): Bounds3i {.inline.} =
  bounds3i(vec3i(min(a.x, b.x), min(a.y, b.y), min(a.z, b.z)),
           vec3i(max(a.x, b.x), max(a.y, b.y), max(a.z, b.z)))

proc union*[T](a: Bounds3[T], p: Vec3[T]): Bounds3[T] {.inline.} =
  Bounds3[T](pMin: vec3f(min(a.pMin.x, p.x),
                         min(a.pMin.y, p.y),
                         min(a.pMin.z, p.z)),
             pMax: vec3f(max(a.pMax.x, p.x),
                         max(a.pMax.y, p.y),
                         max(a.pMax.z, p.z)))

proc union*[T](a: Bounds3[T], b: Bounds3[T]): Bounds3[T] {.inline.} =
  Bounds3[T](pMin: vec3f(min(a.pMin.x, b.pMin.x),
                         min(a.pMin.y, b.pMin.y),
                         min(a.pMin.z, b.pMin.z)),
             pMax: vec3f(max(a.pMax.x, b.pMax.x),
                         max(a.pMax.y, b.pMax.y),
                         max(a.pMax.z, b.pMax.z)))

proc intersect*[T](a: Bounds3[T], b: Bounds3[T]): Bounds3[T] {.inline.} =
  Bounds3[T](pMin: vec3f(max(a.pMin.x, b.pMin.x),
                         max(a.pMin.y, b.pMin.y),
                         max(a.pMin.z, b.pMin.z)),
             pMax: vec3f(min(a.pMax.x, b.pMax.x),
                         min(a.pMax.y, b.pMax.y),
                         min(a.pMax.z, b.pMax.z)))

# }}}
# {{{ Ray

# TODO byref pragma?
type Ray* = object
  o*: Vec3f
  d*: Vec3f
  tMax*: FloatT
  time*: FloatT

proc t*(r: Ray, t: FloatT): Vec3f {.inline.} =
  r.o + r.d*t

# }}}

# {{{ Tests

when isMainModule:
  # {{{ Vec2
  block:
    let
      a = vec2f(1, 2)
      b = vec2f(3, 5)

    try:
      let c = vec2f(1, NaN)
      assert false
    except AssertionError:
      assert true

    assert a.x == 1
    assert a.y == 2

    assert a.s == 1
    assert a.t == 2

    assert a.r == 1
    assert a.g == 2

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
    assert a.absDot(vec2f(-3,-5)) == 13
    assert len(a) == sqrt(FloatT(5))
    assert len2(a) == FloatT(5)
    assert vec2f(10,0).norm == vec2f(1,0)
    assert min(a) == 1
    assert max(a) == 2
    assert min(vec2f(-2,5), vec2f(1,3)) == vec2f(-2,3)
    assert max(vec2f(-2,5), vec2f(1,3)) == vec2f(1,5)
    assert abs(vec2f(-2,3)) == vec2f(2,3)
    assert floor(vec2f(0.2, 1.7)) == vec2f(0, 1)
    assert ceil(vec2f(0.2, 1.7)) == vec2f(1, 2)
    assert clamp(vec2f(-2.5, 0.5), -1, 1) == vec2f(-1, 0.5)
    assert lerp(a, b, 0.25) == vec2f(1.5, 2.75)

  block:
    var a = vec2f(1, 2)
    let b = vec2f(3, 5)

    a[0] = 8
    a[1] = 9
    assert a.x == 8
    assert a.y == 9

    a.s = 5
    a.t = 6
    assert a.x == 5
    assert a.y == 6

    a.r = 3
    a.g = 4
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

  # }}}
  # {{{ Vec3
  block:
    let
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

    assert a.s == 1
    assert a.t == 2
    assert a.u == 3

    assert a.r == 1
    assert a.g == 2
    assert a.b == 3

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
    assert a.absDot(vec3f(-3,-5,-7)) == 34
    # TODO assert a.cross(b) == 0
    assert len(a) == sqrt(FloatT(14))
    assert abs(len2(a) - FloatT(14)) < 0.00001  # TODO comparison helper
    assert vec3f(-10,0,0).norm == vec3f(-1,0,0)
    assert min(a) == 1
    assert max(a) == 3
    assert min(vec3f(-2,5,7), vec3f(1,3,-5)) == vec3f(-2,3,-5)
    assert max(vec3f(-2,5,7), vec3f(1,3,-5)) == vec3f(1,5,7)
    assert abs(vec3f(-2,3,0)) == vec3f(2,3,0)
    assert floor(vec3f(0.2, 1.7, -1.9)) == vec3f(0, 1, -2)
    assert ceil(vec3f(0.2, 1.7, -1.9)) == vec3f(1, 2, -1)
    assert clamp(vec3f(-2.5, 0.5, 1.3), -1, 1) == vec3f(-1, 0.5, 1)
    assert lerp(a, b, 0.25) == vec3f(1.5, 2.75, 4)

  block:
    var a = vec3f(1, 2, 3)
    let b = vec3f(3, 5, 7)

    a[0] = 3
    a[1] = 4
    a[2] = 5
    assert a.x == 3
    assert a.y == 4
    assert a.z == 5

    a.s = 9
    a.t = 10
    a.u = 11
    assert a.x == 9
    assert a.y == 10
    assert a.z == 11

    a.r = 6
    a.g = 7
    a.b = 8
    assert a.r == 6
    assert a.g == 7
    assert a.b == 8

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
  # {{{ Ray
  block:
    let r = Ray(o: vec3f(1, 2, 3), d: vec3f(-1, -0.5, 0))

    assert r.t(3) == vec3f(-2, 0.5, 3)


# }}}

# vim: et:ts=2:sw=2:fdm=marker
