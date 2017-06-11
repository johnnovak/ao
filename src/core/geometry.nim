import core

import math


# WARNING: For efficiency reasons, all geometry types in this file can perform
# mutable operations on themselves and/or expose their encapsulated data for
# modification.

# {{{ Vec2

type Vec2*[T] = object
  x*, y*: T

type
  Vec2f* = Vec2[FloatT]
  Vec2i* = Vec2[int]

proc hasNaNs*[T](a: Vec2[T]): bool {.inline.} =
  isNaN(a.x) or isNaN(a.y)

proc vec2f*(x, y: FloatT): Vec2f {.inline.} =
  result = Vec2f(x: x, y: y)
  assert(not hasNaNs(result))

proc vec2i*(x, y: int): Vec2i {.inline.} =
  Vec2[int](x: x, y: y)

proc `isClose`*[T](a, b: Vec2[T], maxRelDiff = 0.0001): bool {.inline.} =
  a.x.isClose(b.x, maxRelDiff) and a.y.isClose(b.y, maxRelDiff)

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
  Vec2[T](x: T(FloatT(a.x) * s),
          y: T(FloatT(a.y) * s))

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

proc len*[T](a: Vec2[T]): FloatT {.inline.} =
  sqrt(FloatT(a.x * a.x + a.y * a.y))

proc len2*[T](a: Vec2[T]): FloatT {.inline.} =
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

proc lerp*[T](a, b: Vec2[T], t: FloatT): Vec2[T] {.inline.} =
  Vec2[T](x: T(lerp(FloatT(a.x), FloatT(b.x), t)),
          y: T(lerp(FloatT(a.y), FloatT(b.y), t)))

# }}}
# {{{ Vec3

type Vec3*[T] = object
  x*, y*, z*: T

type
  Vec3f* = Vec3[FloatT]
  Vec3i* = Vec3[int]

proc hasNaNs*[T](a: Vec3[T]): bool {.inline.} =
  isNaN(a.x) or isNaN(a.y) or isNaN(a.z)

proc vec3f*(x, y, z: FloatT): Vec3f {.inline.} =
  result = Vec3f(x: x, y: y, z: z)
  assert(not hasNaNs(result))

proc vec3i*(x, y, z: int): Vec3i {.inline.} =
  Vec3i(x: x, y: y, z: z)

proc `isClose`*[T](a, b: Vec3[T], maxRelDiff = 0.0001): bool {.inline.} =
  (a.x.isClose(b.x, maxRelDiff) and
   a.y.isClose(b.y, maxRelDiff) and
   a.z.isClose(b.z, maxRelDiff))

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
  Vec3[T](x: T(FloatT(a.x) * s),
          y: T(FloatT(a.y) * s),
          z: T(FloatT(a.z) * s))

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
  sqrt(FloatT(a.x * a.x + a.y * a.y + a.z * a.z))

proc len2*[T](a: Vec3[T]): FloatT {.inline.} =
  let r = len(a)
  r * r

proc norm*[T](a: Vec3[T]): Vec3[T] {.inline.} =
  a / a.len

proc min*[T](a: Vec3[T]): T {.inline.} =
  min(min(a.x, a.y), a.z)

proc max*[T](a: Vec3[T]): T {.inline.} =
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

proc lerp*[T](a, b: Vec3[T], t: FloatT): Vec3[T] {.inline.} =
  Vec3[T](x: T(lerp(FloatT(a.x), FloatT(b.x), t)),
          y: T(lerp(FloatT(a.y), FloatT(b.y), t)),
          z: T(lerp(FloatT(a.z), FloatT(b.z), t)))

# }}}
# {{{ Box2

type Box2[T] = object
  pMin*: Vec2[T]
  pMax*: Vec2[T]

type
  Box2f = Box2[FloatT]
  Box2i = Box2[int]

proc box2f*(): Box2f {.inline.} =
  Box2f(pMin: vec2f(Inf,    Inf),
        pMax: vec2f(NegInf, NegInf))

proc box2f*(a, b: Vec2f): Box2f {.inline.} =
  Box2f(pMin: vec2f(min(a.x, b.x), min(a.y, b.y)),
        pMax: vec2f(max(a.x, b.x), max(a.y, b.y)))

proc box2f*(p: Vec2f): Box2f {.inline.} =
  box2f(p, p)

proc box2i*(): Box2i {.inline.} =
  let maxInt = high(int)
  let minInt = low(int)
  Box2i(pMin: vec2i(maxInt, maxInt),
        pMax: vec2i(minInt, minInt))

proc box2i*(a, b: Vec2i): Box2i {.inline.} =
  Box2i(pMin: vec2i(min(a.x, b.x), min(a.y, b.y)),
        pMax: vec2i(max(a.x, b.x), max(a.y, b.y)))

proc box2i*(p: Vec2i): Box2i {.inline.} =
  box2i(p, p)

proc `isClose`*[T](a, b: Box2[T], maxRelDiff = 0.0001): bool {.inline.} =
  a.pMin.isClose(b.pMin, maxRelDiff) and a.pMax.isClose(b.pMax, maxRelDiff)

proc `[]`*[T](b: Box2[T], i: int): Vec2[T] {.inline.} =
  assert i == 0 or i == 1
  if i == 0: b.pMin else: b.pMax

proc corner*[T](b: Box2[T], i: int): Vec2[T] {.inline.} =
  assert i >= 0 or i <= 3
  Vec2[T](x: b[ i        and 1].x,
          y: b[(i shr 1) and 1].y)

proc union*[T](b: Box2[T], p: Vec2[T]): Box2[T] {.inline.} =
  Box2[T](pMin: Vec2[T](x: min(b.pMin.x, p.x),
                        y: min(b.pMin.y, p.y)),
          pMax: Vec2[T](x: max(b.pMax.x, p.x),
                        y: max(b.pMax.y, p.y)))

proc union*[T](a: Box2[T], b: Box2[T]): Box2[T] {.inline.} =
  Box2[T](pMin: Vec2[T](x: min(a.pMin.x, b.pMin.x),
                        y: min(a.pMin.y, b.pMin.y)),
          pMax: Vec2[T](x: max(a.pMax.x, b.pMax.x),
                        y: max(a.pMax.y, b.pMax.y)))

proc intersect*[T](a: Box2[T], b: Box2[T]): Box2[T] {.inline.} =
  Box2[T](pMin: Vec2[T](x: max(a.pMin.x, b.pMin.x),
                        y: max(a.pMin.y, b.pMin.y)),
          pMax: Vec2[T](x: min(a.pMax.x, b.pMax.x),
                        y: min(a.pMax.y, b.pMax.y)))

proc overlaps*[T](a: Box2[T], b: Box2[T]): bool {.inline.} =
  (a.pMax.x >= b.pMin.x and a.pMin.x <= b.pMax.x and
   a.pMax.y >= b.pMin.y and a.pMin.y <= b.pMax.y)

proc inside*[T](p: Vec2[T], b: Box2[T]): bool {.inline.} =
  (p.x >= b.pMin.x and p.x <= b.pMax.x and
   p.y >= b.pMin.y and p.y <= b.pMax.y)

proc insideExclusive*[T](p: Vec2[T], b: Box2[T]): bool {.inline.} =
  (p.x >= b.pMin.x and p.x < b.pMax.x and
   p.y >= b.pMin.y and p.y < b.pMax.y)

proc center*[T](b: Box2[T]): Vec2[T] {.inline.} =
  Vec2[T](x: T((b.pMin.x + b.pMax.x) / 2),
          y: T((b.pMin.y + b.pMax.y) / 2))

proc diagonal*[T](b: Box2[T]): Vec2[T] {.inline.} =
  b.pMax - b.pMin

proc maxExtent*[T](b: Box2[T]): int {.inline.} =
  let d = b.diagonal
  if d.x > d.y: result = 0
  else: result = 1

proc area*[T](b: Box2[T]): T {.inline.} =
  let d = b.diagonal
  d.x * d.y

proc expand*[T,U](b: Box2[T], d: U): Box2[T] {.inline.} =
  let delta = Vec2[T](x: T(d), y: T(d))
  Box2[T](pMin: b.pMin - delta,
          pMax: b.pMax + delta)

proc lerp*[T](b: Box2[T], t: FloatT): Vec2[T] {.inline.} =
  lerp(b.pMin, b.pMax, t)

proc offset*[T](b: Box2[T], p: Vec2[T]): Vec2[T] {.inline.} =
  var o = p - b.pMin
  if b.pMax.x > b.pMin.x: o.x /= b.pMax.x - b.pMin.x
  if b.pMax.y > b.pMin.y: o.y /= b.pMax.y - b.pMin.y
  result = o

# }}}
# {{{ Box3

type Box3*[T] = object
  pMin*: Vec3[T]
  pMax*: Vec3[T]

type
  Box3f = Box3[FloatT]
  Box3i = Box3[int]

proc box3f*(): Box3f {.inline.} =
  Box3f(pMin: vec3f(Inf,    Inf,    Inf),
        pMax: vec3f(NegInf, NegInf, NegInf))

proc box3f*(a, b: Vec3f): Box3f {.inline.} =
  Box3f(pMin: vec3f(min(a.x, b.x), min(a.y, b.y), min(a.z, b.z)),
        pMax: vec3f(max(a.x, b.x), max(a.y, b.y), max(a.z, b.z)))

proc box3f*(p: Vec3f): Box3f {.inline.} =
  box3f(p, p)

proc box3i*(): Box3i {.inline.} =
  let maxInt = high(int)
  let minInt = low(int)
  Box3i(pMin: vec3i(maxInt, maxInt, maxInt),
        pMax: vec3i(minInt, minInt, minInt))

proc box3i*(a, b: Vec3i): Box3i {.inline.} =
  Box3i(pMin: vec3i(min(a.x, b.x), min(a.y, b.y), min(a.z, b.z)),
        pMax: vec3i(max(a.x, b.x), max(a.y, b.y), max(a.z, b.z)))

proc box3i*(p: Vec3i): Box3i {.inline.} =
  box3i(p, p)

proc `isClose`*[T](a, b: Box3[T], maxRelDiff = 0.0001): bool {.inline.} =
  a.pMin.isClose(b.pMin, maxRelDiff) and a.pMax.isClose(b.pMax, maxRelDiff)

proc `[]`*[T](b: Box3[T], i: int): Vec3[T] {.inline.} =
  assert i == 0 or i == 1
  if i == 0: b.pMin else: b.pMax

proc corner*[T](b: Box3[T], i: int): Vec3[T] {.inline.} =
  assert i >= 0 or i <= 8
  Vec3[T](x: b[ i        and 1].x,
          y: b[(i shr 1) and 1].y,
          z: b[(i shr 2) and 1].z)

proc union*[T](b: Box3[T], p: Vec3[T]): Box3[T] {.inline.} =
  Box3[T](pMin: Vec3[T](x: min(b.pMin.x, p.x),
                        y: min(b.pMin.y, p.y),
                        z: min(b.pMin.z, p.z)),
          pMax: Vec3[T](x: max(b.pMax.x, p.x),
                        y: max(b.pMax.y, p.y),
                        z: max(b.pMax.z, p.z)))

proc union*[T](a: Box3[T], b: Box3[T]): Box3[T] {.inline.} =
  Box3[T](pMin: Vec3[T](x: min(a.pMin.x, b.pMin.x),
                        y: min(a.pMin.y, b.pMin.y),
                        z: min(a.pMin.z, b.pMin.z)),
          pMax: Vec3[T](x: max(a.pMax.x, b.pMax.x),
                        y: max(a.pMax.y, b.pMax.y),
                        z: max(a.pMax.z, b.pMax.z)))

proc intersect*[T](a: Box3[T], b: Box3[T]): Box3[T] {.inline.} =
  Box3[T](pMin: Vec3[T](x: max(a.pMin.x, b.pMin.x),
                        y: max(a.pMin.y, b.pMin.y),
                        z: max(a.pMin.z, b.pMin.z)),
          pMax: Vec3[T](x: min(a.pMax.x, b.pMax.x),
                        y: min(a.pMax.y, b.pMax.y),
                        z: min(a.pMax.z, b.pMax.z)))

proc overlaps*[T](a: Box3[T], b: Box3[T]): bool {.inline.} =
  (a.pMax.x >= b.pMin.x and a.pMin.x <= b.pMax.x and
   a.pMax.y >= b.pMin.y and a.pMin.y <= b.pMax.z and
   a.pMax.z >= b.pMin.z and a.pMin.z <= b.pMax.z)

proc inside*[T](p: Vec3[T], b: Box3[T]): bool {.inline.} =
  (p.x >= b.pMin.x and p.x <= b.pMax.x and
   p.y >= b.pMin.y and p.y <= b.pMax.y and
   p.z >= b.pMin.z and p.z <= b.pMax.z)

proc insideExclusive*[T](p: Vec3[T], b: Box3[T]): bool {.inline.} =
  (p.x >= b.pMin.x and p.x < b.pMax.x and
   p.y >= b.pMin.y and p.y < b.pMax.y and
   p.z >= b.pMin.z and p.z < b.pMax.z)

proc center*[T](b: Box3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: T((b.pMin.x + b.pMax.x) / 2),
          y: T((b.pMin.y + b.pMax.y) / 2),
          z: T((b.pMin.z + b.pMax.z) / 2))

proc diagonal*[T](b: Box3[T]): Vec3[T] {.inline.} =
  b.pMax - b.pMin

proc maxExtent*[T](b: Box3[T]): int {.inline.} =
  let d = b.diagonal
  if d.x > d.y and d.x > d.z: result = 0
  elif d.y > d.z: result = 1
  else: result = 2

proc area*[T](b: Box3[T]): T {.inline.} =
  let d = b.diagonal
  2 * (d.x * d.y + d.y * d.z + d.x * d.z)

proc volume*[T](b: Box3[T]): T {.inline.} =
  let d = b.diagonal
  d.x * d.y * d.z

proc expand*[T,U](b: Box3[T], d: U): Box3[T] {.inline.} =
  let delta = Vec3[T](x: T(d), y: T(d), z: T(d))
  Box3[T](pMin: b.pMin - delta,
          pMax: b.pMax + delta)

proc lerp*[T](b: Box3[T], t: FloatT): Vec3[T] {.inline.} =
  lerp(b.pMin, b.pMax, t)

proc offset*[T](b: Box3[T], p: Vec3[T]): Vec3[T] {.inline.} =
  var o = p - b.pMin
  if b.pMax.x > b.pMin.x: o.x /= b.pMax.x - b.pMin.x
  if b.pMax.y > b.pMin.y: o.y /= b.pMax.y - b.pMin.y
  if b.pMax.z > b.pMin.z: o.z /= b.pMax.z - b.pMin.z
  result = o

# }}}
# {{{ Ray

type Ray* = object
  o*: Vec3f
  d*: Vec3f
  tMax*: FloatT
  time*: FloatT

proc hasNaNs*(r: Ray): bool {.inline.} =
  hasNaNs(r.o) or hasNaNs(r.d) or isNaN(r.tMax)

proc t*(r: Ray, t: FloatT): Vec3f {.inline.} =
  r.o + r.d*t

# }}}

# {{{ Tests

when isMainModule:
  # {{{ Vec2f
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
    assert len2(a).isClose(FloatT(5))
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

    a.x = 5
    a.y = 6
    assert a.x == 5
    assert a.y == 6

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
  # {{{ Vec2i
  block:
    let
      a = vec2i(1, 2)
      b = vec2i(3, 5)

    assert a.x == 1
    assert a.y == 2

    assert a[0] == 1
    assert a[1] == 2

    assert a + b == vec2i(4, 7)
    assert a - b == vec2i(-2, -3)
    assert a * 2 == vec2i(2, 4)
    assert 2 * a == vec2i(2, 4)
    assert a / 2 == vec2i(0, 1)

    try:
      discard a / 0
      assert false
    except AssertionError:
      assert true

    assert a.dot(b) == 13
    assert a.absDot(vec2i(-3,-5)) == 13
    assert len(a) == sqrt(FloatT(5))
    assert len2(a).isClose(FloatT(5))
    assert vec2i(10,0).norm == vec2i(1,0)
    assert min(a) == 1
    assert max(a) == 2
    assert min(vec2i(-2,5), vec2i(1,3)) == vec2i(-2,3)
    assert max(vec2i(-2,5), vec2i(1,3)) == vec2i(1,5)
    assert abs(vec2i(-2,3)) == vec2i(2,3)
    assert clamp(vec2i(-2, 2), -1, 3) == vec2i(-1, 2)
    assert lerp(a, b, 0.25) == vec2i(1, 2)

  block:
    var a = vec2i(1, 2)
    let b = vec2i(3, 5)

    a[0] = 8
    a[1] = 9
    assert a.x == 8
    assert a.y == 9

    a.x = 5
    a.y = 6
    assert a.x == 5
    assert a.y == 6

    a = vec2i(1, 2)
    a += b
    assert a == vec2i(4, 7)

    a = vec2i(1, 2)
    a -= b
    assert a == vec2i(-2, -3)

    a = vec2i(1, 2)
    a *= 2
    assert a == vec2i(2, 4)

  # }}}
  # {{{ Vec3f
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
    assert a.cross(b) == vec3f(-1,2,-1)
    assert len(a) == sqrt(FloatT(14))
    assert len2(a).isClose(FloatT(14))
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

    a.x = 5
    a.y = 6
    a.z = 7
    assert a.x == 5
    assert a.y == 6
    assert a.z == 7

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
  # {{{ Vec3i
  block:
    let
      a = vec3i(1, 2, 3)
      b = vec3i(3, 5, 7)

    assert a.x == 1
    assert a.y == 2
    assert a.z == 3

    assert a + b == vec3i(4, 7, 10)
    assert a - b == vec3i(-2, -3, -4)
    assert a * 2 == vec3i(2, 4, 6)
    assert 2 * a == vec3i(2, 4, 6)
    assert a / 2 == vec3i(0, 1, 1)

    try:
      discard a / 0
      assert false
    except AssertionError:
      assert true

    assert a.dot(b) == 34
    assert a.absDot(vec3i(-3,-5,-7)) == 34
    assert a.cross(b) == vec3i(-1,2,-1)
    assert len(a) == sqrt(FloatT(14))
    assert len2(a).isClose(FloatT(14))
    assert vec3i(-10,0,0).norm == vec3i(-1,0,0)
    assert min(a) == 1
    assert max(a) == 3
    assert min(vec3i(-2,5,7), vec3i(1,3,-5)) == vec3i(-2,3,-5)
    assert max(vec3i(-2,5,7), vec3i(1,3,-5)) == vec3i(1,5,7)
    assert abs(vec3i(-2,3,0)) == vec3i(2,3,0)
    assert clamp(vec3i(-2, 0, 1), -1, 1) == vec3i(-1, 0, 1)
    assert lerp(a, b, 0.25) == vec3i(1, 2, 4)

  block:
    var a = vec3i(1, 2, 3)
    let b = vec3i(3, 5, 7)

    a[0] = 3
    a[1] = 4
    a[2] = 5
    assert a.x == 3
    assert a.y == 4
    assert a.z == 5

    a.x = 5
    a.y = 6
    a.z = 7
    assert a.x == 5
    assert a.y == 6
    assert a.z == 7

    a = vec3i(1, 2, 3)
    a += b
    assert a == vec3i(4, 7, 10)

    a = vec3i(1, 2, 3)
    a -= b
    assert a == vec3i(-2, -3, -4)

    a = vec3i(1, 2, 3)
    a *= 2
    assert a == vec3i(2, 4, 6)

  # }}}
  # {{{ Box2f
  block:
    let b1 = box2f(vec2f(3,1), vec2f(-2,5))
    assert b1.pMin == vec2f(-2,1)
    assert b1[0] == b1.pMin
    assert b1.pMax == vec2f(3,5)
    assert b1[1] == b1.pMax
    assert b1.corner(0) == vec2f(-2,1)
    assert b1.corner(1) == vec2f(3,1)
    assert b1.corner(2) == vec2f(-2,5)
    assert b1.corner(3) == vec2f(3,5)

    let b2 = box2f(vec2f(1,2))
    assert b2.pMin == vec2f(1,2)
    assert b2.pMax == vec2f(1,2)

    assert box2f().union(vec2f(1,2)) == box2f(vec2f(1,2), vec2f(1,2))
    assert b2.union(vec2f(2,3)) == box2f(vec2f(1,2), vec2f(2,3))
    assert b1.union(box2f(vec2f(0,6),
                          vec2f(8,-7))) == box2f(vec2f(-2,-7), vec2f(8,6))

    let b3 = box2f(vec2f(0,4), vec2f(2,6))
    assert b1.intersect(b3) == box2f(vec2f(0,4), vec2f(2,5))

    assert b1.overlaps(box2f(vec2f(2,3), vec2f(4,7))) == true
    assert b1.overlaps(box2f(vec2f(-3,-1), vec2f(-1,1))) == true
    assert b1.overlaps(box2f(vec2f(-3,-1), vec2f(-1,0))) == false
    assert b1.overlaps(box2f(vec2f(4,-1), vec2f(5,0))) == false
    assert b1.overlaps(box2f(vec2f(2,-1), vec2f(5,0))) == false

    assert vec2f(0,1).inside(b1) == true
    assert vec2f(0.9,4.9).insideExclusive(b1) == true
    assert vec2f(1,5).insideExclusive(b1) == false

    assert b1.center == vec2f(0.5, 3)
    assert b1.diagonal == vec2f(5,4)
    assert b1.maxExtent == 0
    assert b1.area == 20
    assert b1.expand(1) == box2f(vec2f(-3,0), vec2f(4,6))
    assert lerp(b1, 0.25) == vec2f(-0.75, 2)
    assert b1.offset(vec2f(-0.75, 4)) == vec2f(0.25, 0.75)

  # }}}
  # {{{ Box2i
  block:
    let b1 = box2i(vec2i(3,1), vec2i(-2,5))
    assert b1.pMin == vec2i(-2,1)
    assert b1[0] == b1.pMin
    assert b1.pMax == vec2i(3,5)
    assert b1[1] == b1.pMax
    assert b1.corner(0) == vec2i(-2,1)
    assert b1.corner(1) == vec2i(3,1)
    assert b1.corner(2) == vec2i(-2,5)
    assert b1.corner(3) == vec2i(3,5)

    let b2 = box2i(vec2i(1,2))
    assert b2.pMin == vec2i(1,2)
    assert b2.pMax == vec2i(1,2)

    assert box2i().union(vec2i(1,2)) == box2i(vec2i(1,2), vec2i(1,2))
    assert b2.union(vec2i(2,3)) == box2i(vec2i(1,2), vec2i(2,3))
    assert b1.union(box2i(vec2i(0,6),
                          vec2i(8,-7))) == box2i(vec2i(-2,-7), vec2i(8,6))

    let b3 = box2i(vec2i(0,4), vec2i(2,6))
    assert b1.intersect(b3) == box2i(vec2i(0,4), vec2i(2,5))

    assert b1.overlaps(box2i(vec2i(2,3), vec2i(4,7))) == true
    assert b1.overlaps(box2i(vec2i(-3,-1), vec2i(-1,1))) == true
    assert b1.overlaps(box2i(vec2i(-3,-1), vec2i(-1,0))) == false
    assert b1.overlaps(box2i(vec2i(4,-1), vec2i(5,0))) == false
    assert b1.overlaps(box2i(vec2i(2,-1), vec2i(5,0))) == false

    assert vec2i(0,1).inside(b1) == true
    assert vec2i(0,4).insideExclusive(b1) == true
    assert vec2i(1,5).insideExclusive(b1) == false

    assert b1.center == vec2i(0, 3)
    assert b1.diagonal == vec2i(5,4)
    assert b1.maxExtent == 0
    assert b1.area == 20
    assert b1.expand(1) == box2i(vec2i(-3,0), vec2i(4,6))
    assert lerp(b1, 0.25) == vec2i(-0, 2)

  # }}}
  # {{{ Box3f
  block:
    let b1 = box3f(vec3f(3,1,4), vec3f(-2,5,-1))
    assert b1.pMin == vec3f(-2,1,-1)
    assert b1[0] == b1.pMin
    assert b1.pMax == vec3f(3,5,4)
    assert b1[1] == b1.pMax
    assert b1.corner(0) == vec3f(-2, 1, -1)
    assert b1.corner(1) == vec3f( 3, 1, -1)
    assert b1.corner(2) == vec3f(-2, 5, -1)
    assert b1.corner(3) == vec3f( 3, 5, -1)
    assert b1.corner(4) == vec3f(-2, 1,  4)
    assert b1.corner(5) == vec3f( 3, 1,  4)
    assert b1.corner(6) == vec3f(-2, 5,  4)
    assert b1.corner(7) == vec3f( 3, 5,  4)

    let b2 = box3f(vec3f(1,2,3))
    assert b2.pMin == vec3f(1,2,3)
    assert b2.pMax == vec3f(1,2,3)

    assert box3f().union(vec3f(1,2,3)) == box3f(vec3f(1,2,3), vec3f(1,2,3))
    assert b2.union(vec3f(2,3,4)) == box3f(vec3f(1,2,3), vec3f(2,3,4))
    assert b1.union(box3f(vec3f(0,6,-3),
                          vec3f(8,-7,-2))) == box3f(vec3f(-2,-7,-3),
                                                    vec3f(8,6,4))

    let b3 = box3f(vec3f(0,4,3), vec3f(2,6,7))
    assert b1.intersect(b3) == box3f(vec3f(0,4,3), vec3f(2,5,4))

    assert b1.overlaps(box3f(vec3f(2,3,0), vec3f(4,7,5))) == true
    assert b1.overlaps(box3f(vec3f(-3,-1,4), vec3f(-1,1,3))) == true
    assert b1.overlaps(box3f(vec3f(-3,-1,-3), vec3f(-1,0,-1))) == false
    assert b1.overlaps(box3f(vec3f(4,-1,1), vec3f(5,0,2))) == false
    assert b1.overlaps(box3f(vec3f(2,-1,5), vec3f(5,0,6))) == false

    assert vec3f(0,1,3).inside(b1) == true
    assert vec3f(1, 4.9, 3.9).insideExclusive(b1) == true
    assert vec3f(1,4,4).insideExclusive(b1) == false

    assert b1.center == vec3f(0.5, 3, 1.5)
    assert b1.diagonal == vec3f(5,4,5)
    assert b1.maxExtent == 2
    assert b1.area == 130
    assert b1.volume == 100
    assert b1.expand(1) == box3f(vec3f(-3,0,-2), vec3f(4,6,5))
    assert lerp(b1, 0.25) == vec3f(-0.75, 2, 0.25)
    assert b1.offset(vec3f(-0.75, 4, 1.5)) == vec3f(0.25, 0.75, 0.5)

  # }}}
  # {{{ Box3i
  block:
    let b1 = box3i(vec3i(3,1,4), vec3i(-2,5,-1))
    assert b1.pMin == vec3i(-2,1,-1)
    assert b1[0] == b1.pMin
    assert b1.pMax == vec3i(3,5,4)
    assert b1[1] == b1.pMax
    assert b1.corner(0) == vec3i(-2, 1, -1)
    assert b1.corner(1) == vec3i( 3, 1, -1)
    assert b1.corner(2) == vec3i(-2, 5, -1)
    assert b1.corner(3) == vec3i( 3, 5, -1)
    assert b1.corner(4) == vec3i(-2, 1,  4)
    assert b1.corner(5) == vec3i( 3, 1,  4)
    assert b1.corner(6) == vec3i(-2, 5,  4)
    assert b1.corner(7) == vec3i( 3, 5,  4)

    let b2 = box3i(vec3i(1,2,3))
    assert b2.pMin == vec3i(1,2,3)
    assert b2.pMax == vec3i(1,2,3)

    assert box3i().union(vec3i(1,2,3)) == box3i(vec3i(1,2,3), vec3i(1,2,3))
    assert b2.union(vec3i(2,3,4)) == box3i(vec3i(1,2,3), vec3i(2,3,4))
    assert b1.union(box3i(vec3i(0,6,-3),
                          vec3i(8,-7,-2))) == box3i(vec3i(-2,-7,-3),
                                                    vec3i(8,6,4))

    let b3 = box3i(vec3i(0,4,3), vec3i(2,6,7))
    assert b1.intersect(b3) == box3i(vec3i(0,4,3), vec3i(2,5,4))

    assert b1.overlaps(box3i(vec3i(2,3,0), vec3i(4,7,5))) == true
    assert b1.overlaps(box3i(vec3i(-3,-1,4), vec3i(-1,1,3))) == true
    assert b1.overlaps(box3i(vec3i(-3,-1,-3), vec3i(-1,0,-1))) == false
    assert b1.overlaps(box3i(vec3i(4,-1,1), vec3i(5,0,2))) == false
    assert b1.overlaps(box3i(vec3i(2,-1,5), vec3i(5,0,6))) == false

    assert vec3i(0,1,3).inside(b1) == true
    assert vec3i(1, 4, 3).insideExclusive(b1) == true
    assert vec3i(1,4,4).insideExclusive(b1) == false

    assert b1.center == vec3i(0, 3, 1)
    assert b1.diagonal == vec3i(5,4,5)
    assert b1.maxExtent == 2
    assert b1.area == 130
    assert b1.volume == 100
    assert b1.expand(1) == box3i(vec3i(-3,0,-2), vec3i(4,6,5))
    assert lerp(b1, 0.25) == vec3i(-0, 2, 0)

  # }}}
  # {{{ Ray
  block:
    let r = Ray(o: vec3f(1, 2, 3), d: vec3f(-1, -0.5, 0))

    assert r.t(3) == vec3f(-2, 0.5, 3)

  # }}}

# }}}

# vim: et:ts=2:sw=2:fdm=marker
