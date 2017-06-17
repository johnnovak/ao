import common, types
import math

export types.Vec2f, types.Vec2i, types.Vec3f, types.Vec3i
export types.Box2f, types.Box2i, types.Box3f, types.Box3i
export types.Ray
export types.RayDifferential

# {{{ Vec2

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

proc `-`*[T](a: Vec2[T]): Vec2[T] {.inline.} =
  Vec2[T](x: -a.x, y: -a.y)

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

proc distance*[T](a, b: Vec2[T]): FloatT {.inline.} =
  (a - b).len

proc distance2*[T](a, b: Vec2[T]): FloatT {.inline.} =
  (a - b).len2

# }}}
# {{{ Vec3

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

proc `-`*[T](a: Vec3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: -a.x, y: -a.y, z: -a.z)

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

proc maxDimension*[T](a: Vec3[T]): int {.inline.} =
  if a.x > a.y and a.x > a.z: result = 0
  elif a.y > a.z: result = 1
  else: result = 2

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

proc permute*[T](a: Vec3[T], x, y, z: int): Vec3[T] {.inline.} =
  assert x >= 0 and x <= 2
  assert y >= 0 and y <= 2
  assert z >= 0 and z <= 2
  Vec3[T](x: a[x], y: a[y], z: a[z])

proc distance*[T](a, b: Vec3[T]): FloatT {.inline.} =
  (a - b).len

proc distance2*[T](a, b: Vec3[T]): FloatT {.inline.} =
  (a - b).len2

proc faceforward*[T](a, b: Vec3[T]): Vec3[T] {.inline.} =
  if a.dot(b) < 0: -a
  else: a

proc coordinateSystem*[T](v1: Vec3[T]): (Vec3[T], Vec3[T]) {.inline.} =
  var v2: Vec3[T]
  if abs(v1.x) > abs(v1.y):
    v2 = vec3f(0, v1.z, -v1.y) / sqrt(v1.y * v1.y + v1.z * v1.z)
  else:
    v2 = vec3f(v1.z, 0, -v1.x) / sqrt(v1.x * v1.x + v1.z * v1.z)
  let v3 = v1.cross(v2)
  (v2, v3)

# }}}
# {{{ Ray

proc init(r: var Ray, o, d: Vec3f, tMax, time: FloatT,
          medium: ref Medium) {.inline.} =
  r.o = o
  r.d = d
  r.dInv = vec3f(1/d.x, 1/d.y, 1/d.z)
  r.tMax = tMax
  r.time = time
  r.medium = medium

proc initRay*(o, d: Vec3f, tMax, time: FloatT,
              medium: ref Medium): Ray {.inline.} =
  init(result, o, d, tMax, time, medium)

method hasNaNs*(r: Ray): bool {.base, inline.} =
  hasNaNs(r.o) or hasNaNs(r.d) or isNaN(r.tMax)

proc t*(r: Ray, t: FloatT): Vec3f {.inline.} =
  r.o + r.d*t

# }}}
# {{{ RayDifferential

proc initRayDifferential*(o, d: Vec3f, tMax, time: FloatT, medium: ref Medium,
                          hasDifferentials: bool,
                          rxOrigin, ryOrigin: Vec3f,
                          rxDirection, ryDirection: Vec3f): RayDifferential =
  init(result.Ray, o, d, tMax, time, medium)
  result.hasDifferentials = hasDifferentials
  result.rxOrigin = rxOrigin
  result.ryOrigin = ryOrigin
  result.rxDirection = rxDirection
  result.ryDirection = ryDirection

method hasNaNs*(r: RayDifferential): bool {.inline.} =
  (procCall hasNaNs(Ray(r))) or
    r.hasDifferentials and (hasNaNs(r.rxOrigin) or hasNaNs(r.ryOrigin) or
                            hasNaNs(r.rxDirection) or hasNaNs(r.ryDirection))

proc scaleDifferentials*(r: var RayDifferential, s: FloatT) {.inline.} =
  r.rxOrigin = r.o + (r.rxOrigin - r.o) * s
  r.ryOrigin = r.o + (r.ryOrigin - r.o) * s
  r.rxDirection = r.d + (r.rxDirection - r.d) * s
  r.ryDirection = r.d + (r.ryDirection - r.d) * s

# }}}
# {{{ Box2

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

# From https://tavianator.com/fast-branchless-raybounding-box-intersections-part-2-nans/
# Generates false positives when the ray origin lies exactly on the bounding
# box slabs, but overall this is still worth it because it's faster than the
# 100% correct version.
proc intersect*[T](b: Box3[T], r: Ray): (bool, FloatT, FloatT) {.inline.} =
  var
    t1 = (FloatT(b.pMin.x) - r.o.x) * r.dInv.x
    t2 = (FloatT(b.pMax.x) - r.o.x) * r.dInv.x
    tmin = min(t1, t2)
    tmax = max(t1, t2)

  t1 = (FloatT(b.pMin.y) - r.o.y) * r.dInv.y
  t2 = (FloatT(b.pMax.y) - r.o.y) * r.dInv.y
  tmin = max(tmin, min(t1, t2))
  tmax = min(tmax, max(t1, t2))

  t1 = (FloatT(b.pMin.z) - r.o.z) * r.dInv.z
  t2 = (FloatT(b.pMax.z) - r.o.z) * r.dInv.z
  tmin = max(tmin, min(t1, t2))
  tmax = min(tmax, max(t1, t2))

  result = (tmin <= r.tMax and tmax > max(tmin, 0), tmin, tmax)

# }}}

# vim: et:ts=2:sw=2:fdm=marker
