import core
import geometry

import math


# {{{ Mat4x4

type Mat4x4* = object
  m*: array[4, array[4, FloatT]]

proc mat4x4*(m: array[4, array[4, FloatT]]): Mat4x4 {.inline.} =
  Mat4x4(m: m)

proc mat4x4*(): Mat4x4 {.inline.} =
  mat4x4([[FloatT(1), 0, 0, 0],
          [FloatT(0), 1, 0, 0],
          [FloatT(0), 0, 1, 0],
          [FloatT(0), 0, 0, 1]])

proc `[]`*(m: Mat4x4, r, c: int): FloatT {.inline.} =
  m.m[r][c]

proc `[]=`*(m: var Mat4x4, r, c: int, v: FloatT) {.inline.} =
  m.m[r][c] = v

proc `$`*(m: Mat4x4): string =
  result = sprintf("[%20.12f, %20.12f, %20.12f, %20.12f]\n" &
                   "[%20.12f, %20.12f, %20.12f, %20.12f]\n" &
                   "[%20.12f, %20.12f, %20.12f, %20.12f]\n" &
                   "[%20.12f, %20.12f, %20.12f, %20.12f]",
                   m[0,0], m[0,1], m[0,2], m[0,3],
                   m[1,0], m[1,1], m[1,2], m[1,3],
                   m[2,0], m[2,1], m[2,2], m[2,3],
                   m[3,0], m[3,1], m[3,2], m[3,3])

proc `isClose`*(a, b: Mat4x4, maxRelDiff: FloatT = 1e-5): bool =
  for i in 0..3:
    for j in 0..3:
      if not (a[i,j].isClose(b[i,j], maxRelDiff)):
        return false
  result = true

# 64 mul, 48 add (112 ops)
proc `*`(a, b: Mat4x4): Mat4x4 =
  for i in 0..3:
    for j in 0..3:
      result[i,j] = a[i,0] * b[0,j] +
                    a[i,1] * b[1,j] +
                    a[i,2] * b[2,j] +
                    a[i,3] * b[3,j]

proc transpose(m: Mat4x4): Mat4x4 =
  mat4x4([[m[0,0], m[1,0], m[2,0], m[3,0]],
          [m[0,1], m[1,1], m[2,1], m[3,1]],
          [m[0,2], m[1,2], m[2,2], m[3,2]],
          [m[0,3], m[1,3], m[2,3], m[3,3]]])

# 1 div, 94 mul, 49 add/sub, 8 neg (152 ops)
proc inverse(m: Mat4x4): Mat4x4 =
  let
    s0 = m[0,0] * m[1,1] - m[1,0] * m[0,1]
    s1 = m[0,0] * m[1,2] - m[1,0] * m[0,2]
    s2 = m[0,0] * m[1,3] - m[1,0] * m[0,3]
    s3 = m[0,1] * m[1,2] - m[1,1] * m[0,2]
    s4 = m[0,1] * m[1,3] - m[1,1] * m[0,3]
    s5 = m[0,2] * m[1,3] - m[1,2] * m[0,3]

    c5 = m[2,2] * m[3,3] - m[3,2] * m[2,3]
    c4 = m[2,1] * m[3,3] - m[3,1] * m[2,3]
    c3 = m[2,1] * m[3,2] - m[3,1] * m[2,2]
    c2 = m[2,0] * m[3,3] - m[3,0] * m[2,3]
    c1 = m[2,0] * m[3,2] - m[3,0] * m[2,2]
    c0 = m[2,0] * m[3,1] - m[3,0] * m[2,1]

    invDet = 1.0 / (s0 * c5 - s1 * c4 + s2 * c3 +
                    s3 * c2 - s4 * c1 + s5 * c0)

  assert invDet != 0.0 

  result[0,0] = ( m[1,1] * c5 - m[1,2] * c4 + m[1,3] * c3) * invDet
  result[0,1] = (-m[0,1] * c5 + m[0,2] * c4 - m[0,3] * c3) * invDet
  result[0,2] = ( m[3,1] * s5 - m[3,2] * s4 + m[3,3] * s3) * invDet
  result[0,3] = (-m[2,1] * s5 + m[2,2] * s4 - m[2,3] * s3) * invDet

  result[1,0] = (-m[1,0] * c5 + m[1,2] * c2 - m[1,3] * c1) * invDet
  result[1,1] = ( m[0,0] * c5 - m[0,2] * c2 + m[0,3] * c1) * invDet
  result[1,2] = (-m[3,0] * s5 + m[3,2] * s2 - m[3,3] * s1) * invDet
  result[1,3] = ( m[2,0] * s5 - m[2,2] * s2 + m[2,3] * s1) * invDet

  result[2,0] = ( m[1,0] * c4 - m[1,1] * c2 + m[1,3] * c0) * invDet
  result[2,1] = (-m[0,0] * c4 + m[0,1] * c2 - m[0,3] * c0) * invDet
  result[2,2] = ( m[3,0] * s4 - m[3,1] * s2 + m[3,3] * s0) * invDet
  result[2,3] = (-m[2,0] * s4 + m[2,1] * s2 - m[2,3] * s0) * invDet

  result[3,0] = (-m[1,0] * c3 + m[1,1] * c1 - m[1,2] * c0) * invDet
  result[3,1] = ( m[0,0] * c3 - m[0,1] * c1 + m[0,2] * c0) * invDet
  result[3,2] = (-m[3,0] * s3 + m[3,1] * s1 - m[3,2] * s0) * invDet
  result[3,3] = ( m[2,0] * s3 - m[2,1] * s1 + m[2,2] * s0) * invDet


proc affineInverse(m: Mat4x4): Mat4x4 =
  ## Perform an inverse and make sure the bottom row always contains
  ## [0, 0, 0, 1].
  result = m.inverse
  result[3,0] = 0
  result[3,1] = 0
  result[3,2] = 0
  result[3,3] = 1

# 9 mul, 6 add, 9 neg (24 ops)
proc rigidInverse(m: Mat4x4): Mat4x4 =
  ## Only use if the transform matrix only contains rotations and
  ## translations.

  # Multiply the transposed linear 3x3 matrix with the negated translation
  # vector
  let
    dx = m[0,0] * -m[0,3] + m[1,0] * -m[1,3] + m[2,0] * -m[2,3]
    dy = m[0,1] * -m[0,3] + m[1,1] * -m[1,3] + m[2,1] * -m[2,3]
    dz = m[0,2] * -m[0,3] + m[1,2] * -m[1,3] + m[2,2] * -m[2,3]

  mat4x4([[m[0,0], m[1,0], m[2,0], dx],
          [m[0,1], m[1,1], m[2,1], dy],
          [m[0,2], m[1,2], m[2,2], dz],
          [FloatT(0.0), 0.0, 0.0, 1.0]])

# }}}
# {{{ Transform

type Transform* = object
  m*, mInv*: Mat4x4

proc transform*(m: Mat4x4): Transform {.inline.} =
  Transform(m: m, mInv: m.inverse)

proc transform*(m: Mat4x4, mInv: Mat4x4): Transform {.inline.} =
  Transform(m: m, mInv: mInv)

proc inverse*(t: Transform): Transform {.inline.} =
  transform(t.mInv, t.m)

proc `*`*(a, b: Transform): Transform =
  transform(a.m * b.m, b.mInv * a.mInv)

proc translate*(dx, dy, dz: FloatT): Transform =
  let
    m = mat4x4([[FloatT(1), 0, 0, dx],
                [FloatT(0), 1, 0, dy],
                [FloatT(0), 0, 1, dz],
                [FloatT(0), 0, 0,  1]])

    mInv = mat4x4([[FloatT(1), 0, 0, -dx],
                   [FloatT(0), 1, 0, -dy],
                   [FloatT(0), 0, 1, -dz],
                   [FloatT(0), 0, 0,   1]])

  transform(m, mInv)


proc rotateX*(theta: FloatT): Transform =
  let
    sinTheta = sin(degToRad(theta))
    cosTheta = cos(degToRad(theta))
    m = mat4x4([[FloatT(1),        0,         0, 0],
                [FloatT(0), cosTheta, -sinTheta, 0],
                [FloatT(0), sinTheta,  cosTheta, 0],
                [FloatT(0),        0,         0, 1]])

  transform(m, m.transpose)


proc rotateY*(theta: FloatT): Transform =
  let
    sinTheta = sin(degToRad(theta))
    cosTheta = cos(degToRad(theta))
    m = mat4x4([[FloatT(cosTheta),  0, sinTheta, 0],
                [FloatT(0),         1,        0, 0],
                [FloatT(-sinTheta), 0, cosTheta, 0],
                [FloatT(0),         0,        0, 1]])

  transform(m, m.transpose)


proc rotateZ*(theta: FloatT): Transform =
  let
    sinTheta = sin(degToRad(theta))
    cosTheta = cos(degToRad(theta))
    m = mat4x4([[FloatT(cosTheta), -sinTheta, 0, 0],
                [FloatT(sinTheta),  cosTheta, 0, 0],
                [FloatT(0),                0, 1, 0],
                [FloatT(0),                0, 0, 1]])

  transform(m, m.transpose)


proc rotate*(theta: FloatT, axis: Vec3f): Transform =
  let
    a = axis.norm
    sinTheta = sin(degToRad(theta))
    cosTheta = cos(degToRad(theta))

  # Compute rotation of first basis vector
  result.m[0,0] = a.x * a.x + (1.0 - a.x * a.x) * cosTheta
  result.m[0,1] = a.x * a.y * (1 - cosTheta) - a.z * sinTheta
  result.m[0,2] = a.x * a.z * (1 - cosTheta) + a.y * sinTheta
  result.m[0,3] = 0

  # Compute rotations of second and third basis vectors
  result.m[1,0] = a.x * a.y * (1 - cosTheta) + a.z * sinTheta
  result.m[1,1] = a.y * a.y + (1 - a.y * a.y) * cosTheta
  result.m[1,2] = a.y * a.z * (1 - cosTheta) - a.x * sinTheta
  result.m[1,3] = 0

  result.m[2,0] = a.x * a.z * (1 - cosTheta) - a.y * sinTheta
  result.m[2,1] = a.y * a.z * (1 - cosTheta) + a.x * sinTheta
  result.m[2,2] = a.z * a.z + (1 - a.z * a.z) * cosTheta
  result.m[2,3] = 0

  result.mInv = result.m.transpose


proc scale*(sx, sy, sz: FloatT): Transform =
  assert sx != 0
  assert sy != 0
  assert sz != 0
  let
    m = mat4x4([[FloatT(sx), 0,  0, 0],
                [FloatT(0), sy,  0, 0],
                [FloatT(0),  0, sz, 0],
                [FloatT(0),  0,  0, 1]])

    mInv = mat4x4([[FloatT(1/sx),    0,    0, 0],
                   [FloatT(0),    1/sy,    0, 0],
                   [FloatT(0),       0, 1/sz, 0],
                   [FloatT(0),       0,    0, 1]])

  transform(m, mInv)


proc scale*(s: FloatT): Transform =
  scale(s, s, s)

proc mulVec*[T](t: Transform, v: Vec3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: T(t.m[0,0] * v.x + t.m[0,1] * v.y + t.m[0,2] * v.z),
          y: T(t.m[1,0] * v.x + t.m[1,1] * v.y + t.m[1,2] * v.z),
          z: T(t.m[2,0] * v.x + t.m[2,1] * v.y + t.m[2,2] * v.z))

proc mulNorm*[T](t: Transform, n: Vec3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: T(t.m[0,0] * n.x + t.m[1,0] * n.y + t.m[2,0] * n.z),
          y: T(t.m[0,1] * n.x + t.m[1,1] * n.y + t.m[2,1] * n.z),
          z: T(t.m[0,2] * n.x + t.m[1,2] * n.y + t.m[2,2] * n.z))

proc mulPoint*[T](t: Transform, p: Vec3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: T(t.m[0,0] * p.x + t.m[0,1] * p.y + t.m[0,2] * p.z + t.m[0,3]),
          y: T(t.m[1,0] * p.x + t.m[1,1] * p.y + t.m[1,2] * p.z + t.m[1,3]),
          z: T(t.m[2,0] * p.x + t.m[2,1] * p.y + t.m[2,2] * p.z + t.m[2,3]))

proc mul*[T](t: Transform, b: Box3[T]): Box3[T] {.inline.} =
  var bt =    box3f(t.mulPoint(vec3f(b.pMin.x, b.pMin.y, b.pMin.z)))
  bt     = bt.union(t.mulPoint(vec3f(b.pMax.x, b.pMin.y, b.pMin.z)))
  bt     = bt.union(t.mulPoint(vec3f(b.pMin.x, b.pMax.y, b.pMin.z)))
  bt     = bt.union(t.mulPoint(vec3f(b.pMax.x, b.pMax.y, b.pMin.z)))
  bt     = bt.union(t.mulPoint(vec3f(b.pMin.x, b.pMin.y, b.pMax.z)))
  bt     = bt.union(t.mulPoint(vec3f(b.pMax.x, b.pMin.y, b.pMax.z)))
  bt     = bt.union(t.mulPoint(vec3f(b.pMin.x, b.pMax.y, b.pMax.z)))
  bt     = bt.union(t.mulPoint(vec3f(b.pMax.x, b.pMax.y, b.pMax.z)))
  result = bt

# }}}

# {{{ Tests

when isMainModule:
  # {{{ Mat4x4
  block:  # indexing operator tests
    var m = mat4x4([[FloatT(1),   2,  3,  4],
                    [FloatT(5),   6,  7,  8],
                    [FloatT(9),  10, 11, 12],
                    [FloatT(13), 14, 15, 16]])

    assert m[0,0] == 1
    assert m[0,1] == 2
    assert m[0,2] == 3
    assert m[0,3] == 4
    assert m[1,0] == 5
    assert m[1,1] == 6
    assert m[1,2] == 7
    assert m[1,3] == 8
    assert m[2,0] == 9
    assert m[2,1] == 10
    assert m[2,2] == 11
    assert m[2,3] == 12
    assert m[3,0] == 13
    assert m[3,1] == 14
    assert m[3,2] == 15
    assert m[3,3] == 16

    m[0,0] = 11
    m[0,1] = 12
    m[0,2] = 13
    m[0,3] = 14
    m[1,0] = 15
    m[1,1] = 16
    m[1,2] = 17
    m[1,3] = 18
    m[2,0] = 19
    m[2,1] = 110
    m[2,2] = 111
    m[2,3] = 112
    m[3,0] = 113
    m[3,1] = 114
    m[3,2] = 115
    m[3,3] = 116

    assert m[0,0] == 11
    assert m[0,1] == 12
    assert m[0,2] == 13
    assert m[0,3] == 14
    assert m[1,0] == 15
    assert m[1,1] == 16
    assert m[1,2] == 17
    assert m[1,3] == 18
    assert m[2,0] == 19
    assert m[2,1] == 110
    assert m[2,2] == 111
    assert m[2,3] == 112
    assert m[3,0] == 113
    assert m[3,1] == 114
    assert m[3,2] == 115
    assert m[3,3] == 116

  block:  # transpose test
    let m = mat4x4([[FloatT(1),   2,  3,  4],
                    [FloatT(5),   6,  7,  8],
                    [FloatT(9),  10, 11, 12],
                    [FloatT(13), 14, 15, 16]])

    let mt = mat4x4([[FloatT(1),  5,  9, 13],
                     [FloatT(2),  6, 10, 14],
                     [FloatT(3),  7, 11, 15],
                     [FloatT(4),  8, 12, 16]])

    assert m.transpose == mt

  block:  # transform tests:
    let
      a = vec3f(-300,2,8000)

      d = translate(1,2,3)
      rx = rotateX(10)
      ry = rotateY(55)
      rz = rotateY(120)
      s = scale(1.1, 42.42, -8.6)
      t = rx * ry * s * rz * d

      vt = t.mulVec(a)
      pt = t.mulPoint(a)
      nt = t.mulNorm(a)

    assert t.inverse.mulVec(vt).isClose(a)
    assert t.inverse.mulPoint(pt).isClose(a)
    assert t.inverse.mulNorm(nt).isClose(a, 0.001)

    assert t.mInv.isClose(t.m.inverse, 1)

  block:  # rigidInverse & affineInverse tests
    let
      d = translate(1,2,3)
      rx = rotateX(10)
      ry = rotateY(55)
      rz = rotateY(120)
      s = scale(1.1, 42.42, -8.6)
      rigidT = rx * ry * rz * d
      affineT = rigidT * s

    assert rigidT.mInv.isClose(rigidT.m.rigidInverse)
    assert affineT.mInv.isClose(affineT.m.rigidInverse) == false

    assert rigidT.mInv.isClose(rigidT.m.affineInverse, 1)
    assert affineT.mInv.isClose(affineT.m.affineInverse, 1)

  block:  # bounding box transform test
    let
      b = box3f(vec3f(1,2,3), vec3f(2,4,6))
      t = rotateZ(90)

    assert t.mul(b).isClose(box3f(vec3f(-4,1,3), vec3f(-2,2,6)))

  # }}}

# }}}

# vim: et:ts=2:sw=2:fdm=marker
