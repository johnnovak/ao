import common, types, geometry
import math

export types.Mat4x4
export types.Transform

# {{{ Mat4x4

proc mat4x4*(m00, m01, m02, m03,
             m10, m11, m12, m13,
             m20, m21, m22, m23,
             m30, m31, m32, m33: FloatT): Mat4x4 {.inline.} =
  result.m[0][0] = m00
  result.m[0][1] = m01
  result.m[0][2] = m02
  result.m[0][3] = m03
  result.m[1][0] = m10
  result.m[1][1] = m11
  result.m[1][2] = m12
  result.m[1][3] = m13
  result.m[2][0] = m20
  result.m[2][1] = m21
  result.m[2][2] = m22
  result.m[2][3] = m23
  result.m[3][0] = m30
  result.m[3][1] = m31
  result.m[3][2] = m32
  result.m[3][3] = m33

proc mat4x4*(): Mat4x4 {.inline.} =
  mat4x4(1, 0, 0, 0,
         0, 1, 0, 0,
         0, 0, 1, 0,
         0, 0, 0, 1)

proc isIdentity*(t: Mat4x4): bool {.inline.} =
  t == mat4x4()

proc `[]`*(m: Mat4x4, r, c: int): FloatT {.inline.} =
  m.m[r][c]

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
      result.m[i][j] = a[i,0] * b[0,j] +
                       a[i,1] * b[1,j] +
                       a[i,2] * b[2,j] +
                       a[i,3] * b[3,j]

proc transpose*(m: Mat4x4): Mat4x4 =
  mat4x4(m[0,0], m[1,0], m[2,0], m[3,0],
         m[0,1], m[1,1], m[2,1], m[3,1],
         m[0,2], m[1,2], m[2,2], m[3,2],
         m[0,3], m[1,3], m[2,3], m[3,3])

# 1 div, 94 mul, 49 add/sub, 8 neg (152 ops)
proc inverse*(m: Mat4x4): Mat4x4 =
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

  result.m[0][0] = ( m[1,1] * c5 - m[1,2] * c4 + m[1,3] * c3) * invDet
  result.m[0][1] = (-m[0,1] * c5 + m[0,2] * c4 - m[0,3] * c3) * invDet
  result.m[0][2] = ( m[3,1] * s5 - m[3,2] * s4 + m[3,3] * s3) * invDet
  result.m[0][3] = (-m[2,1] * s5 + m[2,2] * s4 - m[2,3] * s3) * invDet

  result.m[1][0] = (-m[1,0] * c5 + m[1,2] * c2 - m[1,3] * c1) * invDet
  result.m[1][1] = ( m[0,0] * c5 - m[0,2] * c2 + m[0,3] * c1) * invDet
  result.m[1][2] = (-m[3,0] * s5 + m[3,2] * s2 - m[3,3] * s1) * invDet
  result.m[1][3] = ( m[2,0] * s5 - m[2,2] * s2 + m[2,3] * s1) * invDet

  result.m[2][0] = ( m[1,0] * c4 - m[1,1] * c2 + m[1,3] * c0) * invDet
  result.m[2][1] = (-m[0,0] * c4 + m[0,1] * c2 - m[0,3] * c0) * invDet
  result.m[2][2] = ( m[3,0] * s4 - m[3,1] * s2 + m[3,3] * s0) * invDet
  result.m[2][3] = (-m[2,0] * s4 + m[2,1] * s2 - m[2,3] * s0) * invDet

  result.m[3][0] = (-m[1,0] * c3 + m[1,1] * c1 - m[1,2] * c0) * invDet
  result.m[3][1] = ( m[0,0] * c3 - m[0,1] * c1 + m[0,2] * c0) * invDet
  result.m[3][2] = (-m[3,0] * s3 + m[3,1] * s1 - m[3,2] * s0) * invDet
  result.m[3][3] = ( m[2,0] * s3 - m[2,1] * s1 + m[2,2] * s0) * invDet


proc affineInverse*(m: Mat4x4): Mat4x4 =
  ## Perform an inverse and make sure the bottom row always contains
  ## [0, 0, 0, 1].
  result = m.inverse
  result.m[3][0] = 0
  result.m[3][1] = 0
  result.m[3][2] = 0
  result.m[3][3] = 1

# 9 mul, 6 add, 9 neg (24 ops)
proc rigidInverse*(m: Mat4x4): Mat4x4 =
  ## Only use if the transform matrix only contains rotations and
  ## translations.

  # Multiply the transposed linear 3x3 matrix with the negated translation
  # vector.
  let
    dx = m[0,0] * -m[0,3] + m[1,0] * -m[1,3] + m[2,0] * -m[2,3]
    dy = m[0,1] * -m[0,3] + m[1,1] * -m[1,3] + m[2,1] * -m[2,3]
    dz = m[0,2] * -m[0,3] + m[1,2] * -m[1,3] + m[2,2] * -m[2,3]

  mat4x4(m[0,0], m[1,0], m[2,0], dx,
         m[0,1], m[1,1], m[2,1], dy,
         m[0,2], m[1,2], m[2,2], dz,
              0,      0,      0,  1)


proc swapsHandedness*(t: Mat4x4): bool =
  let det = t[0,0] * (t[1,1] * t[2,2] - t[1,2] * t[2,1]) -
            t[0,1] * (t[1,0] * t[2,2] - t[1,2] * t[2,0]) +
            t[0,2] * (t[1,0] * t[2,1] - t[1,1] * t[2,0])
  det < 0

# }}}
# {{{ Transform

proc transform*(m: Mat4x4): Transform {.inline.} =
  Transform(m: m, mInv: m.inverse)

proc transform*(m: Mat4x4, mInv: Mat4x4): Transform {.inline.} =
  Transform(m: m, mInv: mInv)

proc m*(t: Transform): Mat4x4 {.inline.} = t.m
proc mInv*(t: Transform): Mat4x4 {.inline.} = t.mInv

proc inverse*(t: Transform): Transform {.inline.} =
  transform(t.mInv, t.m)

proc `*`*(a, b: Transform): Transform =
  transform(a.m * b.m, b.mInv * a.mInv)

proc translate*(dx, dy, dz: FloatT): Transform =
  let
    m = mat4x4(1, 0, 0, dx,
               0, 1, 0, dy,
               0, 0, 1, dz,
               0, 0, 0,  1)

    mInv = mat4x4(1, 0, 0, -dx,
                  0, 1, 0, -dy,
                  0, 0, 1, -dz,
                  0, 0, 0,   1)

  transform(m, mInv)


proc rotateX*(theta: FloatT): Transform =
  let
    sinTheta = sin(degToRad(theta))
    cosTheta = cos(degToRad(theta))
    m = mat4x4(1,        0,         0, 0,
               0, cosTheta, -sinTheta, 0,
               0, sinTheta,  cosTheta, 0,
               0,        0,         0, 1)

  transform(m, m.transpose)


proc rotateY*(theta: FloatT): Transform =
  let
    sinTheta = sin(degToRad(theta))
    cosTheta = cos(degToRad(theta))
    m = mat4x4(cosTheta,  0, sinTheta, 0,
               0,         1,        0, 0,
               -sinTheta, 0, cosTheta, 0,
               0,         0,        0, 1)

  transform(m, m.transpose)


proc rotateZ*(theta: FloatT): Transform =
  let
    sinTheta = sin(degToRad(theta))
    cosTheta = cos(degToRad(theta))
    m = mat4x4(cosTheta, -sinTheta, 0, 0,
               sinTheta,  cosTheta, 0, 0,
               0,                0, 1, 0,
               0,                0, 0, 1)

  transform(m, m.transpose)


proc rotate*(theta: FloatT, axis: Vec3f): Transform =
  let
    a = axis.norm
    sinTheta = sin(degToRad(theta))
    cosTheta = cos(degToRad(theta))

    # Compute rotation of first basis vector
    m00 = a.x * a.x + (1.0 - a.x * a.x) * cosTheta
    m01 = a.x * a.y * (1 - cosTheta) - a.z * sinTheta
    m02 = a.x * a.z * (1 - cosTheta) + a.y * sinTheta

    # Compute rotations of second and third basis vectors
    m10 = a.x * a.y * (1 - cosTheta) + a.z * sinTheta
    m11 = a.y * a.y + (1 - a.y * a.y) * cosTheta
    m12 = a.y * a.z * (1 - cosTheta) - a.x * sinTheta

    m20 = a.x * a.z * (1 - cosTheta) - a.y * sinTheta
    m21 = a.y * a.z * (1 - cosTheta) + a.x * sinTheta
    m22 = a.z * a.z + (1 - a.z * a.z) * cosTheta

  result.m = mat4x4(m00, m01, m02, 0,
                    m10, m11, m12, 0,
                    m20, m21, m22, 0,
                      0,   0,   0, 1)

  result.mInv = result.m.transpose


proc scale*(sx, sy, sz: FloatT): Transform =
  assert sx != 0
  assert sy != 0
  assert sz != 0
  let
    m = mat4x4(sx, 0,  0, 0,
               0, sy,  0, 0,
               0,  0, sz, 0,
               0,  0,  0, 1)

    mInv = mat4x4(1/sx,    0,    0, 0,
                  0,    1/sy,    0, 0,
                  0,       0, 1/sz, 0,
                  0,       0,    0, 1)

  transform(m, mInv)


proc scale*(s: FloatT): Transform =
  scale(s, s, s)


proc lookAt*(eye, at, up: Vec3f): Transform =
  let
    lookDir = (at - eye).norm
    bx = lookDir.cross(up.norm)

  assert bx.len.isClose(0) == false

  let
    by = bx.cross(lookDir)
    bz = -lookDir

    cameraToWorld = mat4x4(bx.x, by.x, bz.x, eye.x,
                           bx.y, by.y, bz.y, eye.y,
                           bx.z, by.z, bz.z, eye.z,
                              0,    0,    0,     1)

    worldToCamera = cameraToWorld.rigidInverse

  transform(worldToCamera, cameraToWorld)


proc mulVec*[T](t: Transform, v: Vec3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: T(t.m[0,0] * v.x + t.m[0,1] * v.y + t.m[0,2] * v.z),
          y: T(t.m[1,0] * v.x + t.m[1,1] * v.y + t.m[1,2] * v.z),
          z: T(t.m[2,0] * v.x + t.m[2,1] * v.y + t.m[2,2] * v.z))

proc mulNorm*[T](t: Transform, n: Vec3[T]): Vec3[T] {.inline.} =
  Vec3[T](x: T(t.m[0,0] * n.x + t.m[1,0] * n.y + t.m[2,0] * n.z),
          y: T(t.m[0,1] * n.x + t.m[1,1] * n.y + t.m[2,1] * n.z),
          z: T(t.m[0,2] * n.x + t.m[1,2] * n.y + t.m[2,2] * n.z))

proc mulPoint*[T](t: Transform, p: Vec3[T]): Vec3[T] {.inline.} =
  let
    tx = T((t.m[0,0] * p.x + t.m[0,1] * p.y) + (t.m[0,2] * p.z + t.m[0,3]))
    ty = T((t.m[1,0] * p.x + t.m[1,1] * p.y) + (t.m[1,2] * p.z + t.m[1,3]))
    tz = T((t.m[2,0] * p.x + t.m[2,1] * p.y) + (t.m[2,2] * p.z + t.m[2,3]))

  result = Vec3[T](x: tx, y: ty, z: tz)


# Returns: (transformedVector, pError)
proc mulVecErr*(t: Transform, p: Vec3f): (Vec3f, Vec3f) {.inline.} =
  let
    xAbsSum = abs(t.m[0,0] * p.x) + abs(t.m[0,1] * p.y) +
              abs(t.m[0,2] * p.z)

    yAbsSum = abs(t.m[1,0] * p.x) + abs(t.m[1,1] * p.y) +
              abs(t.m[1,2] * p.z)

    zAbsSum = abs(t.m[2,0] * p.x) + abs(t.m[2,1] * p.y) +
              abs(t.m[2,2] * p.z)

    pError = gamma(3) * vec3f(xAbsSum, yAbsSum, zAbsSum)

  result = (t.mulVec(p), pError)


# Returns: (transformedPoint, pErr)
proc mulPointErr*(t: Transform, p: Vec3f): (Vec3f, Vec3f) {.inline.} =
  let
    xAbsSum = abs(t.m[0,0] * p.x) + abs(t.m[0,1] * p.y) +
              abs(t.m[0,2] * p.z) + abs(t.m[0,3])

    yAbsSum = abs(t.m[1,0] * p.x) + abs(t.m[1,1] * p.y) +
              abs(t.m[1,2] * p.z) + abs(t.m[1,3])

    zAbsSum = abs(t.m[2,0] * p.x) + abs(t.m[2,1] * p.y) +
              abs(t.m[2,2] * p.z) + abs(t.m[2,3])

    pErr = gamma(3) * vec3f(xAbsSum, yAbsSum, zAbsSum)

  result = (t.mulPoint(p), pErr)


# Returns: (transformedRay, oErr, dErr)
proc mul*(t: Transform, r: Ray): (Ray, Vec3f, Vec3f) {.inline.} =
  let
    (o, oErr) = t.mulPointErr(r.o)
    (d, dErr) = t.mulVecErr(r.d)
    dlen = d.len

  var ray = r
  ray.o = o
  ray.d = d

  if d.len > 0:
    let dt = dot(abs(d), oErr) / dlen
    ray.o += d * dt
    ray.tMax -= dt

  result = (ray, oErr, dErr)


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

proc swapsHandedness*(t: Transform): bool {.inline.} =
  t.m.swapsHandedness()

# }}}

# vim: et:ts=2:sw=2:fdm=marker
