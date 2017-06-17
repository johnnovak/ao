import math, unittest, times
import core


# {{{ Mat4x4

suite "core/transformTests - Mat4x4":

  test "constructor & properties":
    var m = mat4x4(1,   2,  3,  4,
                   5,   6,  7,  8,
                   9,  10, 11, 12,
                   13, 14, 15, 16)

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

  test "transpose":
      let m = mat4x4(1,   2,  3,  4,
                     5,   6,  7,  8,
                     9,  10, 11, 12,
                     13, 14, 15, 16)

      let mt = mat4x4(1,  5,  9, 13,
                      2,  6, 10, 14,
                      3,  7, 11, 15,
                      4,  8, 12, 16)

      assert m.transpose == mt

  test "isIdentity":
      assert isIdentity(mat4x4(1, 0, 0, 0,
                               0, 1, 0, 0,
                               0, 0, 1, 0,
                               0, 0, 0, 1))

# }}}
# {{{ Transform

suite "core/transformTests - Transform":

  test "transforms & inverse transforms":
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

    assert rotateX(20).m.swapsHandedness == false
    assert scale(-1,-1,-1).m.swapsHandedness == true

  test "rigidInverse & affineInverse":
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

  test "lookAt":
      let
        t = lookAt(eye = vec3f(10,5,7), at = vec3f(3,5,7), up = vec3f(0,1,0))
        p1 = vec3f(3,4,5)
        p2 = t.mulPoint(p1)

      assert p2.isClose(vec3f(2,-1,-7))
      assert t.inverse.mulPoint(p2).isClose(p1)

  test "box transform":
    var
      b = box3f(vec3f(1,2,3), vec3f(2,4,6))
      t = rotateZ(90)

    assert t.mul(b).isClose(box3f(vec3f(-4,1,3), vec3f(-2,2,6)))

# }}}

# vim: et:ts=2:sw=2:fdm=marker
