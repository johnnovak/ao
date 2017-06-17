import math, unittest, times
import core


# {{{ Vec2f

suite "core/geometryTests - Vec2f":

  test "constructors":
    let
      a = vec2f(1, 2)
      b = vec2f(3, 5)

    try:
      let c = vec2f(1, NaN)
      assert false
    except AssertionError:
      assert true

  test "properties":
    let
      a = vec2f(1, 2)
      b = vec2f(3, 5)

    assert a.x == 1
    assert a.y == 2
    assert a[0] == 1
    assert a[1] == 2

  test "non-mutating operations":
    let
      a = vec2f(1, 2)
      b = vec2f(3, 5)

    assert a + b == vec2f(4, 7)
    assert -a == vec2f(-1, -2)
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
    assert len2(a).isClose(5)
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
    assert a.distance(b).isClose(sqrt(FloatT(13)))
    assert a.distance2(b).isClose(13)

  test "mutating operations":
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

suite "core/geometryTests - Vec2i":

  test "constructors":
    let
      a = vec2i(1, 2)
      b = vec2i(3, 5)

  test "properties":
    let
      a = vec2i(1, 2)
      b = vec2i(3, 5)

    assert a.x == 1
    assert a.y == 2
    assert a[0] == 1
    assert a[1] == 2

  test "non-mutating operations":
    let
      a = vec2i(1, 2)
      b = vec2i(3, 5)

    assert a + b == vec2i(4, 7)
    assert -a == vec2i(-1, -2)
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
    assert a.distance2(b).isClose(13)

  test "mutating operations":
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
# {{{ Vec2i

suite "core/geometryTests - Vec2i":

  test "constructors":
    let
      a = vec3f(1, 2, 3)
      b = vec3f(3, 5, 7)

    try:
      let c = vec3f(1, 2, NaN)
      assert false
    except AssertionError:
      assert true

  test "properties":
    let
      a = vec3f(1, 2, 3)
      b = vec3f(3, 5, 7)

    assert a.x == 1
    assert a.y == 2
    assert a.z == 3
    assert a[0] == 1
    assert a[1] == 2
    assert a[2] == 3

  test "non-mutating operations":
    let
      a = vec3f(1, 2, 3)
      b = vec3f(3, 5, 7)

    assert a + b == vec3f(4, 7, 10)
    assert -a == vec3f(-1, -2, -3)
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
    assert len2(a).isClose(14)
    assert vec3f(-10,0,0).norm == vec3f(-1,0,0)
    assert min(a) == 1
    assert max(a) == 3
    assert maxDimension(b) == 2
    assert min(vec3f(-2,5,7), vec3f(1,3,-5)) == vec3f(-2,3,-5)
    assert max(vec3f(-2,5,7), vec3f(1,3,-5)) == vec3f(1,5,7)
    assert abs(vec3f(-2,3,0)) == vec3f(2,3,0)
    assert floor(vec3f(0.2, 1.7, -1.9)) == vec3f(0, 1, -2)
    assert ceil(vec3f(0.2, 1.7, -1.9)) == vec3f(1, 2, -1)
    assert clamp(vec3f(-2.5, 0.5, 1.3), -1, 1) == vec3f(-1, 0.5, 1)
    assert lerp(a, b, 0.25) == vec3f(1.5, 2.75, 4)
    assert b.permute(2, 1, 0) == vec3f(7, 5, 3)
    assert a.distance(b).isClose(sqrt(FloatT(29)))
    assert a.distance2(b).isClose(29)

    assert a.faceforward(vec3f(1, 1, 1)) == a
    assert a.faceforward(vec3f(-1, -1, -1)) == -a

  test "coordinateSystem":
    let
      a = vec3f(1, 2, 3)
      v1 = a.norm
      (v2, v3) = v1.coordinateSystem
    assert v2.len.isClose(1)
    assert v3.len.isClose(1)

  test "mutating operations":
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

suite "core/geometryTests - Vec3i":

  test "properties":
    let
      a = vec3i(1, 2, 3)
      b = vec3i(3, 5, 7)

    assert a.x == 1
    assert a.y == 2
    assert a.z == 3
    assert a[0] == 1
    assert a[1] == 2
    assert a[2] == 3

  test "non-mutating operations":
    let
      a = vec3i(1, 2, 3)
      b = vec3i(3, 5, 7)

    assert a + b == vec3i(4, 7, 10)
    assert -a == vec3i(-1, -2, -3)
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
    assert maxDimension(b) == 2
    assert min(vec3i(-2,5,7), vec3i(1,3,-5)) == vec3i(-2,3,-5)
    assert max(vec3i(-2,5,7), vec3i(1,3,-5)) == vec3i(1,5,7)
    assert abs(vec3i(-2,3,0)) == vec3i(2,3,0)
    assert clamp(vec3i(-2, 0, 1), -1, 1) == vec3i(-1, 0, 1)
    assert lerp(a, b, 0.25) == vec3i(1, 2, 4)
    assert b.permute(2, 1, 0) == vec3i(7, 5, 3)
    assert a.distance(b).isClose(sqrt(FloatT(29)))
    assert a.distance2(b).isClose(29)

    assert a.faceforward(vec3i(1, 1, 1)) == a
    assert a.faceforward(vec3i(-1, -1, -1)) == -a

  test "non-mutating operations":
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

suite "core/geometryTests - Box2f":

  test "corner":
    let b1 = box2f(vec2f(3,1), vec2f(-2,5))

    assert b1.pMin == vec2f(-2,1)
    assert b1[0] == b1.pMin
    assert b1.pMax == vec2f(3,5)
    assert b1[1] == b1.pMax
    assert b1.corner(0) == vec2f(-2,1)
    assert b1.corner(1) == vec2f(3,1)
    assert b1.corner(2) == vec2f(-2,5)
    assert b1.corner(3) == vec2f(3,5)

  test "union":
    let b1 = box2f(vec2f(3,1), vec2f(-2,5))
    let b2 = box2f(vec2f(1,2))

    assert b2.pMin == vec2f(1,2)
    assert b2.pMax == vec2f(1,2)

    assert box2f().union(vec2f(1,2)) == box2f(vec2f(1,2), vec2f(1,2))
    assert b2.union(vec2f(2,3)) == box2f(vec2f(1,2), vec2f(2,3))
    assert b1.union(box2f(vec2f(0,6),
                          vec2f(8,-7))) == box2f(vec2f(-2,-7), vec2f(8,6))

  test "intersect":
    let b1 = box2f(vec2f(3,1), vec2f(-2,5))
    let b3 = box2f(vec2f(0,4), vec2f(2,6))

    assert b1.intersect(b3) == box2f(vec2f(0,4), vec2f(2,5))

  test "overlaps":
    let b1 = box2f(vec2f(3,1), vec2f(-2,5))

    assert b1.overlaps(box2f(vec2f(2,3), vec2f(4,7))) == true
    assert b1.overlaps(box2f(vec2f(-3,-1), vec2f(-1,1))) == true
    assert b1.overlaps(box2f(vec2f(-3,-1), vec2f(-1,0))) == false
    assert b1.overlaps(box2f(vec2f(4,-1), vec2f(5,0))) == false
    assert b1.overlaps(box2f(vec2f(2,-1), vec2f(5,0))) == false

  test "inside & insideExclusive":
    let b1 = box2f(vec2f(3,1), vec2f(-2,5))

    assert vec2f(0,1).inside(b1) == true
    assert vec2f(0.9,4.9).insideExclusive(b1) == true
    assert vec2f(1,5).insideExclusive(b1) == false

  test "misc operations":
    let b1 = box2f(vec2f(3,1), vec2f(-2,5))

    assert b1.center == vec2f(0.5, 3)
    assert b1.diagonal == vec2f(5,4)
    assert b1.maxExtent == 0
    assert b1.area == 20
    assert b1.expand(1) == box2f(vec2f(-3,0), vec2f(4,6))
    assert lerp(b1, 0.25) == vec2f(-0.75, 2)
    assert b1.offset(vec2f(-0.75, 4)) == vec2f(0.25, 0.75)

# }}}
# {{{ Box2i

suite "core/geometryTests - Box2i":

  test "properties":
    let b1 = box2i(vec2i(3,1), vec2i(-2,5))

    assert b1.pMin == vec2i(-2,1)
    assert b1[0] == b1.pMin
    assert b1.pMax == vec2i(3,5)
    assert b1[1] == b1.pMax

  test "corner":
    let b1 = box2i(vec2i(3,1), vec2i(-2,5))

    assert b1.corner(0) == vec2i(-2,1)
    assert b1.corner(1) == vec2i(3,1)
    assert b1.corner(2) == vec2i(-2,5)
    assert b1.corner(3) == vec2i(3,5)

  test "union":
    let b1 = box2i(vec2i(3,1), vec2i(-2,5))
    let b2 = box2i(vec2i(1,2))

    assert b2.pMin == vec2i(1,2)
    assert b2.pMax == vec2i(1,2)

    assert box2i().union(vec2i(1,2)) == box2i(vec2i(1,2), vec2i(1,2))
    assert b2.union(vec2i(2,3)) == box2i(vec2i(1,2), vec2i(2,3))
    assert b1.union(box2i(vec2i(0,6),
                          vec2i(8,-7))) == box2i(vec2i(-2,-7), vec2i(8,6))

  test "intersect":
    let b1 = box2i(vec2i(3,1), vec2i(-2,5))
    let b3 = box2i(vec2i(0,4), vec2i(2,6))

    assert b1.intersect(b3) == box2i(vec2i(0,4), vec2i(2,5))

  test "overlaps":
    let b1 = box2i(vec2i(3,1), vec2i(-2,5))

    assert b1.overlaps(box2i(vec2i(2,3), vec2i(4,7))) == true
    assert b1.overlaps(box2i(vec2i(-3,-1), vec2i(-1,1))) == true
    assert b1.overlaps(box2i(vec2i(-3,-1), vec2i(-1,0))) == false
    assert b1.overlaps(box2i(vec2i(4,-1), vec2i(5,0))) == false
    assert b1.overlaps(box2i(vec2i(2,-1), vec2i(5,0))) == false

  test "inside & insideExclusive":
    let b1 = box2i(vec2i(3,1), vec2i(-2,5))

    assert vec2i(0,1).inside(b1) == true
    assert vec2i(0,4).insideExclusive(b1) == true
    assert vec2i(1,5).insideExclusive(b1) == false

  test "misc operations":
    let b1 = box2i(vec2i(3,1), vec2i(-2,5))

    assert b1.center == vec2i(0, 3)
    assert b1.diagonal == vec2i(5,4)
    assert b1.maxExtent == 0
    assert b1.area == 20
    assert b1.expand(1) == box2i(vec2i(-3,0), vec2i(4,6))
    assert lerp(b1, 0.25) == vec2i(-0, 2)

# }}}
# {{{ Box3f

suite "core/geometryTests - Box3f":

  test "properties":
    let b1 = box3f(vec3f(3,1,4), vec3f(-2,5,-1))

    assert b1.pMin == vec3f(-2,1,-1)
    assert b1[0] == b1.pMin
    assert b1.pMax == vec3f(3,5,4)
    assert b1[1] == b1.pMax

  test "corner":
    let b1 = box3f(vec3f(3,1,4), vec3f(-2,5,-1))

    assert b1.corner(0) == vec3f(-2, 1, -1)
    assert b1.corner(1) == vec3f( 3, 1, -1)
    assert b1.corner(2) == vec3f(-2, 5, -1)
    assert b1.corner(3) == vec3f( 3, 5, -1)
    assert b1.corner(4) == vec3f(-2, 1,  4)
    assert b1.corner(5) == vec3f( 3, 1,  4)
    assert b1.corner(6) == vec3f(-2, 5,  4)
    assert b1.corner(7) == vec3f( 3, 5,  4)

  test "union":
    let b1 = box3f(vec3f(3,1,4), vec3f(-2,5,-1))
    let b2 = box3f(vec3f(1,2,3))

    assert b2.pMin == vec3f(1,2,3)
    assert b2.pMax == vec3f(1,2,3)

    assert box3f().union(vec3f(1,2,3)) == box3f(vec3f(1,2,3), vec3f(1,2,3))
    assert b2.union(vec3f(2,3,4)) == box3f(vec3f(1,2,3), vec3f(2,3,4))
    assert b1.union(box3f(vec3f(0,6,-3),
                          vec3f(8,-7,-2))) == box3f(vec3f(-2,-7,-3),
                                                    vec3f(8,6,4))

  test "intersect":
    let b1 = box3f(vec3f(3,1,4), vec3f(-2,5,-1))
    let b3 = box3f(vec3f(0,4,3), vec3f(2,6,7))

    assert b1.intersect(b3) == box3f(vec3f(0,4,3), vec3f(2,5,4))

  test "overlaps":
    let b1 = box3f(vec3f(3,1,4), vec3f(-2,5,-1))

    assert b1.overlaps(box3f(vec3f(2,3,0), vec3f(4,7,5))) == true
    assert b1.overlaps(box3f(vec3f(-3,-1,4), vec3f(-1,1,3))) == true
    assert b1.overlaps(box3f(vec3f(-3,-1,-3), vec3f(-1,0,-1))) == false
    assert b1.overlaps(box3f(vec3f(4,-1,1), vec3f(5,0,2))) == false
    assert b1.overlaps(box3f(vec3f(2,-1,5), vec3f(5,0,6))) == false

  test "inside & insideExclusive":
    let b1 = box3f(vec3f(3,1,4), vec3f(-2,5,-1))

    assert vec3f(0,1,3).inside(b1) == true
    assert vec3f(1, 4.9, 3.9).insideExclusive(b1) == true
    assert vec3f(1,4,4).insideExclusive(b1) == false

  test "misc operations":
    let b1 = box3f(vec3f(3,1,4), vec3f(-2,5,-1))

    assert b1.center == vec3f(0.5, 3, 1.5)
    assert b1.diagonal == vec3f(5,4,5)
    assert b1.maxExtent == 2
    assert b1.area == 130
    assert b1.volume == 100
    assert b1.expand(1) == box3f(vec3f(-3,0,-2), vec3f(4,6,5))
    assert lerp(b1, 0.25) == vec3f(-0.75, 2, 0.25)
    assert b1.offset(vec3f(-0.75, 4, 1.5)) == vec3f(0.25, 0.75, 0.5)

  test "intersect":
    var
      b = box3f(vec3f(1,1,1), vec3f(2,2,2))
      r1 = initRay(o = vec3f(1.5, 1.5, 0), d = vec3f(0.0, 0.0, 1.0),
                   tMax = Inf, time = 0, medium = nil)
      r2 = initRay(o = vec3f(0.5, 0.5, 0), d = vec3f(0.0, 0.0, 1.0),
                   tMax = Inf, time = 0, medium = nil)

    var (isHit, t1, t2) = b.intersect(r1)
    assert isHit
    assert t1 == 1.0
    assert t2 == 2.0

    (isHit, t1, t2) = b.intersect(r2)
    assert isHit == false

    r1.tMax = 0.5
    (isHit, t1, t2) = b.intersect(r1)
    assert isHit == false
    assert t1 == 1.0
    assert t2 == 2.0

# }}}
# {{{ Box3i

suite "core/geometryTests - Box3i":

  test "properties":
    let b1 = box3i(vec3i(3,1,4), vec3i(-2,5,-1))

    assert b1.pMin == vec3i(-2,1,-1)
    assert b1[0] == b1.pMin
    assert b1.pMax == vec3i(3,5,4)
    assert b1[1] == b1.pMax

  test "corner":
    let b1 = box3i(vec3i(3,1,4), vec3i(-2,5,-1))

    assert b1.corner(0) == vec3i(-2, 1, -1)
    assert b1.corner(1) == vec3i( 3, 1, -1)
    assert b1.corner(2) == vec3i(-2, 5, -1)
    assert b1.corner(3) == vec3i( 3, 5, -1)
    assert b1.corner(4) == vec3i(-2, 1,  4)
    assert b1.corner(5) == vec3i( 3, 1,  4)
    assert b1.corner(6) == vec3i(-2, 5,  4)
    assert b1.corner(7) == vec3i( 3, 5,  4)

  test "union":
    let b1 = box3i(vec3i(3,1,4), vec3i(-2,5,-1))
    let b2 = box3i(vec3i(1,2,3))

    assert b2.pMin == vec3i(1,2,3)
    assert b2.pMax == vec3i(1,2,3)

    assert box3i().union(vec3i(1,2,3)) == box3i(vec3i(1,2,3), vec3i(1,2,3))
    assert b2.union(vec3i(2,3,4)) == box3i(vec3i(1,2,3), vec3i(2,3,4))
    assert b1.union(box3i(vec3i(0,6,-3),
                          vec3i(8,-7,-2))) == box3i(vec3i(-2,-7,-3),
                                                    vec3i(8,6,4))

  test "intersect":
    let b1 = box3i(vec3i(3,1,4), vec3i(-2,5,-1))
    let b3 = box3i(vec3i(0,4,3), vec3i(2,6,7))

    assert b1.intersect(b3) == box3i(vec3i(0,4,3), vec3i(2,5,4))

  test "overlaps":
    let b1 = box3i(vec3i(3,1,4), vec3i(-2,5,-1))

    assert b1.overlaps(box3i(vec3i(2,3,0), vec3i(4,7,5))) == true
    assert b1.overlaps(box3i(vec3i(-3,-1,4), vec3i(-1,1,3))) == true
    assert b1.overlaps(box3i(vec3i(-3,-1,-3), vec3i(-1,0,-1))) == false
    assert b1.overlaps(box3i(vec3i(4,-1,1), vec3i(5,0,2))) == false
    assert b1.overlaps(box3i(vec3i(2,-1,5), vec3i(5,0,6))) == false

  test "inside & insideExclusive":
    let b1 = box3i(vec3i(3,1,4), vec3i(-2,5,-1))

    assert vec3i(0,1,3).inside(b1) == true
    assert vec3i(1, 4, 3).insideExclusive(b1) == true
    assert vec3i(1,4,4).insideExclusive(b1) == false

  test "misc operations":
    let b1 = box3i(vec3i(3,1,4), vec3i(-2,5,-1))

    assert b1.center == vec3i(0, 3, 1)
    assert b1.diagonal == vec3i(5,4,5)
    assert b1.maxExtent == 2
    assert b1.area == 130
    assert b1.volume == 100
    assert b1.expand(1) == box3i(vec3i(-3,0,-2), vec3i(4,6,5))
    assert lerp(b1, 0.25) == vec3i(-0, 2, 0)

  test "intersect":
    var
      b = box3i(vec3i(1,1,1), vec3i(2,2,2))
      r1 = initRay(o = vec3f(1.5, 1.5, 0), d = vec3f(0.0, 0.0, 1.0),
                   tMax = Inf, time = 0, medium = nil)
      r2 = initRay(o = vec3f(0.5, 0.5, 0), d = vec3f(0.0, 0.0, 1.0),
                   tMax = Inf, time = 0, medium = nil)

    var (isHit, t1, t2) = b.intersect(r1)
    assert isHit
    assert t1 == 1.0
    assert t2 == 2.0

    (isHit, t1, t2) = b.intersect(r2)
    assert isHit == false

    r1.tMax = 0.5
    (isHit, t1, t2) = b.intersect(r1)
    assert isHit == false
    assert t1 == 1.0
    assert t2 == 2.0

# }}}
# {{{ Ray

suite "core/geometryTests - Ray":

  test "operations":
      let r = initRay(o = vec3f(1, 2, 3), d = vec3f(-1, -0.5, 0),
                      tMax = 111, time = 222, medium = nil)

      assert r.t(3) == vec3f(-2, 0.5, 3)
      assert hasNaNs(r) == false
      assert r.tMax == 111
      assert r.time == 222

# }}}
# {{{ RayDifferential

suite "core/geometryTests - RayDifferential":

  test "operations":
    let r = initRayDifferential(
      o = vec3f(1, 2, 3), d = vec3f(-1, -0.5, 0),
      tMax = 111, time = 222, medium = nil,
      hasDifferentials = true,
      rxOrigin = vec3f(1,1,1), ryOrigin = vec3f(2,2,2),
      rxDirection = vec3f(1,0,0), ryDirection = vec3f(0,1,0))

    assert r.t(3) == vec3f(-2, 0.5, 3)
    assert hasNaNs(r) == false
    assert r.tMax == 111
    assert r.time == 222
    assert r.rxOrigin == vec3f(1,1,1)
    assert r.ryOrigin == vec3f(2,2,2)
    assert r.rxDirection == vec3f(1,0,0)
    assert r.ryDirection == vec3f(0,1,0)

# }}}

# vim: et:ts=2:sw=2:fdm=marker
