import core


# {{{ Mat4x4

type Mat4x4* = object
  m*: array[1..4, array[1..4, FloatT]]

proc mat4x4*(m: array[1..4, array[1..4, FloatT]]): Mat4x4 {.inline.} =
  Mat4x4(m: m)

proc `[]`*(m: Mat4x4, r, c: int): FloatT {.inline.} =
  m.m[r][c]

proc `[]=`*(m: var Mat4x4, r, c: int, v: FloatT) {.inline.} =
  m.m[r][c] = v

proc inverse(m: Mat4x4): Mat4x4 =
  m # TODO

# }}}
# {{{ Transform

type Transform* = object
  m*, mInv*: Mat4x4

proc transform*(m: Mat4x4): Transform {.inline.} =
  Transform(m: m, mInv: m.inverse)

proc transform*(m: Mat4x4, mInv: Mat4x4): Transform {.inline.} =
  Transform(m: m, mInv: mInv)

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


# }}}

# {{{ Tests

when isMainModule:
  # {{{ Mat4x4
  block:
    var m = mat4x4([[FloatT(1),   2,  3,  4],
                    [FloatT(5),   6,  7,  8],
                    [FloatT(9),  10, 11, 12],
                    [FloatT(13), 14, 15, 16]])

    assert m[1,1] == 1
    assert m[1,2] == 2
    assert m[1,3] == 3
    assert m[1,4] == 4
    assert m[2,1] == 5
    assert m[2,2] == 6
    assert m[2,3] == 7
    assert m[2,4] == 8
    assert m[3,1] == 9
    assert m[3,2] == 10
    assert m[3,3] == 11
    assert m[3,4] == 12
    assert m[4,1] == 13
    assert m[4,2] == 14
    assert m[4,3] == 15
    assert m[4,4] == 16

    m[1,1] = 11
    m[1,2] = 12
    m[1,3] = 13
    m[1,4] = 14
    m[2,1] = 15
    m[2,2] = 16
    m[2,3] = 17
    m[2,4] = 18
    m[3,1] = 19
    m[3,2] = 110
    m[3,3] = 111
    m[3,4] = 112
    m[4,1] = 113
    m[4,2] = 114
    m[4,3] = 115
    m[4,4] = 116

    assert m[1,1] == 11
    assert m[1,2] == 12
    assert m[1,3] == 13
    assert m[1,4] == 14
    assert m[2,1] == 15
    assert m[2,2] == 16
    assert m[2,3] == 17
    assert m[2,4] == 18
    assert m[3,1] == 19
    assert m[3,2] == 110
    assert m[3,3] == 111
    assert m[3,4] == 112
    assert m[4,1] == 113
    assert m[4,2] == 114
    assert m[4,3] == 115
    assert m[4,4] == 116

  # }}}

# }}}

# vim: et:ts=2:sw=2:fdm=marker
