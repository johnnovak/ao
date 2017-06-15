import core


type
  # {{{ geometry.nim

  Vec2*[T] = object
    x*, y*: T

  Vec3*[T] = object
    x*, y*, z*: T

  Vec2f* = Vec2[FloatT]
  Vec2i* = Vec2[int]
  Vec3f* = Vec3[FloatT]
  Vec3i* = Vec3[int]

  Box2*[T] = object
    pMin*: Vec2[T]
    pMax*: Vec2[T]

  Box3*[T] = object
    pMin*: Vec3[T]
    pMax*: Vec3[T]

  Box2f* = Box2[FloatT]
  Box2i* = Box2[int]
  Box3f* = Box3[FloatT]
  Box3i* = Box3[int]

  Ray* = object of RootObj
    o*: Vec3f
    d*, dInv*: Vec3f
    tMax*: FloatT
    time*: FloatT
    medium*: ref Medium

  RayDifferential* = object of Ray
    hasDifferentials*: bool
    rxOrigin*, ryOrigin*: Vec3f
    rxDirection*, ryDirection*: Vec3f

  # }}}
  # {{{ interaction.nim

  Interaction* = object of RootObj
    p*, pError*, n*, wo*: Vec3f
    time*: FloatT
    mediumInterface*: ref MediumInterface

  Shading* = object
    n*, dpdu*, dpdv*, dndu*, dndv*: Vec3f

  SurfaceInteraction* = object of Interaction
    uv*: Vec3f
    dpdu*, dpdv*, dndu*, dndv*: Vec3f
    shape*: ref Shape
    shading*: Shading

  # }}}
  # {{{ medium.nim

  Medium* = object
    discard

  # }}}
  # {{{ shape.nim

  Shape* = object of RootObj
    objectToWorld*, worldToObject*: ref Transform
    reverseOrientation*: bool
    transformSwapsHandedness*: bool

  # }}}
  # {{{ transform.nim

  Mat4x4* = object
    # Matrix elements (coefficients) are stored in row-major form.
    m*: array[4, array[4, FloatT]]

  Transform* = object
    m*, mInv*: Mat4x4

  MediumInterface* = object
    discard

  # }}}


# vim: et:ts=2:sw=2:fdm=marker
