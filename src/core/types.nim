import core
import geometry
import medium
import transform


# Common types

type Shape* = object of RootObj
  objectToWorld*, worldToObject*: ref Transform
  reverseOrientation*: bool
  transformSwapsHandedness*: bool

type Interaction* = object of RootObj
  p*, pError*, n*, wo*: Vec3f
  time*: FloatT
  mediumInterface*: ref MediumInterface

type Shading = object
  n*, dpdu*, dpdv*, dndu*, dndv*: Vec3f

type SurfaceInteraction* = object of Interaction
  uv*: Vec3f
  dpdu*, dpdv*, dndu*, dndv*: Vec3f
  shape*: ref Shape
  shading*: Shading

