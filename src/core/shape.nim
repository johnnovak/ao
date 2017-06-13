import core
import geometry
import transform


{.experimental.}

type Shape* = object of RootObj
  objectToWorld*, worldToObject*: ref Transform
  reverseOrientation*: bool
  transformSwapsHandedness*: bool

proc init(s: var Shape, objectToWorld, worldToObject: ref Transform,
          reverseOrientation: bool) {.inline.} =
  s.objectToWorld = objectToWorld
  s.worldToObject = worldToObject
  s.reverseOrientation = reverseOrientation
  s.transformSwapsHandedness = objectToWorld.swapsHandedness()

method objectBound(s: Shape): Box3f {.base.} = nil

method worldBound(s: Shape): Box3f =
  s.objectToWorld.mul(s.objectBound())


# vim: et:ts=2:sw=2:fdm=marker
