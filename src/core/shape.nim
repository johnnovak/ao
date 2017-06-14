import core
import geometry
import types
import interaction
import transform


{.experimental.}

proc init(s: var Shape, objectToWorld, worldToObject: ref Transform,
          reverseOrientation: bool) {.inline.} =
  s.objectToWorld = objectToWorld
  s.worldToObject = worldToObject
  s.reverseOrientation = reverseOrientation
  s.transformSwapsHandedness = objectToWorld.swapsHandedness()

method objectBound*(s: Shape): Box3f {.base.} = nil

method worldBound*(s: Shape): Box3f {.base.} =
  s.objectToWorld.mul(s.objectBound())

# Out params: (isHit, tHit, isect)
method intersect*(
  r: Ray, testAlphaTexture: bool = true
): (bool, FloatT, SurfaceInteraction) {.base.} = nil

method intersectP*(r: Ray, testAlphaTexture: bool = true): bool {.base.} =
  nil

method area(s: Shape): FloatT {.base.} = 0


# vim: et:ts=2:sw=2:fdm=marker
