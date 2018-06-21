import common, efloat, geometry, interaction, transform, shape
import math


type Sphere* = object of Shape
  radius*: FloatT
  zMin*, zMax*: FloatT
  phiMax*: FloatT
  thetaMin, thetaMax: FloatT


proc init(s: var Sphere, objectToWorld, worldToObject: ref Transform,
          reverseOrientation: bool,
          radius, zMin, zMax, phiMax: FloatT) {.inline.} =
  assert radius > 0
  init(s.Shape, objectToWorld, worldToObject, reverseOrientation)
  s.radius = radius
  s.zMin = clamp(min(zMin, zMax), -radius, radius)
  s.zMax = clamp(max(zMin, zMax), -radius, radius)
  s.phiMax = degToRad(clamp(phiMax, 0, 360))
  s.thetaMin = arccos(clamp(s.zMax / radius, -1, 1))
  s.thetaMax = arccos(clamp(s.zMin / radius, -1, 1))

method objectBound(s: Sphere): Box3f =
  box3f(vec3f(-s.radius, -s.radius, s.zMin),
        vec3f( s.radius,  s.radius, s.zMax))

method intersect*(s: Sphere, r: Ray, testAlphaTexture: bool = true):
                                          (bool, FloatT, SurfaceInteraction) =
  let
    ray, oErr, dErr = s.worldToObject[].mul(r)

    ox = efloat(r.o.x, oErr.x)
    oy = efloat(r.o.y, oErr.y)
    oz = efloat(r.o.z, oErr.z)

    dx = efloat(r.d.x, dErr.x)
    dy = efloat(r.d.y, dErr.y)
    dz = efloat(r.d.z, dErr.z)

    a = dx*dx + dy*dy + dz*dz
    b = 2 * (dx*ox + dy*oy + dz*oz)
    c = ox*ox + oy*oy + oz*oz - efloat(radius) * efloat(radius)


method intersectP*(s: Sphere, r: Ray, testAlphaTexture: bool = true): bool =
  discard

method area*(s: Sphere): FloatT  =
  discard

# vim: et:ts=2:sw=2:fdm=marker
