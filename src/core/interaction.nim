import common, types, geometry

export types.Interaction
export types.SurfaceInteraction

# {{{ Interaction

proc init(self: var Interaction, p, pErr, n, wo: Vec3f, time: FloatT,
          mediumInterface: ref MediumInterface) =
  self.p = p
  self.pErr = pErr
  self.n = n
  self.wo = wo
  self.time = time
  self.mediumInterface = mediumInterface

proc initInteraction*(p, pErr, n, wo: Vec3f,
    time: FloatT, mediumInterface: ref MediumInterface = nil): Interaction =
  init(result, p, pErr, n, wo, time, mediumInterface)

proc isSurfaceInteraction*(i: Interaction): bool {.inline.} =
  i.n != vec3f(0,0,0)

proc getMedium*(i: Interaction): ref Medium {.inline.} =
  assert i.mediumInterface.inside == i.mediumInterface.outside
  i.mediumInterface.inside

proc getMedium*(i: Interaction, w: Vec3f): ref Medium {.inline.} =
  if dot(w, i.n) > 0:
    i.mediumInterface.outside
  else:
    i.mediumInterface.inside

proc spawnRay*(i: Interaction, d: Vec3f): Ray {.inline.} =
  let o = offsetRayOrigin(i.p, i.pErr, i.n, d)
  Ray(o: o, d: d, tMax: Inf, time: i.time, medium: i.getMedium(d))

proc spawnRayTo*(i: Interaction, pt: Vec3f): Ray {.inline.} =
  let
    o = offsetRayOrigin(i.p, i.pErr, i.n, pt - i.p)
    d = pt - o
  Ray(o: o, d: d, tMax: 1 - ShadowEpsilon, time: i.time,
      medium: i.getMedium(d))

proc spawnRayTo*(i: Interaction, it: Interaction): Ray {.inline.} =
  let 
    o = offsetRayOrigin(i.p, i.pErr, i.n, it.p - i.p)
    t = offsetRayOrigin(it.p, it.pErr, it.n, o - it.p)
    d = t - o
  # TODO I don't think ShadowEpsilon is needed
  #Ray(o: o, d: d, 1 - ShadowEpsilon, i.time, i.getMedium(d))
  Ray(o: o, d: d, tMax: 1, time: i.time, medium: i.getMedium(d))

# }}}
# {{{ SurfaceInteraction

proc shouldFlipNormal*(s: SurfaceInteraction): bool {.inline.} =
  notNil(s.shape) and (s.shape.reverseOrientation xor
                       s.shape.transformSwapsHandedness)


proc initSurfaceInteraction*(p, pErr, uv, wo, dpdu, dpdv, dndu, dndv: Vec3f,
                             time: FloatT,
                             shape: ref Shape): SurfaceInteraction =

  init(result.Interaction, p, pErr, n = cross(dpdu, dpdv).norm, wo,
       time, nil)

  result.uv = uv
  result.dpdu = dpdu
  result.dpdv = dpdv
  result.dndu = dndu
  result.dndv = dndv
  result.shape = shape

  result.shading.n = result.n
  result.shading.dpdu = result.dpdu
  result.shading.dpdv = result.dpdv
  result.shading.dndu = result.dndu
  result.shading.dndv = result.dndv

  if result.shouldFlipNormal:
    result.n *= -1
    result.shading.n *= -1


proc setShadingGeometry*(s: var SurfaceInteraction,
                         dpdus, dpdvs, dndus, dndvs: Vec3f,
                         orientationIsAuthoritative: bool) =
    s.n = cross(dpdus, dpdvs).norm

    if s.shouldFlipNormal:
      s.shading.n *= -1

    if orientationIsAuthoritative:
      s.n = faceforward(s.n, s.shading.n)
    else:
      s.shading.n = faceforward(s.shading.n, s.n)

    s.shading.dpdu = dpdus
    s.shading.dpdv = dpdvs
    s.shading.dndu = dndus
    s.shading.dndv = dndvs

# }}}

# vim: et:ts=2:sw=2:fdm=marker
