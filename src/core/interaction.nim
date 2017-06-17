# {{{ Interaction

proc init(self: var Interaction, p, pError, n, wo: Vec3f, time: FloatT,
          mediumInterface: ref MediumInterface) =
  self.p = p
  self.pError = pError
  self.n = n
  self.wo = wo
  self.time = time
  self.mediumInterface = mediumInterface

proc initInteraction*(p, pError, n, wo: Vec3f,
    time: FloatT, mediumInterface: ref MediumInterface = nil): Interaction =
  init(result, p, pError, n, wo, time, mediumInterface)

proc p*(i: Interaction): Vec3f {.inline.} = i.p
proc pError*(i: Interaction): Vec3f {.inline.} = i.pError
proc n*(i: Interaction): Vec3f {.inline.} = i.n
proc wo*(i: Interaction): Vec3f {.inline.} = i.wo
proc time*(i: Interaction): FloatT {.inline.} = i.time
proc mediumInterface*(i: Interaction): ref MediumInterface {.inline.} =
  i.mediumInterface

proc isSurfaceInteraction*(i: Interaction): bool {.inline.} =
  i.n != vec3f(0,0,0)

# }}}
# {{{ SurfaceInteraction

proc shouldFlipNormal*(s: SurfaceInteraction): bool {.inline.} =
  notNil(s.shape) and (s.shape.reverseOrientation xor
                       s.shape.transformSwapsHandedness)


proc initSurfaceInteraction*(p, pError, uv, wo, dpdu, dpdv, dndu, dndv: Vec3f,
                             time: FloatT,
                             shape: ref Shape): SurfaceInteraction =

  init(result.Interaction, p, pError, n = cross(dpdu, dpdv).norm, wo,
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


proc uv*(s: SurfaceInteraction): Vec3f {.inline.} = s.uv
proc dpdu*(s: SurfaceInteraction): Vec3f {.inline.} = s.dpdu
proc dpdv*(s: SurfaceInteraction): Vec3f {.inline.} = s.dpdv
proc dndu*(s: SurfaceInteraction): Vec3f {.inline.} = s.dndu
proc dndv*(s: SurfaceInteraction): Vec3f {.inline.} = s.dndv
proc shape*(s: SurfaceInteraction): ref Shape {.inline.} = s.shape


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
