import core
import types
import geometry
import medium
import shape


{.experimental.}

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

proc p(i: Interaction): Vec3f {.inline.} = i.p
proc pError(i: Interaction): Vec3f {.inline.} = i.pError
proc n(i: Interaction): Vec3f {.inline.} = i.n
proc wo(i: Interaction): Vec3f {.inline.} = i.wo
proc time(i: Interaction): FloatT {.inline.} = i.time

proc mediumInterface(i: Interaction): ref MediumInterface {.inline.} =
  i.mediumInterface

proc isSurfaceInteraction(i: Interaction): bool {.inline.} =
  i.n != vec3f(0,0,0)

# }}}
# {{{ SurfaceInteraction

proc shouldFlipNormal(s: SurfaceInteraction): bool {.inline.} =
  notNil(s.shape) and (s.shape.reverseOrientation xor
                       s.shape.transformSwapsHandedness)


proc initSurfaceInteraction(p, pError, uv, wo, dpdu, dpdv, dndu, dndv: Vec3f,
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


proc setShadingGeometry(s: var SurfaceInteraction,
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

# {{{ Tests

when isMainModule:
  block:  # Inte
    let i = initInteraction(p = vec3f(1,2,3), pError = vec3f(7,8,9),
                            n = vec3f(1,0,0), wo = vec3f(4,5,6), time = 1.5)
    assert i.p == vec3f(1,2,3)
    assert i.pError == vec3f(7,8,9)
    assert i.n == vec3f(1,0,0)
    assert i.wo == vec3f(4,5,6)
    assert i.time == 1.5
    assert i.mediumInterface == nil

    assert i.isSurfaceInteraction

  block:  # SurfaceInteraction
    let s = initSurfaceInteraction(p = vec3f(1,2,3), pError = vec3f(7,8,9),
                                   uv = vec3f(5,4,3), wo = vec3f(4,5,6),
                                   dpdu = vec3f(11,22,33),
                                   dpdv = vec3f(44,55,66),
                                   dndu = vec3f(77,88,99),
                                   dndv = vec3f(-77,-88,-99),
                                   time = 1.5, shape = nil)
    assert s.p == vec3f(1,2,3)
    assert s.pError == vec3f(7,8,9)
    assert s.uv == vec3f(5,4,3)
    assert s.wo == vec3f(4,5,6)
    assert s.dpdu == vec3f(11,22,33)
    assert s.dpdv == vec3f(44,55,66)
    assert s.dndu == vec3f(77,88,99)
    assert s.dndv == vec3f(-77,-88,-99)
    assert s.time == 1.5
    assert s.shape == nil
    assert s.n == cross(s.dpdu, s.dpdv).norm

    assert s.isSurfaceInteraction

# }}}

# vim: et:ts=2:sw=2:fdm=marker
