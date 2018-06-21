import common, geometry, interaction
import math, unittest, times

# {{{ Interaction

suite "core/interactionTests - Interaction":

  test "constructor":
    let i = initInteraction(p = vec3f(1,2,3), pError = vec3f(7,8,9),
                            n = vec3f(1,0,0), wo = vec3f(4,5,6), time = FloatT(1.5))
    check:
      i.p == vec3f(1,2,3)
      i.pError == vec3f(7,8,9)
      i.n == vec3f(1,0,0)
      i.wo == vec3f(4,5,6)
      i.time == 1.5
      i.mediumInterface == nil
      i.isSurfaceInteraction

# }}}
# {{{ SurfaceInteraction

suite "core/interactionTests - SurfaceInteraction":

  test "constructor":
    let s = initSurfaceInteraction(p = vec3f(1,2,3), pError = vec3f(7,8,9),
                                   uv = vec3f(5,4,3), wo = vec3f(4,5,6),
                                   dpdu = vec3f(11,22,33),
                                   dpdv = vec3f(44,55,66),
                                   dndu = vec3f(77,88,99),
                                   dndv = vec3f(-77,-88,-99),
                                   time = 1.5, shape = nil)
    check:
      s.p == vec3f(1,2,3)
      s.pError == vec3f(7,8,9)
      s.uv == vec3f(5,4,3)
      s.wo == vec3f(4,5,6)
      s.dpdu == vec3f(11,22,33)
      s.dpdv == vec3f(44,55,66)
      s.dndu == vec3f(77,88,99)
      s.dndv == vec3f(-77,-88,-99)
      s.time == 1.5
      s.shape == nil
      s.n == cross(s.dpdu, s.dpdv).norm
      s.isSurfaceInteraction

# }}}

# vim: et:ts=2:sw=2:fdm=marker
