import math, unittest, times
import core


# {{{ Interaction

suite "core/interactionTests - Interaction":

  test "constructor":
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
