import common, types

export types.Medium
export types.MediumInterface


proc isMediumTransition(m: MediumInterface): bool {.inline.} =
  m.inside != m.outside


# vim: et:ts=2:sw=2:fdm=marker
