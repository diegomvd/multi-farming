package model
/**
Enumeration of the management strategies parametrized by their spatial clustering parameter. This parameter is fixed to
3.0 and yielding quite aggregated patterns that make evident the spatial agency difference of the two strategies.
Land sharing prefers being close with non-agricultural land leading to dispersion, but in some way "clustering" with
non-agricultural. Land sparing prefers clustering agricultural land together.
 */
enum MngStrategy(val clustering: Double):
  case LandSharing extends MngStrategy(3.0)
  case LandSparing extends MngStrategy(3.0)
end MngStrategy
