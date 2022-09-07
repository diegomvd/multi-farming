package model
/**
 * Stochastic event types. Some are children of others but it was simpler to regroup all of them in the same enumeration
 * rather than implementing a hierarchical nested enumeration.
 * */
enum EventType:
  case Spontaneous, Conversion, Demographic, Recovery, Degradation, FertilityLoss, LowIntensityFertilityLoss,
  HighIntensityFertilityLoss, LowIntensityConversion, HighIntensityConversion, Birth, Death