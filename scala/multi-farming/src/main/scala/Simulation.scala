object Simulation{

  final case class Parameters(radius:                       Int,
                              maxTime:                      Double,
                              agriculturalFraction:         Double,
                              degradedFraction:             Double,
                              strategicScale:               Int,
                              managementScale:              Int,
                              sparingFraction:              Double,
                              sensitivityResourceDemand:   Double,
                              scalingExponentES:            Double,
                              yieldContributionES:          Double,
                              highIntensityUnitSupport:     Double,
                              sensitivitySoilFertilityLoss: Double,
                              sensitivityLandRecovery:      Double,
                              sensitivityLandDegradation:   Double,
                              savingTimeStep:               Double,
                              seed:                         Long
                            ){}

  def runSimulation(radius:                       Int = 5,
                    maxTime:                      Double = 1,
                    agriculturalFraction:         Double = 0.2,
                    degradedFraction:             Double = 0.1,
                    strategicScale:               Int = 1,
                    managementScale:              Int = 1,
                    sparingFraction:              Double = 0.5,
                    sensitivityResourceDemand:    Double = 1.0,
                    scalingExponentES:            Double = 0.2,
                    yieldContributionES:          Double = 0.3,
                    highIntensityUnitSupport:     Double = 1.0, // number of househoulds supported by 1 high intensity agricultural unit
                    sensitivitySoilFertilityLoss: Double = 1.0,
                    sensitivityLandRecovery:      Double = 1.0,
                    sensitivityLandDegradation:   Double = 1.0,
                    savingTimeStep:               Double = 1,
                    seed:                         Long = 123456789) = Output {

    val params: Parameters = Parameters(radius,
                                        maxTime,
                                        agriculturalFraction,
                                        degradedFraction,
                                        strategicScale,
                                        management scale,
                                        sparingFraction,
                                        sensitivityResourceDemand,
                                        scalingExponentES,
                                        yieldContributionES,
                                        highIntensityUnitSupport,
                                        sensitivitySoilFertilityLoss,
                                        sensitivityLandRecovery,
                                        sensitivityLandDegradation,
                                        savingTimeStep,
                                        seed)

    World(params).run.metrics
  }
}
