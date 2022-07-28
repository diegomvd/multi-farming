object Simulation{

  final case class Parameters(radius:                       Int,
                              maxTime:                      Double,
                              agriculturalFraction:         Double,
                              degradedFraction:             Double,
                              strategicScale:               Int,
                              managementScale:              Int,
                              sparingFraction:              Double,
                              sensitivityResourceDemand:    Double,
                              scalingExponentES:            Double,
                              yieldContributionES:          Double,
                              highIntensityUnitSupport:     Double,
                              sensitivitySoilFertilityLoss: Double,
                              sensitivityLandRecovery:      Double,
                              sensitivityLandDegradation:   Double,
                              savingTimeStep:               Double,
                              seed:                         Long
                            ){}

  /**
  This function is used to run a dynamic simulation of the social-ecological system.
  The inputs are all the model parameters that are used to create an instance of the
  Parameters class that is feed to a World constructor. Then the world is initialized
  and its evolution simulated via the run function. The final world is analyzed
  to obtain the desired metrics and output a DynSimMetrics object containing them
  for analisis with OpenMole.
  TODO: time series analisis requires to progressively save in memory an attribute
  of the world that stores the sequence of dynamic values, this might be useable
  for integer values such as population and land cover but probably too big to
  store the biophysical landscape's composition graph. Temporal analysis of such graph
  could be done inside the simulation by progressivley using raphtery on the newly
  generated worlds inside the run function of the world class.
  */
  def dynamicSimulation(radius:                       Int = 5,
                        maxTime:                      Double = 1.0,
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
                        savingTimeStep:               Double = 1.0,
                        seed:                         Long = 123456789) = DynSimMetrics {

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

    World(params).run().dynamicSimulationMetrics()
  }

  /**
  This function is used to simulate landscape generation process from the model
  parameters. Inputs are model parameters that influence the generated landscape
  to use as initial state for the simulation. This function constructs a world object
  and calls the run function whose only effect is to initialize the world as maxtime
  is set to 0.0. Then a series of landscapes metrics are computed and sent to
  OpenMole for analysis. Motivation is to understand the spectrum of possible landscapes
  generated that are used as initial states.
  */
  def landscapeGeneration(radius:                       Int = 5,
                          agriculturalFraction:         Double = 0.2,
                          degradedFraction:             Double = 0.1,
                          strategicScale:               Int = 1,
                          managementScale:              Int = 1,
                          sparingFraction:              Double = 0.5,
                          scalingExponentES:            Double = 0.2,
                          yieldContributionES:          Double = 0.3,
                          highIntensityUnitSupport:     Double = 1.0, // number of househoulds supported by 1 high intensity agricultural unit
                          seed:                         Long = 123456789) = LandscapeMetrics {

    val params: Parameters = Parameters(radius,
                                        maxTime = 0.0,
                                        agriculturalFraction,
                                        degradedFraction,
                                        strategicScale,
                                        management scale,
                                        sparingFraction,
                                        sensitivityResourceDemand = 0.0,
                                        scalingExponentES,
                                        yieldContributionES,
                                        highIntensityUnitSupport,
                                        sensitivitySoilFertilityLoss = 0.0,
                                        sensitivityLandRecovery = 0.0,
                                        sensitivityLandDegradation = 0.0,
                                        savingTimeStep = 0.0,
                                        seed)

    World(params).run().landscapeMetrics()
  }
}
