class World(eco: Graph[EcoUnit,Long],
            pln: Graph[PlanningUnit,Long],
            mng: Graph[ManagementUnit,Long],
            pop: Double,
            params: Parameters){

  private var world: World = buildWorld()

  def update(): World = {
    // the biophysical landscape is updated with the changes. the other landscapes
    // cpoy themselves with the new biophysical landscape added

    World.nextEvent() match {
      case ("recovery",_) => {
        val new_unit = EcoUnit() // copy the unit and copy the landscape
        val new_biophy_layer
        val new_planning_layer
        val new_management_layer
        World(new_biophy_layer,new_planning_layer,new_management_layer,this.population_layer,this.params)
      }
      case ("degradation",_) => {
        val new_unit = EcoUnit() // copy the unit and copy the landscape
        val new_biophy_layer
        val new_planning_layer
        val new_management_layer
        World(new_biophy_layer,new_planning_layer,new_management_layer,this.population_layer,this.params)
      }
      case ("fertility_loss",_) => {
        val new_unit = EcoUnit() // copy the unit and copy the landscape
        val new_biophy_layer
        val new_planning_layer
        val new_management_layer
        World(new_biophy_layer,new_planning_layer,new_management_layer,this.population_layer,this.params)
      }
      case ("low_intensity",_) => {
        val new_unit = EcoUnit() // copy the unit and copy the landscape
        val new_biophy_layer
        val new_planning_layer
        val new_management_layer
        World(new_biophy_layer,new_planning_layer,new_management_layer,this.population_layer,this.params)
      }
      case ("high_intensity",_) => {
        val new_unit = EcoUnit() // copy the unit and copy the landscape
        val new_biophy_layer
        val new_planning_layer
        val new_management_layer
        World(new_biophy_layer,new_planning_layer,new_management_layer,this.population_layer,this.params)
      }
      case ("birth",_) =>
      case ("death",_) =>

    }

    landscape = landscape.update
    landscape
  }

  def hasNext(): Boolean = {
    if (time > maxTime || humanPopulation.size = 0 || landscape.countNatural() == 0 || landscape.countNatural() == landscape.size*landscape.size) false
    else true
  }

  def nextEvent(): = {

    // spontaneous propensities
    val p_recovery = this.biophy_layer.recoveryPropensity(params.)
    val p_degradation = this.biophy_layer.degradationPropensity(params.)
    val p_fertility_loss = this.biophy_layer.fertilityLossPropensity(params.)

    // conversion propensity
    val p_management = this.management_layer.conversionPropensity(params.sensitivity*this.population_layer.resourceDemand())

    // demographic propensities
    val p_birth = this.population_layer.birthPropensity()
    val p_death = this.population_layer.deathPropensity(params. , this.biophy_layer.resourceProduction(params))

    // get sums before and don't sum p_management as you already know the total from
    // the resource deficit

    World.eventType(p_recovery,p_degradation,p_management,p_birth,p_death) match {
      case "recovery" => "recovery" -> selectEcoUnit(recovery,0.0,x_rand)
      case "degradation" => "degradation" -> selectEcoUnit(degradation,recovery.last,x_rand)
      case "fertility_loss" => "fertility_loss" -> selectEcoUnit(fertility_loss,recovery.last+degradation.last,x_rand)
      // select planning unit returns "low_intensity" -> Set(coords), "high-intensity" -> Set(coords)
      case "management" => selectPlanningUnit(selectManagementUnit(management,spontaneous.last,x_rand),this.management_landscape)
      case "birth" => "birth" -> 1
      case "death" => "death" -> 1
    }
  }

}

object World{

  def apply(args: Parameters): World = {

    val eco: Graph[EcoUnit,Long] = EcoLandscape.build(args.r,args.ecr)

    val npu: Int =
    val pln: Graph[PlnUnit,Long] = PlnLandscape.build(npu,args.r)

    val nmu: Int =
    val mng: Graph[MngUnit,Long] = MngLandscape.build(nmu,npu,args.fs)

    val res: Double =
      PlnLandscape.totalResources(pln,eco,args.y_es,args.z,args.his)
    val pop: Double = HumanPopulation.build(res)

    World(eco,pln,mng,pop)
  }

  def nextEvent(): String = {}

}
// val parameters = InputOutput.parseArgs(args)
