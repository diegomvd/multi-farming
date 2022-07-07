class World(eco: Graph[EcoUnit,Long],
            pln: Graph[PlanningUnit,Long],
            mng: Graph[ManagementUnit,Long],
            pop: Double,
            args: Parameters){

  private var world: World = apply(args)

  def update(): World = {
    // the biophysical landscape is updated with the changes. the other landscapes
    // cpoy themselves with the new biophysical landscape added

    World.nextEvent() match {
      case ("recovery",_) => {
        val new_unit = EcoUnit() // copy the unit and copy the landscape
        val new_biophy_layer
        World(new_biophy_layer,new_planning_layer,new_management_layer,this.population_layer,this.params)
      }
      case ("degradation",_) => {
        val new_unit = EcoUnit() // copy the unit and copy the landscape
        val new_biophy_layer
        World(new_biophy_layer,new_planning_layer,new_management_layer,this.population_layer,this.params)
      }
      case ("fertility_loss",_) => {
        val new_unit = EcoUnit() // copy the unit and copy the landscape
        val new_biophy_layer
        World(new_biophy_layer,new_planning_layer,new_management_layer,this.population_layer,this.params)
      }
      case ("low_intensity",_) => {
        val new_unit = EcoUnit() // copy the unit and copy the landscape
        val new_biophy_layer
        World(new_biophy_layer,new_planning_layer,new_management_layer,this.population_layer,this.params)
      }
      case ("high_intensity",_) => {
        val new_unit = EcoUnit() // copy the unit and copy the landscape
        val new_biophy_layer
        World(new_biophy_layer,new_planning_layer,new_management_layer,this.population_layer,this.params)
      }
      case ("birth",_) =>
      case ("death",_) =>

    }
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

  def updated(pop: (Double,Double),
              rec: (VertexRDD[Double],Double),
              deg: (VertexRDD[Double],Double),
              flo: (VertexRDD[Double],Double),
              mng: Double,
              eco: Graph[EcoUnit,Long],
              pln: Graph[PlnUnit,Long],
              mng: Graph[MngUnit,Long]): World = {

    val tpop: Double = pop._1 + pop._2
    val tspont: Double = rec._2 + deg._2 + flo._2

    val x_rand: Double = rnd.nextDouble(tpop+tspont+mng)

    selectEventGeneralType(x_rand, spont, mng, pop) match {
      case "Population" => {
        selectBirthOrDeath(x_rand,pop) match {
          case "Birth" => World(eco,pln,mng,pop+1,args)
          case "Death" => World(eco,pln,mng,pop-1,args)
        }
      }
      case "Spontaneous" => {
        selectSpontaneous(x_rand,(rec._2,deg._2,flo._2)) match {
          case "Recovery" => {
            val vid = S3Utils.selectVId(x_rand,rec._1)
            val new_eco = EcoLandscape.updated(vid,"Natural")
          }
          case "Degradation" => {
            val vid = S3Utils.selectVId(x_rand,deg._1)
            val new_eco = EcoLandscape.updated(vid,"Degraded")
          }
          case "FertilityLoss" => {
            val vid = S3Utils.selectVId(x_rand,flo._1)
            val new_eco = EcoLandscape.updated(vid,"Degraded")
          }
        }
        World(new_eco,pln,mng,pop,args)
      }
      case "Management"

    }

  }

  /**
  * @param x_rand is the random number thrown to sample the distributions
  * @param spont is the total spontaneous propensity + the population one
  * @param mng is the total management propensity + spontaneous + population
  * @param pop is the total population propensity
  * @return a string identifying the general type of event
  */
  def selectEventGeneralType(x_rand: Double,
                             spont: Double,
                             mng: Double,
                             pop: Double): String = {
    x_rand match {
      case x if x < pop => "Population"
      case x if x < spont => "Spontaneous"
      case x if x < mng => "Management"
      case other => println(s"Cannot select a transition, propensity upper bound in the selector is larger than expected.")
    }
  }

  /**
  * @param x_rand is the random number thrown to sample the distributions
  * @param prop contains the birth and death propensities in field 1 and 2 respectively
  * @return a string with the type of human population event
  */
  def selectBirthOrDeath(x_rand: Double,
                         prop: (Double, Double)): String = {
    x_rand match {
      case x if x < prop._1 => "Birth"
      case x if x < prop._2 => "Death"
      case other => println(s"selectBirthOrDeath: Cannot select a transition, propensity upper bound in the selector is larger than expected.")
    }
  }

  /**
  * @param x_rand is the random number thrown to sample the distributions
  * @param prop contains the recovery, degradation and fertility loss propensities in field 1,2 and 3 respectively
  * @return a string with the type of spontaneous land cover transition
  */
  def selectSpontaneous(x_rand: Double,
                        prop: (Double,Double,Double)): String = {
    x_rand match {
      case x if x < prop._1 => "Recovery"
      case x if x < prop._2 => "Degradation"
      case x if x < prop._3 => "FertilityLoss"
      case other => println(s"selectSpontaneous: Cannot select a transition, propensity upper bound in the selector is larger than expected.")
  }
}
// val parameters = InputOutput.parseArgs(args)
