// to compile: c++ mainALUMSS.cpp functionsALUMSS.cpp -o alumss-exec -lgsl -lgslcblas -lm -Wall -Weffc++ --std=c++17 -lstdc++fs
//         or: ./compileALUMSS.sh

/*
Main program for the agricultural land-use management spatial simulation (ALUMSS).
All the functions needed for execution are in functionsALUMS.h. Command to compile
can be found in the first line of the program.

0- EQUIVALENCE BETWEEN PARAMTER NAMES IN CODE AND IN PAPER
      CODE    -   PAPER   -   MEANING
      ye     -   beta      -   fraction of the low-intensity productivity explained by ES provision
      z     -   z       -   saturation exponent of ecosystem servicaes with area
      a       -   alpha   -   fraction of sparing farms
      w       -   omega   -   clustering/affinity parameter
      mS   -   sigma   -   average responsiveness to demand
      wS   -   not in the paper   -  responsiveness width between farms
      k0 - K_0 - number of households that can be sustained by a high-intensity agricultural cell
      dES - d_N - connectivity distance, i.e. distance at which ES flow
      nF - |F| - number of farms in the landscape
      sFL   -   rho_L   -   fertility loss sensitivtiy to ecosystem service provision
      sR    -   rho_R   -   recovery sensitivity to ecosystem service provision
      sD    -   rho_D   -   degradation sensitivity to ecosystem service provision

The sensitivities are growth or decay rates with respect to ecosystem
service provision of the transitions propensities (probabilities per unit time).
Note that in the code we use the growth or decay rates of the average time for a transition.
They are the inverse of the growth decay rates for the propensities.

1- INITIALIZATION
The agricultural landscape is initialized by specifying:
  - nSide landscape size-length in number of cells. Total number of cells is nSide*nSide
  - d0 the initial fraction of degraded land
  - a0 the initial fraction of agricultural land
  - a the fraction of sparing farms
  - w the clustering parameter: controls the level of clustering between same
    high-intensity agricultre and off integration between low-intensity and natural
The border conditions are periodic, hence there are not border effects in our
simulations and we use the Von-Neumann neighbourhood.

Human population density p is initialized at equilibrium with the resources
produced Y by the landscape as it is initialized: p(t=0) = Y(t=0)

The simulation can also be initialized via a CONF file that provides the exact
landscape configuration and population density. This can be used to continue
simulations that were too short or explore the impact of precise landscape
configurations.

2- SIMULATION
We use the Gillespie's Stochastic Simulation Algortihm to simulate landscape
dynamics in continuous time and obtain exact solutions of the Master Equation
describing the system's dynamics. This means that we randomly choose the next
land-use transition time and the land-use transition type given the per unit
time probability distributions of each land-use transition. On the contrary, the
human population density ODE is solved every timestep of length dtp.
The total simulated time is SimTime.

*/

#include "functionsALUMSS.h"

#include <stdio.h>
#include <string.h>
#include <fstream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <iostream> //Allows cin/cout
#include <sstream>
#include <stdlib.h> //Allows DOS Commands (I only use "CLS" to clear the screen)
#include <iterator>
#include <filesystem>
#include <numeric>
#include <ctime>
#include <math.h>
#include <chrono>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

using namespace std;

namespace fs = std::filesystem;

#define LOAD_CONF 0

/****************************************************************************
 MAIN PROGRAM
****************************************************************************/

// time at beginning
auto start = chrono::high_resolution_clock::now();
auto start2 = chrono::high_resolution_clock::now();

int main(int argc, const char * argv[]){

  /****************************************************************************
   PARAMETER DECLARATION
  ****************************************************************************/

  unsigned int nSide;  // lenght of the sides of the square landscape: number of cells=nSide*nSide
  double SimTime; // total simulation time

  double a0; //number of agricultural patches at beggining
  double d0; // number of degraded patches at beggining

  unsigned int nFarms; // number of farms in the landscape
  double a; // fraction of managers doing land-sparing
  double mS; // mean sensitivity to resource demand
  double wS; // relative width of the farms' sensitivity weight distribution

  double z; // saturation exponent of the ES-Area relationship
  double dES; // distance at which ES flow and 2 natural cells are considered connected from a EF point of view

  double ye; // fraction of low-intensity production accounted by ecosystem services
  double k0; // number of households that can be sustained by the production of an intense patch

  double sFL; // sensitivity of average fertility loss time with respect to decrease in ES provision
  double sR; // sensitivity of average land recovery time with respect to increases in ES provision
  double sD; // sensitivity of average land degradation time with respect to decrease in ES provision

  double dtSave; // timestep for saving data

  unsigned long int seed; // this is expid

  /****************************************************************************
   IMPORT PARAMETER VALUES
  ****************************************************************************/

  // if the program was exectuted with a command specifying parameter values
  if (argc>1) {
        char * pEnd;

        // time and space specifications for the simulation
        SimTime = strtod(argv[1], &pEnd);
        nSide = (unsigned int) strtod(argv[2], &pEnd);

        // initial values of agricultural land use and consumption
        a0 = strtod(argv[3],&pEnd);
        d0 = strtod(argv[4],&pEnd);

        // management parameters
        nFarms = (unsigned int) strtod(argv[5], &pEnd);
        a = strtod(argv[6], &pEnd);
        mS = strtod(argv[7], &pEnd);
        wS = strtod(argv[8], &pEnd);

        //ES-provision parameters
        z = strtod(argv[9], &pEnd);
        dES = strtod(argv[10], &pEnd);

        // agricultural production parameters
        ye = strtod(argv[11], &pEnd);
        k0 = strtod(argv[12], &pEnd);

        // spontaneous land-cover transitions
        sFL = strtod(argv[13], &pEnd);
        sR = strtod(argv[14], &pEnd);
        sD = strtod(argv[15], &pEnd);

        // save timespace just in case
        dtSave = strtod(argv[16], &pEnd);

        // save seed
        seed = (unsigned long int)abs(atoi(argv[17]));
  }

  /****************************************************************************
   CREATION OF DATA FILES
  ****************************************************************************/

  //creating data directory with today's date
  auto tt = time(nullptr);
  auto tm = *localtime(&tt);
  ostringstream oss;
  oss << put_time(&tm, "%d-%m-%Y");
  string str_date = oss.str();
  string dirname = "DATA_"+str_date;
  if (!(fs::exists(dirname))){
  }
  else{
    fs::create_directory(dirname); // commenting to see if avoids problem with sensitivity OM
  }

  //creating vector of strings to store all the input arguments
  vector<string> allArgs(argv,argv+argc);
  string filename;
  if(argc>1){
    filename = "_T_"+allArgs[1];
    filename += "_n_"+allArgs[2];
    filename += "_a0_"+allArgs[3];
    filename += "_d0_"+allArgs[4];
    filename += "_nF_"+allArgs[5];
    filename += "_a_"+allArgs[6];
    filename += "_mS_"+allArgs[7];
    filename += "_wS_"+allArgs[8];
    filename += "_z_"+allArgs[9];
    filename += "_dES_"+allArgs[10];
    filename += "_ye_"+allArgs[11];
    filename += "_k0_"+allArgs[12];
    filename += "_sFL_"+allArgs[13];
    filename += "_sR_"+allArgs[14];
    filename += "_sD_"+allArgs[15];
    filename += "_dtSave_"+allArgs[16];
    filename += "_seed_"+allArgs[17];
    filename+=".dat";
  }

  // this file is to output the land-metrics meanES, giniES, average distance
  // between natural patches to analyze the role of dES
  // string filename_OUT="DATA_OUT";
  // ofstream tofile_out(filename_OUT);
  // tofile_out.precision(5);
  // tofile_out.setf(ios::scientific,ios::floatfield);
  //
  // // string filename_AGRE=dirname+"/"+"DATA_AGRE"+filename;
  // // string filename_AGRE="DATA_AGRE"+filename;
  // string filename_AGRE="DATA_AGRE";
  // ofstream tofile_agre(filename_AGRE);
  // tofile_agre.precision(5);
  // tofile_agre.setf(ios::scientific,ios::floatfield);
  //
  // string filename_LAND=dirname+"/"+"DATA_LAND"+filename;
  // ofstream tofile_land(filename_LAND);
  // tofile_land.precision(5);
  // tofile_land.setf(ios::scientific,ios::floatfield);
  //
  // string filename_CLUS=dirname+"/"+"DATA_CLUS"+filename;
  // ofstream tofile_clus(filename_CLUS);
  // tofile_clus.precision(5);
  // tofile_clus.setf(ios::scientific,ios::floatfield);
  //
  // string filename_CONF=dirname+"/"+"DATA_CONF"+filename;
  // ofstream tofile_conf(filename_CONF);
  // tofile_conf.precision(5);
  // tofile_conf.setf(ios::scientific,ios::floatfield);

  // this file is to output the fragmentation metrics for PSE with sparing
  string filenameOut ="output.dat";
  ofstream tofile_output(filenameOut);
  tofile_output.precision(5);
  tofile_output.setf(ios::scientific,ios::floatfield);

  // string filename_SENS="sensitivityOut.dat";
  // ofstream tofile_sens(filename_SENS);
  // tofile_sens.precision(5);
  // tofile_sens.setf(ios::scientific,ios::floatfield);
  //
  // string filename_SPEX="SPEXOut.dat";
  // ofstream tofile_spex(filename_SPEX);
  // tofile_spex.precision(5);
  // tofile_spex.setf(ios::scientific,ios::floatfield);

  /****************************************************************************
   CREATING AND SEEDING RNG
  ****************************************************************************/
  gsl_rng * r = gsl_rng_alloc (gsl_rng_taus);
  gsl_rng_set(r, seed);

  /****************************************************************************
   VARIABLE DECLARATION AND INITIALISATION
  ****************************************************************************/

  // time
  double t=0;
  // counter for saving time
  double tSave=0;
  // gillespie's SSA time-step
  double dtg;
  // counter
  unsigned int i;
  // number of natural and degraded cells to stop the simulation in case the landscape is in an absorbant state
  unsigned int natCells, degCells;
  // minimum natural land in the simulation after the transient
  unsigned int nMin=0;
  // maximum natural land in the simulation after the transient
  unsigned int nMax=0;
  // idem for the population
  unsigned int pMin=0;
  unsigned int pMax=0;
  // switch to identify if the estimated transient time is over
  unsigned int firstTime=0;
  // variable to store the resource deficit
  double resourceDeficit;
  // variable to store the total propensity associated with agricultural managament
  double totalManagementPropensity;

  // this vector has only one member and it is the population
  vector<unsigned int> population;
  // vector containing the landscape state
  vector<unsigned int> landscape(nSide*nSide);
  // vector containing neighbours
  vector<vector<unsigned int>> neighbourMatrixES; // this is for the ES flow and natural connection threshold
  vector<vector<unsigned int>> neighbourMatrix; // this is for closest neighbours
  // vector containing the production of each cell initialized directly with the size
  vector<double> agriculturalProduction(nSide*nSide);
  // vector containing the production of each cell initialized directly with the size
  vector<double> ecosystemServices(nSide*nSide);
  // vector containing the natural connected components information
  vector<vector<int>> naturalComponents;
  // vector containing the transitions' propensities
  vector<double> propensityVector;
  // vector to store the number of transitions of each kind
  vector<unsigned int> countTransitions={0,0,0,0,0,0};
  // vector to store the farm information: which cells belong to which farm
  vector<vector<unsigned int>> farms;
  // vector to store the sensitivity to demand of each farm manager
  vector<double> farmSensitivity(nFarms);
  // vector to store the strategy of each farm, column1 is intensification and column2 clustering
  vector<vector<double>> farmStrategy;
  // vector to store the propensities of the spontaneous LUC transitions size number of cells * number of transitions (recovery, degradation, fertility loss)
  vector<double> spontaneousPropensities(nSide*nSide*3);
  // cumulative propensities of spontaneous transitions
  vector<double> spontaneousCumulativePropensities(nSide*nSide*3);
  // demographic propensities: birth and death processes from logistic dynamics
  vector<double> demographicPropensities(2);
  // cumulative
  vector<double> demographicCumulativePropensities(2);

  /****************************************************************************
   STATE INITIALISATION
  ****************************************************************************/

  // BY CONF FILE
  if (LOAD_CONF==1){
    cout << "Starting from conf file \nSide";
    ifstream conf_file("DATA_CONF");
    if(conf_file.is_open()) {

      // first extract the time and population
      double pop;
      if (!(conf_file >> t >> pop)){
        cout << "Error: mainALUMSS.cpp: time and population could not be loaded from CONF file. \nSide";
      }
      population.push_back(pop);
      SimTime+=t;
      // extracting by token moves forward the pointer in the file
      // now extract the landscape
      unsigned int state;
      i=0;
      while(conf_file >> state){
        landscape.push_back(state);
        i+=1;
      }
    }
  }
  else{ // WITH ARGV PARAMETERS

    getNeighbourMatrix(neighbourMatrixES,nSide,dES);
    getNeighbourMatrix(neighbourMatrix,nSide,1.1);
    initializeSES(farms,farmSensitivity,farmStrategy,landscape,population,naturalComponents,agriculturalProduction,ecosystemServices,neighbourMatrix,neighbourMatrixES,nSide,a0,d0,a,mS,wS,ye,k0,z,dES,nFarms,r);
    resourceDeficit = getResourceDeficit(agriculturalProduction, population,k0);
    totalManagementPropensity = getTotalManagementPropensity(landscape, farms, farmSensitivity, resourceDeficit);
    getDemographicPropensities(demographicPropensities,agriculturalProduction,population,k0);
    partial_sum(demographicPropensities.begin(),demographicPropensities.end(),demographicCumulativePropensities.begin());
    getSpontaneousPropensities(spontaneousPropensities,landscape,ecosystemServices,nSide,sR,sD,sFL);
    partial_sum(spontaneousPropensities.begin(),spontaneousPropensities.end(),spontaneousCumulativePropensities.begin());
  }

  /****************************************************************************
   BEGIN OF SIMULATION
  ****************************************************************************/

  // entering the time loop
  while(t<SimTime){


    /****************************************************************************
     STOPPING EXECUTION IF LANDSCAPE IS IN AN ABSORBANT STATE
    ****************************************************************************/
    natCells = 0;
    degCells = 0;
    for(i=0;i<landscape.size();i++){
      if(landscape[i]==0){
        natCells+=1;
      }
      else if(landscape[i]==1){
        degCells+=1;
      }
    }
    if(natCells==landscape.size() || degCells==landscape.size()){
      if(population[0]<1){
        break;
      }
    }

    /****************************************************************************
     CALCULATING THE MINIMUM AND MAXIMUM VARAIBLE VALUES TO GET CYCLES' AMPLITUDE
    ****************************************************************************/
    if(t>SimTime/6){ // let some time for a transient before the cycles

      if (firstTime==0){ // to initialize the value after the transient
        nMax = natCells;
        nMin = natCells;
        pMax = population[0];
        pMin = population[0];
        firstTime=1;
      }

      // i only save the natural area and population for instance
      if (natCells>nMax){
        nMax=natCells; // reset the maximum value
      }
      if (natCells<nMin){
        nMin=natCells; // reset the minimum value
      }
      if (population[0]>pMax){
        pMax=population[0]; // reset the maximum value
      }
      if (population[0]<pMin){
        pMin=population[0]; // reset the minimum value
      }
    }

    /****************************************************************************
     SAVING DATA
    ****************************************************************************/

    if(t>=tSave){
      saveAggregatedMetrics(tofile_output, t, population, landscape, agriculturalProduction, naturalComponents, neighbourMatrixES, ecosystemServices, nSide);

      // saveLandscape(tofile_land,t,landscape);
      // saveComponents(tofile_clus,t,landscape,naturalComponents);

      tSave+=dtSave;
      // cout << "P : " << population[0] << ", N : " << natCells << ", D : " << degCells << "\n";
    }

    // time until next transition
    dtg=-1/(totalManagementPropensity+spontaneousCumulativePropensities.back()+demographicCumulativePropensities.back())*log(gsl_rng_uniform(r));

    /****************************************************************************
     CHOOSING NEXT EVENT, EITHER LUC TRANSITION OR DEMOGRAPHIC
    ****************************************************************************/

    // making the LUC transition happen, spontaneous propensities are updated inside
    solveSSA(landscape,naturalComponents,ecosystemServices, agriculturalProduction, farms,neighbourMatrix,neighbourMatrixES,population,farmSensitivity,farmStrategy,spontaneousPropensities,spontaneousCumulativePropensities,demographicPropensities,demographicCumulativePropensities,totalManagementPropensity,resourceDeficit,nFarms,nSide,ye,k0,sR,sD,sFL,z,dES,r,countTransitions);

    // update total management propensity
    resourceDeficit = getResourceDeficit(agriculturalProduction,population,k0);
    totalManagementPropensity = getTotalManagementPropensity(landscape, farms, farmSensitivity, resourceDeficit);

    // increment the time and update timestep for ODE solving
    t+=dtg;

  }

  saveAggregatedMetrics(tofile_output, t, population, landscape, agriculturalProduction, naturalComponents, neighbourMatrixES, ecosystemServices, nSide);
  // saveLandStructFertLoss(tofile_output, population, landscape, neighbourMatrix, ecosystemServices);
  // print the landscape and the farms to check if it is ok
  // unsigned int ix,jx,lx;
  // cout << "Natural Landscape:\n";
  // for(ix=0;ix<nSide;++ix){
  //   for(jx=0;jx<nSide;++jx){
  //     lx = nSide*ix+jx;
  //     cout << landscape[lx] << " ";
  //   }
  //   cout << "\n";
  // }
  // vector<unsigned int> politicalLandscape(nSide*nSide);
  // vector<unsigned int>::iterator it;
  // for(ix=0;ix<farms.size();++ix){
  //   for(it=farms[ix].begin();it!=farms[ix].end();++it){
  //     politicalLandscape[*it]=ix;
  //   }
  // }
  // cout << "Political Landscape:\n";
  // for(ix=0;ix<nSide;++ix){
  //   for(jx=0;jx<nSide;++jx){
  //     lx = nSide*ix+jx;
  //     cout << politicalLandscape[lx] << " ";
  //   }
  //   cout << "\n";
  // }

  // saving CONF file to re start other simulations from this point
  // tofile_conf << t << " " << population[0];
  // for(i=0 ; i<landscape.size() ; i++){
  //   tofile_conf << " " << landscape[i];
  // }
  // tofile_conf << "\nSide";

  // saving files so ifdtsave was largest than execution time one gets the final
  // values for every output we look at
  // ofstream tofile_sens("DATA_SENSITIVITY");
  // tofile_sens.precision(5);
  // tofile_sens.setf(ios::scientific,ios::floatfield);
  // saveAggregated(tofile_sens,t,population,landscape,agriculturalProduction,naturalComponents,ecosystemServices,nSide,2,(double)nMax/landscape.size(),(double)nMin/landscape.size(),pMax,pMin);


  // saving fragmentation and es metrics for sampling
  // double nFrag = getNumberOfFragments(naturalComponents);
  // double maxSize = getMaximumFragmentSize(naturalComponents);
  // double meanEdgeToArea = getMeanEdgeToAreaRatio(naturalComponents,landscape,neighbourMatrixES);
  // vector<double> metricsES(2);
  // getESMetrics(metricsES,ecosystemServices);
  // double natFraction = getLandCoverArea(landscape,0);
  // tofile_output << population[0] << " " << natFraction << "\n";

  // natCells = 0;
  // for(i=0;i<landscape.size();i++){
  //   if(landscape[i]==0){
  //     natCells+=1;
  //   }
  // }

  // careful I commented the standard output!!
  // saveAggregated(tofile_agre,t,population,landscape,agriculturalProduction,naturalComponents,ecosystemServices,nSide,2,(double)nMax/landscape.size(),(double)nMin/landscape.size(),pMax,pMin);
  // saveLandscapeMetrics(tofile_out,nSide,landscape,ecosystemServices);

  // saveLandscape(tofile_land,t,landscape);
  // saveComponents(tofile_clus,t,landscape,naturalComponents);

  // saving output for sensitivity analysis
  // saveSensitivityOutput(tofile_sens,nSide,1,population,naturalComponents,landscape,ecosystemServices);
  // saving output for pattern exploration space and origin exploration space
  // saveAggregated(tofile_spex,t,population,landscape,agriculturalProduction);

  auto stop = chrono::high_resolution_clock::now();
  auto duration = chrono::duration_cast<chrono::minutes>(stop - start);
  // cout << "simulation time " << t << "\n";
  cout << "total execution time " << duration.count() << endl;

  return 0;
}
