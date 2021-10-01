/*
Implementation of all the functions required for the simulation and used in the
main program.
*/

#include "functionsALUMSS.h"

#include <boost/config.hpp>
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <utility>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#include <math.h>
#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

using namespace std;

/*
1-Helper functions
2-Calculation of Ecosystem Service provision
3-Calculation of events' propensities
4-Initialization functions
5-ODEs and solver
*/


///////////////////////////////////////////////////////////////////////////////
// 1- Helper functions:
//       - getNeighbourMatrix
//       - getNeighbours
//       - getNeighboursState
///////////////////////////////////////////////////////////////////////////////
void getNeighbourMatrix(vector<vector<unsigned int>> &neighbourMatrix, unsigned int nSide, double d)
{

  /*fills a vector containing the neighbours indexes for each patch*/

  unsigned int ix,jx,xi,yi,xj,yj,dx,dy,manhattanDist;

  neighbourMatrix.clear();
  neighbourMatrix.resize(nSide*nSide);

  for (ix=0; ix<neighbourMatrix.size(); ++ix){
    xi = (unsigned int)(ix%nSide);
    yi = (unsigned int)(ix/nSide);
    for (xj=0; xj<nSide; xj++){
      dx=(unsigned int)abs((int)(xi-xj));
      // calculating cyclic distances to account for periodic borders
      if (dx>nSide/2){
        dx=nSide-dx;
      }
      for (yj=0; yj<nSide; yj++){
        dy=(unsigned int)abs((int)(yi-yj));
        // calculating cyclic distances to account for periodic borders
        if (dy>nSide/2){
          dy=nSide-dy;
        }
        manhattanDist = dx+dy;
        if (manhattanDist<=d and manhattanDist>0){
          jx = xj + yj*nSide;
          neighbourMatrix[ix].push_back(jx);
        }
      }
    }
  }


  return;
}

void getNeighbours(vector<unsigned int> &neighboursList, const vector<vector<unsigned int>> &neighbourMatrix, unsigned int i)
{
  neighboursList = neighbourMatrix[i];
  return;
}

void getNeighboursState(vector<unsigned int> &neighboursState, const vector<vector<unsigned int>> &neighbourMatrix, const vector<unsigned int> &landscape, unsigned int i, unsigned int state)
{
  /*
  fills the vector neighboursState so that it contains the indexes of all the
  closest neighbours of i in a given state. the landscape is passed as a constant
  reference so that the vector cannot be modified by the function in main
  */

  /*
  getting the neighbours indexes in neighbour_list vector
  */
  vector<unsigned int> neighboursList;
  getNeighbours(neighboursList,neighbourMatrix,i);

  /*
  getting the index of neighbours in the wanted state
  */
  unsigned long ix;
  for (ix=0 ; ix<neighboursList.size() ; ++ix){
    if (landscape[neighboursList[ix]] == state) {
      neighboursState.push_back( neighboursList[ix] );
    }
  }

  return;
}

void getNeighboursStateInf(vector<unsigned int> &neighboursState, const vector<vector<unsigned int>> &neighbourMatrix, const vector<unsigned int> &landscape, unsigned int i, unsigned int maxState)
{
  /*
  fills the vector neighboursState so that it contains the indexes of all the
  closest neighbours of i in a state smaller than maxState.
  the landscape is passed as a constant reference so that the vector cannot be
  modified by the function in main
  */

  /*
  getting the neighbours indexes in neighbour_list vector
  */
  vector<unsigned int> neighboursList;
  getNeighbours(neighboursList,neighbourMatrix,i);

  /*
  getting the index of neighbours in the wanted state
  */
  unsigned long ix;
  for (ix=0 ; ix<neighboursList.size() ; ++ix){
    if (landscape[neighboursList[ix]] < maxState) {
      neighboursState.push_back( neighboursList[ix] );
    }
  }

  return;
}

void getNeighboursStateSup(vector<unsigned int> &neighboursState, const vector<vector<unsigned int>> &neighbourMatrix, const vector<unsigned int> &landscape, unsigned int i, unsigned int minState)
{
  /*
  fills the vector neighboursState so that it contains the indexes of all the
  closest neighbours of i in a state larger than minState.
  the landscape is passed as a constant reference so that the vector cannot be
  modified by the function in main
  */

  /*
  getting the neighbours indexes in neighbour_list vector
  */
  vector<unsigned int> neighboursList;
  getNeighbours(neighboursList,neighbourMatrix,i);

  /*
  getting the index of neighbours in the wanted state
  */
  unsigned long ix;
  for (ix=0 ; ix<neighboursList.size() ; ++ix){
    if (landscape[neighboursList[ix]] > minState) {
      neighboursState.push_back( neighboursList[ix] );
    }
  }

  return;
}

////////////////////////////////////////////////////////////////////////////////
// 2- Calculation of Ecosystem Service provision:
//       - getNaturalConnectedComponents
//       - updateNCCadding
//       - updateNCCremoving
//       - getEcosystemServiceProvision
////////////////////////////////////////////////////////////////////////////////

void getNaturalConnectedComponents(vector<vector<int>> &naturalComponents, const vector<unsigned int> &landscape, double distanceConnection)
{
  /*
  fills a vector where each member is a vector containing the indexes of all the
  natural patches belonging to the same cluster
  */

  vector<unsigned int> naturalPatches;
  unsigned int manhattanDist;
  unsigned int i, j;
  int xi, xj, yi, yj;
  unsigned int dx, dy;


  unsigned int n = (unsigned int) sqrt(landscape.size());
  // clearing natural components vector for refilling
  naturalComponents.clear();

  /*
  get the list of natural patches in the landscape
  */
  for(i=0 ; i<landscape.size() ; ++i){
    if (landscape[i]==0){
      naturalPatches.push_back(i);
    }
  }

  /*
  create an undirected graph with the set of natural patches to calculate
  the connected components. to estimate whether two patches are connected
  we calculate the manhattan distance between them
  */

  using namespace boost;
  {
    typedef adjacency_list< vecS, vecS, undirectedS > Graph;
    Graph G;


    for(i=0 ; i<naturalPatches.size() ; ++i){
      add_edge(i,i,G);
      // converting 1-D coordinates to 2-D
      xi=naturalPatches[i]%n;
      yi=(int)naturalPatches[i]/n;
      for(j=0 ; j<i ; ++j){
        // converting 1-D coordinates to 2-D
        xj=naturalPatches[j]%n;
        yj=(int)naturalPatches[j]/n;
        // calculating manhattan distance between points
        dx=abs(xi-xj);
        dy=abs(yi-yj);
        // calculating cyclic distances to account for periodic borders
        if (dx>n/2){
          dx=n-dx;
        }
        if (dy>n/2){
          dy=n-dy;
        }
        //
        manhattanDist=dx+dy;
        if ( manhattanDist<distanceConnection ){
          add_edge(i, j, G);
        }
      }
    }

    /*
    initializing the vector containing the components and calculating components
    */
    vector<int> component(num_vertices(G));
    int num = connected_components(G, &component[0]);

    /*
    converting the nodes indexes into actual landscape coordinates
    */
    naturalComponents.resize(num);
    for (i=0 ; i<naturalComponents.size() ; ++i){
      for (j=0; j<component.size(); j++){
        if(component[j]==(int)i){
          naturalComponents[i].push_back(naturalPatches[j]);
        }
      }
    }

  }
  return;
}

void updateNCCadding(vector<vector<int>> &naturalComponents, const vector<vector<unsigned int>> &neighbourMatrix, const vector<unsigned int> &landscape, unsigned int i)
{

  /*
  note: updateNCCadding with non-local neighbourhood doesnt pose a problem as
  long as there is a neighbourMatrix for natural cells that accounts for the
  non-locality
  */
  vector<unsigned int> neighboursNatural;
  getNeighboursState(neighboursNatural,neighbourMatrix,landscape,i,0); // state 0 is natural
  vector<int> newNaturalComponent, newNaturalComponent2;
  newNaturalComponent.push_back(i);



  if(neighboursNatural.size()==0){ //no natural neighbour: simplest case, just create a new component
      naturalComponents.push_back(newNaturalComponent); // add it to the list
  }
  else{

    vector<unsigned int>::iterator it1;
    vector<vector<int>>::iterator it2;
    vector<int>::iterator it3;
    vector<vector<vector<int>>::iterator> toErase;

    /*
    first traversing the natural components and then the neighbours guarantees that
    pointers to components are located in a sorted way. this is key to ensure
    that the erasing process doesn't mess up with the memory.
    */

    unsigned int neighboursFound = 0;
    for(it2=naturalComponents.begin();it2!=naturalComponents.end();it2++){ // traverse all the components
      for(it1=neighboursNatural.begin();it1!=neighboursNatural.end();it1++){ // traverse all the natural neigbhours of new natural cell
        if( find( it2->begin(), it2->end(), *it1) != it2->end() ){ // found a neighbour in this component
          neighboursFound+=1; // store the amount of natural neighbours found  to stop search once all are
          if ( find( toErase.begin(), toErase.end(), it2 ) == toErase.end() ){  // if the currently considered component hasn't been aded in the erase/merging list, then add it
            toErase.push_back(it2);
          }
        }
      }
      if(neighboursFound==neighboursNatural.size()){ // end the search if all neighbours were located
        break;
      }
    }



    // if there is a single component in toErase just add i to that component, no need of merging
    if (toErase.size()>0 && toErase.size()<2){
      toErase[0]->push_back(i);
    }
    else if(toErase.size()>1){ // if there are more, erase them and push back the merged ones
      // erasing components that are going to be merged

      unsigned int ix;
      for(ix=0;ix<toErase.size();ix++){
        for(it3=toErase[ix]->begin();it3!=toErase[ix]->end();it3++){
          newNaturalComponent.push_back(*it3);
        }
      }

      /*now erase the components: traverse erase vector backwards to be sure of
      addressing the correct bits of memory erasing first the furthest pointers*/
      for(ix=toErase.size();ix>=1;--ix){
        naturalComponents.erase(toErase[ix-1]);
      }
      /*now add the new natural component*/
      naturalComponents.push_back(newNaturalComponent);

    }
    else{ // toErase is empty, in which case there is an error in the code
      cout << "Error: toErase size is " << toErase.size() << " but toErase cannot be empty\n";
    }

  }



  return;
}

void updateNCCremoving(vector<vector<int>> &naturalComponents, const vector<unsigned int> &landscape, int l, double distanceConnection)
{
  /*
  note: updateNCCremoving with non-local natural neighbourhoods requires only
  having a parameter for the connection distance
  */

  unsigned int n = (unsigned int) sqrt(landscape.size());

  // find cluster of cell l
  // these iterators are to traverse the naturalComponents
  vector<vector<int>>::iterator it1;
  vector<int>::iterator it2;

  // iterating over natural components and checking to which natural component
  // belonged the natural cell that needs to be removed.

  //this iterator is to store the component to which the removed cell belonged
  vector<vector<int>>::iterator itComp;
  // iterate over components
  for(it1=naturalComponents.begin();it1!=naturalComponents.end();it1++){
    // find whether natural cell l is in this natural component
    it2=find(it1->begin(), it1->end(),l);
    // if it is then erase natural cell l and store the component
    if(it2!=it1->end()){
      it1->erase(it2);
      itComp =it1;
      break;
    }
  }

  // get all the natural cells of the component where the removed cell belonged
  // we need to know if the removal of the cell caused the fragmentation of the
  // component in several pieces
  vector<unsigned int> naturalPatches;
  for (it2=itComp->begin();it2!=itComp->end();it2++){
    naturalPatches.push_back(*it2);
  }

  // erase concerned cluster from naturalComponents, we will calculate the new
  // component(s) and push them back. worst case scenario the component was not
  // fragmentedand the following operation was useless computing time
  naturalComponents.erase(itComp);

  //get connected components from the naturalPatches
  unsigned int manhattanDist;
  unsigned int i, j;
  int xi, xj, yi, yj;
  unsigned int dx, dy;

  /*
  create an undirected graph with the set of natural patches to calculate
  the connected components. to estimate whether two patches are connected
  we calculate the manhattan distance between them, this uses the same method
  than getNaturalConnectedComponents
  */

  using namespace boost;
  {
    typedef adjacency_list< vecS, vecS, undirectedS > Graph;
    Graph G;


    for(i=0 ; i<naturalPatches.size() ; ++i){
      add_edge(i,i,G);
      // converting 1-D coordinates to 2-D
      xi=naturalPatches[i]%n;
      yi=(int)naturalPatches[i]/n;
      for(j=0 ; j<i ; ++j){
        // converting 1-D coordinates to 2-D
        xj=naturalPatches[j]%n;
        yj=(int)naturalPatches[j]/n;
        // calculating manhattan distance between points
        dx=abs(xi-xj);
        dy=abs(yi-yj);
        // calculating cyclic distances to account for periodic borders
        if (dx>n/2){
          dx=n-dx;
        }
        if (dy>n/2){
          dy=n-dy;
        }
        //
        manhattanDist=dx+dy;
        if ( manhattanDist<2 ){
          add_edge(i, j, G);
        }
      }
    }

    /*
    initializing the vector containing the components and calculating components
    */
    vector<int> component(num_vertices(G));
    int num = connected_components(G, &component[0]);

    // adding the new components to naturalComponents
    vector<int> newComponent;
    for(i=0; i<(unsigned int)num; i++){
      for(j=0; j<component.size(); j++){
        if((int)i==component[j]){
          newComponent.push_back(naturalPatches[j]);
        }
      }
      naturalComponents.push_back(newComponent);
      newComponent.clear();
    }
  }

  return;
}

void getEcosystemServiceProvision(vector<double> &ecosystemServices, const vector<vector<int>> &naturalComponents, const vector<vector<unsigned int>> &neighbourMatrix, const vector<unsigned int> &landscape, double z)
{
  /*
  returns the exposure to the wanted state of patch i. currently it is only used
  for the exposure to nature. the exposure to nature scales with biodiversity
  hence like a SAR, where the area is the total natural area in contact with
  patch i
  */

  double area;
  double ecosystemServiceProvision;
  vector<unsigned int> neighboursState;
  unsigned long i,ix,jx;
  unsigned int nNeighbours;

  nNeighbours = neighbourMatrix[0].size();

  for(i=0;i<landscape.size();i++){

    ecosystemServiceProvision=0;
    /*
    getting the state neighbours indexes in neighboursState vector
    */
    getNeighboursState(neighboursState,neighbourMatrix,landscape,i,0); // state 0 is natural

    /*
    calculate the area of each of the neighbour's component
    */
    for (ix=0;ix<neighboursState.size();ix++){
      // for each of the natural neighbours check their cluster membership
      for (jx=0; jx<naturalComponents.size(); jx++){
        // check if neighbour belongs to cluster jx
        if (find( naturalComponents[jx].begin(),naturalComponents[jx].end(),neighboursState[ix]) != naturalComponents[jx].end()){
          area=(double)naturalComponents[jx].size()/landscape.size();
          ecosystemServiceProvision+=(double) pow(area,z)/nNeighbours;
          break;
        }
      }
    }

    ecosystemServices[i]=ecosystemServiceProvision;
    neighboursState.clear();
  }

  return;
}

////////////////////////////////////////////////////////////////////////////////
// 3- Calculation of events' propensities:
//       - esSaturationFunction
//       - getAgriculturalProduction
//       - getResourceDeficit
//       - getSpontaneousPropensities
//       - getAgroPropensity
//       - getAbandonmentPropensity
//       - getPropensityVector
////////////////////////////////////////////////////////////////////////////////

void getAgriculturalProduction(vector<double> &agriculturalProduction, const vector<unsigned int> &landscape, const vector<double> &ecosystemServices, double ye)
{
  /*
  returns the total agricultural production for a given "landscape" and
  minimum yield "y". the minimum yield is for a cropped patch with only
  non natural neighbours. natural neighbours raise yield.
  */

  // initialize the vector with production 0 everywhere
  fill(agriculturalProduction.begin(),agriculturalProduction.end(),0.0);

  unsigned int ix;
  for (ix=0 ; ix<landscape.size() ; ++ix){
    if(landscape[ix]==2){ // cropped patches
      // putting baseline production 0.5 as a test...
      agriculturalProduction[ix] = (1-ye) + ye*ecosystemServices[ix];
    }
    else if(landscape[ix]==3){ //intense
      agriculturalProduction[ix] =  1 ;
    }
  }
  return;
}

double getResourceDeficit(const vector<double> &agriculturalProduction, const vector<unsigned int> &population, double k0)
{
  /*
  Given a total production the function returns the resource deficit experienced
  by the population. The resource deficit translates into demand for agricultural
  expansion or intensification
  */
  double totalAgriculturalProduction;
  double resourceDeficit;

  totalAgriculturalProduction=accumulate(agriculturalProduction.begin(),agriculturalProduction.end(),0.0,plus<double>());
  if(population[0]>0){
    resourceDeficit = (double) population[0]/k0 - totalAgriculturalProduction;
  }
  else{
    resourceDeficit=0;
  }
  return resourceDeficit;
}

double getTotalManagementPropensity(const vector<unsigned int> &landscape, const vector<vector<unsigned int>> &farms, const vector<double> &farmSensitivity, double resourceDeficit)
{

  double totalManagementPropensity=0;
  unsigned int ix;
  vector<unsigned int>::const_iterator it;
  double maxSensitivity = 0;

  if (resourceDeficit>0){

    // if there is no natural land left, then there is no possible land conversion
    if( find( landscape.begin(), landscape.end(), 0) != landscape.end() ){

      // get the maximum sensitivity across the farms that still have room for conversion
      for(ix=0;ix<farms.size();++ix){
        for(it=farms[ix].begin();it!=farms[ix].end();++it){
          if(landscape[*it]==0){ // i.e. there's a natural cell
            if(farmSensitivity[ix]>maxSensitivity){
              maxSensitivity = farmSensitivity[ix];
            }
            // move to the next farm
            break;
          }
        }
      }

      totalManagementPropensity = resourceDeficit * maxSensitivity;
    }
    // else just return 0 since there cannot be any conversion
  }

  return totalManagementPropensity;
}

void getDemographicPropensities(vector<double> &demographicPropensities, const vector<double> &agriculturalProduction, vector<unsigned int> &population, double k0)
{

  double totalAgriculturalProduction = accumulate(agriculturalProduction.begin(),agriculturalProduction.end(),0.0,plus<double>());

  demographicPropensities[0] = 0;
  demographicPropensities[1] = 0;

  if(population[0]>0){
    if (totalAgriculturalProduction > 0){
      // this is to avoid a division by zero
      demographicPropensities[0] = (double) population[0];
      demographicPropensities[1] = (double) population[0]*population[0]/totalAgriculturalProduction/k0;
    }
    else{
      // if there is no production just set population to zero, not the best solution but it is hard
      // to do better with logistic growth
      population[0]=0;
    }
  }

  return;
}

void getSpontaneousPropensities(vector<double> &spontaneousPropensities, const vector<unsigned int> &landscape, const vector<double> &ecosystemServices, unsigned int nSide, double sR, double sD, double sFL)
{
  /*
  Calculate the propensities of recovery, degradation and fertility loss and store
  them in a single vector in that order
  */

  unsigned int ix,jx;

  // initialize the spontaneous propensity with zeros
  fill(spontaneousPropensities.begin(),spontaneousPropensities.end(),0.0);
  // replace the non-null values
  for (ix=0 ; ix<landscape.size() ; ++ix){
    // first calculate the recovery propensity
    if (landscape[ix]==1){
      spontaneousPropensities[ix] = sR * ecosystemServices[ix];
    }
    //now calculate the degradation propensity
    if (landscape[ix]==0){
      jx = (unsigned int) (nSide*nSide + ix);
      spontaneousPropensities[jx] = sD*(1-ecosystemServices[ix]);
    }
    // finally calculate the fertility loss propensity
    if (landscape[ix]==2 || landscape[ix]==3){
      jx = (unsigned int) (2*nSide*nSide + ix);
      spontaneousPropensities[jx] = sFL*(1-ecosystemServices[ix]);
    }

  }
    return;
}

void solveSSA(vector<unsigned int> &landscape, vector<vector<int>> &naturalComponents, vector<double> &ecosystemServices, vector<double> &agriculturalProduction, const vector<vector<unsigned int>> &farms, const vector<vector<unsigned int>> &neighbourMatrix, const vector<vector<unsigned int>> &neighbourMatrixES, vector<unsigned int> &population, const vector<double> &farmSensitivity, const vector<vector<double>> &farmStrategy, vector<double> &spontaneousPropensities, vector<double> &spontaneousCumulativePropensities, vector<double> &demographicPropensities, vector<double> &demographicCumulativePropensities, double totalManagementPropensity, double resourceDeficit, unsigned int nFarms, unsigned int nSide, double ye, double k0, double sR, double sD, double sFL, double z, double dES, gsl_rng  *r, vector<unsigned int> &countTransitions)
{

  vector<double> farmPropensity(nFarms,0);
  vector<double> farmCumulativePropensity(nFarms);
  vector<unsigned int> availableCells;
  vector<unsigned int> agriculturalNeighbours;
  vector<unsigned int> naturalNeighbours;
  vector<double> conversionPropensity, conversionCumulativePropensity;
  unsigned int ix,jx;
  unsigned int transition,cell;
  double conversionCumSum = 0;
  double normFactor = 0;
  vector<double>::iterator it0;
  vector<unsigned int>::const_iterator it1;
  vector<unsigned int>::iterator it2;


  // random number to pick the transition and the cell
  double xRand = gsl_rng_uniform(r)*(totalManagementPropensity + spontaneousCumulativePropensities.back() + demographicCumulativePropensities.back());

  if(xRand < totalManagementPropensity){// if it is a management transition
    // select the farm
    ix=0;
    for(ix=0;ix<farms.size();++ix){
      // if there is no natural land in a farm then there is no conversion
      for(it1 = farms[ix].begin(); it1!=farms[ix].end(); ++it1){
        if(landscape[*it1]==0){
          farmPropensity[ix]=farmSensitivity[ix];
          normFactor += farmSensitivity[ix];
          break;
        }
      }
    }
    for(it0=farmPropensity.begin();it0!=farmPropensity.end();++it0){
      *it0 = *it0/normFactor*totalManagementPropensity;
    }
    partial_sum(farmPropensity.begin(),farmPropensity.end(),farmCumulativePropensity.begin());
    ix=0;
    while (xRand > farmCumulativePropensity[ix]){
      ix++;
    }
    // now select the cell to transform
    // iterate over the cells belonging to the selected farm
    for(it1=farms[ix].begin();it1!=farms[ix].end();++it1){
      // check if the cell is natural
      if(landscape[*it1]==0){
        // add the cell
        availableCells.push_back(*it1);
      }
    }

    // clear and resize the probability of conversion vector
    conversionPropensity.resize(availableCells.size());
    conversionCumulativePropensity.resize(availableCells.size());
    // check the strategy of the farm
    if(farmStrategy[ix][0]==0){ // if it is sharing
      jx=0; // counter to fill probConversion
      for (it2=availableCells.begin();it2!=availableCells.end();++it2){
        // calculate the number of natural neighbours
        naturalNeighbours.clear();
        getNeighboursState(naturalNeighbours,neighbourMatrix,landscape,*it2,0);
        conversionPropensity[jx]=pow( max(0.1 , (double)naturalNeighbours.size() ) , farmStrategy[ix][1] );
        conversionCumSum += conversionPropensity[jx];
        jx+=1;
      }
      for(jx=0;jx<conversionPropensity.size();++jx){
        // creating the 0-1 weights accordign to clustering and multiplying by total farm cumulative sensitivity to get converion propensity
        conversionPropensity[jx] = conversionPropensity[jx] * farmCumulativePropensity[ix] / conversionCumSum;
      }
    }
    else{ // if there is clustering
      jx=0; // counter to fill probConversion
      for (it2=availableCells.begin();it2!=availableCells.end();++it2){
        // calculate the number of agricultural neigbhours. In this occasion,
        // lowInt and highInt are considered both with the same weighting into
        // the calculation
        agriculturalNeighbours.clear();
        getNeighboursStateSup(agriculturalNeighbours,neighbourMatrix,landscape,*it2,1);
        conversionPropensity[jx]=pow( max(0.1 , (double)agriculturalNeighbours.size() ) , farmStrategy[ix][1] );
        conversionCumSum += conversionPropensity[jx];
        jx+=1;
      }
      for(jx=0;jx<conversionPropensity.size();++jx){
        // creating the 0-1 weights accordign to clustering and multiplying by total farm cumulative sensitivity to get converion propensity
        conversionPropensity[jx] = conversionPropensity[jx] * farmCumulativePropensity[ix] / conversionCumSum;
      }
    }

    jx=0;
    it2=availableCells.begin();
    partial_sum(conversionPropensity.begin(),conversionPropensity.end(),conversionCumulativePropensity.begin());
    while (xRand > conversionCumulativePropensity[jx]){
      jx++;
      it2++;
    }

    // update the landscape according to the strategy: either natural to low-intense, or intense
    if(farmStrategy[ix][0]==0){
      landscape[*it2]=2;
      countTransitions[4]+=1;
    }
    else if(farmStrategy[ix][0]==1){
      landscape[*it2]=3;
      countTransitions[5]+=1;
    }

    // update natural components
    updateNCCremoving(naturalComponents,landscape,*it2,dES);
    // update ecosystem service provision
    getEcosystemServiceProvision(ecosystemServices,naturalComponents,neighbourMatrix,landscape,z); // update ES
    // update the propensity of spontaneous transitions
    getSpontaneousPropensities(spontaneousPropensities,landscape,ecosystemServices,nSide,sR,sD,sFL);
    partial_sum(spontaneousPropensities.begin(),spontaneousPropensities.end(),spontaneousCumulativePropensities.begin());
    // updating agricultural production after the management LUC transition
    getAgriculturalProduction(agriculturalProduction, landscape, ecosystemServices, ye);

  }
  else if(xRand < totalManagementPropensity + spontaneousCumulativePropensities.back()){ // if it is a spontaneous transition
    ix=0;
    while(xRand > totalManagementPropensity + spontaneousCumulativePropensities[ix]){
      ix++;
    }

    transition=(unsigned int) (ix/(nSide*nSide));
    cell=(unsigned int) (ix%(nSide*nSide));

    if (transition==0){ // land recovery

      landscape[cell]=0; // update the landscape
      countTransitions[0]+=1; // update the transitions' count
      updateNCCadding(naturalComponents,neighbourMatrixES,landscape,cell); // update the NCC
      getEcosystemServiceProvision(ecosystemServices,naturalComponents,neighbourMatrix,landscape,z); // update ES
      // update the propensity of spontaneous transitions
      getSpontaneousPropensities(spontaneousPropensities,landscape,ecosystemServices,nSide,sR,sD,sFL);
      partial_sum(spontaneousPropensities.begin(),spontaneousPropensities.end(),spontaneousCumulativePropensities.begin());

    }
    else if(transition==1){ // land degradation

      landscape[cell]=1;
      countTransitions[1]+=1;
      updateNCCremoving(naturalComponents,landscape,cell,dES);
      getEcosystemServiceProvision(ecosystemServices,naturalComponents,neighbourMatrix,landscape,z);
      // update the propensity of spontaneous transitions
      getSpontaneousPropensities(spontaneousPropensities,landscape,ecosystemServices,nSide,sR,sD,sFL);
      partial_sum(spontaneousPropensities.begin(),spontaneousPropensities.end(),spontaneousCumulativePropensities.begin());

    }
    else if(transition==2){ // fertility loss

      if(landscape[cell]==2){ // if it was low-intense agriculture

        landscape[cell] = 0;
        countTransitions[2]+=1;

        updateNCCadding(naturalComponents,neighbourMatrixES,landscape,cell);
        getEcosystemServiceProvision(ecosystemServices,naturalComponents,neighbourMatrix,landscape,z);
        // update the propensity of spontaneous transitions
        getSpontaneousPropensities(spontaneousPropensities,landscape,ecosystemServices,nSide,sR,sD,sFL);
        partial_sum(spontaneousPropensities.begin(),spontaneousPropensities.end(),spontaneousCumulativePropensities.begin());

      }
      else if(landscape[cell]==3){ // if it was high-intense agriculture

        landscape[cell] = 1;
        countTransitions[3]+=1;
        // update the propensity of spontaneous transitions
        spontaneousPropensities[ix]=0; // transition that just occurred has now null propensity
        spontaneousPropensities[cell] = sR*ecosystemServices[cell]; // applying the recovery transition formula
        partial_sum(spontaneousPropensities.begin(),spontaneousPropensities.end(),spontaneousCumulativePropensities.begin());

      }
      else{
        cout << "Error: functionsALUMSS.cpp : solveSSA: fertility loss of non-agricultural cell. Land cover : "<< landscape[cell] <<"\n";
      }

    }
    else{
      cout << "Error: functionsALUMSS.cpp : solveSSA: spontaneous transition "<< transition << " does not exist.\n";
    }
    // updating agricultural production after the spontaneous LUC transition
    getAgriculturalProduction(agriculturalProduction, landscape, ecosystemServices, ye);
  }
  else{
    // it is a population event
    if(xRand < totalManagementPropensity + spontaneousCumulativePropensities.back() + demographicCumulativePropensities[0]){
      // it is a birth
      population[0]+=1;
    }
    else{
      // it is a death
      population[0]-=1;
    }
    // demographic propensities are updated below
  }
  // updating demographic propensities after LUC transition or population event
  getDemographicPropensities(demographicPropensities,agriculturalProduction,population,k0);
  partial_sum(demographicPropensities.begin(),demographicPropensities.end(),demographicCumulativePropensities.begin());

  return;
}


////////////////////////////////////////////////////////////////////////////////
// 4- Initialization functions:
//       - initializeLandscape
//       - initializePopulation
//       - initializeSES
////////////////////////////////////////////////////////////////////////////////

void initializeVoronoiFarms( vector<vector<unsigned int>> &farms, const vector<vector<unsigned int>> &neighbourMatrix, unsigned int nSide, unsigned int nFarms, gsl_rng  *r)
{
  /*
  This function initializes the political subdivisions representing each farm
  managed by a different agent. The information is stored in vector<vector<unsigned int>> &farms
  where each "line" correspond to a farm and the cells' indexes belonging to it
  are stored in the columns. To create the subdivisions we do Voronoi tesselation
  of the landscape. The process is as follows:
  1- randomly place nFarms points with uniform probability in the landscape
  2- each point is the "seed" of the voronoi cell. Perform radial growth process
    from each seed until fronts of different farms collide.
  */

  unsigned long ix,jx;
  vector<double> voronoiSeedProbability(nSide*nSide,1.0);
  vector<double> voronoiSeedCumulativeProbability;
  voronoiSeedCumulativeProbability.resize(nSide*nSide);
  vector<unsigned int> voronoiSeeds(nFarms);
  vector<unsigned int>::iterator it;
  double xRand;

  // initialize shape of farms vector
  farms.clear();
  farms.resize(nFarms);

  // iterate over the number of farms to randomly place each voronoi farm seed
  // on the landscape
  for(ix=0;ix<nFarms;++ix){
    // calculate the cumulative sum of each cell's probability to be chosen
    partial_sum(voronoiSeedProbability.begin(),voronoiSeedProbability.end(),voronoiSeedCumulativeProbability.begin(), plus<double>());
    jx=0;
    // draw a random number between [0,nSide*nSide[ to uniformly choose one of
    // the cells as a seed
    xRand = gsl_rng_uniform(r)*voronoiSeedCumulativeProbability.back();
    while ( xRand > voronoiSeedCumulativeProbability[jx] ){
      // as long as the condition isn't fulfill pass to the next cell by incrementing jx
      jx++ ;
    }
    // once out of the loop, asociate a probability 0 to the cell that has already
    // been attributed and store the cell index in the voronoiSeeds vector
    voronoiSeedProbability[jx]=0.0;
    voronoiSeeds[ix]=jx;
  }

  // perform a continuous time stochastic process for the radial growth departing
  // from the seeds

  // create the political landscape initializing the seeds
  // the value nFarms indicate the cell hasn't been colonized
  vector<unsigned int> politicalLandscape(nSide*nSide,nFarms);
  // initializing farm count at 0 so that we reach nFarms-1 for the colonized
  ix=0;
  for(it = voronoiSeeds.begin(); it != voronoiSeeds.end(); ++it){
    politicalLandscape[*it] = ix;
    farms[ix].push_back(*it);
    ix++;
  }

  vector<double> propensitiesRadialGrowth(nSide*nSide);
  vector<double> cumulativePropensitiesRadialGrowth(nSide*nSide);
  vector<unsigned int> neighbours;

  vector<double> farmNeighboursPropensity(nFarms);
  vector<double> farmNeighboursCumulativePropensity(nFarms);

  unsigned int nColonized = nFarms;
  unsigned int farmId;
  // iterate until the whole landscape is colonized


  while (nColonized<politicalLandscape.size()){
    // initialize to 0 the propensity vector for the radial growth
    fill(propensitiesRadialGrowth.begin(),propensitiesRadialGrowth.end(),0.0);

    // calculate propensities
    for(ix = 0; ix < politicalLandscape.size(); ix++){
      // check if cell ix is non-colonized
      if(politicalLandscape[ix]==nFarms){
        // get all the colonized neighbours
        getNeighboursStateInf(neighbours, neighbourMatrix, politicalLandscape, ix, nFarms);
        // update the propensity = number of colonized neighbours
        propensitiesRadialGrowth[ix]=neighbours.size();
        neighbours.clear();
      }
    }

    // get the cumulative propensity
    partial_sum(propensitiesRadialGrowth.begin(),propensitiesRadialGrowth.end(),cumulativePropensitiesRadialGrowth.begin(), plus<double>());

    ix=0;
    // choose the cell at which the colonization happens
    xRand = gsl_rng_uniform(r)*cumulativePropensitiesRadialGrowth.back();
    while(xRand > cumulativePropensitiesRadialGrowth[ix]){
        ix++;
    }

    // get neighbours of cell ix and choose which farmer colonized the cell
    neighbours.clear();
    getNeighboursStateInf(neighbours, neighbourMatrix, politicalLandscape, ix, nFarms);
    fill(farmNeighboursPropensity.begin(),farmNeighboursPropensity.end(),0.0);
    // iterate neighbours and identify potential farmer colonizers
    for(it = neighbours.begin(); it != neighbours.end(); ++it){
      // check to which farm belongs the neighbour
      farmId = politicalLandscape[*it];
      // increment the amount of neighbours from a given farm
      farmNeighboursPropensity[farmId]+=1;
    }

    // select the colonizing farm
    partial_sum(farmNeighboursPropensity.begin(),farmNeighboursPropensity.end(),farmNeighboursCumulativePropensity.begin(), plus<double>());
    jx=0;
    xRand = gsl_rng_uniform(r)*farmNeighboursCumulativePropensity.back();
    while(xRand > farmNeighboursCumulativePropensity[jx]){
      jx++;
    }

    // update newly-colonized cell
    politicalLandscape[ix] = jx;

    // update counter to control end of simulation
    nColonized+=1;

    // store the newly colonized cell in the farms vector
    farms[jx].push_back(ix);
  }
  return;
}

void initializeFarmStrategy( vector<vector<double>> &farmStrategy, unsigned int nFarms, double a, gsl_rng *r)
{
  /*
  for instance we only consider the fraction of farms on each strategy and
  not the spatial arrangement of the strategies (i.e. clustering of sharing farms)
  hence strategies are attributed with uniform probability over space
  this means we just attribute a strategy to each farm irrespective of the
  voronoi tesselation
  */

  unsigned int ix, jx, nSparing;
  vector<double> probSparing(nFarms,1);
  vector<double> cumProbSparing(nFarms);

  farmStrategy.clear();
  farmStrategy.resize(nFarms);
  nSparing = a*nFarms;
  double xRand;


  // initialize all the farms as sharing and then change nSparing to sparing
  for(ix=0;ix<farmStrategy.size();++ix){
    farmStrategy[ix].push_back(0); // intensification
    farmStrategy[ix].push_back(5); // clustering
  }

  for(ix=0;ix<nSparing;++ix){
    // choose in which Farm to spare
    partial_sum(probSparing.begin(),probSparing.end(),cumProbSparing.begin());
    jx=0;
    xRand= gsl_rng_uniform(r)*cumProbSparing.back();
    while( xRand > cumProbSparing[jx]){
      jx++;
    }
    // update probSparing
    probSparing[jx]=0;
    // update farmStrategy
    farmStrategy[jx][0]=1; // intensification
  }

  return;
}

void initializeFarmSensitivity( vector<double> &farmSensitivity, unsigned int nFarms, double mS, double wS, gsl_rng *r)
{
  /*
  the weigths for each farm sensitivity are drawn uniformly with widht relative to the mean = wS
  */

  vector<double>::iterator it;
  double sensitivity;
  double wSEffective = wS*(nFarms-1)/nFarms;

  // fill the vector with weights drawn from a uniform distribution with mean 1 and width wS
  for(it=farmSensitivity.begin();it!=farmSensitivity.end();++it){
    sensitivity = gsl_ran_flat(r,mS*(1-wSEffective),mS*(1+wSEffective));
    *it = sensitivity;
  }

  return;
}

void initializeLandscape( vector<unsigned int> &landscape, const vector<vector<unsigned int>> &farms, const vector<double> &farmSensitivity, const vector<vector<double>> &farmStrategy, const vector<vector<unsigned int>> &neighbourMatrix, unsigned int nSide, unsigned int nFarms, double a0, double d0, gsl_rng  *r)
{
  /*
  initializes the landscape given a fraction of initial agricultural patches a0
  and degraded patches d0 considering the farm distribution and farmers strategies
  */

  //unsigned int number_cropped_patches = 1;

  unsigned long ix,jx,lx;
  vector<unsigned int>::const_iterator it1;
  vector<unsigned int>::iterator it2;
  // total number of agricultural cells
  unsigned int na0=(unsigned int) (a0*nSide*nSide);
  // calculate the number of cells that need to be degraded
  unsigned int nd0=(unsigned int) (d0*nSide*nSide);

  // initialize a completely natural landscape
  fill(landscape.begin(),landscape.end(),0);

  // begin by filling the degraded cells with uniform probability over space
  // initialize the degradation probability and calculate cumulative probabilitiy
  vector<double> probDegradation(nSide*nSide,1.0);
  vector<double> cumProbDegradation(nSide*nSide);
  // this vector contains the indexes of all the natural patches
  vector<double> probConversion;
  vector<double> cumProbConversion;
  // vector storing the cells available for conversion given a farm
  vector<unsigned int> availableCells;
  // vector to store neibhours in clustering calculation
  vector<unsigned int> agriculturalNeighbours;
  vector<unsigned int> naturalNeighbours;
  // farm propensities
  vector<double> farmPropensity(nFarms);
  vector<double> farmCumulativePropensity(nFarms);
  double xRand;

  // start the degradation process until all the required cells are degraded

  for (ix=0; ix<nd0; ++ix){
    jx=0;

    // select to be degraded cell with uniform spatial distribution
    partial_sum(probDegradation.begin(),probDegradation.end(),cumProbDegradation.begin());
    xRand = gsl_rng_uniform(r)*cumProbDegradation.back();
    while ( xRand > cumProbDegradation[jx]){
      jx++;
    }

    // update the state of the landscape
    landscape[jx]=1;
    // update the degradation probability
    probDegradation[jx]=0.0;
  }

  // adding of degraded cells done

  // now add the agricultural cells: this is done in two steps, first choose the
  // farm based on the farm's sensitivity to demand and then perform an action
  // based on the farm's strategy
  for (ix=0; ix<na0; ++ix){

    // discard the farms with no natural land
    jx=0;
    // reinitialize the farm propensities at 0
    fill(farmPropensity.begin(),farmPropensity.end(),0.0);
    for(jx=0;jx<farms.size();++jx){
      // if there is no natural land in a farm then there is no conversion
      for(it1 = farms[jx].begin(); it1!=farms[jx].end(); ++it1){
        if(landscape[*it1]==0){
          farmPropensity[jx]=farmSensitivity[jx];
          break;
        }
      }
    }

    // select the farm
    partial_sum(farmPropensity.begin(),farmPropensity.end(),farmCumulativePropensity.begin());
    jx=0;
    xRand = gsl_rng_uniform(r)*farmCumulativePropensity.back();
    while (xRand > farmCumulativePropensity[jx]){
      jx++;
    }

    // now that the farm has been selected check which cells are available
    // for conversion
    // iterate over the cells belonging to the selected farm
    availableCells.clear();
    for(it1=farms[jx].begin();it1!=farms[jx].end();++it1){
      // check if the cell is natural
      if(landscape[*it1]==0){
        // add the cell
        availableCells.push_back(*it1);
      }
    }
    // clear and resize the probability of conversion vector
    probConversion.clear();
    probConversion.resize(availableCells.size());
    cumProbConversion.clear();
    cumProbConversion.resize(availableCells.size());
    // check the strategy of the farm
    if(farmStrategy[jx][0]==0){ // if it is sharing
      lx=0; // counter to fill probConversion
      for (it2=availableCells.begin();it2!=availableCells.end();++it2){
        // calculate the number of natural neighbours
        naturalNeighbours.clear();
        getNeighboursState(naturalNeighbours,neighbourMatrix,landscape,*it2,0);
        probConversion[lx]=pow( max(0.1 , (double)naturalNeighbours.size() ) , farmStrategy[jx][1] ) ;
        lx+=1;
      }
    }
    else{ // if there is clustering
      lx=0; // counter to fill probConversion
      for (it2=availableCells.begin();it2!=availableCells.end();++it2){
        // calculate the number of agricultural neigbhours. In this occasion,
        // lowInt and highInt are considered both with the same weighting into
        // the calculation
        agriculturalNeighbours.clear();
        getNeighboursStateSup(agriculturalNeighbours,neighbourMatrix,landscape,*it2,1);
        probConversion[lx]=pow( max(0.1 , (double)agriculturalNeighbours.size() ) , farmStrategy[jx][1] ) ;
        lx+=1;
      }
    }

    lx=0;

    it2=availableCells.begin();
    partial_sum(probConversion.begin(),probConversion.end(),cumProbConversion.begin());
    xRand = gsl_rng_uniform(r)*cumProbConversion.back();
    while ( xRand > cumProbConversion[lx]){
      lx++;
      it2++;
    }

    // update the landscape
    if(farmStrategy[jx][0]==0){
      landscape[*it2]=2;
    }
    else if(farmStrategy[jx][0]==1){
      landscape[*it2]=3;
    }
    else{
      cout << "Error: initializeLandscape: farmStrategy[jx][0] has an unauthorized value.";
    }

  }

  return;
}

void initializePopulation( vector<unsigned int> &population, const vector<double> &agriculturalProduction, double k0)
{
  /*given an agricultural production, it sets the population at an equilibrium
  level
  */
  unsigned int ix;
  double totalAgriculturalProduction=0;
  for(ix=0; ix<agriculturalProduction.size(); ++ix){
    totalAgriculturalProduction+=agriculturalProduction[ix];
  }
  population.push_back((unsigned int)totalAgriculturalProduction*k0);

  return;
}

void initializeSES( vector<vector<unsigned int>> &farms, vector<double> &farmSensitivity, vector<vector<double>> &farmStrategy, vector<unsigned int> &landscape, vector<unsigned int> &population, vector<vector<int>> &naturalComponents, vector<double> &agriculturalProduction, vector<double> &ecosystemServices, vector<vector<unsigned int>> &neighbourMatrix, vector<vector<unsigned int>> &neighbourMatrixES, unsigned int nSide, double a0, double d0, double a, double mS, double wS, double ye, double k0, double z, double dES, unsigned int nFarms, gsl_rng  *r)
{

  initializeVoronoiFarms(farms,neighbourMatrix,nSide,nFarms,r);
  initializeFarmStrategy(farmStrategy,nFarms,a,r);
  initializeFarmSensitivity(farmSensitivity,nFarms,mS,wS,r);
  initializeLandscape(landscape,farms,farmSensitivity,farmStrategy,neighbourMatrix,nSide,nFarms,a0,d0,r);
  getNaturalConnectedComponents(naturalComponents,landscape,dES);
  getEcosystemServiceProvision(ecosystemServices,naturalComponents,neighbourMatrix,landscape,z);
  getAgriculturalProduction(agriculturalProduction, landscape, ecosystemServices, ye);
  initializePopulation(population,agriculturalProduction,k0);

  return;

}

////////////////////////////////////////////////////////////////////////////////
// 6- Outputs:
//       - getRadiusOfGyration
//       - saveAggregated
//       - saveLandscape
//       - saveComponents
////////////////////////////////////////////////////////////////////////////////

double getRadiusOfGyration(const vector<int> &naturalComponent, unsigned int nSide)
{

  vector<int>::const_iterator it;
  unsigned int xi,yi;
  double radiusOfGyration=0;
  double xMean,yMean;

  if (naturalComponent.size()>0){
    xMean=0;
    yMean=0;
    // iterate over the cells of the natural component to get their mean position
    for(it=naturalComponent.begin();it!=naturalComponent.end();++it){
      xi=(int)*it%nSide;
      yi=(int)*it/nSide;
      xMean+=xi;
      yMean+=yi;
    }
    xMean/= (double) naturalComponent.size();
    yMean/= (double) naturalComponent.size();

    // now iterate again to calculate the radius of gyration
    for(it=naturalComponent.begin();it!=naturalComponent.end();++it){
      xi=(int)*it%nSide;
      yi=(int)*it/nSide;
      radiusOfGyration+=sqrt((xi-xMean)*(xi-xMean)+(yi-yMean)*(yi-yMean));
    }
    radiusOfGyration/= (double) naturalComponent.size();
  }

  return radiusOfGyration;
}

double getCorrelationLength(const vector<vector<int>> &naturalComponents, const vector<unsigned int> &landscape, unsigned int nSide)
{
  vector<vector<int>>::const_iterator it;
  double correlationLength = 0;

  if (naturalComponents.size()>0){
    for(it=naturalComponents.begin();it!=naturalComponents.end();++it){
      correlationLength += (*it).size() * getRadiusOfGyration(*it,nSide);
    }
    correlationLength /= (double) (getLandCoverArea(landscape,0)*nSide);
  }

  return correlationLength;
}

double getMeanEdgeToAreaRatio(const vector<vector<int>> &naturalComponents, const vector<unsigned int> &landscape, const vector<vector<unsigned int>> &neighbourMatrixES)
{

  double meanEdgeToAreaRatio=0;
  double edges, area;
  unsigned int ix;
  vector<unsigned int> nonNaturalNeighbours;
  vector<int>::const_iterator it;

  if (naturalComponents.size()>0){

    // iterate over the natural components
    for(ix=0; ix<naturalComponents.size(); ++ix){
      edges=0;
      area = naturalComponents[ix].size();
      for(it=naturalComponents[ix].begin();it!=naturalComponents[ix].end();++it){
        nonNaturalNeighbours.clear();
        getNeighboursStateSup(nonNaturalNeighbours,neighbourMatrixES,landscape,*it,0);
        edges+=(double)nonNaturalNeighbours.size();
      }
      meanEdgeToAreaRatio+=(double)edges/(double)area;
    }

    meanEdgeToAreaRatio/=(double)naturalComponents.size();
  }
  return meanEdgeToAreaRatio;
}

double getMaximumFragmentSize(const vector<vector<int>> &naturalComponents)
{
  double maximumFragmentSize = 0;
  unsigned int ix;

  if (naturalComponents.size()>0){
    for(ix=0; ix<naturalComponents.size(); ++ix){
      if(naturalComponents[ix].size()>maximumFragmentSize){
        maximumFragmentSize = (double)naturalComponents[ix].size();
      }
    }
  }
  return maximumFragmentSize;
}

double getNumberOfFragments(const vector<vector<int>> &naturalComponents)
{

  double numberOfFragments = (double) naturalComponents.size();
  return numberOfFragments;

}

double getLandCoverArea(const vector<unsigned int> &landscape, unsigned int landCover)
{
  double area = count(landscape.begin(), landscape.end(), landCover);
  area/=(double)landscape.size();
  return area;

}

double getAverageFertilityLossPropensity(const vector<unsigned int> &landscape, const vector<double> &ecosystemServices)
{
  double averageFertilityLossPropensity=0;
  unsigned int nAgricultural = 0;
  unsigned int ix;

  for(ix=0;ix<landscape.size();++ix){
    if (landscape[ix]==2 || landscape[ix]==3){
      averageFertilityLossPropensity += (1-ecosystemServices[ix]);
      nAgricultural+=1;
    }
  }

  averageFertilityLossPropensity /= (double)nAgricultural;
  return averageFertilityLossPropensity;
}

double getMoranI(const vector<unsigned int> &landscape, const vector<vector<unsigned int>> &neighbourMatrix)
{
  unsigned int ix;
  // vector to store neighbours of each cell
  vector<unsigned int> neighboursVector;
  vector<unsigned int>::iterator it;

  double sumDenom = 0;
  double sumNumer = 0;
  double moranI;

  // create a vector where 0 is natural and 1 is non-natural to represent the landscape
  vector<unsigned int> naturalLandscape(landscape.size(),0);
  for(ix=0;ix<landscape.size();++ix){
    if(landscape[ix]>0){
      naturalLandscape[ix]=1;
    }
  }

  double naturalLand = getLandCoverArea(naturalLandscape,0);
  unsigned int nNeighbours = neighbourMatrix[0].size();

  for(ix=0;ix<naturalLandscape.size();++ix){
    sumDenom += (naturalLandscape[ix]-naturalLand)*(naturalLandscape[ix]-naturalLand);
    getNeighbours(neighboursVector,neighbourMatrix,ix);
    for(it=neighboursVector.begin();it!=neighboursVector.end();++it){
      sumNumer += (naturalLandscape[ix]-naturalLand)*(naturalLandscape[*it]-naturalLand);
    }
    neighboursVector.clear();
  }

  moranI = sumNumer/sumDenom/(double)nNeighbours;

  return moranI;
}

void saveLandStructFertLoss(ofstream &file, const vector<unsigned int> &population, const vector<unsigned int> &landscape, const vector<vector<unsigned int>> &neighbourMatrix, const vector<double> &ecosystemServices )
{
  double moranI = getMoranI(landscape,neighbourMatrix);
  double averageFertilityLossPropensity = getAverageFertilityLossPropensity(landscape,ecosystemServices);
  double populationSize = population[0];

  file << moranI << " " << averageFertilityLossPropensity << " " << populationSize << "\n";

}

void getESMetrics(vector<double> &metrics, const vector<double> &ecosystemServices)
{
  double meanES=0;
  double giniES=0;
  vector<double>::const_iterator it,it2;
  // check if the vector is not empty: it shoudn't but just in case...
  if (ecosystemServices.size()>0){
    for (it=ecosystemServices.begin();it!=ecosystemServices.end();++it){
      // here we calculate the mean
      meanES+=*it;
      for (it2=ecosystemServices.begin();it2!=ecosystemServices.end();it2++){
        // we add a loop to calculate the gini
        giniES += abs(*it-*it2);
      }
    }
    meanES/=ecosystemServices.size();
    giniES/=ecosystemServices.size()*ecosystemServices.size()*2*meanES;
  }
  metrics[0] = meanES;
  metrics[1] = giniES;

  return;
}

void getESMetricsAgri(vector<double> &metrics, const vector<double> &ecosystemServices, const vector<unsigned int> &landscape)
{
  double meanES=0;
  double giniES=0;
  unsigned int countAgricultural=0;
  unsigned int ix,jx;
  vector<unsigned int> agriculturalLandscape(landscape.size(),0);

  if (landscape.size()>0){
    for(ix=0;ix<landscape.size();++ix){
      if(landscape[ix]>1){
        agriculturalLandscape[ix]=1; // fill the locations with agricultural cells
        countAgricultural+=1;
      }
    }
  }

  // check if the vector is not empty: it shoudn't but just in case...
  if (ecosystemServices.size()>0){
    for(ix=0;ix<ecosystemServices.size();++ix){
      meanES+=ecosystemServices[ix]*agriculturalLandscape[ix]; // if non agricultural do not count it
      for (jx=0;jx<ecosystemServices.size();++jx){
        giniES += abs(ecosystemServices[ix]-ecosystemServices[jx])*agriculturalLandscape[ix]*agriculturalLandscape[jx]; // both need to be agricultural
      }
    }
    if(countAgricultural>0){
      meanES/=countAgricultural;
      giniES/=countAgricultural*countAgricultural*2*meanES;
    }
  }
  metrics[0] = meanES;
  metrics[1] = giniES;

  return;
}

void saveAggregatedMetrics(ofstream &file, double t, const vector<unsigned int> &population, const vector<unsigned int> &landscape, const vector<double> &agriculturalProduction, const vector<vector<int>> &naturalComponents, const vector<vector<unsigned int>> &neighbourMatrixES, const vector<double> &ecosystemServices, unsigned int nSide)
{

  // population Size
  double populationSize = population[0];

  // fraction of land cover
  double naturalLand = getLandCoverArea(landscape,0);
  double degradedLand = getLandCoverArea(landscape,1);
  double lowIntenseLand = getLandCoverArea(landscape,2);
  double highIntenseLand = getLandCoverArea(landscape,3);

  // resource production
  double totalResource = accumulate(agriculturalProduction.begin(),agriculturalProduction.end(),0.0,plus<double>());

  // number of fragments
  double nFrag = getNumberOfFragments(naturalComponents);
  // maximum fragment size
  double maxFragSize = getMaximumFragmentSize(naturalComponents);

  // edge to area ratio
  double meanEdgeToArea = getMeanEdgeToAreaRatio(naturalComponents, landscape, neighbourMatrixES);
  // correlation length of natural cover
  double correlationLength = getCorrelationLength(naturalComponents,landscape,nSide);
  // ecosystem service metrics
  vector<double> metricsES(2);
  // getESMetrics(metricsES, ecosystemServices);
  getESMetricsAgri(metricsES, ecosystemServices,landscape);
  double meanES = metricsES[0];
  double giniES = metricsES[1];

  double moranI = getMoranI(landscape, neighbourMatrixES);

  double averageFertilityLossPropensity = getAverageFertilityLossPropensity(landscape,ecosystemServices);

  file << t << " " << populationSize << " " << naturalLand << " " << degradedLand << " " << lowIntenseLand << " " << highIntenseLand << " " << totalResource << " " << nFrag << " " << maxFragSize << " " << meanEdgeToArea << " " << correlationLength << " " << meanES << " " << giniES << " " << moranI << " " << averageFertilityLossPropensity << "\n";

  return;

}

void saveAggregated(ofstream &file, double t, const vector<unsigned int> &population, const vector<unsigned int> &landscape, const vector<double> &agriculturalProduction, const vector<vector<int>> &naturalComponents, const vector<double> &ecosystemServices, unsigned int nn, double ripleyDistance, double nMax, double nMin, double pMax, double pMin)
{
  unsigned long numComponents = naturalComponents.size();
  unsigned long ix;
  double meanSize=0;
  double squaredMeanSize=0;
  double maxSize=0;
  double stdSize;
  double componentSize;

  for (ix=0;ix<naturalComponents.size();ix++){
    componentSize=(double)naturalComponents[ix].size()/ecosystemServices.size();
    meanSize+=componentSize;
    squaredMeanSize+=componentSize*componentSize;
    if(componentSize>maxSize){
      maxSize=componentSize;
    }
  }
  if (numComponents>0){
    meanSize/=numComponents;
    squaredMeanSize/=numComponents;
  }
  stdSize=squaredMeanSize-meanSize*meanSize;
  if(stdSize<0){
    stdSize=0;
  }
  else{
    stdSize=sqrt(stdSize);
  }

  double meanES=0;
  double squaredMeanES=0;
  double stdES=0;
  if (ecosystemServices.size()>0){
    for (ix=0;ix<ecosystemServices.size();ix++){
      meanES+=ecosystemServices[ix];
      squaredMeanES+=ecosystemServices[ix]*ecosystemServices[ix];
    }
    meanES/=ecosystemServices.size();
    squaredMeanES/=ecosystemServices.size();
    stdES=sqrt(squaredMeanES-meanES*meanES);
  }
  /*
  Second part is a copy of saveAggregated
  */
  double n=0,d=0,a0=0,a1=0;

  for(ix=0;ix<landscape.size();ix++){
    if (landscape[ix]==0){
      n+=1;
    }
    else if (landscape[ix]==1){
      d+=1;
    }
    else if (landscape[ix]==2){
      a0+=1;
    }
    else{
      a1+=1;
    }
  }
  n/=landscape.size();d/=landscape.size();a0/=landscape.size();a1/=landscape.size();

  double totalY;
  for(ix=0; ix<agriculturalProduction.size(); ++ix){
    totalY+=agriculturalProduction[ix];
  }

  /*
  Save the connectance of the natural environment
  */
  double connectance;
  unsigned int realizedConnections=0;
  unsigned int totalConnections=0;
  vector<unsigned int> naturalNeighbours;
  vector<vector<unsigned int>> neighbourMooreMatrix;
  getNeighbourMatrix(neighbourMooreMatrix,nn,1);

  for(ix=0; ix<landscape.size(); ++ix){
    if(landscape[ix]==0){// natural cell
      // check the number of natural neighburs
      getNeighboursState(naturalNeighbours,neighbourMooreMatrix,landscape,ix, 0);
      realizedConnections += naturalNeighbours.size();
      totalConnections += 4; // sum 4 foreach natural patch you encounter
      naturalNeighbours.clear();
    }
  }
  connectance = (double)realizedConnections/totalConnections;

  /*
  Last part is a copy of saveRipley
  */

  // first get the neighbour matrix given a ripley distance, here it is 1
  vector<vector<unsigned int>> neighbourMatrix;
  getNeighbourMatrix(neighbourMatrix,nn,ripleyDistance);

  // then determine the number of points for each type of land
  double nN=0,nD=0,nA0=0,nA1=0;
  for(ix=0;ix<landscape.size();ix++){
    if (landscape[ix]==0){
      nN+=1;
    }
    else if (landscape[ix]==1){
      nD+=1;
    }
    else if (landscape[ix]==2){
      nA0+=1;
    }
    else{
      nA1+=1;
    }
  }

  // now calculate ripley without normalizing
  double ripleyN=0, ripleyD=0, ripleyA0=0, ripleyA1=0;
  vector<unsigned int> stateNeighbours;
  for(ix=0;ix<landscape.size();ix++){
    stateNeighbours.clear();
    if (landscape[ix]==0){
      getNeighboursState(stateNeighbours, neighbourMatrix, landscape, ix, 0);
      ripleyN+=stateNeighbours.size();
    }
    else if (landscape[ix]==1){
      getNeighboursState(stateNeighbours, neighbourMatrix, landscape, ix, 1);
      ripleyD+=stateNeighbours.size();
    }
    else if (landscape[ix]==2){
      getNeighboursState(stateNeighbours, neighbourMatrix, landscape, ix, 2);
      ripleyA0+=stateNeighbours.size();
    }
    else{
      getNeighboursState(stateNeighbours, neighbourMatrix, landscape, ix, 3);
      ripleyA1+=stateNeighbours.size();
    }
  }

  if (nN>0){ripleyN *= landscape.size()/(nN*nN);}
  if (nD>0){ripleyD *= landscape.size()/(nD*nD);}
  if (nA0>0){ripleyA0 *= landscape.size()/(nA0*nA0);}
  if (nA1>0){ripleyA1 *= landscape.size()/(nA1*nA1);}

  /////////////////////////////////////////////////////////////////////////////
  // get radius of gyration and correlation length

  vector<vector<int>>::const_iterator it;
  double radiusOfGyration=0;
  double stdRadiusOfGyration=0;
  double correlationLength = 0;
  double meanRadiusOfGyration = 0;
  double squaredRadiusOfGyration = 0;

  if (naturalComponents.size()>0){
    for(it=naturalComponents.begin();it!=naturalComponents.end();++it){
      radiusOfGyration = getRadiusOfGyration(*it,nn);
      // division by nn is to rescale by landscape size
      meanRadiusOfGyration += (double)radiusOfGyration/nn;
      squaredRadiusOfGyration += (double)radiusOfGyration*radiusOfGyration/(nn*nn);
      correlationLength += (*it).size() * radiusOfGyration;
    }
    squaredRadiusOfGyration /= (double) naturalComponents.size();
    meanRadiusOfGyration /= (double) naturalComponents.size();
    stdRadiusOfGyration = squaredRadiusOfGyration - meanRadiusOfGyration*meanRadiusOfGyration;
    // use the fact that we already calculated the fraction of natural cells and rescale it by landscape size nn
    if (nN>0){
      correlationLength /= (double) (nN*nn);
    }
  }
  file << t << " " << population[0] << " " << n << " " << d << " " << a0 << " " << a1 << " " << totalY << " " << numComponents << " " << meanSize << " " << stdSize << " " << maxSize << " " << meanES << " " << stdES << " " << connectance << " " << nMax << " " << nMin << " " << pMax << " " << pMin << " " <<  ripleyN << " " << ripleyD << " " << ripleyA0 << " " << ripleyA1 << " " << meanRadiusOfGyration << " " << stdRadiusOfGyration << " " << correlationLength << "\n";

  return;
}

void saveLandscape(ofstream &file, double t, const vector<unsigned int> &landscape)
{
  unsigned long ix;

  file << t;
  for (ix=0;ix<landscape.size();ix++){
    file << " " << landscape[ix];
  }
  file << "\n";

  return;
}

void saveComponents(ofstream &file, double t, const vector<unsigned int> &landscape, const vector<vector<int>> &naturalComponents)
{
  unsigned long ix,jx;
  unsigned int test;
  vector<int>::const_iterator it;

  file << t;
  for (ix=0;ix<landscape.size();ix++){
    test=0;
    for (jx=0;jx<naturalComponents.size();jx++){
      it = find( naturalComponents[jx].begin(), naturalComponents[jx].end(), ix );
      if (it!=naturalComponents[jx].end()){
        file << " " << jx;
        test = 1;
        break;
      }
    }
    if (test==0){
      file << " " << nan("");
    }
  }

  file << "\n";

  return;
}

void saveSensitivityOutput(ofstream &file, unsigned int nn, double ripleyDistance, const vector<unsigned int> &population, const vector<vector<int>> &naturalComponents, const vector<unsigned int> &landscape, const vector<double> &ecosystemServices)
{

  /*
  First part is a copy of the saveLandMetrics. Of course this can be improved to
  avoid code repetition
  */
  unsigned long numComponents = naturalComponents.size();
  unsigned long ix;
  double meanSize=0;
  double squaredMeanSize=0;
  double maxSize=0;
  double stdSize;
  double componentSize;

  for (ix=0;ix<naturalComponents.size();ix++){
    componentSize=(double)naturalComponents[ix].size()/ecosystemServices.size();
    meanSize+=componentSize;
    squaredMeanSize+=componentSize*componentSize;
    if(componentSize>maxSize){
      maxSize=componentSize;
    }
  }
  meanSize/=numComponents;
  squaredMeanSize/=numComponents;
  stdSize=sqrt(squaredMeanSize-meanSize*meanSize);

  double meanES=0;
  double squaredMeanES=0;
  double stdES;
  for (ix=0;ix<ecosystemServices.size();ix++){
    meanES+=ecosystemServices[ix];
    squaredMeanES+=ecosystemServices[ix]*ecosystemServices[ix];
  }
  meanES/=ecosystemServices.size();
  squaredMeanES/=ecosystemServices.size();
  stdES=sqrt(squaredMeanES-meanES*meanES);

  /*
  Second part is a copy of saveAggregated
  */
  double n=0,d=0,a0=0,a1=0;

  for(ix=0;ix<landscape.size();ix++){
    if (landscape[ix]==0){
      n+=1;
    }
    else if (landscape[ix]==1){
      d+=1;
    }
    else if (landscape[ix]==2){
      a0+=1;
    }
    else{
      a1+=1;
    }
  }
  n/=landscape.size();d/=landscape.size();a0/=landscape.size();a1/=landscape.size();

  /*
  Last part is a copy of saveRipley
  */

  // first get the neighbour matrix given a ripley distance, here it is 1
  vector<vector<unsigned int>> neighbourMatrix;
  getNeighbourMatrix(neighbourMatrix,nn,ripleyDistance);

  // then determine the number of points for each type of land
  double nN=0,nD=0,nA0=0,nA1=0;
  for(ix=0;ix<landscape.size();ix++){
    if (landscape[ix]==0){
      nN+=1;
    }
    else if (landscape[ix]==1){
      nD+=1;
    }
    else if (landscape[ix]==2){
      nA0+=1;
    }
    else{
      nA1+=1;
    }
  }

  // now calculate ripley without normalizing
  double ripleyN=0, ripleyD=0, ripleyA0=0, ripleyA1=0;
  vector<unsigned int> stateNeighbours;
  for(ix=0;ix<landscape.size();ix++){
    stateNeighbours.clear();
    if (landscape[ix]==0){
      getNeighboursState(stateNeighbours, neighbourMatrix, landscape, ix, 0);
      ripleyN+=stateNeighbours.size();
    }
    else if (landscape[ix]==1){
      getNeighboursState(stateNeighbours, neighbourMatrix, landscape, ix, 1);
      ripleyD+=stateNeighbours.size();
    }
    else if (landscape[ix]==2){
      getNeighboursState(stateNeighbours, neighbourMatrix, landscape, ix, 2);
      ripleyA0+=stateNeighbours.size();
    }
    else{
      getNeighboursState(stateNeighbours, neighbourMatrix, landscape, ix, 3);
      ripleyA1+=stateNeighbours.size();
    }
  }

  if (nN>0){ripleyN *= landscape.size()/(nN*nN);}
  if (nD>0){ripleyD *= landscape.size()/(nD*nD);}
  if (nA0>0){ripleyA0 *= landscape.size()/(nA0*nA0);}
  if (nA1>0){ripleyA1 *= landscape.size()/(nA1*nA1);}

  file << numComponents << " " << meanSize << " " << stdSize << " " << maxSize << " " << meanES << " " << stdES << " " << population[0] << " " << n << " " << d << " " << a0 << " " << a1 << " " << ripleyN << " " << ripleyD << " " << ripleyA0 << " " << ripleyA1 <<"\n";

  return;
}

void saveLandscapeMetrics(ofstream &file, unsigned int n, const vector<unsigned int> &landscape, const vector<double> &ecosystemServices)
{
  /*
  In this function we save the mean ES provision in the landscape as well as
  the gini index for ES provision to quantify inequalities across the landscape.
  It would be good to calculate Ripley's-K to see the spatial distribution of
  ES i.e. at which scales it is homogeneous or not. To binarize the data check
  which cells are above or beyond the mean provision. We also save the mean and
  std distance between natural cells to compare with the natural distance connection
  and check whether the larger NDC has an impact on the ES after percolation.
  */

  // here we calculate the mean and gini ecosystem service provision
  double meanES=0;
  double giniES=0;
  vector<double>::const_iterator it,it2;
  // check if the vector is not empty: it shoudn't but just in case...
  if (ecosystemServices.size()>0){
    for (it=ecosystemServices.begin();it!=ecosystemServices.end();++it){
      // here we calculate the mean
      meanES+=*it;
      for (it2=ecosystemServices.begin();it2!=ecosystemServices.end();it2++){
        // we add a loop to calculate the gini
        giniES += abs(*it-*it2);
      }
    }
    meanES/=ecosystemServices.size();
    giniES/=ecosystemServices.size()*ecosystemServices.size()*2*meanES;
  }
  // comment on gini calculation: might be more efficient not to iterate the
  // second loop over all the cells to avoid double counting and computing time.
  // i guess that would require to remove the division by 2 at the end.

  // here we calculate the average and std distance between natural cells
  double avgDistance=0;
  double squaredAvgDistance=0;
  double stdDistance=0;
  unsigned int dx,dy, ix,jx, nCount;
  nCount = 0 ;
  int xi,yi,xj,yj;
  vector<unsigned int > naturalCells;
  // check if the vector is not empty: it shoudn't but just in case...
  if (landscape.size()>0){

    // first store all the indexes of the natural cells
    for(ix=0;ix<landscape.size();ix++){
      // if the cell is natural store the index in another vector
      if (landscape[ix]==0){
        naturalCells.push_back(ix);
      }
    }

    if (naturalCells.size()>0){
      for(ix=0; ix<naturalCells.size(); ix++){
        xi=naturalCells[ix]%n;
        yi=(int)naturalCells[ix]/n;
        for (jx=0; jx<ix ; jx ++){
          xj=naturalCells[jx]%n;
          yj=(int)naturalCells[jx]/n;
          dx=abs(xi-xj);
          dy=abs(yi-yj);
          if (dx>n/2){
            dx=n-dx;
          }
          if (dy>n/2){
            dy=n-dy;
          }
          nCount+=1;
          avgDistance += (dx + dy);
          squaredAvgDistance += (dx + dy)*(dx+dy);
        }
      }
      avgDistance/=nCount;
      squaredAvgDistance/=nCount;
      stdDistance=sqrt(squaredAvgDistance-avgDistance*avgDistance);
    }
  }

  file << meanES << " " << giniES << " " << avgDistance << " " << stdDistance <<"\n";

  // here we calculate the ripley's coefficient for high and low ES provision,
  // where high means above and low below the average
  // note: this section is still under comments need to think in which case it will
  // be interesting to analyze the ripleys k for ES

  // vector <unsigned int> esIndicator;
  // unsigned int nAbove=0;
  // unsigned int dRipley;
  // if (ecosystemServices.size()>0){
  //
  //   // get a vector with 0 where ES<meanES and 1 otherwise.
  //   for (it=ecosystemServices.begin();it!=ecosystemServices.end();++it){
  //     if(*it>meanES){
  //       esIndicator.push_back(1);
  //       nAbove+=1;
  //     }
  //     else{
  //       esIndicator.push_back(0);
  //     }
  //   }
  //
  //   // iterate over several distance thresholds and calculate the ripley
  //   vector<vector<unsigned int>> neighbourMatrix;
  //   vector<unsigned int> stateNeighbours;
  //   for(dRipley=1;dRipley<(int)n/2;dRipley++){
  //     getNeighbourMatrix(neighbourMatrix,n,dRipley);
  //     for(ix=0;ix<esIndicator.size();ix++){
  //       // select only the cells with high ES provision
  //       if(esIndicator[ix]>0){
  //         getNeighboursState(stateNeighbours, neighbourMatrix, esIndicator, ix, 1);
  //         ripleyES += stateNeighbours.size();
  //       }
  //     }
  //     if (nAbove>0){ripleyES *= esIndicator.size()/(nAbove*nAbove);}
  //     // save the ripley's K and the associated threshold distance
  //     fileRipley << dRipley << " " << ripleyES << "\n";
  //   }
  //
  // }

  return;
}
