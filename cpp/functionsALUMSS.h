#ifndef DECLARATION_FUNCTIONS_ALUMSS_H
#define DECLARATION_FUNCTIONS_ALUMSS_H

#include <vector>
#include <gsl/gsl_rng.h>
#include <fstream>

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

void getNeighbourMatrix(vector<vector<unsigned int>> &neighbourMatrix, unsigned int nSide, double d);
void getNeighbours(vector<unsigned int> &neighboursList, const vector<vector<unsigned int>> &neighbourMatrix, unsigned int i);
void getNeighboursState(vector<unsigned int> &neighboursState, const vector<vector<unsigned int>> &neighbourMatrix, const vector<unsigned int> &landscape, unsigned int i, unsigned int state);
void getNeighboursStateInf(vector<unsigned int> &neighboursState, const vector<vector<unsigned int>> &neighbourMatrix, const vector<unsigned int> &landscape, unsigned int i, unsigned int maxState);
void getNeighboursStateSup(vector<unsigned int> &neighboursState, const vector<vector<unsigned int>> &neighbourMatrix, const vector<unsigned int> &landscape, unsigned int i, unsigned int minState);

////////////////////////////////////////////////////////////////////////////////
// 2- Calculation of Ecosystem Service provision:
//       - getNaturalConnectedComponents
//       - updateNCCadding
//       - updateNCCremoving
//       - getEcosystemServiceProvision
////////////////////////////////////////////////////////////////////////////////

void getNaturalConnectedComponents(vector<vector<int>> &naturalComponents, const vector<unsigned int> &landscape, double distanceConnection);
void updateNCCadding(vector<vector<int>> &naturalComponents, const vector<vector<unsigned int>> &neighbourMatrix, const vector<unsigned int> &landscape, unsigned int i);
void updateNCCremoving(vector<vector<int>> &naturalComponents, const vector<unsigned int> &landscape, int i, double distanceConnection);
void getEcosystemServiceProvision(vector<double> &ecosystemServices, const vector<vector<int>> &naturalComponents, const vector<vector<unsigned int>> &neighbourMatrix, const vector<unsigned int> &landscape, double sar);

////////////////////////////////////////////////////////////////////////////////
// 3- Calculation of events' propensities:
//       - esSaturationFunction
//       - getAgriculturalProduction
//       - getConsumptionDeficit
//       - getSpontaneousPropensity
//       - getAgroPropensity
//       - getAbandonmentPropensity
//       - getPropensityVector
////////////////////////////////////////////////////////////////////////////////

void getAgriculturalProduction(vector<double> &agriculturalProduction, const vector<unsigned int> &landscape, const vector<double> &ecosystemServices, double ye);
double getResourceDeficit(const vector<double> &agriculturalProduction, const vector<unsigned int> &population, double k0);
double getTotalManagementPropensity(const vector<unsigned int> &landscape, const vector<vector<unsigned int>> &farms, const vector<double> &farmSensitivity, double resourceDeficit);
void getDemographicPropensities(vector<double> &demographicPropensities, const vector<double> &agriculturalProduction, vector<unsigned int> &population, double k0);
void getSpontaneousPropensities(vector<double> &spontaneousPropensities, const vector<unsigned int> &landscape, const vector<double> &ecosystemServices, unsigned int nSide, double sR, double sD, double sFL);
void solveSSA(vector<unsigned int> &landscape, vector<vector<int>> &naturalComponents, vector<double> &ecosystemServices, vector<double> &agriculturalProduction, const vector<vector<unsigned int>> &farms, const vector<vector<unsigned int>> &neighbourMatrix, const vector<vector<unsigned int>> &neighbourMatrixES, vector<unsigned int> &population, const vector<double> &farmSensitivity, const vector<vector<double>> &farmStrategy, vector<double> &spontaneousPropensities, vector<double> &spontaneousCumulativePropensities, vector<double> &demographicPropensities, vector<double> &demographicCumulativePropensities, double totalManagementPropensity, double resourceDeficit, unsigned int nFarms, unsigned int nSide, double ye, double k0, double sR, double sD, double sFL, double z, double dES, gsl_rng  *r, vector<unsigned int> &countTransitions);

////////////////////////////////////////////////////////////////////////////////
// 4- Initialization functions:
//       - initializeLandscape
//       - initializePopulation
//       - initializeSES
////////////////////////////////////////////////////////////////////////////////

void initializeVoronoiFarms( vector<vector<unsigned int>> &farms, const vector<vector<unsigned int>> &neighbourMatrix, unsigned int nSide, unsigned int nFarms, gsl_rng  *r);
void initializeFarmStrategy( vector<vector<unsigned int>> &farmStrategy, unsigned int nFarms, double a, gsl_rng *r);
void initializeFarmSensitivity( vector<double> &farmSensitivity, unsigned int nFarms, double mS, double wS, gsl_rng *r);
void initializeLandscape(vector<unsigned int> &landscape, const vector<vector<unsigned int>> &farms, const vector<double> &farmSensitivity, const vector<vector<double>> &farmStrategy, const vector<vector<unsigned int>> &neighbourMatrix, unsigned int nSide, unsigned int nFarms, double a0, double d0, gsl_rng  *r);
void initializePopulation( vector<unsigned int> &population, const vector<double> &agriculturalProduction, double k0);
void initializeSES( vector<vector<unsigned int>> &farms, vector<double> &farmSensitivity, vector<vector<double>> &farmStrategy, vector<unsigned int> &landscape, vector<unsigned int> &population, vector<vector<int>> &naturalComponents, vector<double> &agriculturalProduction, vector<double> &ecosystemServices, vector<vector<unsigned int>> &neighbourMatrix, vector<vector<unsigned int>> &neighbourMatrixES, unsigned int nSide, double a0, double d0, double a, double mS, double wS, double ye, double k0, double z, double dES, unsigned int nFarms, gsl_rng  *r);

////////////////////////////////////////////////////////////////////////////////
// 6- Outputs:
//       - getRadiusOfGyration
//       - saveAggregated
//       - saveLandscape
//       - saveComponents
////////////////////////////////////////////////////////////////////////////////

double getRadiusOfGyration(const vector<int> &naturalComponent, unsigned int n);
double getCorrelationLength(const vector<vector<int>> &naturalComponents, const vector<unsigned int> &landscape, unsigned int n);
double getMeanEdgeToAreaRatio(const vector<vector<int>> &naturalComponents, const vector<unsigned int> &landscape, const vector<vector<unsigned int>> &neighbourMatrixES);
double getMaximumFragmentSize(const vector<vector<int>> &naturalComponents);
double getNumberOfFragments(const vector<vector<int>> &naturalComponents);
double getLandCoverArea(const vector<unsigned int> &landscape, unsigned int landCover);
double getAverageFertilityLossPropensity(const vector<unsigned int> &landscape, const vector<double> &ecosystemServices);
double getMoranI(const vector<unsigned int> &landscape, const vector<vector<unsigned int>> &neighbourMatrix);
void saveLandStructFertLoss(ofstream &file, const vector<unsigned int> &population, const vector<unsigned int> &landscape, const vector<vector<unsigned int>> &neighbourMatrix, const vector<double> &ecosystemServices );
void getESMetrics(vector<double> &metrics, const vector<double> &ecosystemServices);
void getESMetricsAgri(vector<double> &metrics, const vector<double> &ecosystemServices, const vector<unsigned int> &landscape);
void saveAggregatedMetrics(ofstream &file, double t, const vector<unsigned int> &population, const vector<unsigned int> &landscape, const vector<double> &agriculturalProduction, const vector<vector<int>> &naturalComponents, const vector<vector<unsigned int>> &neighbourMatrixES, const vector<double> &ecosystemServices, unsigned int n);
void saveAggregated(ofstream &file, double t, const vector<unsigned int> &population, const vector<unsigned int> &landscape, const vector<double> &agriculturalProduction, const vector<vector<int>> &naturalComponents, const vector<double> &ecosystemServices, unsigned int n, double ripleyDistance, double nMax, double nMin, double pMax, double pMin);
void saveLandscape(ofstream &file, double t, const vector<unsigned int> &landscape);
void saveComponents(ofstream &file, double t, const vector<unsigned int> &landscape, const vector<vector<int>> &naturalComponents);
void saveSensitivityOutput(ofstream &file, unsigned int nn, double ripleyDistance, const vector<unsigned int> &population, const vector<vector<int>> &naturalComponents, const vector<unsigned int> &landscape, const vector<double> &ecosystemServices);
void saveLandscapeMetrics(ofstream &file, unsigned int n, const vector<unsigned int> &landscape, const vector<double> &ecosystemServices);

#endif
