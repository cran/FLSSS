// # pragma once
// [[Rcpp::depends(RcppParallel)]]
# include <Rcpp.h>
# include <RcppParallel.h>
# include "header/BaB01kp.hpp"
// # include <fstream>
// # include "header/macros.hpp"
// # include "header/dnyTasking.hpp"
using namespace Rcpp;
// # define sINT std::int64_t;




template<typename valtype, typename indtype, typename fmove, bool timeConstraint>
struct paraBkpForCaps: public RcppParallel::Worker
{
  indtype Xsize;
  valtype *capV;
  indtype *lenCapV;
  double endtime;
  kpEle<valtype, indtype> *X;
  vec<indtype> *bestV, *currentV;
  valtype *bestVal;
  dynamicTasking *dT;
  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      INT objI = 0;
      if(!dT->nextTaskID(objI)) break;
      bestVal[objI] = bkp<valtype, indtype, fmove, timeConstraint> (
          X, Xsize, capV[objI], lenCapV[objI], bestV[objI], currentV[st], endtime);
    }
  }
  paraBkpForCaps(indtype Xsize, valtype *capV, indtype *lenCapV,
                 indtype capVsize, double endtime, kpEle<valtype, indtype> *X,
                 vec<vec<indtype> > &bestVec, // of size capVsize
                 valtype *bestVal, std::size_t maxCore): Xsize(Xsize),
                 capV(capV), lenCapV(lenCapV), endtime(endtime),
                 X(X), bestVal(bestVal)
  {
    dynamicTasking dt(maxCore, capVsize); dT = &dt;
    vec<vec<indtype> > current(maxCore, vec<indtype>(Xsize + 2));
    currentV = &current[0];
    bestV = &bestVec[0];
      parallelFor(0, dT->NofCore, *this);
  }
};




// List bkpOrdered(NumericVector weight, NumericVector value, double cap)
// [[Rcpp::export]]
List auxKnapsack01bb(
    NumericVector weight, NumericVector value, NumericVector caps,
    IntegerVector itemNcaps = IntegerVector(0), int maxCore = 7,
    double tlimit = 60, String ub = "MT", bool simplify = true)
{
  int Xsize = value.size();
  vec<kpEle<double, int> > Xcontain(Xsize + 3);
  kpEle<double, int> *X = &Xcontain[0] + 1;
  // Initialize Xcontain.
  double cap = *std::max_element(caps.begin(), caps.end());
  int *itemNcapV = nullptr;
  vec<int> itemNcapVcontainer;
  if(itemNcaps.size() == 0)
  {
    itemNcapVcontainer.assign(caps.size(), Xsize + 2);
    // Xsize + 2 is unncessary, Xsize is sufficient.
    itemNcapV = &itemNcapVcontainer[0];
  }
  else itemNcapV = &itemNcaps[0];


  // Rcout << 1.1 << "\n";
  vec<int> unitValOrder(Xsize);
  // int nonZeroSize = 0;
  {
    X[-1].accValue = 0;
    // X[-1].accWeight = cap + 1;
    X[-1].accWeight = 0;
    vec<double> valuePerWeight(Xsize);
    for(int i = 0; i < Xsize; ++i)
    {
      unitValOrder[i] = i;
      valuePerWeight[i] = value[i] / weight[i];
    }


    std::sort(unitValOrder.begin(), unitValOrder.end(),
              cmp<double, int> (&valuePerWeight[0]));


    // Initialize X.accWeight, X.accValue, X.valuePerWeight.
    for(int i = 0; i < Xsize; ++i)
    {
      // if(value[unitValOrder[i]] <= 0) {nonZeroSize = i; break;}
      X[i].accWeight = X[i - 1].accWeight + weight[unitValOrder[i]];
      X[i].accValue = X[i - 1].accValue + value[unitValOrder[i]];
      X[i].valuePerWeight = valuePerWeight[unitValOrder[i]];
    }


    X[Xsize].accWeight = X[Xsize - 1].accWeight + (cap + 1);
    X[Xsize].valuePerWeight = X[Xsize - 1].valuePerWeight / 2;
    X[Xsize].accValue = X[Xsize - 1].accValue + X[Xsize].valuePerWeight * (cap + 1);
    X[Xsize].minWeightAfter = cap + 2;


    X[Xsize + 1].accWeight = X[Xsize].accWeight + (cap + 2);
    X[Xsize + 1].valuePerWeight = X[Xsize].valuePerWeight / 2;
    X[Xsize + 1].accValue = X[Xsize].accValue + X[Xsize + 1].valuePerWeight * (cap + 2);
    X[Xsize + 1].minWeightAfter = cap + 3;


    X[Xsize - 1].minWeightAfter = cap + 1;
    for(int i = Xsize - 2; i >= -1; --i)
    {
      X[i].minWeightAfter = std::min<double> (
        weight[unitValOrder[i + 1]], X[i + 1].minWeightAfter);
    }
  }


  maxCore = std::min<int> (maxCore, caps.size());
  vec<vec<int> > best(caps.size(), vec<int> (Xsize + 2));
  NumericVector bestVal(best.size());
  double endtime = (double)std::clock() + tlimit * CLOCKS_PER_SEC;


  if(ub == "MT")
    paraBkpForCaps<double, int, MTfmoveUB<double, int>, true> (
        Xsize, &caps[0], itemNcapV, caps.size(), endtime, X, best, &bestVal[0], maxCore);
  else
    paraBkpForCaps<double, int, HSfmoveUB<double, int>, true> (
        Xsize, &caps[0], itemNcapV, caps.size(), endtime, X, best, &bestVal[0], maxCore);


  List rst(caps.size());
  // Rcout << "Inside knapsack, best = ";
  for(int i = 0, iend = rst.size(); i < iend; ++i)
  {
    IntegerVector v(best[i].size());
    for(int j = 0, jend = best[i].size(); j < jend; ++j)
    {
      v[j] = unitValOrder[best[i][j]] + 1;
    }
    rst[i] = v;
  }


  if(simplify and caps.size() == 1)
  {
    IntegerVector theSolution = rst[0];
    return List::create(Named("maxVal") = bestVal, Named("selection") = theSolution);
  }
  return List::create(Named("maxVal") = bestVal, Named("selection") = rst);
}




/*
template<typename valtype, typename indtype>
struct cmptmp
{
  valtype *val;
  cmptmp(){}
  cmptmp(valtype *val): val(val){}
  bool operator() (indtype i, indtype j) {return val[i] > val[j];}
};


// [[Rcpp::export]]
IntegerVector testCmp(NumericVector x)
{
  IntegerVector a(x.size());
  for(int i = 0, iend = a.size(); i < iend; ++i) a[i] = i;
  std::sort(a.begin(), a.end(), cmptmp<double, int>(&x[0]));
  return a;
}
*/



























