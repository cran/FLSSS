# pragma once
# include <fstream>
# include <RcppParallel.h>
# include "macros.hpp"
# include "mPATclass.hpp"
# include "dnyTasking.hpp"
// # include "multiDstack.hpp"




template<typename valtype, typename indtype>
struct shared
{
  // bool useBiSrch;
  indtype subsetSize;
  indtype N, d, dlst, dl, dust, du;
  int sizeNeed;
  tbb::atomic<int> totalSize;
  double endTime;
  valtype ***M;
  INT *mask; // could be nullptr


  shared(){totalSize = 0;}


  shared(indtype subsetSize,
    indtype N, indtype d, indtype dlst, indtype dl,
    indtype dust, indtype du, int sizeNeed,
    double endTime, valtype ***M, INT *mask):
      subsetSize(subsetSize),
      N(N), d(d), dlst(dlst), dl(dl),
      dust(dust), du(du), sizeNeed(sizeNeed),
      endTime(endTime), M(M), mask(mask)
  {
    totalSize = 0;
  }


  // the following 4 members work for knapsack()
  // valtype *profitVec;
  double *profitVec;
  indtype *optimalSolution;
  // valtype optimalProfit;
  double optimalProfit;
  shared(indtype subsetSize,
         indtype N, indtype d, indtype dlst, indtype dl,
         indtype dust, indtype du,
         double endTime, valtype ***M, INT *mask,
         // valtype *profitVec,
         double *profitVec,
         indtype *optimalSolution):
    subsetSize(subsetSize),
    N(N), d(d), dlst(dlst), dl(dl),
    dust(dust), du(du), endTime(endTime), M(M), mask(mask),
    profitVec(profitVec), optimalSolution(optimalSolution)
  {
    optimalProfit = 0;
    std::fill(optimalSolution, optimalSolution + subsetSize, 0);
    sizeNeed = 0;
    totalSize = 0;
  }
};


template<typename valtype, typename indtype, bool mk, bool useBiSearch>
struct dummyContainers
{
  vec<indtype> hopeV;
  vec<mPAT<valtype, indtype, mk, useBiSearch> > SKvec;
  vec<indtype> indvec;
  vec<valtype> valvec;
  vec<valtype> SRVcntr;
  void swap(vec<indtype> &ahopeV,
            vec<mPAT<valtype, indtype, mk, useBiSearch> > &aSKvec,
            vec<indtype> &aindvec,
            vec<valtype> &avalvec, vec<valtype> &aSRVcntr)
  {
    hopeV.swap(ahopeV);
    SKvec.swap(aSKvec);
    indvec.swap(aindvec);
    valvec.swap(avalvec);
    SRVcntr.swap(aSRVcntr);
  }
};


template<typename valtype, typename indtype, bool mk, bool useBiSearch>
struct mflsssOBJ
{
  indtype *hope; // '*hope' points to the first element to write in 'hopeV'
  shared<valtype, indtype> *f;
  vec<indtype> hopeV;


  double existingProfitSum;
  // Memorize the subset sum in the extra dimension (profit dimension).
  // It is only used in knapsack problem.


  vec<mPAT<valtype, indtype, mk, useBiSearch> > SKvec;
  mPAT<valtype, indtype, mk, useBiSearch> *SKback;
  vec<indtype> indvec;
  vec<valtype> valvec;
  vec<valtype> SRVcntr;
  vec<vec<indtype> > result;


  void swap(mflsssOBJ &X)
  {
    std::swap(X.hope, hope);
    std::swap(X.f, f);
    std::swap(X.hopeV, hopeV);
    std::swap(X.existingProfitSum, existingProfitSum);
    std::swap(X.SKvec, SKvec);
    std::swap(X.SKback, SKback);
    std::swap(X.indvec, indvec);
    std::swap(X.valvec, valvec);
    std::swap(X.SRVcntr, SRVcntr);
    std::swap(X.result, result);
  }


  void setAnSK(mPAT<valtype, indtype, mk, useBiSearch> *sk,
               indtype *ind, valtype *val, indtype len)
  {
    sk->beenUpdated = 1;
    sk->MIN = val;
    sk->MAX = sk->MIN + f->dl;
    sk->sumLB = sk->MAX + f->du;
    sk->sumUB = sk->sumLB + f->d;
    sk->sumBresv = sk->sumUB + f->d;
    sk->LB = ind;
    sk->UB = sk->LB + len;
    sk->Bresv = sk->UB + len;
    sk->len = len;
  }


  void initialize(shared<valtype, indtype> *fixedInfo,
                  valtype *target, valtype *ME,
                  indtype *LB, indtype *UB,
                  dummyContainers<valtype, indtype, mk, useBiSearch>
                    *dummies = nullptr)
  {
    f = fixedInfo;
    std::size_t stackLen = (unsigned)f->subsetSize + 2;
    unsigned biscaleFactor = (unsigned)std::log2(f->N + 0.0 - f->subsetSize) + 3;
    // about '+3': once, there was a failed test case given '+0'
    if(dummies != nullptr)
    {
      dummies->swap(hopeV, SKvec, indvec, valvec, SRVcntr);
    }
    indvec.assign(stackLen * (stackLen + 1) / 2 * 3 * biscaleFactor, 0);
    valvec.assign((3 * (std::size_t)f->d + (std::size_t)f->dl +
      (std::size_t)f->du) * stackLen * biscaleFactor, 0);
    SKvec.resize((unsigned)f->subsetSize * biscaleFactor);
    hopeV.assign(f->subsetSize, 0);
    SRVcntr.assign(f->d, 0);
    hope = &hopeV[0];
    // extraDimSum = 0; // No need to initialize this for subset sum
    mPAT<valtype, indtype, mk, useBiSearch> *SKbegin = &SKvec.front();


    setAnSK(SKbegin, &indvec[0], &valvec[0], f->subsetSize);
    SKback = SKbegin + 1;


    for(indtype i = 0; i < SKbegin->len; ++i)
    {
      SKbegin->LB[i] = LB[i];
      SKbegin->UB[i] = UB[i];
    }


    // assign MIN and MAX
    {
      for(indtype i = f->dlst, iend = f->dlst + f->dl; i < iend; ++i)
      {
        SKbegin->MIN[i - f->dlst] = target[i] - ME[i];
      }
      for(indtype i = f->dust, iend = f->dust + f->du; i < iend; ++i)
      {
        SKbegin->MAX[i - f->dust] = target[i] + ME[i];
      }
    }


    iterSum<valtype, indtype> (SKbegin->sumLB, f->M[0], SKbegin->LB, SKbegin->len, f->d);
    iterSum<valtype, indtype> (SKbegin->sumUB, f->M[0], SKbegin->UB, SKbegin->len, f->d);


    result.reserve(f->sizeNeed);
    result.resize(0);
  }


  void finalize(dummyContainers<valtype, indtype, mk, useBiSearch> *dummies = nullptr)
  {
    if(dummies != nullptr)
    {
      dummies->swap(hopeV, SKvec, indvec, valvec, SRVcntr);
    }
  }


  int TTTstackRun( // std::ofstream *outfile = nullptr
                  )
  {
    mPAT<valtype, indtype, mk, useBiSearch> *SK = &SKvec[0];
    int rstCurrentSize = result.size();


    // std::ofstream outfile("debug.csv", std::ofstream::app);
    // outfile << "output\n\n\n";
    // outfile << "(SKback - 1)->len = " << int((SKback - 1)->len) << "\n";


    valtype **V = f->M[0];


    if((SKback - 1)->len == 1)
    {
      SK = SKback - 1;
      for(indtype i = 0; i < f->N; ++i)
      {
        bool allBetween = 1;
        for(indtype k = 0; k < f->dl and allBetween; ++k)
        {
          if(V[i][k + f->dlst] < SK->MIN[k]) allBetween = 0;
        }
        for(indtype k = 0; k < f->du and allBetween; ++k)
        {
          if(V[i][k + f->dust] > SK->MAX[k]) allBetween = 0;
        }
        if(allBetween)
        {
          *hope = i;
          result.push_back(hopeV);
        }
      }
      // update totalSize
      {
        int addSize = result.size() - rstCurrentSize;
        if(addSize > 0) f->totalSize.fetch_and_add(addSize);
      }
      return SK - &SKvec[0];
    }


    while(true)
    {
      // outfile << "rstCurrentSize = " << (int)rstCurrentSize << ", ";
      // (SKback - 1)->print(f->d, f->dl, f->du, outfile);
      // outfile << "parent printed ___________________________________\n\n";


      SKback->copyParentGene(*(SKback - 1), f->d, f->dl, f->du);


      // SKback->print(f->d, f->dl, f->du, outfile);
      // outfile << "parent copied ___________________________________\n\n";


      indtype boo = SKback->grow(f->M, f->d, f->dlst, f->dl, f->dust, f->du, hope,
                                 f->mask, SRVcntr //, &outfile
                                 );


      // outfile << "boo == " << (int)boo << "\n";
      // SKback->print(f->d, f->dl, f->du, outfile);
      // outfile << "child grown ___________________________________\n\n";


      // continue to give birth.
      if(boo == 1)
      {
        ++SKback;
        continue;
      }


      if(boo == 3) // if len in the child becomes 1
      {
        indtype i = SKback->LB[0], iend = SKback->UB[0] + 1;
        for(; i < iend; ++i)
        {
          hopeV.back() = i;
          result.push_back(hopeV);
        }
      }
      else if(boo == 2) // if lower bounds and upper bounds overlap
      {
        std::copy(SKback->UB, SKback->UB + SKback->len, hope);
        result.push_back(hopeV);
      }


      while(true)
      {
        bool updateBool = (SKback - 1)->update(f->M, f->d, f->dlst, f->dl, f->dust, f->du);


        // (SKback - 1)->print(f->d, f->dl, f->du, outfile);
        // outfile << "parent updated ___________________________________\n\n";


        if(updateBool != 0) break;


        hope -= (SKback - 1)->Nzeroed;
        --SKback;


        if(SKback - SK <= 1)
        {
          // update totalSize
          int addSize = result.size() - rstCurrentSize;
          if(addSize > 0) f->totalSize.fetch_and_add(addSize);
          return 0; // all the combinations have been tried
        }
      }


      // update totalSize
      {
        int addSize = result.size() - rstCurrentSize;
        if(addSize > 0) f->totalSize.fetch_and_add(addSize);
        rstCurrentSize += addSize;
      }


      if(f->totalSize >= f->sizeNeed or (double)std::clock() > f->endTime)
      {
        break;
      }
    }


    return SKback - SK;
  }


  void initializeForKnapsack(
      shared<valtype, indtype>*, valtype*, valtype*, indtype*, indtype*);


  int TTTstackRunForKnapsack(tbb::spin_mutex*, bool);
};




template<typename valtype, typename indtype, bool mk, bool useBiSearch>
indtype growTwin(mflsssOBJ<valtype, indtype, mk, useBiSearch> &Xmflsss,
                 mflsssOBJ<valtype, indtype, mk, useBiSearch> &Ymflsss,
                 std::ofstream *outfile = nullptr) // outfile prints log
{
  shared<valtype, indtype> &f = *Xmflsss.f;
  mPAT<valtype, indtype, mk, useBiSearch> &X = *Xmflsss.SKback;
  X.copyParentGene(*(Xmflsss.SKback - 1), f.d, f.dl, f.du);


  indtype boo = findBoundCpp<valtype, indtype, mk, useBiSearch> (
    X.len, f.d, f.dlst, f.dl, f.dust, f.du, X.MIN, X.MAX,
    X.LB, X.sumLB, X.UB, X.sumUB, f.M, f.mask, Xmflsss.SRVcntr);


  if(outfile != nullptr)
  {
    X.print(f.d, f.dl, f.du, *outfile);
    *outfile << "\n\nBounds found ___________________________________, boo = " << (int)boo << "\n";
  }


  if(boo == 0) return 0;
  if(X.len == 1) return 3;
  if(boo == 2) return 2;


  // find the slot that has the least gap
  X.position = 0;
  indtype nonzeroMin = -1;


  vec<indtype> acntr(X.len);
  indtype *overlapPosition = &*acntr.begin(), *olpend = overlapPosition;


  for(indtype i = 0; i < X.len; ++i)
  {
    indtype tmp = X.UB[i] - X.LB[i];
    if(tmp == 0)
    {
      *Xmflsss.hope = X.UB[i];
      ++Xmflsss.hope;
      *olpend = i;
      ++olpend;
    }
    else if(nonzeroMin > tmp or nonzeroMin < 0)
    {
      nonzeroMin = tmp;
      X.position = i;
    }
  }


  // erase all positions where LB and UB meet.
  indtype &Nzeroed = X.Nzeroed;
  Nzeroed = olpend - overlapPosition;
  if(Nzeroed > 0)
  {
    vec<valtype> cntrS(f.d); valtype *S = &*cntrS.begin();
    *olpend = X.len;
    for(indtype i = 0; i < Nzeroed; ++i)
    {
      indtype &st = overlapPosition[i], &end = overlapPosition[i + 1];
      mvalPlus(S, S, f.M[0][X.UB[st]], f.d);
      std::copy(X.LB + st + 1, X.LB + end, X.LB + st - i);
      std::copy(X.UB + st + 1, X.UB + end, X.UB + st - i);
    }
    X.len -= Nzeroed;


    mvalMinus(X.MIN, X.MIN, S + f.dlst, f.dl); // target changes, so MIN and MAX change
    mvalMinus(X.MAX, X.MAX, S + f.dust, f.du);
    mvalMinus(X.sumLB, X.sumLB, S, f.d);
    mvalMinus(X.sumUB, X.sumUB, S, f.d);


    // after erasion, position may change. Adjust position
    {
      indtype tmp = 0;
      for(indtype *i = overlapPosition; i < olpend; ++i)
      {
        if(X.position > *i) ++tmp;
        else break;
      }
      X.position -= tmp;
    }
  }


  // x, pass wisdom to your twin!
  {
    Ymflsss.f = &f;
    Ymflsss.hopeV.assign(f.subsetSize, 0);
    std::copy(&Xmflsss.hopeV[0], Xmflsss.hope, &Ymflsss.hopeV[0]);
    Ymflsss.hope = &Ymflsss.hopeV[0] + (Xmflsss.hope - &Xmflsss.hopeV[0]);
    Ymflsss.SKvec.resize(Xmflsss.SKvec.size() - 1);
    Ymflsss.SKback = &Ymflsss.SKvec[1];
    Ymflsss.indvec.assign(Xmflsss.indvec.size() - 2 * (int)Xmflsss.SKvec[0].len, 0);
    Ymflsss.valvec.assign(Xmflsss.valvec.size() - 2 * ((int)f.d + f.dl + f.du), 0);
    Ymflsss.SRVcntr.assign(f.d, 0);
    Ymflsss.result.reserve(f.sizeNeed);
  }


  // initialize Ymflsss.SKvec, the first element
  Ymflsss.setAnSK(&Ymflsss.SKvec[0], &Ymflsss.indvec[0],
                  &Ymflsss.valvec[0], Xmflsss.SKback->len);


  // X takes the lower half, Y takes the upper half
  mPAT<valtype, indtype, mk, useBiSearch> &Y = *(Ymflsss.SKback - 1);


  X.beenUpdated = 1;
  Y.beenUpdated = 1;
  std::copy(X.MIN, X.MIN + f.dl + f.du, Y.MIN);


  std::copy(X.sumUB, X.sumUB + f.d, Y.sumUB);
  std::copy(X.UB, X.UB + X.len, Y.UB);
  indtype cap = (X.UB[X.position] + X.LB[X.position]) / 2;
  indtype capResv = cap;
  indtype i = X.position;
  for(; i >= 0; --i, --cap)
  {
    if(X.UB[i] <= cap) break;
    mvalMinus(X.sumUB, X.sumUB, f.M[0][X.UB[i]], f.d);
    X.UB[i] = cap;
  }
  mvalPlus(X.sumUB, X.sumUB, f.M[X.position - i - 1][X.UB[i + 1]], f.d);


  cap = capResv;
  ++cap;
  i = X.position;
  std::copy(X.LB, X.LB + i, Y.LB);
  std::copy(X.sumLB, X.sumLB + f.d, Y.sumLB);
  for(; i < X.len; ++i, ++cap)
  {
    if(X.LB[i] >= cap)
    {
      std::copy(X.LB + i, X.LB + X.len, Y.LB + i);
      break;
    }
    mvalMinus(Y.sumLB, Y.sumLB, f.M[0][X.LB[i]], f.d);
    Y.LB[i] = cap;
  }
  mvalPlus(Y.sumLB, Y.sumLB, f.M[i - X.position - 1][Y.LB[X.position]], f.d);


  ++Xmflsss.SKback;
  return 1;
}




template<typename valtype, typename indtype, bool mk, bool useBiSearch>
void mitosis(vec<mflsssOBJ<valtype, indtype, mk, useBiSearch> > &descendants,
             shared<valtype, indtype> &f,
             vec<vec<indtype> > &rstCollection,
             indtype *LB, indtype *UB, valtype *target, valtype *ME,
             int threads, int avgThreadLoad)
{
  int Ndescendants = 1;
  if(threads > 1)
  {
    Ndescendants = 1 << ((int)std::log2(threads * avgThreadLoad + 0.0) + 1);
  }
  descendants.resize(Ndescendants);
  descendants[0].initialize(&f, target, ME, LB, UB);
  vec<unsigned char> acntr(Ndescendants, 0);
  unsigned char *dead = &*acntr.begin();
  int j = 1;


  while(j < Ndescendants)
  {
    int iend = j;
    for(int i = 0; i < iend; ++i, ++j)
    {
      if(f.totalSize >= f.sizeNeed) return;


      if(dead[i])
      {
        dead[j] = 1;
        continue;
      }


      indtype boo = growTwin(descendants[i], descendants[j] //, &mfile
                             );


      // std::cout << "grown\n";
      mPAT<valtype, indtype, mk, useBiSearch> &tmp = *descendants[i].SKback;
      if(boo == 0)
      {
        dead[i] = 1;
        dead[j] = 1;
      }
      else if(boo == 3)
      {
        indtype i = tmp.LB[0], iend = tmp.UB[0] + 1;
        for(; i < iend; ++i)
        {
          descendants[i].hopeV.back() = i;
          rstCollection.push_back(descendants[i].hopeV);
          ++f.totalSize;
        }
        dead[i] = 1;
        dead[j] = 1;
      }
      else if(boo == 2)
      {
        std::copy(tmp.UB, tmp.UB + tmp.len, descendants[i].hope);
        rstCollection.push_back(descendants[i].hopeV);
        ++f.totalSize;
        dead[i] = 1;
        dead[j] = 1;
      }
    }
  }


  if(f.totalSize >= f.sizeNeed) return;


  // cleansing descendants[]
  int validDescendents = Ndescendants - std::accumulate(dead, dead + Ndescendants, (int)0);
  if(validDescendents == Ndescendants) return;


  vec<mflsssOBJ<valtype, indtype, mk, useBiSearch> >
    descendantsRemain(validDescendents);
  for(int i = 0, k = 0; i < Ndescendants; ++i)
  {
    if(dead[i]) continue;
    descendants[i].swap(descendantsRemain[k]);
    ++k;
  }
  descendants.swap(descendantsRemain);
}




template<typename valtype, typename indtype, bool mk, bool useBiSearch>
struct parMflsssOBJ: public RcppParallel::Worker // works for mflsssComoPar()
{
  vec<mflsssOBJ<valtype, indtype, mk, useBiSearch> > &mflsssOBJvec;
  shared<valtype, indtype> *f;
  dynamicTasking *dT;
  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      mflsssOBJvec[objI].TTTstackRun();
      if(f->totalSize >= f->sizeNeed or (double)std::clock() > f->endTime) break;
    }
  }


  parMflsssOBJ(vec<mflsssOBJ<valtype, indtype, mk, useBiSearch> >
                 &mflsssOBJvec, int maxCore): mflsssOBJvec(mflsssOBJvec)
  {
    dynamicTasking dt(maxCore, mflsssOBJvec.size());
    dT = &dt;
    f = mflsssOBJvec.begin()->f;
    RcppParallel::parallelFor(0, maxCore, *this);
  }
};




template<typename valtype, typename indtype, bool mk, bool useBiSearch>
struct parMflsssOBJbyCore: public RcppParallel::Worker // works for mflsssPar()
{
  vec<mflsssOBJ<valtype, indtype, mk, useBiSearch> > &mflsssOBJvec; // mflsssOBJvec is only of size maxCore
  shared<valtype, indtype> *f;
  valtype *target;
  dummyContainers<valtype, indtype, mk, useBiSearch> *dummyCs;
  valtype *ME;
  indtype *commonLB, *commonUB;
  vec<vec<vec<indtype> > > &rst; // rst is of size
  dynamicTasking *dT;
  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      mflsssOBJvec[st].initialize(
          f, target + objI * f->d, ME, commonLB, commonUB, dummyCs + st);
      mflsssOBJvec[st].TTTstackRun();
      mflsssOBJvec[st].finalize(dummyCs + st);


      // harvest
      {
        for(int i = 0, iend = mflsssOBJvec[st].result.size(); i < iend; ++i)
        {
          rst[st].resize(rst[st].size() + 1);
          rst[st].back().swap(mflsssOBJvec[st].result[i]);
        }
      }


      if(f->totalSize >= f->sizeNeed or (double)std::clock() > f->endTime) break;
    }
  }


  parMflsssOBJbyCore(vec<mflsssOBJ<valtype, indtype, mk, useBiSearch> > &mflsssOBJvec,
                     shared<valtype, indtype> *f, valtype *target,
                     dummyContainers<valtype, indtype, mk, useBiSearch> *dummyCs,
                     valtype *ME, indtype *commonLB, indtype *commonUB,
                     vec<vec<vec<indtype> > > &rst,
                     unsigned keyTargetSize, int maxCore):
    mflsssOBJvec(mflsssOBJvec), f(f), target(target),
    dummyCs(dummyCs), ME(ME), commonLB(commonLB), commonUB(commonUB), rst(rst)
  {
    mflsssOBJvec.resize(maxCore);
    dynamicTasking dt(maxCore, keyTargetSize);
    dT = &dt;
    RcppParallel::parallelFor(0, maxCore, *this);
  }
};







// ================================================================================================
// Knapsack problems
// ================================================================================================
template<typename valtype, typename indtype, bool mk, bool useBiSearch>
inline void mflsssOBJ<valtype, indtype, mk, useBiSearch>::
  initializeForKnapsack(
    shared<valtype, indtype> *fixedInfo, valtype *target,
    valtype *ME, indtype *LB, indtype *UB)
{
  f = fixedInfo;
  std::size_t stackLen = (unsigned)f->subsetSize + 2;
  unsigned biscaleFactor = (unsigned)std::log2(f->N + 0.0 - f->subsetSize) + 3;
  indvec.assign(stackLen * (stackLen + 1) / 2 * 3 * biscaleFactor, 0);
  valvec.assign((3 * (std::size_t)f->d + (std::size_t)f->dl +
    (std::size_t)f->du) * stackLen * biscaleFactor, 0);
  SKvec.resize((unsigned)f->subsetSize * biscaleFactor);
  SRVcntr.assign(f->d, 0);


  hopeV.assign(f->subsetSize, 0);
  hope = &hopeV[0];
  existingProfitSum = 0;


  mPAT<valtype, indtype, mk, useBiSearch> *SKbegin = &SKvec.front();


  setAnSK(SKbegin, &indvec[0], &valvec[0], f->subsetSize);
  SKback = SKbegin + 1;


  for(indtype i = 0; i < SKbegin->len; ++i)
  {
    SKbegin->LB[i] = LB[i];
    SKbegin->UB[i] = UB[i];
  }


  // assign MIN and MAX
  {
    for(indtype i = f->dlst, iend = f->dlst + f->dl; i < iend; ++i)
    {
      SKbegin->MIN[i - f->dlst] = target[i] - ME[i];
    }
    for(indtype i = f->dust, iend = f->dust + f->du; i < iend; ++i)
    {
      SKbegin->MAX[i - f->dust] = target[i] + ME[i];
    }
  }


  iterSum<valtype, indtype> (SKbegin->sumLB, f->M[0], SKbegin->LB, SKbegin->len, f->d);
  iterSum<valtype, indtype> (SKbegin->sumUB, f->M[0], SKbegin->UB, SKbegin->len, f->d);
}




template<typename valtype, typename indtype, bool mk, bool useBiSearch>
inline int mflsssOBJ<valtype, indtype, mk, useBiSearch>::
  TTTstackRunForKnapsack(
    tbb::spin_mutex *mx, bool verbose //, std::ofstream *outfile = nullptr
  )
{
  mPAT<valtype, indtype, mk, useBiSearch> *SK = &SKvec[0];


  // std::ofstream outfile("proboutput.csv", std::ofstream::out|std::ofstream::app);
  // outfile << "output\n\n\n";


  // valtype **V = f->M[0];
  while(true)
  {
    // outfile << "rstCurrentSize = " << rstCurrentSize << ", ";
    // (SKback - 1)->print(f->d, f->dl, f->du, outfile);
    // outfile << "parent printed ___________________________________\n\n";


    SKback->copyParentGene(*(SKback - 1), f->d, f->dl, f->du);


    // SKback->print(f->d, f->dl, f->du, outfile);
    // outfile << "parent copied ___________________________________\n\n";


    indtype boo = SKback->growForKnapsack(
      f->M, f->d, f->dlst, f->dl, f->dust, f->du, hope,
      f->mask, f->profitVec, existingProfitSum, f->optimalProfit, SRVcntr);


    // outfile << "boo == " << (int)boo << "\n";
    // SKback->print(f->d, f->dl, f->du, outfile);
    // outfile << "child grown ___________________________________\n\n";


    // continue to give birth.
    if(boo == 1)
    {
      ++SKback;
      continue;
    }


    // if(boo == 3 or boo == 2)
    if(boo != 0)
    {
      std::copy(SKback->UB, SKback->UB + SKback->len, hope);
      valtype tmpProfit = 0;
      for(indtype i = 0; i < f->subsetSize; ++i)
      {
        tmpProfit += f->profitVec[hopeV[i]];
      }
      mx->lock();
      {
        if(tmpProfit > f->optimalProfit)
        {
          f->optimalProfit = tmpProfit;
          std::copy(hopeV.begin(), hopeV.end(), f->optimalSolution);
          // if(verbose) std::cout << "Updated profit = " << tmpProfit << "\n";
          // In Linux environment, the above still smashes C stack.
        }
      }
      mx->unlock();
    }


    while(true)
    {
      bool updateBool = (SKback - 1)->update(f->M, f->d, f->dlst, f->dl, f->dust, f->du);


      // (SKback - 1)->print(f->d, f->dl, f->du, outfile);
      // outfile << "parent updated ___________________________________\n\n";


      if(updateBool != 0) break;


      // Recover hope and the profit sum. Still struggling to decide if using an array to store previous
      // 'existingProfitSum' would be more efficient.
      {
        indtype *i = hope - 1;
        --SKback;
        hope -= SKback->Nzeroed;
        for(; i >= hope; --i)
        {
          existingProfitSum -= f->profitVec[*i];
        }
      }


      if(SKback - SK <= 1)
      {
        // std::cout << " All combinations have been tried\n";
        return 0; // all the combinations have been tried
      }
    }


    if((double)std::clock() > f->endTime)
    {
      return -1;
    }
  }


  return SKback - SK;
}




// This growTwin() function is made for knapsack
template<typename valtype, typename indtype, bool mk, bool useBiSearch>
inline indtype growTwinForKnapsack(
    mflsssOBJ<valtype, indtype, mk, useBiSearch> &Xmflsss,
    mflsssOBJ<valtype, indtype, mk, useBiSearch> &Ymflsss,
    std::ofstream *outfile = nullptr) // outfile prints log
{
  shared<valtype, indtype> &f = *Xmflsss.f;
  mPAT<valtype, indtype, mk, useBiSearch> &X = *Xmflsss.SKback;
  X.copyParentGene(*(Xmflsss.SKback - 1), f.d, f.dl, f.du);


  // X.print(f.d, f.dl, f.du, *outfile);
  // *outfile << "\n\n";


  indtype boo = findBoundCpp<valtype, indtype, mk, useBiSearch> (
    X.len, f.d, f.dlst, f.dl, f.dust, f.du, X.MIN, X.MAX,
    X.LB, X.sumLB, X.UB, X.sumUB, f.M, f.mask, Xmflsss.SRVcntr);


  // See if the sum of upper bounds is less than the current maximal profit
  if(boo != 0)
  {
    double S = Xmflsss.existingProfitSum;
    for(indtype i = 0; i < X.len; ++i)
    {
      S += f.profitVec[X.UB[i]];
    }
    if(S <= f.optimalProfit) return 0;
  }


    if(outfile != nullptr)
    {
      X.print(f.d, f.dl, f.du, *outfile);
      *outfile << "\n\nBounds found ___________________________________, boo = "
        << (int)boo << "\n";
    }


    if(boo == 0) return 0;
    if(X.len == 1) return 3;
    if(boo == 2) return 2;


    // find the slot that has the least gap
    X.position = 0;
    indtype nonzeroMin = -1;


    vec<indtype> acntr(X.len);
    indtype *overlapPosition = &*acntr.begin(), *olpend = overlapPosition;


    for(indtype i = 0; i < X.len; ++i)
    {
      indtype tmp = X.UB[i] - X.LB[i];
      if(tmp == 0)
      {
        *Xmflsss.hope = X.UB[i];
        // std::cout << "in growTwinForKnapsack(), we get a hope\n";
        Xmflsss.existingProfitSum += f.profitVec[*Xmflsss.hope];
        // std::cout << "Xmflsss.existingProfitSum becomes = " << Xmflsss.existingProfitSum << "\n";
        ++Xmflsss.hope;
        *olpend = i;
        ++olpend;
      }
      else if(nonzeroMin > tmp or nonzeroMin < 0)
      {
        nonzeroMin = tmp;
        X.position = i;
      }
    }


    // erase all positions where LB and UB meet.
    indtype &Nzeroed = X.Nzeroed;
    Nzeroed = olpend - overlapPosition;
    if(Nzeroed > 0)
    {
      vec<valtype> cntrS(f.d);
      valtype *S = &*cntrS.begin();
      *olpend = X.len;
      for(indtype i = 0; i < Nzeroed; ++i)
      {
        indtype &st = overlapPosition[i], &end = overlapPosition[i + 1];
        mvalPlus(S, S, f.M[0][X.UB[st]], f.d);
        std::copy(X.LB + st + 1, X.LB + end, X.LB + st - i);
        std::copy(X.UB + st + 1, X.UB + end, X.UB + st - i);
      }
      X.len -= Nzeroed;


      mvalMinus(X.MIN, X.MIN, S + f.dlst, f.dl); // target changes, so MIN and MAX change
      mvalMinus(X.MAX, X.MAX, S + f.dust, f.du);
      mvalMinus(X.sumLB, X.sumLB, S, f.d);
      mvalMinus(X.sumUB, X.sumUB, S, f.d);


      // after erasion, position may change. Adjust position
      {
        indtype tmp = 0;
        for(indtype *i = overlapPosition; i < olpend; ++i)
        {
          if(X.position > *i) ++tmp;
          else break;
        }
        X.position -= tmp;
      }
    }


    // x, pass wisdom to your twin!
    {
      Ymflsss.f = &f;
      Ymflsss.hopeV.assign(f.subsetSize, 0);
      std::copy(&Xmflsss.hopeV[0], Xmflsss.hope, &Ymflsss.hopeV[0]);
      Ymflsss.hope = &Ymflsss.hopeV[0] + (Xmflsss.hope - &Xmflsss.hopeV[0]);


      Ymflsss.existingProfitSum = Xmflsss.existingProfitSum;


      Ymflsss.SKvec.resize(Xmflsss.SKvec.size() - 1);
      Ymflsss.SKback = &Ymflsss.SKvec[1];
      Ymflsss.indvec.assign(Xmflsss.indvec.size() - 2 * (int)Xmflsss.SKvec[0].len, 0);
      Ymflsss.valvec.assign(Xmflsss.valvec.size() - 2 * ((int)f.d + f.dl + f.du), 0);
      Ymflsss.SRVcntr.assign(f.d, 0);
      Ymflsss.result.reserve(f.sizeNeed);
    }


    // initialize Ymflsss.SKvec, the first element
    Ymflsss.setAnSK(&Ymflsss.SKvec[0], &Ymflsss.indvec[0],
                    &Ymflsss.valvec[0], Xmflsss.SKback->len);


    // X takes the lower half, Y takes the upper half
    mPAT<valtype, indtype, mk, useBiSearch> &Y = *(Ymflsss.SKback - 1);


    X.beenUpdated = 1;
    Y.beenUpdated = 1;
    std::copy(X.MIN, X.MIN + f.dl + f.du, Y.MIN);


    std::copy(X.sumUB, X.sumUB + f.d, Y.sumUB);
    std::copy(X.UB, X.UB + X.len, Y.UB);
    indtype cap = (X.UB[X.position] + X.LB[X.position]) / 2;
    indtype capResv = cap;
    indtype i = X.position;
    for(; i >= 0; --i, --cap)
    {
      if(X.UB[i] <= cap) break;
      mvalMinus(X.sumUB, X.sumUB, f.M[0][X.UB[i]], f.d);
      X.UB[i] = cap;
    }
    mvalPlus(X.sumUB, X.sumUB, f.M[X.position - i - 1][X.UB[i + 1]], f.d);


    cap = capResv;
    ++cap;
    i = X.position;
    std::copy(X.LB, X.LB + i, Y.LB);
    std::copy(X.sumLB, X.sumLB + f.d, Y.sumLB);
    for(; i < X.len; ++i, ++cap)
    {
      if(X.LB[i] >= cap)
      {
        std::copy(X.LB + i, X.LB + X.len, Y.LB + i);
        break;
      }
      mvalMinus(Y.sumLB, Y.sumLB, f.M[0][X.LB[i]], f.d);
      Y.LB[i] = cap;
    }
    mvalPlus(Y.sumLB, Y.sumLB, f.M[i - X.position - 1][Y.LB[X.position]], f.d);


    ++Xmflsss.SKback;
    return 1;
}




template<typename valtype, typename indtype, bool mk, bool useBiSearch>
inline void mitosisForKnapsack(
    vec<mflsssOBJ<valtype, indtype, mk, useBiSearch> > &descendants,
    shared<valtype, indtype> &f,
    indtype *LB, indtype *UB, valtype *target, valtype *ME,
    int threads, int avgThreadLoad, bool verbose)
{
  int Ndescendants = 1;
  if(threads > 1)
  {
    Ndescendants = 1 << ((int)std::log2(threads * avgThreadLoad) + 1);
  }


  descendants.resize(Ndescendants);
  descendants[0].initializeForKnapsack(&f, target, ME, LB, UB);
  vec<unsigned char> acntr(Ndescendants, 0);
  unsigned char *dead = &*acntr.begin();
  int j = 1;


  while(j < Ndescendants)
  {
    int iend = j;
    for(int i = 0; i < iend; ++i, ++j)
    {
      if(dead[i])
      {
        dead[j] = 1;
        continue;
      }


      indtype boo = growTwinForKnapsack(descendants[i], descendants[j] //, &outfile
                                        );


      mPAT<valtype, indtype, mk, useBiSearch> &tmp = *descendants[i].SKback;
      if(boo == 0)
      {
        dead[i] = 1;
        dead[j] = 1;
      }
      else if(boo != 1)
      {
        std::copy(tmp.UB, tmp.UB + tmp.len, descendants[i].hope);
        valtype tmpProfit = 0;
        for(indtype k = 0; k < f.subsetSize; ++k)
        {
          tmpProfit += f.profitVec[descendants[i].hopeV[k]];
        }
        if(tmpProfit > f.optimalProfit)
        {
          f.optimalProfit = tmpProfit;
          std::copy(descendants[i].hopeV.begin(),
                    descendants[i].hopeV.end(), f.optimalSolution);
          if(verbose) Rcpp::Rcout << "Updated profit = " << tmpProfit << "\n";
        }
        dead[i] = 1;
        dead[j] = 1;
      }
    }
  }


  // cleansing descendants[]
  int validDescendents = Ndescendants - std::accumulate(dead, dead + Ndescendants, (int)0);
  // std::cout << "validDescendents = " << validDescendents << "\n";
  if(validDescendents == Ndescendants) return;


  vec<mflsssOBJ<valtype, indtype, mk, useBiSearch> > descendantsRemain(validDescendents);
  // std::cout << "validDescendents = " << validDescendents << "\n";
  for(int i = 0, k = 0; i < Ndescendants; ++i)
  {
    if(dead[i]) continue;
    descendants[i].swap(descendantsRemain[k]);
    ++k;
  }
  descendants.swap(descendantsRemain);
  return;
}




template<typename valtype, typename indtype, bool mk, bool useBiSearch>
struct parMflsssOBJforKnapsack: public RcppParallel::Worker // works for mflsssComoPar()
{
  bool verbose;
  tbb::spin_mutex *mx;
  vec<mflsssOBJ<valtype, indtype, mk, useBiSearch> > &mflsssOBJvec;
  // shared<valtype, indtype> *f;
  dynamicTasking *dT;
  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      // std::cout << "objI = " << objI << "\n";
      int tmp = mflsssOBJvec[objI].TTTstackRunForKnapsack(mx, verbose);
      // std::cout << "tmp = " << tmp << "\n";
      if(tmp == -1) break;
    }
  }


  parMflsssOBJforKnapsack(
    vec<mflsssOBJ<valtype, indtype, mk, useBiSearch> > &mflsssOBJvec, int maxCore):
    mflsssOBJvec(mflsssOBJvec)
  {
    dynamicTasking dt(maxCore, mflsssOBJvec.size());
    dT = &dt;
    tbb::spin_mutex mtx;
    mx = &mtx;
    RcppParallel::parallelFor(0, maxCore, *this);
  }
};



