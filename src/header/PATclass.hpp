# pragma once
# include <fstream>
# include "triMatOneD.hpp"
# include "macros.hpp"
# include "oneDfindBound.hpp"
# include "raiseSupressBound.hpp"




/*
template<typename valtype, typename indtype>
struct PAT
{
  indtype position; // position would also be the length of UBleftReserve
  indtype s, send;
  indtype len;
  valtype target, sumLB, sumUB;
  indtype *LB, *UB, *UBleftResv; // UBleftResv always needs to be initialized


  inline void print()
  {
    Rcpp::Rcout << "position = " << (int)position << ", s = "<< (int)s <<", send = " << (int)send <<
      ", len = " << (int)len << "\n";
    Rcpp::Rcout << "target = " << target << "\n";
    Rcpp::Rcout << "sumLB = " << sumLB << "\n";
    Rcpp::Rcout << "sumUB = " << sumUB << "\n";


    Rcpp::Rcout << "LB = ";
    for(int i = 0, iend = len; i < iend; ++i)
    {
      Rcpp::Rcout << (int)LB[i] << ", ";
    }
    Rcpp::Rcout<<"\n";


    Rcpp::Rcout<<"UB = ";
    for(int i = 0, iend = len; i < iend; ++i)
    {
      Rcpp::Rcout << (int)UB[i] << ", ";
    }
    Rcpp::Rcout << "\n";


    Rcpp::Rcout << "UBleftResv = ";
    indtype UBleftResvSize = 0;
    if(position != len) UBleftResvSize = position;
    for(int i = 0, iend = UBleftResvSize; i < iend; ++i)
    {
      Rcpp::Rcout << (int)UBleftResv[i] <<", ";
    }
    Rcpp::Rcout << "\n\n";
  }


  inline void print(std::ofstream &outfile)
  {
    outfile << "position =, " << (int)position << ", s =, "<< (int)s <<", send =, " << (int)send <<
      ", len =, " << (int)len << "\n";
    outfile << "target =, " << target << "\n";
    outfile << "sumLB =, " << sumLB << "\n";
    outfile << "sumUB =, " << sumUB << "\n";


    outfile << "LB =, ";
    for(int i = 0, iend = len; i < iend; ++i)
    {
      outfile << (int)LB[i] << ", ";
    }
    outfile << "\n";


    outfile << "UB =, ";
    for(int i = 0, iend = len; i < iend; ++i)
    {
      outfile << (int)UB[i] << ", ";
    }
    outfile << "\n";


    outfile << "UBleftResv =, ";
    indtype UBleftResvSize = 0;
    if(position != len) UBleftResvSize = position;
    for(int i = 0, iend = UBleftResvSize; i < iend; ++i)
    {
      outfile << (int)UBleftResv[i] <<", ";
    }
    outfile << "\n";
  }


  // len is the parent's subset size
  inline void copyParentGene(PAT &x) // x is the parent
  {
    len = x.len;


    indtype UBleftResvSize = 0;
    if(x.position != len) UBleftResvSize = x.position;


    target = x.target;
    sumLB = x.sumLB;
    sumUB = x.sumUB;


    LB = x.UBleftResv + UBleftResvSize;
    UB = LB + len;


    std::memcpy(LB, x.LB, sizeof(indtype) * (std::size_t)len);
    std::memcpy(UB, x.UB, sizeof(indtype) * (std::size_t)len);
  }


  // equavalent to giveBirth(), and len here is still gene from the parent
  inline indtype grow(valtype  ME, valtype **M, bool useBiSearch, std::ofstream *outfile = nullptr)
  {
    // std::clock_t t = std::clock();
    indtype boo = findBoundCpp(len, target, ME, LB, sumLB, UB, sumUB, M, useBiSearch);
    // findBoundTime += std::clock() - t;
    // this->print(d, *outfile);
    // *outfile << "Bounds found ___________________________________\n";
    // std::cout << "LB == ";
    // for(int i = 0; i < len; ++i)
    // {
    //   std::cout << (int)LB[i] << ", ";
    // }
    // std::cout << "    sumLB == " << sumLB << "\n";


    // std::cout << "UB == ";
    // for(int i = 0; i < len; ++i)
    // {
    //   std::cout << (int)UB[i] << ", ";
    // }
    // std::cout << "    sumUB == " << sumUB << "\n";


    if(boo == 0) return 0;
    if(len == 1) return 3;
    if(boo == 2) return 2;


    // find the slot that has the least gap
    position = 0;
    indtype Min = *UB - *LB;
    for(indtype i = 1; i < len; ++i)
    {
      indtype tmp = UB[i] - LB[i];
      if(Min > tmp)
      {
        Min = tmp;
        position = i;
      }
    }


    if(position == 0)
    {
      s = LB[position];
      send = UB[position];


      target -= M[0][s];
      sumLB -= M[0][s];
      sumUB -= M[0][send];


      // erase LB's and UB's first elements
      ++LB;
      ++UB;
      --len;
      UBleftResv = UB + len;
    }
    else if(position == len - 1)
    {
      s = UB[position];
      send = LB[position];


      target -= M[0][s];
      sumLB -= M[0][send];
      sumUB -= M[0][s];


      --len;
      UBleftResv = UB + len;
    }
    else
    {
      s = LB[position];
      send = UB[position];


      target -= M[0][s];
      sumLB -= M[0][s];
      sumUB -= M[0][send];


      std::copy(LB + position + 1, LB + len, LB + position);
      std::copy(UB + position + 1, UB + len, UB + position);


      --len;
      UBleftResv = UB + len;
      std::memcpy(UBleftResv, UB, sizeof(indtype) * position);


      // supress the left part of UB
      indtype cap = s - 1;
      indtype i = position - 1;
      for(; i >= 0; --i, --cap)
      {
        if(UB[i] <= cap) break;
        else
        {
          sumUB -= M[0][UB[i]];
          UB[i] = cap;
        }
      }
      if(i != position - 1) sumUB += M[position - 1 - i - 1][UB[i + 1]];
    }
    return 1;
  }


  inline indtype update(valtype **M)
  {
    if(s == send) return 0;
    target += M[0][s];


    if(position == 0)
    {
      ++s;
      target -= M[0][s];


      indtype bottom = s + 1;
      indtype i = 0;
      for(; i < len; ++i, ++bottom)
      {
        if(LB[i] >= bottom) break;
        else
        {
          LB[i] = bottom;
        }
      }
      if(i != 0) sumLB += M[i - 1][LB[0]] - M[i - 1][LB[0] - 1];
    }
    else if(position == len)
    {
      --s;
      target -= M[0][s];


      indtype cap = s - 1;
      indtype i = len - 1;
      for(; i >= 0; --i, --cap)
      {
        if(UB[i] <= cap) break;
        else
        {
          UB[i] = cap;
        }
      }
      if(i != len - 1) sumUB += M[len - 1 - i - 1][UB[i + 1]] - M[len - 1 - i - 1][UB[i + 1] + 1];
    }
    else
    {
      ++s;
      target -= M[0][s];


      indtype bottom = s + 1;
      indtype i = position;
      for(; i < len; ++i, ++bottom)
      {
        if(LB[i] >= bottom) break;
        else
        {
          LB[i] = bottom;
        }
      }
      if(i != position) sumLB += M[i - position - 1][LB[position]] - M[i - position - 1][LB[position] - 1];


      i = position - 1;
      for(; i >= 0; --i)
      {
        if(UB[i] >= UBleftResv[i]) break;
        else
        {
          ++UB[i];
        }
      }
      if(i != position - 1) sumUB += M[position - 1 - i - 1][UB[i + 1]] - M[position - 1 - i - 1][UB[i + 1] - 1];
    }
    return 1;
  }
};
*/




template<typename valtype, typename indtype>
struct PAT
{
  bool beenUpdated;
  indtype position; // position would also be the length of UBleftReserve
  indtype len;
  indtype Nzeroed;
  valtype target, sumLB, sumUB, sumBresv;
  indtype *LB, *UB, *Bresv;


  inline void print(std::ofstream &outfile)
  {
    outfile << "position =, " << (int)position << // ", s =, " << (int)s <<", send =, " << (int)send <<
      ", len =, " << (int)len << ",beenUpdated =," << beenUpdated << "\n";
    outfile << "target =," << target;
    outfile << "\n";


    outfile << "sumLB =," << sumLB;
    outfile << ",,";


    outfile << "sumUB =," << sumUB;
    outfile << ",,";


    outfile << "sumBresv =," << sumBresv;
    outfile << "\n";


    outfile << "LB =,";
    for(int i = 0, iend = len; i < iend; ++i)
    {
      outfile << (int)LB[i] << ",";
    }
    outfile << "\n";


    outfile << "UB =,";
    for(int i = 0, iend = len; i < iend; ++i)
    {
      outfile << (int)UB[i] << ",";
    }
    outfile << "\n";


    outfile << "Bresv =,";
    indtype BresvSize = position + 1;
    for(int i = 0, iend = BresvSize; i < iend; ++i)
    {
      outfile << (int)Bresv[i] << ",";
    }
    outfile << "\n";
  }


  // len is the parent's subset size
  inline void copyParentGene(PAT &x) // x is the parent
  {
    beenUpdated = 0;
    Nzeroed = 0;
    len = x.len;
    indtype *&parentIndEnd = LB;
    {
      if(x.beenUpdated)
      {
        if(x.position <= len / 2) parentIndEnd = x.Bresv + x.position + 1;
        else parentIndEnd = x.Bresv + (len - x.position);
      }
      else
      {
        if(x.position <= len / 2) parentIndEnd = x.Bresv + x.position + 1;
        else parentIndEnd = x.Bresv + (len - x.position);
      }
    }


    UB = LB + len;
    Bresv = UB + len;


    target = x.target;
    sumLB = x.sumLB;
    sumUB = x.sumUB;


    std::memcpy(LB, x.LB, sizeof(indtype) * len); // ! do not think x.LB and x.UB are continous !
    std::memcpy(UB, x.UB, sizeof(indtype) * len);
  }


  // equavalent to giveBirth(), and len here is still gene from the parent
  // inline indtype grow(valtype *ME, valtype ***M, indtype d, bool useBiSearch, std::ofstream *outfile = nullptr)
  inline indtype grow(
      valtype **M, valtype ME, indtype *&hope, bool useBiSearch, std::ofstream *outfile = nullptr)
  {
    // std::clock_t t = std::clock();
    indtype boo = findBoundCpp(len, target, ME, LB, sumLB, UB, sumUB, M, useBiSearch);
    // findBoundTime += std::clock() - t;


    // print(*outfile);
    // *outfile << "Bounds found ___________________________________, boo = " << (int)boo << "\n\n";


    if(boo == 0) return 0;
    if(len == 1) return 3;
    if(boo == 2) return 2;


    // find the slot that has the least gap
    position = 0;
    indtype nonzeroMin = -1;
    // indtype overlapPosition[len], *olpend = overlapPosition;
    vec<indtype> acntr(len);
    indtype *overlapPosition = &*acntr.begin(), *olpend = overlapPosition;
    for(indtype i = 0; i < len; ++i)
    {
      indtype tmp = UB[i] - LB[i];
      if(tmp == 0)
      {
        *hope = UB[i];
        ++hope;
        *olpend = i;
        ++olpend;
      }
      else if(nonzeroMin > tmp or nonzeroMin < 0)
      {
        nonzeroMin = tmp;
        position = i;
      }
    }


    // erase all positions where LB and UB meet.
    Nzeroed = olpend - overlapPosition;
    if(Nzeroed > 0)
    {
      valtype S = 0;
      *olpend = len;
      for(indtype i = 0; i < Nzeroed; ++i)
      {
        indtype &st = overlapPosition[i], &end = overlapPosition[i + 1];
        S += M[0][UB[st]];
        std::copy(LB + st + 1, LB + end, LB + st - i);
        std::copy(UB + st + 1, UB + end, UB + st - i);
      }
      len -= Nzeroed;


      target -= S;
      sumLB -= S;
      sumUB -= S;


      // after erasion, position may change. Adjust position
      {
        indtype tmp = 0;
        for(indtype *i = overlapPosition; i < olpend; ++i)
        {
          if(position > *i) ++tmp;
          else break;
        }
        position -= tmp;
      }
    }


    // reserve sumUB
    // halve space at position
    beenUpdated = 0;
    if(position <= len / 2)
    {
      indtype *&UBleftResv = Bresv;
      valtype &sumUBresv = sumBresv;
      indtype cap = (UB[position] + LB[position]) / 2;
      UBleftResv = UB + len;
      std::copy(UB, UB + position + 1, UBleftResv); // including UB at position, Bresv is of size position + 1!
      sumUBresv = sumUB;
      indtype i = position;
      for(; i >= 0; --i, --cap)
      {
        if(UB[i] <= cap) break;
        sumUB -= M[0][UB[i]];
        UB[i] = cap;
      }
      sumUB += M[position - i - 1][UB[i + 1]];
    }
    else
    {
      indtype *&LBrightResv = Bresv;
      valtype &sumLBresv = sumBresv;
      indtype cap = (UB[position] + LB[position]) / 2 + 1;
      LBrightResv = UB + len;
      std::copy(LB + position, LB + len, LBrightResv);
      sumLBresv = sumLB;
      indtype i = position;
      for(; i < len; ++i, ++cap)
      {
        if(LB[i] >= cap) break;
        sumLB -= M[0][LB[i]];
        LB[i] = cap;
      }
      sumLB += M[i - position - 1][LB[position]];
    }


    return 1;
  }




  inline indtype update(valtype **M)
  {
    if(beenUpdated) return 0;


    if(position <= len / 2)
    {
      indtype cap = UB[position] + 1;
      indtype *&UBleftResv = Bresv;
      valtype &sumUBresv = sumBresv;
      std::copy(UBleftResv, UBleftResv + position + 1, UB);
      sumUB = sumUBresv;
      indtype i = position;
      for(; i < len; ++i, ++cap)
      {
        if(LB[i] >= cap) break;
        sumLB -= M[0][LB[i]];
        LB[i] = cap;
      }
      sumLB += M[i - position - 1][LB[position]];
    }
    else
    {
      indtype cap = LB[position] - 1;
      indtype *&LBrightResv = Bresv;
      valtype &sumLBresv = sumBresv;
      std::copy(LBrightResv, LBrightResv + (len - position), LB + position);
      sumLB = sumLBresv;
      indtype i = position;
      for(; i >= 0; --i, --cap)
      {
        if(UB[i] <= cap) break;
        sumUB -= M[0][UB[i]];
        UB[i] = cap;
      }
      sumUB += M[position - i - 1][UB[i + 1]];
    }


    beenUpdated = true;
    return 1;
  }
};


