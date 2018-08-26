# pragma once
// [[Rcpp::depends(RcppParallel)]]
# include <Rcpp.h>
# include <RcppParallel.h>
// using namespace Rcpp;
// using namespace RcppParallel;


struct dynamicTasking
{
  std::size_t NofCore;
  std::size_t NofAtom;
  tbb::atomic<std::size_t> counter;


  void reset(std::size_t NofCPU, std::size_t NofTask)
  {
    NofCore = NofCPU;
    if(NofCore > NofTask)NofCore = NofTask;
    NofAtom = NofTask;
    counter = 0;
  }


  dynamicTasking(std::size_t NofCPU, std::size_t NofTask)
  {
    reset(NofCPU, NofTask);
  }


  bool nextTaskID(std::size_t &taskID)
  {
    // taskID = counter.fetch_and_add(1);
    taskID = counter.fetch_and_increment();
    if(taskID >= NofAtom) return 0;
    return 1;
  }
};








/*
 struct dynamicTaskingPartition
{
bool internalArrayAssigned;
volatile bool beingReassigned;
tbb::mutex m;


std::size_t NofCore;
std::size_t NofAtom;
tbb::atomic<std::size_t>counter;
volatile unsigned char*dispatchingID;
volatile std::size_t*current, *UB;


dynamicTaskingPartition(){}
~ dynamicTaskingPartition()
{
if(internalArrayAssigned)
{
delete [] UB;
delete [] current;
delete [] dispatchingID;
}
}


void reset(std::size_t NofCPU, std::size_t NofTask, bool atomize){
internalArrayAssigned = 0;
NofCore = NofCPU;
NofAtom = NofTask;
counter = 0;
}


dynamicTaskingPartition(std::size_t NofCPU, std::size_t NofTask, bool atomize){
reset(NofCPU, NofTask, atomize);
}


bool nextTaskID(std::size_t&taskID)
{
taskID = counter.fetch_and_add(1);
if(taskID >= NofAtom)return 0;
return 1;
}


void reset(std::size_t NofCPU, std::size_t NofTask)
{
internalArrayAssigned = 1;
beingReassigned = 0;
NofAtom = NofTask;
// NofCore = NofCPU;
std::size_t avgTask = NofTask / NofCPU;
if(avgTask == 0)
{
NofCore = NofTask;
avgTask = 1;
}
else
{
NofCore = NofCPU;
}
// container = new volatile std::size_t[2 * NofCore];
current = new volatile std::size_t[NofCore];
UB = new volatile std::size_t[NofCore];
// current = container;
// UB = container + NofCore;
// std::size_t avgTask = NofTask / NofCPU;
current[0] = 0;
UB[0] = current[0] + (NofTask - NofCore * avgTask) + avgTask;
for(std::size_t i = 1, iend = NofCore; i != iend; ++i)
{
current[i] = UB[i - 1];
UB[i] = current[i] + avgTask;
// Rcout<<current[i]<<", "<<UB[i]<<"\n";
}
dispatchingID = new volatile unsigned char[NofCore];
std::fill(dispatchingID, dispatchingID + NofCore, 0);
}


dynamicTaskingPartition(std::size_t NofCPU, std::size_t NofTask)
{
reset(NofCPU, NofTask);
}


bool nextTaskID(std::size_t cpuI, std::size_t&taskID)
{
while(beingReassigned);
// because beingReassigned is read and written only under the mutex,
// a volatile would be good enough
dispatchingID[cpuI] = 1;


//what u gonna do once this chunk is finished
if(current[cpuI] == UB[cpuI])
{
dispatchingID[cpuI] = 0;
m.lock();
beingReassigned = 1;
// wait until all the other threads are waiting.
while (std::accumulate(dispatchingID, dispatchingID + NofCore, 0) != 0);
std::size_t maxleft = 0, whichone = 0;
for(std::size_t i = 0; i != NofCore; ++i)
{
if(UB[i] - current[i] > maxleft)
{
maxleft = UB[i] - current[i];
whichone = i;
}
}
if(maxleft <= 1)
{
// dispatchingID[cpuI] = 0;
beingReassigned = 0;
m.unlock();
return 0;
}
UB[cpuI] = UB[whichone];
// must gurantee current[whichone] + 1 is not touched because
// other threads could be working on or will be working on that element
current[cpuI] = current[whichone] + (UB[whichone] - current[whichone]) / 2;
UB[whichone] = current[cpuI];
taskID = current[cpuI];
++current[cpuI];
// dispatchingID[cpuI] = 0;
beingReassigned = 0;
m.unlock();
}
else
{
taskID = current[cpuI];
++current[cpuI];
dispatchingID[cpuI] = 0;
}
return 1;
}
};
*/


















