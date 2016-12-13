\name{FLSSS}
\alias{FLSSS}
%\docType{package}
\title{
Single-dimensional fixed size subset sum with bounded error solver
}
\description{Given subset size "len", sorted numeric set "v", subset-sum "target", error "ME", find one or more subsets of size len whose elements sum into [ target - ME, target + ME ]
}
\usage{FLSSS(len, v, target, ME, sizeNeeded = 1L, tlimit=60, catch=NULL,
throw=F, LB=1L:len, UB=(length(v)-len+1L):length(v), useFloat=F)}
\arguments{
\item{len}{An integer as the subset size. If 0, activate the general-case (unfixed subset size) solver. Looping over different subset sizes to solve the general subset sum problem is preferred than setting len to 0 if v is relatively large. Example II method 2 below shows how the solver progresses for the unfixed subset size scenario.}
\item{v}{Sorted numeric vector as the set}
\item{target}{Subset sum}
\item{ME}{Error}
\item{sizeNeeded}{How many solutions at least are wanted; sizeNeeded="all"
returns all the solutions}
\item{tlimit}{Time Limit in seconds. E.g. tlimit=10 let FLSSS stop to
return all the solutions found in 10 seconds. tlimit="none" means no time limit}
\item{catch}{Catch the stack object implemented as a list}
\item{throw}{Throw the stack object as a list}
\item{LB}{Lower bound initializer}
\item{UB}{Upper bound initializer}
\item{useFloat}{Whether to use single-precision floating point in computing}
}

\details{
Please read the package vignette for more details.
}

\value{
throw=F, returns a list of integer vectors. Each vector contains a solution's
indexes.

throw=T, returns a list L, L$roots is the list of solutions, L$node is the list of
the stack.
}

\note{
1. Although the function processes fixed size subset, the general subset sum problem can be transformed to a fixed size one by padding 0s in the superset and setting the subset size to half of the superset size afterwards. Please refer to example II method 2.

2. Parameter "catch" and "throw": for example, if sizeNeeded=10 and throw=1, FLSSS will return a list L where L$roots are the solutions and L$node is a "stack" list. If 10 more solutions are needed then FLSSS with catch=L$node will start looking for the 11th solution directly.

3. Number of output solutions may be greater than sizeNeeded. A resulting empty list indicates no solutions. Setting sizeNeeded="all" and tlimit="none"
could take considerable amount of time when v is long and sparse. Assigning at least one of them numeric value is strongly recommended.

4. When v is real and an error of 0 is expected, it is recommended to set ME=0.01, 0.001 or whatever less than the decimal point v's elements have reached.
}


\examples{
# Example I:
# play random numbers
v=100*sort(rnorm(1000)^3+2*rnorm(1000)^2+3*rnorm(1000)+4)
len=200
target=runif(1, sum(v[1:200]), sum(v[(1000-200+1):1000]))
ME=1e-4
rst=FLSSS(len, v, target, ME, tlimit=30)
if(length(rst)>0)
all(unlist(lapply(rst, function(x)sum(v[x])>=target-ME&sum(v[x])<=target+ME)))

# losses of 50 insurance companies
v=c(-1119924501,-793412295,-496234747,-213654767,16818148,26267601,26557292
,27340260,28343800,32036573,32847411,34570996,34574989,43633028,44003100
,47724096,51905122,52691025,53600924,56874435,58207678,60225777,60639161
,60888288,60890325,61742932,63780621,63786876,65167464,66224357,67198760
,69366452,71163068,72338751,72960793,73197629,76148392,77779087,78308432
,81196763,82741805,85315243,86446883,87820032,89819002,90604146,9376129,
97920291,98315039,310120088)

# try which 10 of them sum up to target
len=10
target=139254955

# try finding all solutions:
FLSSS(len,v,target,0.1,"all")

# try finding all solutions in 2 seconds with error=10:
FLSSS(len,v,target,10,"all",tlimit=2)

# try finding at least 5 solutions with error=20:
FLSSS(len,v,target,20,5)

# experiment throw and catch functions:
result1=FLSSS(len, v, target, 20, "all", tlimit=0.5, throw=1)
result2=FLSSS(len, v, target, 20, "all", tlimit=0.5, catch=result1$node)


# ------------------------------------------------------------------------


# Example II:
# use FLSSS to solve the general subset sum problem:
# method 1: loop over all possible subset sizes
for(len in 1:length(v))
{
  lis=FLSSS(len,v,target,0.1)
  if(length(lis)!=0)break
}

# method 2: add 50 dummy 0s, set the subset size to 50.
v1=c(rep(0L,50), v)
v1index=c(rep(0,50L), 1L:50L) # first 50 0s indicate the dummies
sortOrder=order(v1)
v1=v1[sortOrder]
v1index=v1index[sortOrder]
lis2=FLSSS(len=50, v1, target, 0.1)
lis2=lapply(lis2, function(x)v1index[x][v1index[x]>0])
# This method is exactly how FLSSS(0, v, target, 0.1) would proceed

# exam solutions:
all(unlist(lapply(lis2, function(x)sum(v[x])>=target-0.1&
  sum(v[x])<=target+0.1)))


# ----------------------------------------------------------------------------


# Example III:
# use FLSSS to solve an unbounded single dimensional knapsack problem:

bagCapacity=361
# capacity of the knapsack

objectMass<-sort(sample(1:100,10))
# mass of each of the 10 kinds of objects that can
# be put in the knapsack

objectAmount<-sample(1:30,10)
# number of each kind

v<-unlist(mapply(rep,objectMass,objectAmount))
# set of choices

for(len in 1:length(v))
{
  lis=FLSSS(len,v,bagCapacity,0)
  # try using up the capacity of the knapsack with the smallest number of
  # objects
  if(length(lis)!=0)break
}
}
