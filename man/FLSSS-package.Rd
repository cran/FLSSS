\name{FLSSS}
\alias{FLSSS}
\alias{FLSSS}
\docType{package}
\title{
Fixed Size Subset Sum Solution
}
\description{Given a target T, a marginal error ME, an integer len, a numeric ascending set S, find one or more subsets of size len whose elements sum into [T-ME, T+ME].
}
\usage{FLSSS(len, v, target, ME, sizeNeeded = 1, tlimit="none", catch=NULL, throw=F, 
LB=1:len, UB=(length(v)-len+1):length(v))}
\arguments{
\item{len}{Subset size}
\item{v}{Sorted numeric vector as set S}
\item{target}{Target T}
\item{ME}{Marginal Error}
\item{sizeNeeded}{How many solutions at least are wanted; sizeNeeded="all" 
returns all the solutions}
\item{tlimit}{Time Limit in seconds. E.g. tlimit=10 let FLSSS stop to 
return all the solutions found in 10 seconds. tlimit="none" means no time limit}
\item{catch}{Catch the stack object implemented as a list}
\item{throw}{Throw the stack object as a list}
\item{LB}{Lower bound initializer}
\item{UB}{Upper bound initializer}
}

\details{
\href{https://docs.google.com/a/bu.edu/file/d/0B8MJ_hAGrDrwNHFScDZqS0x4elE/edit}{An introduction to the algorithm}

\href{https://docs.google.com/a/bu.edu/file/d/0B8MJ_hAGrDrwUWhkUnpNeld4N28/edit}{C++ and R codes for version 1.0}

\href{https://docs.google.com/a/bu.edu/file/d/0B8MJ_hAGrDrwZVJkXzd1RGo4Q2M/edit}{Explanatory R codes}

\href{https://docs.google.com/a/bu.edu/file/d/0B8MJ_hAGrDrwUDR4dUVGbG9iMzA/edit}{Additional C++ codes}
}

\value{
throw=F, returns a list of integer vectors. Each vector contains a solution's 
indexes.

throw=T, returns a list L, L$roots is the list of solutions, L$node is the list of 
the stack.
}

\note{1. Parameter "catch" and "throw": if sizeNeeded=10 and throw=1, 
then FLSSS returns a list L where L$roots are the solutions and L$node is a "stack" 
list. If 10 more solutions are found to be needed, then running FLSSS with catch
=L$node can let it start looking for the 11th solution.

2. Number of output solutions may be greater than sizeNeeded. A resultant empty 
list indicates no solutions. Setting sizeNeeded="all" and tlimit="none" 
could take considerable amount of time when v is long and sparse. Assigning at 
least one of them numeric value is strongly recommended.

3. When v is real and a marginal error of 0 is expected, it is recommended to set 
ME=0.01, 0.001 or whatever less than the decimal point v's elements have reached.
}


\examples{
v<-c(-1119924501,-793412295,-496234747,-213654767,16818148,26267601,26557292
,27340260,28343800,32036573,32847411,34570996,34574989,43633028,44003100,47724096
,51905122,52691025,53600924,56874435,58207678,60225777,60639161,60888288,60890325
,61742932,63780621,63786876,65167464,66224357,67198760,69366452,71163068,72338751
,72960793,73197629,76148392,77779087,78308432,81196763,82741805,85315243,86446883
,87820032,89819002,90604146,93761292,97920291,98315039,310120088)
len<-10
target<-139254955
FLSSS(len,v,target,0,"all")
FLSSS(len,v,target,10,"all",tlimit=2)
FLSSS(len,v,target,20,5)

result1<-FLSSS(len,v,target,20,"all",tlimit=0.03,throw=1)
result2<-FLSSS(len,v,target,20,"all",tlimit=0.1,catch=result1$node)

# Example of using FLSSS to solve Subset Sum Problem
for(len in 1:length(v))
{
  lis=FLSSS(len,v,target,0)
  if(length(lis)!=0)break
}

# Example of using FLSSS to solve a knapsack problem
bagCapacity=361                    # Capacity of the knapsack
objectMass<-sort(sample(1:100,10)) # Mass of each of the 10 kinds of objects
                                   # that can be put in the knapsack
objectAmount<-sample(1:30,10)      # Number of each kind
v<-unlist(mapply(rep,objectMass,objectAmount)) #Set of choices
for(len in 1:length(v))
{
  lis=FLSSS(len,v,bagCapacity,0) # Use up the capacity of the knapsack - 
                                 # with the smallest number of objects.
  if(length(lis)!=0)break
}
}