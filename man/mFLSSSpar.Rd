\name{mFLSSSpar}
\alias{mFLSSSpar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multithreaded multidimensional fixed size subset sum with bounded error solver
}
\description{FLSSS extended to multidimensional real domain
}
\usage{
mFLSSSpar(len, mV, mTarget, mME, viaConjugate = F, maxCore = 7L,
totalSolutionNeeded = 1L, tlimit = 60, singleTimeLimit = 10,
randomizeTargetOrder=sample(1L:(len*(nrow(mV)-len)+1), len*(nrow(mV)-len)+1),
LB = 1L:len, UB = (nrow(mV) - len + 1L):nrow(mV))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{len}{
An integer as the subset size. If 0, activate the general-case (unfixed subset size) solver. Looping over different subset sizes to solve the general subset sum problem is preferred than setting len to 0 if mV is relatively large. Example II in FLSSS help page shows how the solver progresses for the unfixed subset size scenario.
}
  \item{mV}{
A data frame as the multidimensional numeric vector/set. Each row is an element of the vector/set.
}
  \item{mTarget}{
A numeric vector as the subset sum for each dimension. Its length should equal the number of columns/dimensions in mV.
}
  \item{mME}{
A numeric vector as the error for each dimension. Should have the same length as mTarget.
}
  \item{viaConjugate}{
Please refer to the details section
}
  \item{maxCore}{
Number of threads to invoke. Better not be greater than the maximal concurrentable threads in system.
}
  \item{totalSolutionNeeded}{
Number of solutions needed.
}
  \item{tlimit}{
Time limit in seconds. Default to 60s
}
 \item{singleTimeLimit}{
Please refer to details section or the package vignette for more information.
}
  \item{randomizeTargetOrder}{
Discussed in the details section.
}
  \item{LB}{
Lower bound initializer.
}
  \item{UB}{
Upper bound initializer.
}
}
\details{
Intuitively, arithmetic among single-dimensional reals can be extended into multidimensional space for solving a multidimensional fixed size subset sum (denoted by MFSSS) problem. However, this requires all dimensions in mV (the multidimensional vector/set) are comonotonic (perfectly positively rank-correlated).

We denote a MFSSS problem where all dimensions of mV are comonotonic by cMFSSS.%, and denote the overloaded algorithm that can directly solve a cMFSSS problem by mFLSSS.

An algorithm is invented to tackle the general MFSSS problem. First, it will comonotonize the dimensions (transform mV such that its dimensions become comonotonic). Second, it will decompose the MFSSS problem into no more than L*(N--L)+1 (L being the subset size, N being the superset size) cMFSSS problems where each one of them will work on the same comonotonized mV but with a different subset sum target. The parameter "singleTimeLimit" aims to bound the time cost for solving each of the cMFSSS problems.

The cMFSSS problems out of the decomposition are independent, so they can be run in a parallel fashion. If parameter "randomizeTargetOrder" is set false, then the cMFSSS problems will be sorted by the associated subset sum targets, lining up for threads; if true, they will be randomly ordered inside the function. To give user the control of the order, "randomizeTargetOrder" also accepts any index vector of size L*(N--L)+1.

Experiments show randomly ordered cMFSSS increases the chance of finding a solution in shorter time. Different order of the cMFSSS problems usually results in different time costs for finding the required number of solutions; therefore, kicking off several instances of the function with different "randomizeTargetOrder" could be beneficial. However, if computing resource allows, spawning as many as possible threads is the best way to find the required number of solutions in the shortest time.

Any single-dimensional fixed size subset sum (denote it by SFSSS) problem has a conjugate: finding a subset of size L that sums to value S from a superset of size N where all the elements sum to value T is equivalent to finding a subset of size N--L that sums to T--S. The same conclusion holds for any MFSSS problem. %It is observed for SFSSS, when L is less than half of N, solving the conjugate is often faster. A rough but intuitive explanation could be: the more elements in the subset, the less "freedom" can each element have, thus the searching paths converge faster. For this reason,
Parameter "viaConjugate" controls whether the function should solve via the original MFSSS problem (0) or via its conjugate (1). %or let the function decide: if the subset size is less than half of the superset size, go for the conjugate, otherwise stay with the original.
The two different solving paths are sometimes observed having considerable efficiency gap. It is recommended to try both if the computing resource allows.

For more details, please refer to the package vignette.
}
\value{
A list of integer vectors. Each vector is a solution's indexes.
}
\note{
Unlike SFSSS which "rarely contains no solution" in the entire combinatorial space, the multidimensional fixed size Subset Sum "rarely contains a solution" when the number of dimensions is not trivial.

This fact could make a seemingly trivial task in FLSSS pretty heavy in mFLSSSpar. It is recommended to run mFLSSSpar with small subset size or large error a few times to learn its performance for non-trivial task.
}

\examples{
L=5L # subset size

d=5L # number of dimensions

N=50L # superset size

# generate a multidimensional vector/set
v=as.data.frame(matrix(rnorm(d*N)*1000,ncol=d))

# make an underlying solution
solution=sample(1L:N, L)

# the subset sum target of the underlying solution
target=as.numeric(colSums(v[solution,]))

# bound the error as 5\% of each dimension's target magnitude
ME=abs(target)*0.05

randomOrder=sample(1L:((nrow(v)-L)*L+1L), (nrow(v)-L)*L+1L)
# randomizeTargetOrder should be TRUE or FALSE or
# an index vector of size (N-L)*L+1

system.time({tmp=mFLSSSpar(L, v, target, ME, maxCore=7,
randomizeTargetOrder = randomOrder, tlimit = 5)})
# try finding at least one solution within total time 5 seconds
# with 7 CPUs. Change tlimit for heavier tasks. The 5s limit is
# only for passing R package check

if(length(tmp)!=0)abs(as.numeric(colSums(v[tmp[[1]],]))/target-1)
# exam the solution

system.time({tmp=mFLSSSpar(L,v,target,ME,viaConjugate=1,maxCore=7,
randomizeTargetOrder = TRUE, tlimit = 5)})
# try finding at least one solution within 5 seconds with 7 CPUs

# exam the solution
if(length(tmp)!=0)abs(as.numeric(colSums(v[tmp[[1]],]))/target-1)


# ----------------------------------------------------------------------------

# try the unfixed size subset sum solver

d=5L # number of dimensions

N=20L # superset size

# generate a multidimensional vector/set
v=as.data.frame(matrix(rnorm(d*N)*1000,ncol=d))

# make an underlying solution
solution=sample(1L:N, sample(1L:N, 1L))

# the subset sum target of the underlying solution
target=as.numeric(colSums(v[solution,]))

# bound the error as 5\% of each dimension's target magnitude
ME=abs(target)*0.05

system.time({tmp=mFLSSSpar(0, v, target, ME, maxCore=7,
  randomizeTargetOrder = TRUE, tlimit = 5)})
tmp
solution
}
