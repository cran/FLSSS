\name{mFLSSSpar}
\alias{mFLSSSpar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multithreaded fixed size Subset Sum with bounded error in multidimensional real domain
}
\description{FLSSS expanded to multidimensional space with parallel computing option
}
\usage{
mFLSSSpar(len, mV, mTarget, mME, maxCore = 8L,
totalSolutionNeeded = 1L, tlimit = 60,
singleSolutionNeeded = 1L, randomizeTargetOrder=T,
LB = 1L:len, UB = (nrow(mV) - len + 1L):nrow(mV))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{len}{
An integer as the subset size
}
  \item{mV}{
A data frame as the multidimensional numeric vector/set. Each row is an element of the vector/set.
}
  \item{mTarget}{
A numeric vector as the subset-sum for each dimension. Its length should equal the number of columns/dimensions in mV.
}
  \item{mME}{
A numeric vector as the error for each dimension. Should have the same length as mTarget.
}
  \item{maxCore}{
Number of threads to invoke. Better not be greater than the available CPUs on machine. Number of cores can be found by your operating system's Task Manager.
}
  \item{totalSolutionNeeded}{
Number of solutions needed.
}
  \item{tlimit}{
Time limit in seconds. Default to 60s
}
  \item{singleSolutionNeeded}{
Default to 1. Discussed in the Details section.
}
  \item{randomizeTargetOrder}{
Default to TRUE. Discussed in the Details section.
}
  \item{LB}{
Lower bound initializer.
}
  \item{UB}{
Upper bound initializer.
}
}
\details{
Since FLSSS is a comparison-based algorithm, it's intuitive to expand all its arithemetic operations to multidimensional reals to solve a multidimensional fixed size Subset Sum problem, denoted by MFSSS. However, this requires all the dimensions in mV(the multidimensional vector/set) are comonotonic, i.e., perfectly positively rank-correlated. Denote this overloaded algorithm "mFLSSS".

An algorithm is invented to comonotonize the dimensions. To solve MFSSS appropriately, it will trigger no more than k*(N-k)+1 calls for mFLSSS (N is the length of the multidimensional vector and k is the subset size), each time with a different subset-sum target. The parameter "singleSolutionNeeded", equivalent to "sizeNeeded" in FLSSS, feeds mFLSSS each time when it's being called.

The no more than k*(N-k)+1 subset-sum targets are independent of mFLSSS, so the sequence of mFLSSS calls are parallelizable. The final algorithm is written in a multithreaded fashion, denoted by "mFLSSSpar".

In mFLSSSpar, each thread will communicate with its peers and track the total number of solutions that have been mined. Any thread that finds parameter "totalSolutionNeeded" reached will have all the threads quit immediately.

To gain performance, the order of the no more than k*(N-k)+1 subset-sum targets are randomized by default parameter "randomizeTargetOrder", so mFLSSSpar could give different solutions and performances each run with the same input. The targets are ordered ascendingly if randomizeTargetOrder is set FALSE.
}
\value{
A list of integer vectors. Each vector is a solution's indexes.
}
\note{
Unlike single-dimensional fixed size Subset Sum which "rarely contains no solution" in the entire combinatorial, the multidimensional fixed size Subset Sum "rarely contains a solution" when the number of dimensions is not trivial.

This fact could make a seemingly trivial task in FLSSS pretty heavy in mFLSSSpar, considering only the sizes of the subset and superset. It is recommended to run mFLSSSpar with smaller subset size or larger error a few times to learn its performance if you are dealing with non-trivial task.
}

\examples{
len=6L
# subset size

d=5L
# number of dimensions

lenv=100L
# size of the vector/set

v=as.data.frame(matrix(rnorm(d*lenv)*1000,ncol=d))
# generate a multidimensional vector/set

solution=sample(1L:lenv, len)
# make an underlying solution

target=as.numeric(colSums(v[solution,]))
# the subset-sum target of the underlying solution

ME=abs(target)*0.05
# bound the error as 5\% of each dimension's magnitude

tmp=mFLSSSpar(len,v,target,ME)
# try finding at least one solution within a minute

if(length(tmp)!=0)abs(as.numeric(colSums(v[tmp[[1]],]))/target-1)
# exam the solution
}
