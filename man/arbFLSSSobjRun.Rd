\name{arbFLSSSobjRun}
\alias{arbFLSSSobjRun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run an \code{arbFLSSS} instance
}
\description{
Run an \code{arbFLSSS} instance decomposed from \code{decomposeArbFLSSS()}.
}
\usage{
arbFLSSSobjRun(
  X,
  solutionNeed = 1L,
  tlimit = 60,
  maxCore = 7L,
  ksumK = 0L,
  ksumTableSizeScaler = 30L,
  verbose = TRUE
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
An \code{arbFLSSS} object from \code{decomposeArbFLSSS()}.
}
  \item{solutionNeed}{
See the same argument in \code{arbFLSSS()}.
}
  \item{tlimit}{
See the same argument in \code{arbFLSSS()}.
}
  \item{maxCore}{
See the same argument in \code{arbFLSSS()}. Mining subsets is single-threaded, but if \code{X} has no k-sum accelerator, users have the option of computing one on the fly, which is multithreaded. Will be ignored if \code{X} already has a k-sum accelerator.
}
  \item{ksumK}{
See the same argument in \code{arbFLSSS()}. Will be ignored if \code{X} already has a k-sum accelerator.
}
  \item{ksumTableSizeScaler}{
See the same argument in \code{arbFLSSS()}. Will be ignored if \code{X} already has a k-sum accelerator.
}
  \item{verbose}{
See the same argument in \code{arbFLSSS()}. Will be ignored if \code{X} already has a k-sum accelerator.
}
}
\details{
The rationale follows \code{mFLSSSobjRun()}. The pair \code{decomposeArbFLSSS()} and \code{arbFLSSSobjRun()} makes up the distributed computing counterpart of \code{arbFLSSS()}.
}
\value{
Has the same return from \code{arbFLSSS()}.
}


\examples{
set.seed(42)
d = 5L # Set dimension.
N = 30L # Set size.
len = 10L # Subset size.
roundN = 2L # For rounding the numeric values before conversion to strings.


V = matrix(round(runif(N * d, -1e5, 1e5), roundN), nrow = N) # Make superset.
sol = sample(N, len) # Make a solution.
target = round(colSums(V[sol, ]), roundN) # Target subset sum.


options(scipen = 999) # Ensure numeric => string conversion does not
# produce strings like 2e-3.
Vstr = matrix(as.character(V), nrow = N) # String version of V.
targetStr = as.character(target)


system.time({
  theDecomposed = FLSSS::decomposeArbFLSSS(
    len = len, V = Vstr, target = targetStr, approxNinstance = 1000,
    maxCore = 2, ksumTable = NULL, ksumK = 4, verbose = TRUE)
})


# Run the objects sequentially.
rst = unlist(lapply(theDecomposed$arbFLSSSobjects, function(x)
{
  FLSSS::arbFLSSSobjRun(x, solutionNeed = 1e9, tlimit = 5, verbose = TRUE)
}), recursive = FALSE)
str(rst)
}




















