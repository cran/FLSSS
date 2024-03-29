\name{ksumHash}
\alias{ksumHash}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build k-sum accelerator
}
\description{
Compute k-sum lookup tables given a set.
}
\usage{
ksumHash(
  ksumK,
  V,
  ksumTableSizeScaler = 30L,
  target = NULL,
  len = 0L,
  approxNinstance = 1000L,
  verbose = TRUE,
  maxCore = 7L
  )
}
\arguments{
  \item{ksumK}{
See the same argument in \code{arbFLSSS()}.
}
  \item{V}{
See the same argument in \code{arbFLSSS()}.
}
  \item{ksumTableSizeScaler}{
See the same argument in \code{arbFLSSS()}.
}
  \item{target}{
See the same argument in \code{arbFLSSS()}. If \code{target != NULL}, the function will (i) decompose the \code{arbFLSSS} instance of (\code{len}, \code{target}, \code{V}) into about \code{approxNinstance} subproblems, (ii) from these subproblems infer the lower and upper index bounds for the k-subsets, and then (iii) compute & hash k-sums to build the accelerator. If \code{target = NULL}, no bounds will be imposed on the k-subsets and the accelerator built can be used for any subset sum instance.
}
  \item{len}{
See the same argument in \code{arbFLSSS()}. Will be ignored if \code{target == NULL}.
}
  \item{approxNinstance}{
See the same argument in \code{arbFLSSS()}.
}
  \item{verbose}{
See the same argument in \code{arbFLSSS()}.
}
  \item{maxCore}{
See the same argument in \code{arbFLSSS()}.
}
}
\details{
k-sums are hashed using Yann Collet's xxHash that is the fastest among all non-cryptographic hash algorithms by 202204. See the benchmark <https://github.com/Cyan4973/xxHash>.
}
\value{
Either an empty list (happens when, e.g. \code{ksumK < 3}), or a list of lists. The first list would be the 3-sum lookup table, and the last would be the \code{ksumK}-sum lookup table.
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


optionSave = options()
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
  FLSSS::arbFLSSSobjRun(x, solutionNeed = 1e9, tlimit = 5, verbose = FALSE)
}), recursive = FALSE)
str(rst)


options(optionSave)
}










 