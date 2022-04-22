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
set.seed(123)
d = 5L # Set dimension.
N = 50L # Set size.
roundN = 2L # For rounding the numeric values before conversion to strings.


V = matrix(round(runif(N * d, -1e2, 1e2), roundN), nrow = N)
options(scipen = 999) # Ensure numeric => string conversion does not
# produce strings like 2e-3.
Vstr = matrix(as.character(V), nrow = N) # String version of V.


system.time({
  ksumAccelerator = FLSSS::ksumHash(
    ksumK = 4, V = Vstr, ksumTableSizeScaler = 30, target = NULL,
    approxNinstance = 1000, verbose = FALSE, maxCore = 2)
}) # target is NULL, thus the accelerator built can be used for any subset sum
# instance.


rst = list()
for (len in c(9:10, 42:43))
{

  sol = sample(N, len)
  targetStr = as.character(round(colSums(V[sol, , drop = FALSE]), roundN))
  system.time({
    rst[[length(rst) + 1]] = FLSSS::arbFLSSS(
      len = len, V = Vstr, target = targetStr, givenKsumTable = ksumAccelerator,
      tlimit = 2, solutionNeed = 1, maxCore = 2, verbose = TRUE)
  })


  # Validation.
  cat(all(unlist(lapply(rst[[length(rst)]], function(x)
  {
    all(apply(Vstr[x, , drop = FALSE], 2, function(u)
      FLSSS:::addNumStrings(u)) == targetStr)
  }))), "\n\n")

}
}










