\name{mFLSSSparImposeBoundsIntegerized}
\alias{mFLSSSparImposeBoundsIntegerized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An advanced version of \code{mFLSSSparImposeBounds()}
}
\description{
See the description of \code{mFLSSSparIntegerized()}.
}
\usage{
mFLSSSparImposeBoundsIntegerized(
  maxCore = 7L,
  len,
  mV,
  mTarget,
  mME,
  LB = 1L:len,
  UB = (nrow(mV) - len + 1L) : nrow(mV),
  solutionNeed = 1L,
  precisionLevel = integer(ncol(mV)),
  returnBeforeMining = FALSE,
  tlimit = 60,
  dl = ncol(mV),
  du = ncol(mV),
  targetsOrder = NULL,
  useBiSrchInFB = FALSE,
  avgThreadLoad = 8L,
  verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{maxCore}{
See \code{maxCore} in \code{mFLSSSpar()}.
}
  \item{len}{
See \code{len} in \code{mFLSSSpar()}.
}
  \item{mV}{
See \code{mV} in \code{mFLSSSpar()}.
}
  \item{mTarget}{
See \code{mTarget} in \code{mFLSSSpar()}.
}
  \item{mME}{
See \code{mME} in \code{mFLSSSpar()}.
}
  \item{LB}{
See \code{LB} in \code{FLSSS()}.
}
  \item{UB}{
See \code{UB} in \code{FLSSS()}.
}
  \item{solutionNeed}{
See \code{solutionNeed} in \code{mFLSSSpar()}.
}
\item{precisionLevel}{
See \code{precisionLevel} in \code{mFLSSSparIntegerized()}.
}
\item{returnBeforeMining}{
See \code{returnBeforeMining} in \code{mFLSSSparIntegerized()}.
}
  \item{tlimit}{
See \code{tlimit} in \code{mFLSSSpar()}.
}
  \item{dl}{
See \code{dl} in \code{mFLSSSpar()}.
}
  \item{du}{
See \code{dl} in \code{mFLSSSpar()}.
}
  \item{targetsOrder}{
See \code{targetsOrder} in \code{mFLSSSparImposeBounds()}.
}
  \item{useBiSrchInFB}{
See \code{useBiSrchInFB} in \code{mFLSSSpar()}.
}
  \item{avgThreadLoad}{
See \code{avgThreadLoad} in \code{mFLSSSpar()}.
}
\item{verbose}{
If TRUE, prints mining progress.
}
}
\value{
See \code{Value} in \code{mFLSSSparIntegerized()}.
}
\note{32-bit architecture unsupported.}
\examples{
if(.Machine$sizeof.pointer == 8L){
# =====================================================================================
# 64-bit architecture required.
# =====================================================================================
# rm(list = ls()); gc()
subsetSize = 7L
supersetSize = 60L
dimension = 5L # dimensionality

\donttest{
# Create a superset at random:
N = supersetSize * dimension
superset = matrix(1000 * (rnorm(N) ^ 3 + 2 * runif(N) ^ 2 +
                  3 * rgamma(N, 5, 1) + 4), ncol = dimension)
rm(N)


# Make up the lower and upper bounds for the solution space:
tmp = sort(sample(1L : supersetSize, subsetSize))
tmp2 = sort(sample(1L : supersetSize, subsetSize))
lowerBounds = pmin(tmp, tmp2)
upperBounds = pmax(tmp, tmp2)
rm(tmp, tmp2)


# 'mFLSSSparImposeBoundsIntegerized()' does not work if there are elements not
# under the hood of 'lowerBounds' + 'upperBounds'. Exclude these elements first:
remainIndex = unique(unlist(
  apply(cbind(lowerBounds, upperBounds), 1, function(x) x[1] : x[2])))
lowerBounds = match(lowerBounds, remainIndex)
upperBounds = match(upperBounds, remainIndex)
superset = superset[remainIndex, ]


# Plant a subset sum:
solution = integer(subsetSize)
solution[1] = sample(lowerBounds[1] : upperBounds[1], 1)
for(i in 2L : subsetSize)
{
  l = max(lowerBounds[i], solution[i - 1] + 1L)
  u = upperBounds[i]
  if(l == u) solution[i] = u
  else solution[i] = sample(l : u, 1)
}
subsetSum = colSums(superset[solution, ])
subsetSumError = abs(subsetSum) * 0.01 # relative error within 1%
rm(solution)


system.time({rst = FLSSS::mFLSSSparImposeBoundsIntegerized(
  maxCore = 2L, len = subsetSize, mV = superset, mTarget = subsetSum,
  mME = subsetSumError, LB = lowerBounds, UB = upperBounds,
  solutionNeed = 1, tlimit = 3, dl = ncol(superset), du = ncol(superset),
  targetsOrder = NULL, useBiSrchInFB = FALSE, avgThreadLoad = 8L)})


# Compare the time cost of 'mFLSSSparImposeBoundsIntegerized()' and
# 'mFLSSSparImposeBounds()'. The speed advantage of 'mFLSSSparIntegerized()'
# may not be pronounced for toy examples.
system.time(FLSSS::mFLSSSparImposeBounds(
  maxCore = 2L, len = subsetSize, mV = superset, mTarget = subsetSum,
  mME = subsetSumError, LB = lowerBounds, UB = upperBounds,
  solutionNeed = 1, tlimit = 2, dl = ncol(superset), du = ncol(superset),
  targetsOrder = NULL, useBiSrchInFB = FALSE, avgThreadLoad = 8L))


# Verify:
cat("Number of solutions = ", length(rst$solution), "\n")
if(length(rst$solution) > 0)
{
  cat("Solutions unique: ")
  cat(length(unique(lapply(rst$solution, function(x)
    sort(x)))) == length(rst$solution), "\n")
  cat("Solution in bounded space: ")
  cat(all(unlist(lapply(rst$solution, function(x)
    sort(x) <= upperBounds & sort(x) >= lowerBounds))), "\n")


  cat("Solutions correct regarding integerized data: ")
  cat(all(unlist(lapply(rst$solution, function(x)
    abs(colSums(rst$INT$mV[x, ]) - rst$INT$mTarget) <= rst$INT$mME))), "\n")


  cat("Solutions correct regarding original data: ")
  boolean = all(unlist(lapply(rst$solution, function(x)
    abs(colSums(superset[x, ]) - subsetSum) <= subsetSumError)))
  cat(boolean, "\n")
  if(!boolean)
  {
    cat("The given error threshold relative to subset sum:\n")
    givenRelaErr = round(abs(subsetSumError / subsetSum), 5)
    cat(givenRelaErr, "\n")


    cat("Solution subset sum relative error:\n")
    tmp = lapply(rst$solution, function(x)
    {
      err = round(abs(colSums(superset[x, ]) / subsetSum -1), 5)
      for(i in 1L : length(err))
      {
        if(givenRelaErr[i] < err[i]) message(paste0(err[i], " "), appendLF = FALSE)
        else cat(err[i], "")
      }
      cat("\n")
    })
    cat("Integerization caused the errors. Future versions of")
    cat("'mFLSSSparIntegerized()' would have a parameter of precision level.\n")
  }
} else
{
  cat("No solutions exist or timer ended too soon.\n")
}
}
# =====================================================================================
# =====================================================================================
}
}
 