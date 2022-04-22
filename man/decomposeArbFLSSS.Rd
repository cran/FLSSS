\name{decomposeArbFLSSS}
\alias{decomposeArbFLSSS}
\title{
arbFLSSS decomposition
}
\description{
Decompose an \code{arbFLSSS} instance into sub-problems for distributed computing.
}
\usage{
decomposeArbFLSSS(
  len,
  V,
  target,
  approxNinstance = 1000L,
  maxCore = 7L,
  ksumTable = NULL,
  ksumK = 4L,
  ksumTableSizeScaler = 30L,
  verbose = TRUE
  )
}
\arguments{
  \item{len}{
See the same argument in \code{arbFLSSS()}.
}
  \item{V}{
See the same argument in \code{arbFLSSS()}.
}
  \item{target}{
See the same argument in \code{arbFLSSS()}.
}
  \item{approxNinstance}{
See the same argument in \code{arbFLSSS()}.
}
  \item{maxCore}{
See the same argument in \code{arbFLSSS()}. The decomposition is single-threaded, but building the k-sum accelerator is multithreaded.
}
  \item{ksumTable}{
Either \code{NULL} or the return value from \code{ksumHash()}. \code{ksumTable} is not necessary for the decomposition. The function merely store a reference to it in every arbFLSSS object.
}
  \item{ksumK}{
See the same argument in \code{arbFLSSS()}. If \code{ksumK >= 3} and \code{ksumTable == NULL}, the function will build a k-sum accelerator and store a reference in every arbFLSSS object.
}
  \item{ksumTableSizeScaler}{
See the same argument in \code{arbFLSSS()}.
}
  \item{verbose}{
See the same argument in \code{arbFLSSS()}.
}
}
\details{
The rationale follows \code{decomposeMflsss()}. The pair \code{decomposeArbFLSSS()} and \code{arbFLSSSobjRun()} makes up the distributed computing counterpart of \code{arbFLSSS()}.
}
\value{
A list of two:

\code{$arbFLSSSobjects}: a list. Each element is an \code{arbFLSSS} object that would be supplied to \code{arbFLSSSobjRun()}.

\code{$solutionsFound}: a list. Solutions found during decomposition.
}


\examples{
set.seed(42)
d = 5L # Set dimension.
N = 60L # Set size.
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


# Check if any solution has been found during decomposition.
str(theDecomposed$solutionsFound)


# Check the first arbFLSSS object.
str(theDecomposed$arbFLSSSobjects[[1]])
}


















