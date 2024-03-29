\name{mmKnapsackIntegerized}
\alias{mmKnapsackIntegerized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An advanced version of \code{mmKnapsack()}
}
\description{
See the description of \code{mFLSSSparIntegerized()}.
}
\usage{
mmKnapsackIntegerized(
  maxCore = 7L,
  len,
  itemsProfits,
  itemsCosts,
  capacities,
  heuristic = FALSE,
  precisionLevel = integer(length(capacities)),
  returnBeforeMining = FALSE,
  tlimit = 60,
  useBiSrchInFB = FALSE,
  threadLoad = 8L,
  verbose = TRUE
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{maxCore}{
See \code{maxCore} in \code{mmKnapsack()}.
}
  \item{len}{
See \code{len} in \code{mmKnapsack()}.
}
  \item{itemsProfits}{
See \code{itemsProfits} in \code{mmKnapsack()}.
}
  \item{itemsCosts}{
See \code{itemsCosts} in \code{mmKnapsack()}.
}
  \item{capacities}{
See \code{capacities} in \code{mmKnapsack()}.
}
  \item{heuristic}{
See \code{heuristic} in \code{mmKnapsack()}.
}
\item{precisionLevel}{
See \code{precisionLevel} in \code{mFLSSSparIntegerized()}.
}
\item{returnBeforeMining}{
See \code{returnBeforeMining} in \code{mFLSSSparIntegerized()}.
}
  \item{tlimit}{
See \code{tlimit} in \code{mmKnapsack()}.
}
  \item{useBiSrchInFB}{
See \code{useBiSrchInFB} in \code{FLSSS()}.
}
  \item{threadLoad}{
See \code{avgThreadLoad} in \code{mFLSSSpar()}.
}
  \item{verbose}{
If \code{TRUE}, function prints progress.
}
}
\value{
A list of six:
\item{solution}{ The optimal solution.}
\item{selectionCosts}{ Solution costs.}
\item{budgets}{ Knapsack capacities.}
\item{selectionProfit}{ Solution total profit.}
\item{unconstrainedMaxProfit}{ Maximal profit given infinite budgets.}
\item{INT}{ A list of four:}
\item{INT$mV}{ The integerized superset.}
\item{INT$mTarget}{ The integerized subset sum.}
\item{INT$mME}{ The integerized subset sum error threshold.}
\item{INT$compressedDim}{ The dimensionality after integerization.}
}
\note{32-bit architecture unsupported.}
\examples{
if(.Machine$sizeof.pointer == 8L){
# =====================================================================================
# 64-bit architecture required.
# =====================================================================================
# =====================================================================================
# Play random numbers
# =====================================================================================
# rm(list = ls()); gc()
subsetSize = 6
supersetSize = 60
NcostsAttr = 4

\donttest{
# Make up costs for each item.
costs = abs(6 * (rnorm(supersetSize * NcostsAttr) ^ 3 +
  2 * runif(supersetSize * NcostsAttr) ^ 2 +
  3 * rgamma(supersetSize * NcostsAttr, 5, 1) + 4))
costs = matrix(costs, ncol = NcostsAttr)


# Make up cost limits.
budgets = apply(costs, 2, function(x)
{
  x = sort(x)
  Min = sum(x[1L : subsetSize])
  Max = sum(x[(supersetSize - subsetSize + 1L) : supersetSize])
  runif(1, Min, Max)
})


# Make up item profits.
gains = rnorm(supersetSize) ^ 2 * 10000 + 100


rst1 = FLSSS::mmKnapsackIntegerized(
  maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, tlimit = 2, useBiSrchInFB = FALSE,
  threadLoad = 4L, verbose = TRUE)


# Examine if 'mmKnapsackIntegerized()' gives the same solution as 'mmKnapsack()'.
rst2 = FLSSS::mmKnapsack(
  maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, tlimit = 2, useBiSrchInFB = FALSE,
  threadLoad = 4L, verbose = TRUE)
# Possible differences in solutions are due to real-integer conversion




# Let 'x' be the solution given 'heuristic = T'. The sum of ranks of the
# profits subsetted by 'x' would be no less than that of the optimal solution.
rst3 = FLSSS::mmKnapsackIntegerized(
  maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = TRUE, tlimit = 2, useBiSrchInFB = FALSE,
  threadLoad = 4L, verbose = TRUE)


# Exam difference in total profits given by the heuristic and the optimal:
if(length(rst3$solution) > 0 & length(rst1$solution) > 0)
  sum(gains[rst3$solution]) / sum(gains[rst1$solution])
}




# =====================================================================================
# Test case P08 from
# https://people.sc.fsu.edu/~jburkardt/datasets/knapsack_01/knapsack_01.html
# =====================================================================================
costs = matrix(c(382745, 799601, 909247, 729069, 467902,  44328,  34610, 698150,
                 823460, 903959, 853665, 551830, 610856, 670702, 488960, 951111,
                 323046, 446298, 931161,  31385, 496951, 264724, 224916, 169684),
               ncol = 1)


gains = c( 825594, 1677009, 1676628, 1523970,  943972,   97426,  69666, 1296457,
          1679693, 1902996, 1844992, 1049289, 1252836, 1319836, 953277, 2067538,
           675367,  853655, 1826027,   65731,  901489,  577243, 466257,  369261)


budgets = 6404180


# 'mmKnapsackIntegerized()' is designed for the multidimensional Knapsack
# and may not be ideal for one-dimensional 0-1 Knapsack regarding computing speed.
# 'len = 0' would cause severe deceleration. Looping 'len' over possible
# values is recommended if 'len' is ungiven.
rst = FLSSS::mmKnapsackIntegerized(
  maxCore = 2L, len = 12L, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, tlimit = 2, threadLoad = 4L, verbose = TRUE)
rst = sort(rst$solution)


cat("Correct solution:\n1 2 4 5 6 10 11 13 16 22 23 24\nFLSSS solution =\n")
cat(rst, "\n")
# The difference is due to rounding errors in real-integer conversion. The default
# 'precisionLevel' shifts, scales and rounds 'itemCosts' such that its
# maximal element is no less than 8 times the number of items.


# Increase the precision level
rst = FLSSS::mmKnapsackIntegerized(
  maxCore = 2L, len = 12L, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, precisionLevel = rep(500L, 1),
  tlimit = 2, threadLoad = 4L, verbose = TRUE)
# 'precisionLevel = 500' shifts, scales and rounds 'itemCosts' such that its
# maximal element is no less than 500.
rst = sort(rst$solution)
cat("Correct solution:\n1 2 4 5 6 10 11 13 16 22 23 24\nFLSSS solution =\n")
cat(rst, "\n")
}
# =====================================================================================
# =====================================================================================
}
 