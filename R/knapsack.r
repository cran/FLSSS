

mmKnapsack <- function(maxCore = 7L, len, itemsProfits, itemsCosts, capacities, heuristic = FALSE, tlimit = 60, useBiSrchInFB = FALSE, threadLoad = 8L, verbose = TRUE)
{
  d = length(capacities)
  mV = itemsCosts


  fixedSize = T
  if(len == 0)
  {
    fixedSize = F
    len = nrow(mV)
    mV = as.matrix(rbind(matrix(numeric(len * d), ncol = d), mV))
    itemsProfits = c(numeric(len), itemsProfits)
    backout = function(x, len){sort(x[x > len] - len)}
  }


  N = length(itemsProfits)
  # minCosts = apply(mV, 2, function(x) sum(sort(x)[1L : len]))
  minCosts = 0
  mTarget = (capacities + minCosts) / 2
  mME = (capacities - minCosts) / 2
  if(!all(mME >= 0)) stop("Minimal costs surpass capacities")


  shouldConjugate = F
  if(2L * len > N)
  {
    shouldConjugate = T
    len = N - len
    itemsProfits = max(itemsProfits) - itemsProfits
    capacities = 0 - (colSums(mV) - capacities)
    mV = as.matrix(0 - mV)
    minV = apply(mV, 2, function(x) min(x))
    mV = apply(mV, 2, function(x) x - min(x))
    capacities = capacities - minV * len
    minCosts = apply(mV, 2, function(x) sum(sort(x)[1L : len]))
    mTarget = (capacities + minCosts) / 2
    mME = (capacities - minCosts) / 2
  }


  theOrder = order(itemsProfits)
  profits = itemsProfits[theOrder]
  mV = as.matrix(mV[theOrder, ])
  rst = z_mTargetMatForKnapsack(len, mV, mTarget, mME)
  mV = rst$mV
  targetMat = rst$targetMat
  mME = rst$mME


  dimnames(mV) = NULL
  # return(list(len = len, mV = mV, profits = profits, targetMat = targetMat, mME = mME))


  rst = z_Gknapsack(len, mV, numeric(0), profits, targetMat, mME, 1L : len, (N - len + 1L) : N, tlimit, useBiSrchInFB, maxCore, threadLoad, verbose, heuristic)


  # print(targetMat[, 1] - mME)
  # print(colSums(mV[rst, ]))
  # print(targetMat[, 1] + mME)


  if(rst[1] == 1L & rst[length(rst)] == 1L) return(integer(0))


  # rst = theOrder[rst$optimalSelection]
  rst = theOrder[rst]
  if(shouldConjugate)
  {
    tmp = 1L : N
    rst = tmp[-rst]
  }
  if(!fixedSize) rst = backout(rst, len)
  rst
}








mmKnapsackIntegerized <- function(maxCore = 7L, len, itemsProfits, itemsCosts, capacities, heuristic = FALSE, precisionLevel = integer(length(capacities)), returnBeforeMining = FALSE, tlimit = 60, useBiSrchInFB = FALSE, threadLoad = 8L, verbose = TRUE)
{
  if(.Machine$sizeof.pointer == 4L)
  {
    message("32-bit architecture unsupported")
    return()
  }


  d = length(capacities)
  mV = itemsCosts


  # substract minima
  {
    minV = apply(mV, 2L, function(x) min(x))
    mV = apply(mV, 2L, function(x) x - min(x))
    capacities = capacities - len * minV
  }


  fixedSize = T
  if(len == 0)
  {
    fixedSize = F
    len = nrow(mV)
    mV = rbind(matrix(numeric(len * d), ncol = d), mV)
    itemsProfits = c(numeric(len), itemsProfits)
    backout = function(x, len){sort(x[x > len] - len)}
  }


  N = length(itemsProfits)
  minCosts = apply(mV, 2, function(x) sum(sort(x)[1L : len]))
  mTarget = (capacities + minCosts) / 2
  mME = (capacities - minCosts) / 2
  if(!all(mME >= 0)) stop("Minimal costs surpass capacities")


  shouldConjugate = F
  if(2L * len > N)
  {
    shouldConjugate = T
    len = N - len
    itemsProfits = max(itemsProfits) - itemsProfits
    capacities = 0 - (colSums(mV) - capacities)
    mV = as.matrix(0 - mV)
    minV = apply(mV, 2, function(x) min(x))
    mV = apply(mV, 2, function(x) x - min(x))
    capacities = capacities - minV * len
    minCosts = apply(mV, 2, function(x) sum(sort(x)[1L : len]))
    mTarget = (capacities + minCosts) / 2
    mME = (capacities - minCosts) / 2
  }


  # integerize
  {
    tmp = z_integerize(len, mV, mTarget, mME, precisionLevel)
    mV = tmp$integerized
    mTarget = tmp$target
    mME = tmp$ME
    minV = apply(mV, 2, function(x) min(x))
    mV = apply(mV, 2, function(x) x - min(x))
    mTarget = mTarget - minV * len
  }


  INT = list(len = len, mV = mV, mTarget = mTarget, mME = mME)


  theOrder = order(itemsProfits)
  profits = itemsProfits[theOrder]
  mV = as.matrix(mV[theOrder, ])
  rst = z_mTargetMatForKnapsackINT(len, mV, mTarget, mME)
  mV = rst$mV
  targetMat = rst$targetMat
  mME = rst$mME


  dimnames(mV) = NULL


  # subtract minima again
  {
    minV = mV[1, ]
    mV = apply(mV, 2, function(x) x - x[1])
    tmp = minV * as.integer(len)
    targetMat = apply(targetMat, 2, function(x) x - tmp)
    dimnames(mV) = NULL
    dimnames(targetMat) = NULL
    dimnames(mME) = NULL
  }


  targetMatAndMaxMag = z_filterTargetFindLargestMagnitude(len, mV, targetMat, mME)
  targetMat = targetMatAndMaxMag$targetMat
  maxMag = targetMatAndMaxMag$maxMag


  # crunch integers
  {
    tmp = z_crunchIntegers(len, mV, targetMat, mME, maxMag = maxMag)
    mV = tmp$mV
    targetMat = tmp$targetMat
    mME = tmp$mME
    maskV = tmp$maskV
  }


  tlimit = tlimit * maxCore


  if(verbose) cat("Dimensionality reduced from", ncol(INT$mV) + 1L, "to", ncol(mV), "\n")
  if(returnBeforeMining) return(list(soltution = integer(0), INT = c(INT, list(compressedDim = ncol(mV)))))


  rst = z_Gknapsack(len, mV, maskV, profits, targetMat, mME, 1L : len, (N - len + 1L) : N, tlimit, useBiSrchInFB, maxCore, threadLoad, verbose, heuristic)


  # print(targetMat[, 1] - mME)
  # print(colSums(mV[rst, ]))
  # print(targetMat[, 1] + mME)


  if(rst[1] == 1L & rst[length(rst)] == 1L) return(list(soltution = integer(0), INT = c(INT, list(compressedDim = ncol(mV)))))



  # rst = theOrder[rst$optimalSelection]
  rst = theOrder[rst]
  if(shouldConjugate)
  {
    tmp = 1L : N
    rst = tmp[-rst]
  }
  if(!fixedSize) rst = backout(rst, len)
  list(soltution = rst, INT = c(INT, list(compressedDim = ncol(mV))))
}



































