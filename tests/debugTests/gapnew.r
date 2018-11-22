rm(list = ls()); gc()
agents = 4L
tasks = 8L
costs = t(as.data.frame(lapply(1L : agents, function(x) runif(tasks) * 1000)))
budgets = apply(costs, 1, function(x) runif(1, min(x), sum(x)))
profits = t(as.data.frame(lapply(1L : agents, function(x)
  abs(rnorm(tasks) + runif(1, 0, 4)) * 10000)))
dimnames(profits) = NULL
names(budgets) = NULL


# donttest{
rst = FLSSS:::GAPpre(
  maxCore = 7L, agentsCosts = costs, agentsProfits = profits,
  agentsBudgets = budgets, tlimit = 30, threadLoad = 8L, verbose = TRUE)


if(F){
# save.image()


tmp = FLSSS:::testFindBoundGAP(rst$dividedV, rst$targetMat[, 2], rst$ME)
tmp2 = FLSSS:::testFindBound002GAP(rst$dividedV, rst$targetMat[, 2], rst$ME)
tmp; tmp2
}


# X is rst$dividedV
gapfindB <-function(X, Min, Max, LB = NULL, UB = NULL)
{
  nagent = length(Min) - 1L
  ntask = ncol(X) / nagent
  v = list()
  k = 1L
  for(i in seq(1L, by = nagent, len = ntask))
  {
    v[[k]] = X[, i : (i + nagent - 1L)]
    v[[k]] = rbind(v[[k]], 0L : (nagent - 1L))
    k = k + 1L
  }
  if(is.null(LB) | is.null(UB))
  {
    LB = rep(1L, ntask)
    UB = rep(nagent, ntask)
  }


  # print("========================")
  # print(Min - rowSums(as.data.frame(mapply(function(x, y)
  # {
  #   x[, y]
  # }, v, UB, SIMPLIFY = F))))
  # print("========================")
  # print("========================")
  # print(Max - rowSums(as.data.frame(mapply(function(x, y)
  # {
  #   x[, y]
  # }, v, LB, SIMPLIFY = F))))
  # print("========================")


  first = T
  while(T)
  {
    LBresv = LB
    for(i in 1L : ntask)
    {
      S = Min - rowSums(as.data.frame(mapply(function(x, y)
      {
        x[, y]
      }, v[-i], UB[-i], SIMPLIFY = F)))
      names(S) = NULL
      currentTask = v[[i]]
      # cat("Min_rowSums =", S, "\n")
      for(k in LB[i] : UB[i])
      {
        # cat("k =", k, ", ")
        # print(currentTask[, k])
        # if(all(currentTask[, k] >= S - 1e-10))
        if(currentTask[nrow(currentTask), k] >= S[nrow(currentTask)] - 1e-10)
        {
          # print("all(currentTask[, k] >= S - 1e-10)")
          break
        }
      }
      # if(k >= UB[i] & !all(currentTask[, k] >= S - 1e-10)) return(list(LB, UB, F))
      if(k >= UB[i] & !(currentTask[nrow(currentTask), k] >= S[nrow(currentTask)] - 1e-10)) return(list(LB, UB, F))
      LB[i] = k
      # cat('LB[i] = ', LB[i], '\n')
    }


    cat("LB =", LB - 1L, "\n")


    if(all(LBresv == LB) & !first) break
    first = F


    UBresv = UB
    for(i in 1L : ntask)
    {
      S = Max - rowSums(as.data.frame(mapply(function(x, y)
      {
        x[, y]
      }, v[-i], LB[-i], SIMPLIFY = F)))
      currentTask = v[[i]]
      # cat("Max_rowSums =", S, "\n")
      for(k in UB[i] : LB[i])
      {
        if(all(currentTask[, k] <= S + 1e-10))
        {
          # print("all(currentTask[, k] <= S + 1e-10)")
          break
        }
      }
      if(k <= LB[i] & !all(currentTask[, k] <= S + 1e-10)) return(list(LB, UB, F))
      UB[i] = k
    }


    cat("UB =", UB - 1L, "\n")


    if(all(UBresv == UB)) break
  }
  return(list(LB - 1L, UB - 1L))
}


targetChosen = sample(1L : ncol(rst$MAXmat), 1)


tmp = gapfindB(t(rst$V), Min = c(rep(-1e10, agents), rst$MAXmat[agents + 1, targetChosen]), Max = rst$MAXmat[, targetChosen], LB = NULL, UB = NULL)
# tmp

# save.image()


# tmp2 = FLSSS:::testFindBound002GAP(rst$dividedV, rst$targetMat[, targetChosen], rst$ME)
target = (c(numeric(agents), rst$MAXmat[agents + 1, targetChosen]) + rst$MAXmat[, targetChosen]) / 2
ME = target
ME[length(ME)] = 0
tmp3 = FLSSS:::testFindBound003GAP(t(rst$V), target, profits, ME)
tmp4 = FLSSS:::testFindBound003GAP2(t(rst$V), rst$MAXmat[, targetChosen])


# if(tmp3$boo == 0 | tmp[[3]] == 0)
difference = sum(abs(unlist(tmp[1 : 2]) - unlist(tmp3[1 : 2])))
print(difference)
if(difference != 0 & tmp3[[3]])
{
  print(tmp); print(tmp3)
}








rm(list = ls()); gc()
agents = 3L
tasks = 6L
costs = t(as.data.frame(lapply(1L : agents, function(x) runif(tasks) * 1000)))
budgets = apply(costs, 1, function(x) runif(1, min(x), sum(x)))
budgets2 = budgets * runif(1, 0.5, 0.99)
profits = t(as.data.frame(lapply(1L : agents, function(x)
  abs(rnorm(tasks) + runif(1, 0, 4)) * 10000)))


rst = FLSSS::GAP(
  maxCore = 7L, agentsCosts = costs, agentsProfits = profits,
  agentsBudgets = budgets, tlimit = 30, threadLoad = 8L, verbose = TRUE)


targetChosen = sample(1L : ncol(rst$targetMat), 1)


tmp = FLSSS:::testFindBound003GAP(rst$dividedV, rst$targetMat[, targetChosen], rst$ME)


rst2 = FLSSS::GAP(
  maxCore = 7L, agentsCosts = costs, agentsProfits = profits,
  agentsBudgets = budgets2, tlimit = 30, threadLoad = 8L, verbose = TRUE)


if(exists('rst2')) tmp2 = FLSSS:::testFindBound003GAP(rst2$dividedV, rst2$targetMat[, targetChosen], rst2$ME)


if(tmp$boo & tmp2$boo)
{
  print(all(tmp$UB >= tmp2$UB))
  print(all(tmp$LB >= tmp2$LB))
}




# =======================================================================================
# New gap testing
# =======================================================================================
rm(list = ls()); gc()
agents = 3L
tasks = 6L
costs = t(as.data.frame(lapply(1L : agents, function(x) runif(tasks) * 1000)))
budgets = apply(costs, 1, function(x) runif(1, min(x), sum(x) * 0.5))
profits = t(as.data.frame(lapply(1L : agents, function(x)
  abs(rnorm(tasks) + runif(1, 0, 4)) * 10000)))


# save.image()

if(F)
{
  # sink('output.txt')
  system.time({rst = FLSSS::GAP(maxCore = 7L, agentsCosts = costs, agentsProfits = profits, agentsBudgets = budgets, heuristic = T, threadLoad = 8L, verbose = TRUE, tlimit = 3600)})
  # sink()
  all(rst$assignmentCosts <= rst$agentsBudgets)
}



names(budgets) = NULL
system.time({rst2 = FLSSSpre::GAP(maxCore = 7L, agentsCosts = costs, agentsProfits = profits, agentsBudgets = budgets, heuristic = T, tlimit = 3600)})
all(rst2$assignmentCosts <= rst2$agentsBudgets)
rst2$assignmentProfit / rst2$unconstrainedMaxProfit




sum(abs(rst$assignedAgents$agent - rst2$assignedAgents$agent))


k = 3L
MIN = c(numeric(agents), rst$MAXmat[agents + 1L, k])
MAX = rst$MAXmat[, k]
v = t(cbind(rst$FLSSSvec, c(0, 1, 0, 1, 0, 1, 0, 1)))
FLSSS:::gapfindB(v[1 : 2, ], MIN, MAX, LB = NULL, UB = NULL)
MIN - rowSums(v[, c(1, 3, 5, 7) + 1])
MAX - rowSums(v[, c(1, 3, 5, 7)])


MIN = MIN - v[, 5]
MAX = MAX - v[, 5]
v = v[, -(5 : 6)]
FLSSS:::gapfindB(v[1 : 2, ], MIN, MAX, LB = NULL, UB = NULL)
MIN - rowSums(v[, c(1, 3, 5) + 1])
MAX - rowSums(v[, c(1, 3, 5)])


MIN = MIN - v[, 2]
MAX = MAX - v[, 2]
v = v[, -(1 : 2)]
FLSSS:::gapfindB(v[1 : 2, ], MIN, MAX, LB = NULL, UB = NULL)
MIN - rowSums(v[, c(1, 3) + 1])
MAX - rowSums(v[, c(1, 3)])




agents = 5L; tasks = 100L
tmp = c(17,40,35,24,50,15,31,38,48,18,16,44,41,10,26,50,48,20,24,12,48,24,34,39,14,32,47,10,45,15,22,20,44,41,49,10,30,46,20,37,19,12,14,31,28,22,38,47,12,29,31,30,30,29,50,39,44,23,37,48,23,18,21,26,26,22,22,11,48,19,43,37,42,34,46,28,14,50,30,15,36,42,21,18,29,38,42,45,37,39,48,39,42,50,12,34,49,12,25,29,40,34,20,13,14,49,13,42,49,43,12,12,21,31,44,18,36,44,20,22,16,50,18,50,20,13,34,29,36,50,38,24,46,19,49,41,39,33,13,38,41,23,49,30,37,20,14,31,21,30,48,36,45,36,42,25,22,45,16,15,41,19,39,37,11,11,36,12,21,10,14,21,47,38,16,41,36,39,50,14,21,47,39,46,31,24,44,28,15,25,24,12,35,19,37,23,24,35,26,40,32,13,20,33,26,46,48,38,12,49,13,47,17,13,45,35,47,13,26,42,29,29,16,24,37,33,35,22,36,24,35,13,23,43,29,37,41,32,47,31,48,15,49,44,11,39,48,43,47,49,21,48,39,35,45,28,28,11,33,29,22,22,14,43,33,30,31,31,17,12,12,48,23,40,50,41,32,19,47,49,30,24,18,24,43,13,14,23,33,39,20,38,22,21,49,15,23,32,10,16,26,31,39,17,27,25,22,32,50,43,12,49,40,44,38,30,23,42,46,26,42,45,22,42,19,40,34,39,38,39,26,30,42,39,31,46,30,49,11,50,20,22,49,19,43,19,15,43,25,27,32,33,45,49,18,33,20,49,44,38,45,46,11,28,21,18,30,18,46,32,10,45,42,21,35,36,10,31,42,39,40,25,40,19,40,37,21,42,15,44,43,38,17,17,10,48,27,19,49,39,13,28,50,48,37,12,24,32,35,49,37,32,41,41,30,17,28,49,36,18,19,13,10,41,11,49,25,41,24,40,36,30,35,18,32,48,32,37,41,40,15,11,40,36,23,26,17,40,10,48,23,31,30,12,19,29,47,41,47,43,41,28,33,13,14,32,15,31,36,45,47,30,13,24,40,24,50,44,30,37,38,49,16,26,29,45,37,43,27,27,23,43,50,40,14,30,23,29,49,25,18,12,12,13,19,13,13,9,15,6,20,23,7,24,19,15,25,13,8,19,13,12,25,7,11,18,7,24,16,19,10,16,14,11,23,21,13,5,12,10,5,20,5,14,21,25,18,22,18,9,12,6,9,23,6,11,15,7,24,12,16,18,14,15,24,23,16,7,15,5,13,22,10,23,6,25,8,7,9,11,8,9,11,13,9,8,16,13,17,11,7,8,7,8,14,14,20,12,8,11,7,10,8,13,13,22,15,6,10,5,12,8,7,16,14,22,6,19,17,23,7,23,18,22,12,7,6,5,7,21,21,18,14,22,9,15,11,18,15,21,9,21,24,15,15,5,16,24,16,7,5,11,6,9,20,7,11,16,6,24,5,22,24,5,22,24,22,18,12,7,12,21,7,16,18,12,6,11,19,20,15,12,18,5,10,15,24,12,12,22,7,17,23,24,10,20,18,7,8,8,16,20,24,25,13,21,24,22,6,17,12,21,22,14,5,16,25,16,8,10,20,14,16,19,21,10,5,14,11,6,10,16,15,19,9,25,11,7,8,24,12,25,12,5,17,22,17,9,5,24,19,7,24,9,19,18,13,20,17,14,15,20,25,8,12,24,19,18,14,12,16,19,12,9,21,5,23,24,19,14,24,25,7,16,13,6,18,24,24,22,23,13,22,11,25,12,24,6,7,15,11,18,7,5,9,18,9,17,25,17,12,15,22,18,13,22,23,9,12,6,11,25,25,15,6,7,23,14,17,15,17,13,17,11,20,22,15,16,14,8,9,20,17,20,11,19,22,16,25,5,19,13,23,8,13,12,7,20,5,19,11,11,19,20,5,21,8,7,15,20,18,18,23,17,10,12,19,16,5,21,16,8,9,12,21,7,5,7,24,25,18,11,17,11,7,12,16,23,5,18,5,23,11,11,17,17,25,18,21,12,5,7,15,15,14,10,17,12,5,10,15,24,23,18,19,5,5,16,25,15,17,11,15,11,11,5,24,11,9,21,6,6,6,18,21,5,21,14,16,5,25,23,5,20,16,20,10,18,25,6,14,23,10,24,25,5,24,5,15,19,12,25,17,9,24,22,16,7,12,10,15,5,18,14,20,25,16,22,19,9,21,10,7,12,5,10,5,21,17,5,221,224,254,235,232)
budgets = tail(tmp, n = agents)
costs = t(matrix(tmp[1L : (tasks * agents)], nrow = tasks))
profits = t(matrix(tmp[(tasks * agents + 1L) : (tasks * agents * 2L)], nrow = tasks))
profits = max(profits) - profits


system.time({rst = FLSSS::GAP(maxCore = 7L, agentsCosts = costs, agentsProfits = profits, agentsBudgets = budgets, heuristic = T, threadLoad = 128L, verbose = TRUE, tlimit = 3600)})
all(rst$assignmentCosts <= rst$agentsBudgets)









rst = FLSSS:::GAPpre(maxCore = 7L, agentsCosts = costs, agentsProfits = profits, agentsBudgets = budgets, tlimit = 30, threadLoad = 8L, verbose = TRUE)
FLSSS:::testFindBound003GAP2(t(rst$V), rst$MAXmat[, 11])
FLSSS:::gapfindB(t(rst$FLSSSvec), numeric(4), rst$MAXmat[, 11], LB = NULL, UB = NULL)















