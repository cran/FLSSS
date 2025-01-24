if(.Machine$sizeof.pointer == 8L){
# =====================================================================================
# 64-bit architecture required.
# =====================================================================================
# =====================================================================================
# Play random numbers
# =====================================================================================
rm(list = ls()); gc()
agents = 5L
tasks = 12L
costs = t(as.data.frame(lapply(1L : agents, function(x) runif(tasks) * 1000)))
budgets = apply(costs, 1, function(x) runif(1, min(x), sum(x)))
profits = t(as.data.frame(lapply(1L : agents, function(x)
  abs(rnorm(tasks) + runif(1, 0, 4)) * 10000)))


# A dirty function for examining the result's integrity. The function takes in
# the task-agent assignment, the profit or cost matrix M, and calculates the cost
# or profit generated by each agent. 'assignment' is a 2-column data
# frame, first column task, second column agent.
agentCostsOrProfits <- function(assignment, M)
{
  n = ncol(M) * nrow(M)
  M2 = matrix(numeric(n), ncol = tasks)
  for(i in 1L : nrow(assignment))
  {
    x = as.integer(assignment[i, ])
    M2[x[2], x[1]] = M[x[2], x[1]]
  }
  apply(M2, 1, function(x) sum(x))
}


dimnames(costs) = NULL
dimnames(profits) = NULL
names(budgets) = NULL

# \donttest{
rst = FLSSS::GAPintegerized(maxCore = 7L, agentsCosts = costs, agentsProfits = profits,
                            agentsBudgets = budgets, heuristic = FALSE,
                            precisionLevel = rep(tasks * 4L, agents), tlimit = 30,
                            useBiSrchInFB = FALSE, threadLoad = 8L, verbose = TRUE)


# Function also saves the assignment costs and profits
rst$assignedAgents
rst$assignmentProfit
rst$assignmentCosts


# Examine rst$assignmentCosts
if(sum(rst$assignedAgents) > 0) # all zeros mean the function has not found a solution.
  agentCostsOrProfits(rst$assignedAgents, costs)
# Should equal rst$assignmentCosts and not surpass budgets


# Examine rst$assignmentProfits
if(sum(rst$assignedAgents) > 0)
  sum(agentCostsOrProfits(rst$assignedAgents, profits))
# Should equal rst$assignmentProfit
# }



# =====================================================================================
# Test case P03 from
# https://people.sc.fsu.edu/~jburkardt/datasets/generalized_assignment/
# =====================================================================================
agents = 3L
tasks = 8L
profits = matrix(c(
27, 12, 12, 16, 24, 31, 41, 13,
14,  5, 37,  9, 36, 25,  1, 34,
34, 34, 20,  9, 19, 19,  3, 34), ncol = tasks)
costs = matrix(c(
21, 13,  9,  5,  7, 15,  5, 24,
20,  8, 18, 25,  6,  6,  9,  6,
16, 16, 18, 24, 11, 11, 16, 18), ncol = tasks)
budgets = c(26, 25, 34)


rst = FLSSS::GAPintegerized(maxCore = 7L, agentsCosts = costs, agentsProfits = profits,
                            agentsBudgets = budgets, heuristic = FALSE, tlimit = 30,
                            useBiSrchInFB = FALSE, threadLoad = 8L, verbose = TRUE)
agentCostsOrProfits(rst$assignedAgents, costs)
# Should equal rst$assignmentCosts and not surpass budgets


knownOptSolution = as.integer(c(3, 3, 1, 1, 2, 2, 1, 2))
knownOptSolution = data.frame(task = 1L : tasks, agent = knownOptSolution)


# Total profit from knownOptSolution:
sum(agentCostsOrProfits(knownOptSolution, profits))
# Total profit frim FLSSS::GAP():
rst$assignmentProfit
# FLSSS::GAP() generated a better solution.
# =====================================================================================
# =====================================================================================
}



