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
  rst = FLSSS::GAP(maxCore = 7L, agentsCosts = costs, agentsProfits = profits,
                   agentsBudgets = budgets, heuristic = FALSE, tlimit = 2,
                   threadLoad = 8L, verbose = TRUE)
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


rst = FLSSS::GAP(maxCore = 2L, agentsCosts = costs, agentsProfits = profits,
                 agentsBudgets = budgets, heuristic = FALSE, tlimit = 2,
                 threadLoad = 8L, verbose = TRUE)
agentCostsOrProfits(rst$assignedAgents, costs)
# Should equal rst$assignmentCosts and not surpass budgets


knownOptSolution = as.integer(c(3, 3, 1, 1, 2, 2, 1, 2))
knownOptSolution = data.frame(task = 1L : tasks, agent = knownOptSolution)


# Total profit from knownOptSolution:
sum(agentCostsOrProfits(knownOptSolution, profits))
# Total profit from FLSSS::GAP():
rst$assignmentProfit
# FLSSS::GAP() granted a better solution.




# =====================================================================================
# Test case P03 from
# http://support.sas.com/documentation/cdl/en/ormpug/67517/HTML/default
# /viewer.htm#ormpug_decomp_examples02.htm
# =====================================================================================
rm(list = ls()); gc()
agents = 8L
tasks = 24L
profits = matrix(c(
25, 23, 20, 16, 19, 22, 20, 16, 15, 22, 15, 21, 20, 23, 20, 22, 19, 25, 25, 24, 21, 17,
23, 17, 16, 19, 22, 22, 19, 23, 17, 24, 15, 24, 18, 19, 20, 24, 25, 25, 19, 24, 18, 21,
16, 25, 15, 20, 20, 18, 23, 23, 23, 17, 19, 16, 24, 24, 17, 23, 19, 22, 23, 25, 23, 18,
19, 24, 20, 17, 23, 23, 16, 16, 15, 23, 15, 15, 25, 22, 17, 20, 19, 16, 17, 17, 20, 17,
17, 18, 16, 18, 15, 25, 22, 17, 17, 23, 21, 20, 24, 22, 25, 17, 22, 20, 16, 22, 21, 23,
24, 15, 22, 25, 18, 19, 19, 17, 22, 23, 24, 21, 23, 17, 21, 19, 19, 17, 18, 24, 15, 15,
17, 18, 15, 24, 19, 21, 23, 24, 17, 20, 16, 21, 18, 21, 22, 23, 22, 15, 18, 15, 21, 22,
15, 23, 21, 25, 25, 23, 20, 16, 25, 17, 15, 15, 18, 16, 19, 24, 18, 17, 21, 18, 24, 25,
18, 23, 21, 15, 24, 23, 18, 18, 23, 23, 16, 20, 20, 19, 25, 21), ncol = tasks)
costs = matrix(c(
8, 18, 22, 5, 11, 11, 22, 11, 17, 22, 11, 20, 13, 13, 7, 22, 15, 22, 24, 8, 8, 24, 18,
8, 24, 14, 11, 15, 24, 8, 10, 15, 19, 25, 6, 13, 10, 25, 19, 24, 13, 12, 5, 18, 10, 24,
8, 5, 22, 22, 21, 22, 13, 16, 21, 5, 25, 13, 12, 9, 24, 6, 22, 24, 11, 21, 11, 14, 12,
10, 20, 6, 13, 8, 19, 12, 19, 18, 10, 21, 5, 9, 11, 9, 22, 8, 12, 13, 9, 25, 19, 24,
22, 6, 19, 14, 25, 16, 13, 5, 11, 8, 7, 8, 25, 20, 24, 20, 11, 6, 10, 10, 6, 22, 10,
10, 13, 21, 5, 19, 19, 19, 5, 11, 22, 24, 18, 11, 6, 13, 24, 24, 22, 6, 22, 5, 14, 6,
16, 11, 6, 8, 18, 10, 24, 10, 9, 10, 6, 15, 7, 13, 20, 8, 7, 9, 24, 9, 21, 9, 11, 19,
10, 5, 23, 20, 5, 21, 6, 9, 9, 5, 12, 10, 16, 15, 19, 18, 20, 18, 16, 21, 11, 12, 22,
16, 21, 25, 7, 14, 16, 10), ncol = tasks)
budgets = c(36, 35, 38, 34, 32, 34, 31, 34)


system.time({rst = FLSSS::GAP(
  maxCore = 7L, agentsCosts = costs, agentsProfits = profits,
  agentsBudgets = budgets, heuristic = TRUE,
  tlimit = 3600 * 10, threadLoad = 80L, verbose = TRUE)})






















