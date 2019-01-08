



auxGAPbb <- function(cost, profitOrLoss, budget, maxCore = 7, tlimit = 60, ub = "MT", greedyBranching = TRUE, optim = "max", multhreadOn = "nodes", threadLoad = 32)
{
  if(multhreadOn == "nodes")
    auxGAPbbMulthreadNodes(cost, profitOrLoss, budget, maxCore, threadLoad, tlimit, ub, greedyBranching, optim)
  else auxGAPbbMulthreadKPs(cost, profitOrLoss, budget, maxCore, tlimit, ub, greedyBranching, optim)
}


auxGAPbbDp <- function(cost, profitOrLoss, budget, maxCore = 7, tlimit = 60, greedyBranching = TRUE, optim = "max", multhreadOn = "nodes", threadLoad = 32)
{
  if(multhreadOn == "nodes")
    auxGAPbbDpMulthreadNodes(cost, profitOrLoss, budget, maxCore, threadLoad, tlimit, greedyBranching, optim)
  else auxGAPbbDpMulthreadKPs(cost, profitOrLoss, budget, maxCore, tlimit, greedyBranching, optim)
}




