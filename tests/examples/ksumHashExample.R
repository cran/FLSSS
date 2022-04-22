
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








