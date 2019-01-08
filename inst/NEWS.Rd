\name{NEWS}
\alias{NEWS}
\title{FLSSS updates}


\section{Changes in FLSSS version 8.0 (20190106)}{
  \itemize{
    \item Implemented binary knapsack solvers and generalized assignment solvers via different algorithms. Functions prefixed with 'aux' are new.
    \item Future updates will dedicate to implementing existing foundational or state-of-the-art algorithms for combinatorial problems of interest. The implementation will focus on computing speed as usual.
  }
}


\section{Changes in FLSSS version 7.7 (20181121)}{
  \itemize{
    \item Integerization for the generalized assignment solver is canceled. A new compact representation for cost matrix is implemented and accelerates mining speed.
    \item Bug fix to the output format of \code{GAP()} given no solution exists.
    \item Package vignette is pushed to arXiv.
  }
}

