\name{NEWS}
\alias{NEWS}
\title{FLSSS updates}


\section{Changes in FLSSS version 8.5 (20190106)}{
  \itemize{
    \item Bug fix. The previous branch-and-bound knapsack solver did not initialize containers with sufficient sizes. In rare cases a pointer would be realized at the end of the container.
    \item Added a genetic algorithm with local heuristics for generalized assignment problem.
  }
}


\section{Changes in FLSSS version 8.3 (20190106)}{
  \itemize{
    \item Implemented different algorithms for the binary knapsack and the generalized assignment problems. Functions prefixed with 'aux' (auxiliary) are new.
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

