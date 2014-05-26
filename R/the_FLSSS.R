#' Fixed size subset sum solution
#' 
#' Given a target T, a marginal error ME, an integer len, a numeric ascending set S, find one or more subsets of size len whose elements sum into [T-ME, T+ME].
#' @param len Subset size
#' @param v Sorted numeric vector as set S
#' @param target Target T
#' @param ME Marginal Error
#' @param sizeNeeded How many solutions at least are wanted; sizeNeeded="all" returns all the solutions
#' @return A list of integer vectors. Each vector contains a solution's elements' positions in v
#' @note Number of solutions output may be greater than sizeNeeded. A resultant empty list indicates there are no solutions. sizeNeeded="all" can take considerable time when v is long.
#' @details \href{https://drive.google.com/file/d/0B8MJ_hAGrDrwNHFScDZqS0x4elE/edit?usp=sharing}{An introduction to the algorithm}
#'
#' \href{https://drive.google.com/file/d/0B8MJ_hAGrDrwUWhkUnpNeld4N28/edit?usp=sharing}{C++ and R codes for package building}
#' 
#' \href{https://drive.google.com/file/d/0B8MJ_hAGrDrwZVJkXzd1RGo4Q2M/edit?usp=sharing}{Explanatory R codes}
#' 
#' \href{https://drive.google.com/file/d/0B8MJ_hAGrDrwUDR4dUVGbG9iMzA/edit?usp=sharing}{Additional C++ codes}
#' @export
#' @examples v<-c(-1119924501,-793412295,-496234747,-213654767,16818148,26267601,26557292,27340260,28343800,
#' 32036573,32847411,34570996,34574989,43633028,44003100,47724096,51905122,52691025,53600924,56874435,
#' 58207678,60225777,60639161,60888288,60890325,61742932,63780621,63786876,65167464,66224357,67198760,
#' 69366452,71163068,72338751,72960793,73197629,76148392,77779087,78308432,81196763,82741805,85315243,
#' 86446883,87820032,89819002,90604146,93761292,97920291,98315039,310120088)
#' len<-10
#' target<-139254955
#' FLSSS(len,v,target,0,"all")
#' FLSSS(len,v,target,1.7,"all")
#' FLSSS(len,v,target,20,5)
FLSSS<-function(len,v,target,ME,sizeNeeded=1){
if(is.numeric(sizeNeeded))return(.Call('FLSSS_FLSSS_PART', PACKAGE = 'FLSSS', len, v, target, ME, sizeNeeded))
else return(.Call('FLSSS_FLSSS_ALL', PACKAGE = 'FLSSS', len, v, target, ME))
}