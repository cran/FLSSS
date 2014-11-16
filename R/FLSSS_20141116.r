FLSSS<-function(len,v,target,ME,sizeNeeded=1,tlimit="none",catch=NULL,throw=F,LB=1:len,UB=(length(v)-len+1):length(v)){
if(len==0||length(v)==0||(is.numeric(sizeNeeded)&sizeNeeded<=0)||(is.numeric(tlimit)&tlimit<=0))return(list())
if(is.numeric(sizeNeeded))
{
    if(is.numeric(tlimit))
    {
        if(is.null(catch))
        {
          if(!throw).Call('FLSSS_FLSSS_SK', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, sizeNeeded, tlimit)
          else .Call('FLSSS_FLSSS_SK_throw', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, sizeNeeded, tlimit)
        }
        else 
        {
          if(!throw).Call('FLSSS_FLSSS_SK_catch', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]], sizeNeeded, tlimit)
          else .Call('FLSSS_FLSSS_SK_catch_throw', PACKAGE = 'FLSSS', len, v, ME,catch[[1]],catch[[2]],catch[[3]],catch[[4]],sizeNeeded, tlimit)
        }
    }
    else 
    {
        if(is.null(catch))
        {
          if(!throw).Call('FLSSS_FLSSS_SK', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, sizeNeeded,-1)
          else .Call('FLSSS_FLSSS_SK_throw', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, sizeNeeded, -1)
        }
        else
        {
          if(!throw).Call('FLSSS_FLSSS_SK_catch', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]], sizeNeeded, -1)
          else .Call('FLSSS_FLSSS_SK_catch_throw', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]], sizeNeeded, -1)
        }
    }
}
else 
{
    if(is.numeric(tlimit))
    {
        if(is.null(catch))
        {
          if(!throw).Call('FLSSS_FLSSS_SK', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, 0, tlimit)
          else .Call('FLSSS_FLSSS_SK_throw', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, 0, tlimit)
        }
        else 
        {
          if(!throw).Call('FLSSS_FLSSS_SK_catch', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]],0,tlimit)
          else .Call('FLSSS_FLSSS_SK_catch_throw', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]], 0, tlimit)
        }
    }
    else 
    {
        if(is.null(catch))
        {
          if(!throw).Call('FLSSS_FLSSS_SK', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB,0,-1)
          else .Call('FLSSS_FLSSS_SK_throw', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, 0, -1)
        }
        else 
        {
          if(!throw).Call('FLSSS_FLSSS_SK_catch', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]], 0, -1)
          else .Call('FLSSS_FLSSS_SK_catch_throw', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]],0,-1)
        }
    }
}
}