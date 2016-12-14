FLSSS<-function(len,v,target,ME,sizeNeeded=1L,tlimit=60,catch=NULL,throw=F,LB=1L:len,UB=(length(v)-len+1L):length(v),useFloat=F){
if(length(v)==0|(is.numeric(sizeNeeded)&sizeNeeded<=0)|(is.numeric(tlimit)&tlimit<=0))return(list())

fixedSize=T
if(len==0)
{
  fixedSize=F
  len=length(v)
  v=c(rep(0,len), v)
  vindex=c(rep(0L,len), 1L:len)
  sortOrder=order(v)
  v=v[sortOrder]
  vindex=vindex[sortOrder]
}

rst=NULL

if(is.numeric(sizeNeeded))
{
    if(is.numeric(tlimit))
    {
        if(is.null(catch))
        {
          if(!throw)rst=.Call('FLSSS_FLSSS_SK', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, sizeNeeded, tlimit ,useFloat)
          else rst=.Call('FLSSS_FLSSS_SK_throw', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, sizeNeeded, tlimit ,useFloat)
        }
        else
        {
          if(!throw)rst=.Call('FLSSS_FLSSS_SK_catch', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]], sizeNeeded, tlimit ,useFloat)
          else rst=.Call('FLSSS_FLSSS_SK_catch_throw', PACKAGE = 'FLSSS', len, v, ME,catch[[1]],catch[[2]],catch[[3]],catch[[4]],sizeNeeded, tlimit ,useFloat)
        }
    }
    else
    {
        if(is.null(catch))
        {
          if(!throw)rst=.Call('FLSSS_FLSSS_SK', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, sizeNeeded,-1 ,useFloat)
          else rst=.Call('FLSSS_FLSSS_SK_throw', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, sizeNeeded, -1 ,useFloat)
        }
        else
        {
          if(!throw)rst=.Call('FLSSS_FLSSS_SK_catch', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]], sizeNeeded, -1 ,useFloat)
          else rst=.Call('FLSSS_FLSSS_SK_catch_throw', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]], sizeNeeded, -1 ,useFloat)
        }
    }
}
else
{
    if(is.numeric(tlimit))
    {
        if(is.null(catch))
        {
          if(!throw)rst=.Call('FLSSS_FLSSS_SK', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, 0, tlimit ,useFloat)
          else rst=.Call('FLSSS_FLSSS_SK_throw', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, 0, tlimit ,useFloat)
        }
        else
        {
          if(!throw)rst=.Call('FLSSS_FLSSS_SK_catch', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]],0,tlimit ,useFloat)
          else rst=.Call('FLSSS_FLSSS_SK_catch_throw', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]], 0, tlimit ,useFloat)
        }
    }
    else
    {
        if(is.null(catch))
        {
          if(!throw)rst=.Call('FLSSS_FLSSS_SK', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB,0,-1 ,useFloat)
          else rst=.Call('FLSSS_FLSSS_SK_throw', PACKAGE = 'FLSSS', len, v, target, ME, LB, UB, 0, -1 ,useFloat)
        }
        else
        {
          if(!throw)rst=.Call('FLSSS_FLSSS_SK_catch', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]], 0, -1 ,useFloat)
          else rst=.Call('FLSSS_FLSSS_SK_catch_throw', PACKAGE = 'FLSSS', len, v, ME, catch[[1]],catch[[2]],catch[[3]],catch[[4]],0,-1 ,useFloat)
        }
    }
}

if(fixedSize)return(rst)

unique(lapply(rst, function(x)sort(vindex[x][vindex[x]>0L])))
}









mFLSSSpar<-function(len, mV, mTarget, mME, viaConjugate=F, maxCore=7L, totalSolutionNeeded=1L, tlimit=60, singleTimeLimit=10, randomizeTargetOrder=sample(1L:(len*(nrow(mV)-len)+1L), len*(nrow(mV)-len)+1L), LB=1L:len, UB=(nrow(mV)-len+1L):nrow(mV)){

fixedSize=T
if(len==0)
{
  fixedSize=F
  len=nrow(mV)
  mV=as.data.frame(lapply(mV, function(x)c(rep(0,len), x)))
  backout=function(mV, len){unique(lapply(mV,function(x)sort(x[x>len]-len)))}
  randomizeTargetOrder=T
  LB=1L:len
  UB=(nrow(mV)-len+1L):nrow(mV)
}

shouldConjugate=F
if(is.null(viaConjugate)&2L*len>nrow(mV))shouldConjugate=T
else if(!is.null(viaConjugate))
{
  if(viaConjugate)shouldConjugate=T
}

if(shouldConjugate)
{
  len=nrow(mV)-len
  mTarget=colSums(mV)-mTarget
}

if(ncol(mV)==1L){print("Please call FLSSS for single dimensional set");return(list());}

bestCol=colSums(cor(mV,method="spearman"))

if(abs(bestCol[1]-ncol(mV))<1e-10)
{
  mV=data.frame(mV,id=1L:nrow(mV))
  mV=mV[order(mV[[1L]]),]
  mVid=mV$id
  mV$id=NULL
  rst=.Call('FLSSS_mFLSSS_SK_como', PACKAGE = 'FLSSS', len, nrow(mV), mV, mTarget, mME, LB, UB, totalSolutionNeeded, tlimit)
  rst=lapply(rst,function(x)mVid[x])
  tmp=1L:nrow(mV)
  if(shouldConjugate)rst=lapply(rst, function(x)tmp[-x])
  if(!fixedSize)rst=backout(rst, len)
  return(rst)
}

bestCol=which(bestCol==max(bestCol))[1]
mV=data.frame(mV,id=1L:nrow(mV))
mV=mV[order(mV[[bestCol]]),]
mVid=mV$id

mV$id=NULL

keyDiff=diff(mV[[bestCol]])
if(min(keyDiff)>1e-12)
{
  k=unlist(lapply(mV[-bestCol],function(x)max(0,max(-diff(x)/keyDiff))))
  if(all(k<=mME[-bestCol]/mME[bestCol]))
  {
    mV[-bestCol]=mapply(function(x,y)x*mV[[bestCol]]+y,k,mV[-bestCol],SIMPLIFY=F)
    lbound=mTarget-mME
    ubound=mTarget+mME
    lbound[-bestCol]=lbound[-bestCol]+k*ubound[bestCol]
    ubound[-bestCol]=ubound[-bestCol]+k*lbound[bestCol]
    mTarget=(lbound+ubound)/2
    mME=(-lbound+ubound)/2
    rst=.Call('FLSSS_mFLSSS_SK_como', PACKAGE = 'FLSSS', len, nrow(mV), mV, mTarget, mME, LB, UB, totalSolutionNeeded, tlimit)
    rst=lapply(rst,function(x)mVid[x])
    tmp=1L:nrow(mV)
    if(shouldConjugate)rst=lapply(rst,function(x)tmp[-x])
    if(!fixedSize)rst=backout(rst, len)
    return(rst)
  }
}


mV=data.frame(key=1L:nrow(mV),mV)

k=unlist(lapply(mV[-1],function(x)max(0,max(-diff(x)/1))))

# print(k)
#
# print(mME)

mV[-1]=mapply(function(x,y)x*mV[[1]]+y,k,mV[-1],SIMPLIFY=F)

keyME=min(mME/k)
keyME[is.nan(keyME)]=0

# print(keyME)

firstKeyTarget=sum(1L:len)+as.integer(keyME)
lastKeyTarget=sum((nrow(mV)-len+1L):nrow(mV))-as.integer(keyME)

# print(firstKeyTarget)
# print(lastKeyTarget)

if(firstKeyTarget>=lastKeyTarget)
{
  keylbound=sum(1L:len)
  keyubound=sum((nrow(mV)-len+1L):nrow(mV))
  lbound=mTarget-mME;
  ubound=mTarget+mME;
  lbound=lbound+k*keyubound
  ubound=ubound+k*keylbound
  mTarget=(ubound+lbound)/2
  mME=(ubound-lbound)/2
  mV=mV[-1]
  rst=.Call('FLSSS_mFLSSS_SK_como', PACKAGE = 'FLSSS', len, nrow(mV), mV, mTarget, mME, LB, UB, totalSolutionNeeded, tlimit)
  rst=lapply(rst,function(x)mVid[x])
  tmp=1L:nrow(mV)
  if(shouldConjugate)rst=lapply(rst,function(x)tmp[-x])
  if(!fixedSize)rst=backout(rst, len)
  return(rst)
}

if(2*keyME<1)By=1L
else if(2L*as.integer(keyME)==as.integer(2*keyME))
  By=as.integer(2*keyME)+1L
else By=as.integer(2*keyME)

keyTarget=seq(firstKeyTarget, lastKeyTarget+as.integer(keyME), by=By)
if(keyTarget[length(keyTarget)]<lastKeyTarget+as.integer(keyME))
  keyTarget=c(keyTarget, lastKeyTarget+as.integer(keyME))


if(length(randomizeTargetOrder)!=1L)
{
  if(length(randomizeTargetOrder)!=len*(nrow(mV)-len)+1L)
  {
    return("wrong randomizedTargetOrder size")
  }
  keyTarget=keyTarget[randomizeTargetOrder[randomizeTargetOrder<=length(keyTarget)]]
}
else keyTarget=sample(keyTarget,length(keyTarget))

mME=-k*rep(as.integer(keyME), length(mME))+mME

if(as.integer(keyME)<1L)keyME=1e-1
mME=c(keyME, mME)


if(maxCore<2)rst=.Call('FLSSS_mFLSSS_SK', PACKAGE = 'FLSSS', len, nrow(mV), mV, keyTarget, mTarget, k, mME, LB, UB, totalSolutionNeeded, singleTimeLimit, tlimit)
else rst=.Call('FLSSS_mFLSSS_SK_par', PACKAGE = 'FLSSS', maxCore, len, nrow(mV), mV, keyTarget, mTarget, k, mME, LB, UB, totalSolutionNeeded, singleTimeLimit, tlimit)

rst=lapply(rst,function(x)mVid[x])

tmp=1L:nrow(mV)
if(shouldConjugate)rst=lapply(rst,function(x)tmp[-x])
if(!fixedSize)rst=backout(rst, len)
rst
}









mmFLknapsack<-function(len, mV, lbound, ubound, viaConjugate=F, maxCore=7L, totalSolutionNeeded=1L, tlimit=60, singleTimeLimit=10, randomizeTargetOrder=sample(1L:(len*(nrow(mV)-len)+1L), len*(nrow(mV)-len)+1L), LB=1L:len, UB=(nrow(mV)-len+1L):nrow(mV)){

#if(len<1)randomizeTargetOrder=T

tmp=as.data.frame(t(as.data.frame(mapply(function(l,u,v)
{
  v=sort(v)
  if(len>0)
  {
    tmpLow=sum(v[1L:len])
    tmpHigh=sum(v[(length(v)-len+1L):length(v)])
  }
  else
  {
    if(v[1]>0)tmpLow=v[1]
    else tmpLow=sum(v[v<=0])

    if(v[length(v)]<0)tmpHigh=v[length(v)]
    else tmpHigh=sum(v[v>=0])
  }

  if(!is.finite(l)|l<tmpLow)l=tmpLow
  else if(l>tmpHigh)
  {
    stop("One of the finite lower bound is larger than the greatest possible subset sum in that dimension")
  }

  if(!is.finite(u)|u>tmpHigh)u=tmpHigh
  else if(u<tmpLow)
  {
    stop("One of the finite upper bound is less than the smallest possible subset sum in that dimension")
  }

  c(l, u)
}, lbound, ubound, mV, SIMPLIFY = F))))

lbound=tmp[[1]]
ubound=tmp[[2]]

mTarget=(lbound+ubound)/2
mME=(ubound-lbound)/2

mFLSSSpar(len, mV, mTarget, mME, viaConjugate, maxCore, totalSolutionNeeded, tlimit, singleTimeLimit, randomizeTargetOrder, LB, UB)
}




