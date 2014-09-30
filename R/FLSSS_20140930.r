FLSSS<-function(len,v,target,ME,sizeNeeded=1){
if(is.numeric(sizeNeeded))return(.Call('FLSSS_FLSSS_PART_1', PACKAGE = 'FLSSS', len, v, target, ME, sizeNeeded))
else return(.Call('FLSSS_FLSSS_ALL_1', PACKAGE = 'FLSSS', len, v, target, ME))
}