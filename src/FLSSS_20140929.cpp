#include <Rcpp.h>
using namespace Rcpp;


//void printv(NumericVector&v,std::vector<NumericVector::iterator>&BI){
//std::vector<NumericVector::iterator>::iterator i=BI.begin();
//for(;i!=BI.end();++i){
//std::cout<<*i-v.begin()+1<<", ";
//}
//}

//intention: sums the actual values pointed by UBI or LBI
double itersum(std::vector<NumericVector::iterator>::iterator start,std::vector<NumericVector::iterator>::iterator end){
double S=0;
for(std::vector<NumericVector::iterator>::iterator i=start;i!=end;i++)S=S+**i;
return S;
}


std::vector<std::vector<double>*>* theMatrix(NumericVector&v, unsigned short&len) {
   std::vector<std::vector<double>*>*M=new std::vector<std::vector<double>*>(len);
   std::vector<std::vector<double>*>::iterator Mi=M->begin(), Miend;
   std::vector<double>::iterator tmpi, tmpiend;
   NumericVector::iterator vi=v.begin(), vii;
   unsigned short i=v.size();
   *Mi=new std::vector<double>(i);
   for(tmpi=(**Mi).begin();vi!=v.end();++tmpi,++vi)*tmpi=*vi;
   ++Mi;
   --i;
   std::vector<double>::iterator prior;
   vi=v.begin()+1;
   Miend=M->end();
   for(;Mi!=Miend;++Mi,--i,++vi)
   {
     vii=vi;
     *Mi=new std::vector<double>(i);
     tmpi=(**Mi).begin();
     prior=(**(Mi-1)).begin();
     tmpiend=(**Mi).end();
     for(;tmpi!=tmpiend;++tmpi,++prior,++vii)
       *tmpi=*prior+*vii;
   }
   return M;
}






//intention: find infima and suprema for every position. 
//LBI and UBI are equivalent to LBFEP and UBFEP in the R functions.
bool FindBoundsCpp9_1_1(unsigned short& len,NumericVector&v,double& x,double&ME,
                        std::vector<NumericVector::iterator>&LBI,double& sumLBI,
                        std::vector<NumericVector::iterator>&UBI,double& sumUBI, 
                        bool&equal,std::vector<std::vector<double>*>*&M){
NumericVector::iterator the, sup;
std::vector<double>::iterator Mi_begin, Mi_last, mid;
std::vector<std::vector<double>*>::iterator Mi;
unsigned short i, fb_len;
std::vector<NumericVector::iterator>::iterator LBi, UBi, tmp/*, fb_st*/;
double Max=x+ME, Min=x-ME, sumnew;
if(sumUBI<Min||sumLBI>Max)return 0;
else if(sumUBI==sumLBI){equal=1;return 1;}
double fb_sum_target;
bool boo=0;
while(1){   
i=0;
LBi=LBI.begin();
UBi=UBI.begin();

fb_len=0;
fb_sum_target=Min-sumUBI;
sumnew=0;

for(;LBi!=LBI.end();i++,LBi++,UBi++,++fb_len){

fb_sum_target=fb_sum_target+**UBi;
if(i>0)
{
    the=*(LBi-1)+1;
    if(*LBi<the)*LBi=the;
    else the=*LBi;
    sup=the-fb_len;
    tmp=UBi-fb_len;
}
else {sup=*LBi;tmp=UBi;}
Mi=M->begin()+fb_len;
while(1)
{
    Mi_last=(**Mi).begin()+(*tmp-v.begin());
    if(*Mi_last<fb_sum_target||*tmp+fb_len<*LBi)
    {
        if(Mi==M->begin()&& *Mi_last<fb_sum_target)return 0;
        fb_sum_target=fb_sum_target-**tmp;
        --Mi;
        --fb_len;
        ++tmp;
    }
    else break;
}
sup=*LBi-fb_len;

Mi_begin=(**Mi).begin()+(sup-v.begin());


if(*Mi_begin>=fb_sum_target)
{
    sumnew=sumnew+**LBi;
    continue;
}

while(1)
{
    mid=Mi_begin+int((Mi_last-Mi_begin)/2);
    if(*mid<fb_sum_target)
    {
        if(mid==Mi_begin)
        {
            *LBi=v.begin()+(Mi_last-(**Mi).begin())+fb_len;
            break;
        }
        Mi_begin=mid;
    }
    else if(*(mid-1)>=fb_sum_target)Mi_last=mid;
    else
    {
        *LBi=v.begin()+(mid-(**Mi).begin())+fb_len;
        break;
    }
}

sumnew=sumnew+**LBi;
}

if(!boo)boo=1;
else
{
    if(sumLBI==sumnew)
    {
        if(sumLBI==sumUBI)equal=1;
        break;
    }
}

sumLBI=sumnew;
fb_sum_target=Max-sumLBI;

i=len-1;
LBi=LBI.end()-1;
UBi=UBI.end()-1;

fb_len=0;
sumnew=0;

for(;UBi>=UBI.begin();--i,--LBi,--UBi,++fb_len){
fb_sum_target=fb_sum_target+**LBi;
if(i<len-1)
{
    the=*(UBi+1)-1;
    if(*UBi>the)*UBi=the;
    else the=*UBi;
    sup=the+fb_len;
    tmp=LBi+fb_len;
}
else {sup=*UBi;tmp=LBi;}
Mi=M->begin()+fb_len;
while(1)
{
    Mi_last=(**Mi).begin()+(*tmp-fb_len-v.begin());
    if(*Mi_last>fb_sum_target||*tmp-fb_len>*UBi)
    {
        if(Mi==M->begin()&& *Mi_last>fb_sum_target)return 0;
        fb_sum_target=fb_sum_target-**tmp;
        --Mi;
        --tmp;
        --fb_len;
    }
    else break;
}
sup=*UBi+fb_len;
Mi_begin=(**Mi).begin()+(sup-fb_len-v.begin());

if(*Mi_begin<=fb_sum_target)
{
    sumnew=sumnew+**UBi;
    continue;
}

while(1)
{
    mid=Mi_begin-int((Mi_begin-Mi_last)/2);
    if(*mid>fb_sum_target)
    {
        if(mid==Mi_begin)
        {
            *UBi=v.begin()+(Mi_last-(**Mi).begin());
            break;
        }
        Mi_begin=mid;
    }
    else if(*(mid+1)<=fb_sum_target)Mi_last=mid;
    else
    {
        *UBi=v.begin()+(mid-(**Mi).begin());
        break;
    }
}
sumnew=sumnew+**UBi;
}

if(sumnew==sumUBI)
{
    if(sumUBI==sumLBI)equal=1;
    break;
}
sumUBI=sumnew;
}
return 1;
}










//List FB2(unsigned short len, NumericVector v, double target, double ME){
//std::vector<NumericVector::iterator>LBI(len),UBI(len);
//std::vector<NumericVector::iterator>::iterator LBi=LBI.begin(), UBi=UBI.end()-1;
//NumericVector::iterator left=v.begin(), right=v.end()-1;
//for(;LBi!=LBI.end();LBi++,UBi--,left++,right--){*LBi=left;*UBi=right;}
//double sumLBI=itersum(LBI.begin(),LBI.end()), sumUBI=itersum(UBI.begin(),UBI.end());
//std::vector<std::vector<double>*>*M=theMatrix(v,len);
//bool equal=0;
//bool boo;
//boo=FindBoundsCpp9_1_1(len,v,target,ME,LBI,sumLBI,UBI,sumUBI,equal,M);
//std::vector<std::vector<double>*>::iterator Mi=M->begin();
//for(;Mi!=M->end();++Mi)delete *Mi;
//delete M;
//if(!boo)return List(0);
//IntegerVector LB(len), UB(len);
//for(unsigned short i=0;i<len;++i){LB[i]=LBI[i]-v.begin()+1;UB[i]=UBI[i]-v.begin()+1;}
//
//List result(2);
//result[0]=LB;
//result[1]=UB;
//
//return result;
//}




void TTT_direct3_1ALL_1(unsigned short&LEN,unsigned short len,NumericVector&v,double target,double&ME,
                std::vector<NumericVector::iterator>LBI, double sumLBI,
                std::vector<NumericVector::iterator>UBI, double sumUBI,
                std::vector<unsigned short*>&result,
                std::vector<std::vector<double>*>*&M){
bool equal=0,boo=FindBoundsCpp9_1_1(len,v,target,ME,LBI,sumLBI,UBI,sumUBI,equal,M);
if(!boo)return;
if(equal)
{
    unsigned short *x=new unsigned short[LEN], *xp=x;
    std::vector<NumericVector::iterator>::iterator i=UBI.begin();
    for(;i!=UBI.end();++i,++xp)*xp=*i-v.begin();
    result.push_back(x);
    return;
}
else if(len==1)
{
    unsigned short i=*LBI.begin()-v.begin(), end=*UBI.begin()-v.begin(), *x=NULL;
    for(;i<=end;i++)
    {
        x=new unsigned short[LEN];
        *x=i;
        result.push_back(x);
    }
    return;
}
std::vector<NumericVector::iterator>::iterator LBi=LBI.begin(), UBi=UBI.begin();
unsigned short Min=*UBi-*LBi, temp, position=0, i=1;
LBi++;UBi++;
for(;i<len;i++,LBi++,UBi++)
{
    temp=*UBi-*LBi;
    if(Min>temp){Min=temp;position=i;}
}
unsigned short resultsize;
if(position==0)
{
    NumericVector::iterator i=*LBI.begin(),end=*UBI.begin();
    sumLBI=sumLBI-**LBI.begin();
    LBI.erase(LBI.begin());
    sumUBI=sumUBI-**UBI.begin();
    UBI.erase(UBI.begin());
    NumericVector::iterator iter, tmp;
    std::vector<NumericVector::iterator>::iterator bound;
    for(;;)
    {
        resultsize=result.size();
        TTT_direct3_1ALL_1(LEN,len-1,v,target-*i,ME,LBI,sumLBI,UBI,sumUBI,result,M);
        if(resultsize!=result.size())
        {
            std::vector<unsigned short*>::iterator j=result.begin()+resultsize;
            for(;j!=result.end();j++)*(*j+len-1)=i-v.begin();
        }
        if(i==end)break;
        i++;
        iter=i+1;
        bound=LBI.begin();
        tmp=*bound;           
        if(iter>*bound)
        {
            for(;bound!=LBI.end();bound++,iter++)
            {
                if(iter>*bound)*bound=iter;
                else 
                { 
                    sumLBI=sumLBI-*tmp+*(iter-1); 
                    break;
                }
            }
        }
    }
}
else if(position==len-1)
{ 
    NumericVector::iterator i=*(UBI.end()-1),end=*(LBI.end()-1);
    sumLBI=sumLBI-**(LBI.end()-1);
    LBI.pop_back();
    sumUBI=sumUBI-**(UBI.end()-1);
    UBI.pop_back();
    NumericVector::iterator iter, tmp;
    std::vector<NumericVector::iterator>::iterator bound;
    for(;;)
    {
        resultsize=result.size();
        TTT_direct3_1ALL_1(LEN,len-1,v,target-*i,ME,LBI,sumLBI,UBI,sumUBI,result,M);
        if(resultsize!=result.size())
        {
            std::vector<unsigned short*>::iterator j=result.begin()+resultsize;
            for(;j!=result.end();j++)*(*j+len-1)=i-v.begin();
        }
        if(i==end)break;
        i--;
        iter=i-1;
        bound=UBI.end()-1;
        tmp=*bound;
        if(iter<*bound)
        {
            for(;bound>=UBI.begin();bound--,iter--)
            {
                if(iter<*bound)*bound=iter;
                else 
                { 
                    sumUBI=sumUBI-*tmp+*(iter+1);
                    break;
                }
            }
        }
    }
}
else
{  
    std::vector<NumericVector::iterator>::iterator 
    Lp=LBI.begin()+position, Up=UBI.begin()+position, breakpoint;
    NumericVector::iterator i=*Lp, end=*Up;
    sumLBI=sumLBI-**Lp;
    LBI.erase(Lp);
    sumUBI=sumUBI-**Up;
    UBI.erase(Up);
    std::vector<NumericVector::iterator>::iterator Lp_right=LBI.begin()+position, Up_left=UBI.begin()+position-1;
    std::vector<NumericVector::iterator>UBILeftReserve(position);
    std::vector<NumericVector::iterator>::iterator ULRi=UBILeftReserve.begin();
    for(UBi=UBI.begin();ULRi!=UBILeftReserve.end();ULRi++,UBi++)*ULRi=*UBi;
    NumericVector::iterator iter, tmp;
    std::vector<NumericVector::iterator>::iterator bound;
    double sumUBIright=sumUBI-itersum(UBI.begin(),Up_left+1);  
    for(;;)
    {
        resultsize=result.size();
        iter=i-1;
        ULRi=UBILeftReserve.end()-1;
        bound=Up_left;
        for(;bound>=UBI.begin();bound--,iter--,ULRi--)
        {
            if(iter<=*ULRi)*bound=iter;
            else break;
        }
        sumUBI=itersum(UBI.begin(),Up_left+1)+sumUBIright;
        TTT_direct3_1ALL_1(LEN,len-1,v,target-*i,ME,LBI,sumLBI,UBI,sumUBI,result,M);
        if(resultsize!=result.size())
        {
            std::vector<unsigned short*>::iterator j=result.begin()+resultsize;
            for(;j!=result.end();j++)*(*j+len-1)=i-v.begin();
        }
        if(i==end)break;
        i++;
        iter=i+1;
        bound=Lp_right;
        tmp=*bound;
        if(iter>*bound)
        {
            for(;bound!=LBI.end();bound++,iter++)
            {
                if(iter>*bound)*bound=iter;
                else 
                { 
                    sumLBI=sumLBI-*tmp+*(iter-1);
                    break; 
                }
            }
        }
    }
}
}





void TTT_direct3_1PART_1(unsigned short&LEN,unsigned short len,NumericVector&v,double target,double&ME,
                std::vector<NumericVector::iterator>LBI, double sumLBI,
                std::vector<NumericVector::iterator>UBI, double sumUBI,
                std::vector<unsigned short*>&result,unsigned short&sizeNeeded,
                std::vector<std::vector<double>*>*&M){
bool equal=0,boo=FindBoundsCpp9_1_1(len,v,target,ME,LBI,sumLBI,UBI,sumUBI,equal,M);
if(!boo)return;
if(equal)
{
    unsigned short *x=new unsigned short[LEN], *xp=x;
    std::vector<NumericVector::iterator>::iterator i=UBI.begin();
    for(;i!=UBI.end();++i,++xp)*xp=*i-v.begin();
    result.push_back(x);
    return;
}
else if(len==1)
{
    unsigned short i=*LBI.begin()-v.begin(), end=*UBI.begin()-v.begin(), *x=NULL;
    for(;i<=end;i++)
    {
        x=new unsigned short[LEN];
        *x=i;
        result.push_back(x);
    }
    return;
}
std::vector<NumericVector::iterator>::iterator LBi=LBI.begin(), UBi=UBI.begin();
unsigned short Min=*UBi-*LBi, temp, position=0, i=1;
LBi++;UBi++;
for(;i<len;i++,LBi++,UBi++)
{
    temp=*UBi-*LBi;
    if(Min>temp){Min=temp;position=i;}
}
unsigned short resultsize;
if(position==0)
{
    NumericVector::iterator i=*LBI.begin(),end=*UBI.begin();
    sumLBI=sumLBI-**LBI.begin();
    LBI.erase(LBI.begin());
    sumUBI=sumUBI-**UBI.begin();
    UBI.erase(UBI.begin());
    NumericVector::iterator iter, tmp;
    std::vector<NumericVector::iterator>::iterator bound;
    for(;;)
    {
        resultsize=result.size();
        TTT_direct3_1PART_1(LEN,len-1,v,target-*i,ME,LBI,sumLBI,UBI,sumUBI,result,sizeNeeded,M);
        if(resultsize!=result.size())
        {
            std::vector<unsigned short*>::iterator j=result.begin()+resultsize;
            for(;j!=result.end();j++)*(*j+len-1)=i-v.begin();
        }
        if(result.size()>=sizeNeeded)return;
        if(i==end)break;
        i++;
        iter=i+1;
        bound=LBI.begin();
        tmp=*bound;           
        if(iter>*bound)
        {
            for(;bound!=LBI.end();bound++,iter++)
            {
                if(iter>*bound)*bound=iter;
                else 
                { 
                    sumLBI=sumLBI-*tmp+*(iter-1); 
                    break;
                }
            }
        }
    }
}
else if(position==len-1)
{ 
    NumericVector::iterator i=*(UBI.end()-1),end=*(LBI.end()-1);
    sumLBI=sumLBI-**(LBI.end()-1);
    LBI.pop_back();
    sumUBI=sumUBI-**(UBI.end()-1);
    UBI.pop_back();
    NumericVector::iterator iter, tmp;
    std::vector<NumericVector::iterator>::iterator bound;
    for(;;)
    {
        resultsize=result.size();
        TTT_direct3_1PART_1(LEN,len-1,v,target-*i,ME,LBI,sumLBI,UBI,sumUBI,result,sizeNeeded,M);
        if(resultsize!=result.size())
        {
            std::vector<unsigned short*>::iterator j=result.begin()+resultsize;
            for(;j!=result.end();j++)*(*j+len-1)=i-v.begin();
        }
        if(result.size()>=sizeNeeded)return;
        if(i==end)break;
        i--;
        iter=i-1;
        bound=UBI.end()-1;
        tmp=*bound;
        if(iter<*bound)
        {
            for(;bound>=UBI.begin();bound--,iter--)
            {
                if(iter<*bound)*bound=iter;
                else 
                { 
                    sumUBI=sumUBI-*tmp+*(iter+1);
                    break;
                }
            }
        }
    }
}
else
{  
    std::vector<NumericVector::iterator>::iterator 
    Lp=LBI.begin()+position, Up=UBI.begin()+position, breakpoint;
    NumericVector::iterator i=*Lp, end=*Up;
    sumLBI=sumLBI-**Lp;
    LBI.erase(Lp);
    sumUBI=sumUBI-**Up;
    UBI.erase(Up);
    std::vector<NumericVector::iterator>::iterator Lp_right=LBI.begin()+position, Up_left=UBI.begin()+position-1;
    std::vector<NumericVector::iterator>UBILeftReserve(position);
    std::vector<NumericVector::iterator>::iterator ULRi=UBILeftReserve.begin();
    for(UBi=UBI.begin();ULRi!=UBILeftReserve.end();ULRi++,UBi++)*ULRi=*UBi;
    NumericVector::iterator iter, tmp;
    std::vector<NumericVector::iterator>::iterator bound;
    double sumUBIright=sumUBI-itersum(UBI.begin(),Up_left+1);  
    for(;;)
    {
        resultsize=result.size();
        iter=i-1;
        ULRi=UBILeftReserve.end()-1;
        bound=Up_left;
        for(;bound>=UBI.begin();bound--,iter--,ULRi--)
        {
            if(iter<=*ULRi)*bound=iter;
            else break;
        }
        sumUBI=itersum(UBI.begin(),Up_left+1)+sumUBIright;
        TTT_direct3_1PART_1(LEN,len-1,v,target-*i,ME,LBI,sumLBI,UBI,sumUBI,result,sizeNeeded,M);
        if(resultsize!=result.size())
        {
            std::vector<unsigned short*>::iterator j=result.begin()+resultsize;
            for(;j!=result.end();j++)*(*j+len-1)=i-v.begin();
        }
        if(result.size()>=sizeNeeded)return;
        if(i==end)break;
        i++;
        iter=i+1;
        bound=Lp_right;
        tmp=*bound;
        if(iter>*bound)
        {
            for(;bound!=LBI.end();bound++,iter++)
            {
                if(iter>*bound)*bound=iter;
                else 
                { 
                    sumLBI=sumLBI-*tmp+*(iter-1);
                    break; 
                }
            }
        }
    }
}
}



List FLSSS_ALL_1(unsigned short len, NumericVector v, double target, double ME){
std::vector<unsigned short*>result;
std::vector<NumericVector::iterator>LBI(len),UBI(len);
std::vector<NumericVector::iterator>::iterator LBi=LBI.begin(), UBi=UBI.end()-1;
NumericVector::iterator left=v.begin(), right=v.end()-1;
for(;LBi!=LBI.end();LBi++,UBi--,left++,right--){*LBi=left;*UBi=right;}
double sumLBI=itersum(LBI.begin(),LBI.end()), sumUBI=itersum(UBI.begin(),UBI.end());
std::vector<std::vector<double>*>*M=theMatrix(v,len);
TTT_direct3_1ALL_1(len,len,v,target,ME,LBI,sumLBI,UBI,sumUBI,result,M);
std::vector<std::vector<double>*>::iterator Mi=M->begin();
for(;Mi!=M->end();++Mi)delete *Mi;
delete M;
if(result.size()==0)return List(0);
List lis(result.size());
IntegerVector IntVec(len);
for(unsigned int i=0;i<result.size();i++)
{
    for(int j=0;j<len;j++)IntVec[j]=result[i][j]+1;
    delete[] result[i];
    lis[i]=clone(IntVec);
}
return lis;
}


List FLSSS_PART_1(unsigned short len, NumericVector v, double target, double ME, unsigned short sizeNeeded=1){
std::vector<unsigned short*>result;
std::vector<NumericVector::iterator>LBI(len),UBI(len);
std::vector<NumericVector::iterator>::iterator LBi=LBI.begin(), UBi=UBI.end()-1;
NumericVector::iterator left=v.begin(), right=v.end()-1;
for(;LBi!=LBI.end();LBi++,UBi--,left++,right--){*LBi=left;*UBi=right;}
double sumLBI=itersum(LBI.begin(),LBI.end()), sumUBI=itersum(UBI.begin(),UBI.end());
std::vector<std::vector<double>*>*M=theMatrix(v,len);
TTT_direct3_1PART_1(len,len,v,target,ME,LBI,sumLBI,UBI,sumUBI,result,sizeNeeded,M);
std::vector<std::vector<double>*>::iterator Mi=M->begin();
for(;Mi!=M->end();++Mi)delete *Mi;
delete M;
if(result.size()==0)return List(0);
List lis(result.size());
IntegerVector IntVec(len);
for(unsigned int i=0;i<result.size();i++)
{
    for(int j=0;j<len;j++)IntVec[j]=result[i][j]+1;
    delete[] result[i];
    lis[i]=clone(IntVec);
}
return lis;
}


// FLSSS_ALL_1
//List FLSSS_ALL_1(unsigned short len, NumericVector v, double target, double ME);
RcppExport SEXP FLSSS_FLSSS_ALL_1(SEXP lenSEXP, SEXP vSEXP, SEXP targetSEXP, SEXP MESEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< unsigned short >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type target(targetSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        List __result = FLSSS_ALL_1(len, v, target, ME);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// FLSSS_PART_1
//List FLSSS_PART_1(unsigned short len, NumericVector v, double target, double ME, unsigned short sizeNeeded = 1);
RcppExport SEXP FLSSS_FLSSS_PART_1(SEXP lenSEXP, SEXP vSEXP, SEXP targetSEXP, SEXP MESEXP, SEXP sizeNeededSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< unsigned short >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type target(targetSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        Rcpp::traits::input_parameter< unsigned short >::type sizeNeeded(sizeNeededSEXP );
        List __result = FLSSS_PART_1(len, v, target, ME, sizeNeeded);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
