#include <Rcpp.h>
using namespace Rcpp;





//intension: sums the actual values pointed by UBI or LBI
double itersum(std::vector<NumericVector::iterator>::iterator start,std::vector<NumericVector::iterator>::iterator end){
double S=0;
for(std::vector<NumericVector::iterator>::iterator i=start;i!=end;i++)S=S+**i;
return S;
}






//intension: find infima and suprema for every position. 
//LBI and UBI are equivalent to LBFEP and UBFEP in the R functions.
bool FindBoundsCpp9_1(unsigned short& len,NumericVector&v,double& x,double&ME,
                        std::vector<NumericVector::iterator>&LBI,
                        std::vector<NumericVector::iterator>&UBI,bool&equal){
NumericVector::iterator the, seqend;
int i;
std::vector<NumericVector::iterator>::iterator LBi, UBi, bp;

double Max=x+ME, Min=x-ME, sumUBI=itersum(UBI.begin(),UBI.end()), sumLBI=itersum(LBI.begin(),LBI.end()), temp;
if(sumUBI<Min||sumLBI>Max)return 0;
else if(sumUBI==sumLBI){equal=1;return 1;}

double Min_right=Min-sumUBI, Max_left, S;
bool boo=0;

while(1){
i=0;
LBi=LBI.begin();
UBi=UBI.begin();
S=0;//S in fact is the sum of the "left vector"
bp=UBI.begin();
seqend=*LBI.begin();
//bp stands for the breakpoint, the first one in sequence that is strictly less than its suprema or the current node
for(;LBi!=LBI.end();i++,LBi++,UBi++){
Min_right=Min_right+**UBi;
if(i>0)
{
    the=*(LBi-1)+1;
    if(seqend>=*bp){seqend++;bp++;}
    if(*LBi<the)*LBi=the;
    else
    {
        for(;the<*LBi;the++)
        {
            S=S+*the-*seqend;
            seqend++;
            while(seqend>=*bp&&bp!=UBi){seqend++;bp++;}
        }
    }
}
else the=*LBi;
if(the>*UBi)return 0;
S=S+*the;
while(S<Min_right)
{
    if(the==*UBi)return 0;
    the++;
    S=S+*the-*seqend;
    seqend++;
    while(seqend>=*bp&&bp!=UBi){bp++;seqend++;}
}
if(the!=*LBi)*LBi=the;
}

temp=itersum(LBI.begin(),LBI.end());
if(!boo)boo=1;
else
{
    if(sumLBI==temp)break;
    sumLBI=temp;
}
i=len-1;
LBi=LBI.end()-1;
UBi=UBI.end()-1;
Max_left=Max-temp;
S=0;
bp=LBI.end()-1;
seqend=*(UBI.end()-1);
for(;i>-1;i--,LBi--,UBi--){
Max_left=Max_left+**LBi;
if(i<len-1)
{
    the=*(UBi+1)-1;
    if(seqend<=*bp){seqend--;bp--;}
    if(*UBi>the)*UBi=the;
    else
    {
        for(;the>*UBi;the--)
        {
            S=S+*the-*seqend; 
            seqend--;
            while(seqend<=*bp&&bp!=LBi){seqend--;bp--;}
        }
    }
}
else the=*UBi;
if(the<*LBi)return 0;
S=S+*the;
while(S>Max_left)
{
    if(the==*LBi)return 0;
    the--;
    S=S+*the-*seqend;
    seqend--;
    while(seqend<=*bp&&bp!=LBi){seqend--;bp--;}
    //Above is to locate the new break point. Important and Tricky!
}
if(the!=*UBi)*UBi=the;
}
temp=itersum(UBI.begin(),UBI.end());
if(temp==sumUBI)break;
sumUBI=temp;
Min_right=Min-temp;
}
return 1;
}





void TTT_direct3_1ALL(unsigned short&LEN,unsigned short len,NumericVector&v,double target,double&ME,
                std::vector<NumericVector::iterator>LBI,
                std::vector<NumericVector::iterator>UBI,
                std::vector<unsigned short*>&result){
bool equal=0,boo=FindBoundsCpp9_1(len,v,target,ME,LBI,UBI,equal);
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
    LBI.erase(LBI.begin());
    UBI.erase(UBI.begin());
    NumericVector::iterator iter;
    std::vector<NumericVector::iterator>::iterator bound;
    for(;;)
    {
        resultsize=result.size();
        TTT_direct3_1ALL(LEN,len-1,v,target-*i,ME,LBI,UBI,result);
        if(resultsize!=result.size())
        {
            std::vector<unsigned short*>::iterator j=result.begin()+resultsize;
            for(;j!=result.end();j++)*(*j+len-1)=i-v.begin();
        }
        if(i==end)break;
        i++;
        iter=i+1;
        bound=LBI.begin();
        for(;bound!=LBI.end();bound++,iter++){if(iter>*bound)*bound=iter;else break;}
    }
}
else if(position==len-1)
{
    NumericVector::iterator i=*(UBI.end()-1),end=*(LBI.end()-1);
    LBI.pop_back();
    UBI.pop_back();
    NumericVector::iterator iter;
    std::vector<NumericVector::iterator>::iterator bound;
    for(;;)
    {
        resultsize=result.size();
        TTT_direct3_1ALL(LEN,len-1,v,target-*i,ME,LBI,UBI,result);
        if(resultsize!=result.size())
        {
            std::vector<unsigned short*>::iterator j=result.begin()+resultsize;
            for(;j!=result.end();j++)*(*j+len-1)=i-v.begin();
        }
        if(i==end)break;
        i--;
        iter=i-1;
        bound=UBI.end()-1;
        for(;bound>=UBI.begin();bound--,iter--){if(iter<*bound)*bound=iter;else break;}
    }
}
else
{
    std::vector<NumericVector::iterator>::iterator 
    Lp=LBI.begin()+position, Up=UBI.begin()+position, breakpoint;
    NumericVector::iterator i=*Lp, end=*Up;
    LBI.erase(Lp);
    UBI.erase(Up);
    std::vector<NumericVector::iterator>::iterator Lp_right=LBI.begin()+position, Up_left=UBI.begin()+position-1;
    std::vector<NumericVector::iterator>UBILeftReserve(position);
    std::vector<NumericVector::iterator>::iterator ULRi=UBILeftReserve.begin();
    for(UBi=UBI.begin();ULRi!=UBILeftReserve.end();ULRi++,UBi++)*ULRi=*UBi;
    NumericVector::iterator iter;
    std::vector<NumericVector::iterator>::iterator bound;
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
        TTT_direct3_1ALL(LEN,len-1,v,target-*i,ME,LBI,UBI,result);
        if(resultsize!=result.size())
        {
            std::vector<unsigned short*>::iterator j=result.begin()+resultsize;
            for(;j!=result.end();j++)*(*j+len-1)=i-v.begin();
        }
        if(i==end)break;
        i++;
        iter=i+1;
        bound=Lp_right;
        for(;bound!=LBI.end();bound++,iter++){if(iter>*bound)*bound=iter;else break;}
    }
}
}





void TTT_direct3_1PART(unsigned short&LEN,unsigned short len,NumericVector&v,double target,double&ME,
                std::vector<NumericVector::iterator>LBI,
                std::vector<NumericVector::iterator>UBI,
                std::vector<unsigned short*>&result,unsigned short&sizeNeeded){
bool equal=0,boo=FindBoundsCpp9_1(len,v,target,ME,LBI,UBI,equal);
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
    LBI.erase(LBI.begin());
    UBI.erase(UBI.begin());
    NumericVector::iterator iter;
    std::vector<NumericVector::iterator>::iterator bound;
    for(;;)
    {
        resultsize=result.size();
        TTT_direct3_1PART(LEN,len-1,v,target-*i,ME,LBI,UBI,result,sizeNeeded);
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
        for(;bound!=LBI.end();bound++,iter++){if(iter>*bound)*bound=iter;else break;}
    }
}
else if(position==len-1)
{
    NumericVector::iterator i=*(UBI.end()-1),end=*(LBI.end()-1);
    LBI.pop_back();
    UBI.pop_back();
    NumericVector::iterator iter;
    std::vector<NumericVector::iterator>::iterator bound;
    for(;;)
    {
        resultsize=result.size();
        TTT_direct3_1PART(LEN,len-1,v,target-*i,ME,LBI,UBI,result,sizeNeeded);
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
        for(;bound>=UBI.begin();bound--,iter--){if(iter<*bound)*bound=iter;else break;}
    }
}
else
{
    std::vector<NumericVector::iterator>::iterator 
    Lp=LBI.begin()+position, Up=UBI.begin()+position, breakpoint;
    NumericVector::iterator i=*Lp, end=*Up;
    LBI.erase(Lp);
    UBI.erase(Up);
    std::vector<NumericVector::iterator>::iterator Lp_right=LBI.begin()+position, Up_left=UBI.begin()+position-1;
    std::vector<NumericVector::iterator>UBILeftReserve(position);
    std::vector<NumericVector::iterator>::iterator ULRi=UBILeftReserve.begin();
    for(UBi=UBI.begin();ULRi!=UBILeftReserve.end();ULRi++,UBi++)*ULRi=*UBi;
    NumericVector::iterator iter;
    std::vector<NumericVector::iterator>::iterator bound;
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
        TTT_direct3_1PART(LEN,len-1,v,target-*i,ME,LBI,UBI,result,sizeNeeded);
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
        for(;bound!=LBI.end();bound++,iter++){if(iter>*bound)*bound=iter;else break;}
    }
}
}





List FLSSS_ALL(unsigned short len, NumericVector v, double target, double ME){
std::vector<unsigned short*>result;
std::vector<NumericVector::iterator>LBI(len),UBI(len);
std::vector<NumericVector::iterator>::iterator LBi=LBI.begin(), UBi=UBI.end()-1;
NumericVector::iterator left=v.begin(), right=v.end()-1;
for(;LBi!=LBI.end();LBi++,UBi--,left++,right--){*LBi=left;*UBi=right;}
TTT_direct3_1ALL(len,len,v,target,ME,LBI,UBI,result);
if(result.size()==0)return List(0);
List lis(result.size());
IntegerVector IntVec(len);
for(unsigned int i=0;i<result.size();i++)
{
    for(int j=0;j<len;j++)IntVec[j]=result[i][j]+1;
    lis[i]=clone(IntVec);
}
return lis;
}





List FLSSS_PART(unsigned short len, NumericVector v, double target, double ME, unsigned short sizeNeeded=1){
std::vector<unsigned short*>result;
std::vector<NumericVector::iterator>LBI(len),UBI(len);
std::vector<NumericVector::iterator>::iterator LBi=LBI.begin(), UBi=UBI.end()-1;
NumericVector::iterator left=v.begin(), right=v.end()-1;
for(;LBi!=LBI.end();LBi++,UBi--,left++,right--){*LBi=left;*UBi=right;}
TTT_direct3_1PART(len,len,v,target,ME,LBI,UBI,result,sizeNeeded);
if(result.size()==0)return List(0);
List lis(result.size());
IntegerVector IntVec(len);
for(unsigned int i=0;i<result.size();i++)
{
    for(int j=0;j<len;j++)IntVec[j]=result[i][j]+1;
    lis[i]=clone(IntVec);
}
return lis;
}





/*** R
FLSSS<-function(len,v,target,ME,sizeNeeded=1){
if(is.numeric(sizeNeeded))return(FLSSS_PART(len,v,target,ME,sizeNeeded))
else return(FLSSS_ALL(len,v,target,ME))
}
*/





//Wrap them as R objects
// FLSSS_ALL
//List FLSSS_ALL(unsigned short len, NumericVector v, double target, double ME);
RcppExport SEXP FLSSS_FLSSS_ALL(SEXP lenSEXP, SEXP vSEXP, SEXP targetSEXP, SEXP MESEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< unsigned short >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type target(targetSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        List __result = FLSSS_ALL(len, v, target, ME);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// FLSSS_PART
//List FLSSS_PART(unsigned short len, NumericVector v, double target, double ME, unsigned short sizeNeeded = 1);
RcppExport SEXP FLSSS_FLSSS_PART(SEXP lenSEXP, SEXP vSEXP, SEXP targetSEXP, SEXP MESEXP, SEXP sizeNeededSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< unsigned short >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type target(targetSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        Rcpp::traits::input_parameter< unsigned short >::type sizeNeeded(sizeNeededSEXP );
        List __result = FLSSS_PART(len, v, target, ME, sizeNeeded);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
