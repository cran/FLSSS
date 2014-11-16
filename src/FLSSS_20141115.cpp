#include <Rcpp.h>
#include <ctime>
using namespace Rcpp;

double itersum(std::vector<NumericVector::iterator>::iterator start,
std::vector<NumericVector::iterator>::iterator end){
double S=0;
for(std::vector<NumericVector::iterator>::iterator i=start;i!=end;i++)S=S+**i;
return S;
}

void theMatrix(NumericVector&v,unsigned&len,std::vector<std::vector<double> >&M){
std::vector<std::vector<double> >::iterator Mi=M.begin(), Miend;
std::vector<double>::iterator tmpi, tmpiend;
NumericVector::iterator vi=v.begin(), vii;
unsigned i=v.size();
Mi->resize(i);
for(tmpi=Mi->begin();vi!=v.end();++tmpi,++vi)*tmpi=*vi;
++Mi;
--i;
std::vector<double>::iterator prior;
vi=v.begin()+1;
Miend=M.end();
for(;Mi!=Miend;++Mi,--i,++vi)
{
  vii=vi;
  Mi->resize(i);
  tmpi=Mi->begin();
  prior=(Mi-1)->begin();
  tmpiend=Mi->end();
  for(;tmpi!=tmpiend;++tmpi,++prior,++vii)*tmpi=*prior+*vii;
}
}

void to_num(std::vector<NumericVector::iterator>&BI,
std::vector<std::vector<unsigned> >::iterator x,NumericVector::iterator vbegin)
{
    x->resize(BI.size());
    std::vector<unsigned>::iterator xi=x->begin(),end=x->end();
    std::vector<NumericVector::iterator>::iterator j=BI.begin();
    for(;xi!=end;++xi,++j)*xi=*j-vbegin;
}

void to_iter(std::vector<NumericVector::iterator>&BI,
std::vector<std::vector<unsigned> >::iterator&x,NumericVector::iterator vbegin)
{
    BI.resize(x->size());
    std::vector<unsigned>::iterator xi=x->begin(),end=x->end();
    std::vector<NumericVector::iterator>::iterator j=BI.begin();
    for(;xi!=end;++xi,++j)*j=*xi+vbegin;
}

unsigned FindBoundsCpp9_1_1(unsigned&len,NumericVector&v,double& x,double&ME,
                        std::vector<NumericVector::iterator>&LBI,double& sumLBI,
                        std::vector<NumericVector::iterator>&UBI,double& sumUBI, 
                        std::vector<std::vector<double> >&M){
NumericVector::iterator the, sup;
std::vector<double>::iterator Mi_begin, Mi_last, mid;
std::vector<std::vector<double> >::iterator Mi;
unsigned i, fb_len;
std::vector<NumericVector::iterator>::iterator LBi, UBi, tmp;
double Max=x+ME, Min=x-ME, sumnew;
if(sumUBI<Min||sumLBI>Max)return 0;
if(sumUBI==sumLBI)return 2;
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
Mi=M.begin()+fb_len;
while(1)
{
    Mi_last=Mi->begin()+(*tmp-v.begin());
    if(*Mi_last<fb_sum_target||*tmp+fb_len<*LBi)
    {
        if(Mi==M.begin()&& *Mi_last<fb_sum_target)return 0;
        fb_sum_target=fb_sum_target-**tmp;
        --Mi;
        --fb_len;
        ++tmp;
    }
    else break;
}
sup=*LBi-fb_len;

Mi_begin=Mi->begin()+(sup-v.begin());


if(*Mi_begin>=fb_sum_target)
{
    sumnew=sumnew+**LBi;
    continue;
}

while(1)
{
    mid=Mi_begin+unsigned((Mi_last-Mi_begin)/2);
    if(*mid<fb_sum_target)
    {
        if(mid==Mi_begin)
        {
            *LBi=v.begin()+(Mi_last-Mi->begin())+fb_len;
            break;
        }
        Mi_begin=mid;
    }
    else if(*(mid-1)>=fb_sum_target)Mi_last=mid;
    else
    {
        *LBi=v.begin()+(mid-Mi->begin())+fb_len;
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
        if(sumLBI==sumUBI)return 2;
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
Mi=M.begin()+fb_len;
while(1)
{
    Mi_last=Mi->begin()+(*tmp-fb_len-v.begin());
    if(*Mi_last>fb_sum_target||*tmp-fb_len>*UBi)
    {
        if(Mi==M.begin()&& *Mi_last>fb_sum_target)return 0;
        fb_sum_target=fb_sum_target-**tmp;
        --Mi;
        --tmp;
        --fb_len;
    }
    else break;
}
sup=*UBi+fb_len;
Mi_begin=Mi->begin()+(sup-fb_len-v.begin());

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
            *UBi=v.begin()+(Mi_last-Mi->begin());
            break;
        }
        Mi_begin=mid;
    }
    else if(*(mid+1)<=fb_sum_target)Mi_last=mid;
    else
    {
        *UBi=v.begin()+(mid-Mi->begin());
        break;
    }
}
sumnew=sumnew+**UBi;
}

if(sumnew==sumUBI)
{
    if(sumUBI==sumLBI)return 2;
    break;
}
sumUBI=sumnew;
}
return 1;
}

struct PAT{
unsigned position;
NumericVector::iterator s,send;
double target,sumLBI,sumUBI;
std::vector<NumericVector::iterator>LBI,UBI,UBILeftReserve;
};

unsigned update(std::vector<PAT>::iterator P){
if(P->s==P->send)return 0;
P->target=P->target+*(P->s);
if(P->position==0)
{
    ++P->s;
    P->target=P->target-*(P->s);
    std::vector<NumericVector::iterator>::iterator bound=P->LBI.begin();
    NumericVector::iterator iter=P->s+1, tmp=*bound;
    for(;bound!=P->LBI.end();bound++,iter++)
    {
        if(iter>*bound)*bound=iter;
        else 
        { 
            P->sumLBI=P->sumLBI-*tmp+*(iter-1); 
            break;
        }
    }
}
else if(P->position==P->UBI.size())
{
    --P->s;
    P->target=P->target-*(P->s);
    std::vector<NumericVector::iterator>::iterator bound=P->UBI.end()-1;
    NumericVector::iterator iter=P->s-1, tmp=*bound;
    for(;bound>=P->UBI.begin();bound--,iter--)
    {
        if(iter<*bound)*bound=iter;
        else 
        { 
            P->sumUBI=P->sumUBI-*tmp+*(iter+1);
            break;
        }
    }
}
else
{
    ++P->s;
    P->target=P->target-*(P->s);
    std::vector<NumericVector::iterator>::iterator Lp_right=P->LBI.begin()+P->position, Up_left=P->UBI.begin()+P->position-1;
    std::vector<NumericVector::iterator>::iterator ULRi=P->UBILeftReserve.end()-1;
    NumericVector::iterator iter=P->s-1, tmp;
    std::vector<NumericVector::iterator>::iterator bound=Up_left;
    double sumUBIright=P->sumUBI-itersum(P->UBI.begin(),Up_left+1);
    for(;bound>=P->UBI.begin();bound--,iter--,ULRi--)
    {
        if(iter<=*ULRi)*bound=iter;
        else break;
    }
    P->sumUBI=itersum(P->UBI.begin(),Up_left+1)+sumUBIright;
    iter=P->s+1;
    bound=Lp_right;
    tmp=*bound;
    for(;bound!=P->LBI.end();bound++,iter++)
    {
        if(iter>*bound)*bound=iter;
        else 
        { 
            P->sumLBI=P->sumLBI-*tmp+*(iter-1);
            break; 
        }
    }
}
return 1;
}

unsigned giveBirth(std::vector<PAT>::iterator&child,NumericVector&v,
double&ME,std::vector<std::vector<double> >&M){
unsigned len=child->UBI.size();
unsigned boo=FindBoundsCpp9_1_1(len,v,child->target,ME,child->LBI,child->sumLBI,child->UBI,child->sumUBI,M);
if(boo==0)return 0;
if(len==1)return 3;
if(boo==2)return 2;
std::vector<NumericVector::iterator>::iterator LBi=child->LBI.begin(), UBi=child->UBI.begin();
unsigned Min=*UBi-*LBi, temp=Min, i=1; 
child->position=0;
LBi++;
UBi++;
for(;i<len;i++,LBi++,UBi++)
{
    temp=*UBi-*LBi;
    if(Min>temp){Min=temp;child->position=i;}
}
if(child->position==0)
{
    child->s=*child->LBI.begin();
    child->send=*child->UBI.begin();
    child->sumLBI=child->sumLBI-*child->s;
    child->target=child->target-*child->s;
    child->sumUBI=child->sumUBI-*child->send;
    child->LBI.erase(child->LBI.begin());
    child->UBI.erase(child->UBI.begin());
}
else if(child->position==len-1)
{
    child->s=*(child->UBI.end()-1);
    child->send=*(child->LBI.end()-1);
    child->sumUBI=child->sumUBI-*child->s;
    child->sumLBI=child->sumLBI-*child->send;
    child->target=child->target-*child->s;
    child->LBI.pop_back();
    child->UBI.pop_back();
}
else
{
    std::vector<NumericVector::iterator>::iterator 
    Lp=child->LBI.begin()+child->position, Up=child->UBI.begin()+child->position;
    child->s=*Lp;
    child->target=child->target-*(child->s);
    child->send=*Up;
    child->sumLBI=child->sumLBI-**Lp;
    child->sumUBI=child->sumUBI-**Up;
    child->LBI.erase(Lp);
    child->UBI.erase(Up);
    child->UBILeftReserve.assign(child->UBI.begin(), child->UBI.begin()+child->position);
    std::vector<NumericVector::iterator>::iterator Up_left=child->UBI.begin()+child->position-1,
    ULRi=child->UBILeftReserve.end()-1;
    NumericVector::iterator iter=child->s-1;
    std::vector<NumericVector::iterator>::iterator bound=Up_left;
    double sumUBIright=child->sumUBI-itersum(child->UBILeftReserve.begin(),child->UBILeftReserve.end());
    for(;bound>=child->UBI.begin();bound--,iter--,ULRi--)
    {
        if(iter<=*ULRi)*bound=iter;
        else break;
    }
    child->sumUBI=itersum(child->UBI.begin(),Up_left+1)+sumUBIright;    
}
return 1;
};

void TTT_stack_1_1(unsigned&LEN,NumericVector&v,double&ME,
                std::vector<std::vector<unsigned> >&result, unsigned&sizeNeeded,
                std::vector<std::vector<double> >&M, std::vector<PAT>&SK, 
                std::vector<PAT>::iterator&SK_end, double&Duration){
if(LEN==1)
{
    double Max=SK.front().target+ME, Min=Max-2*ME;
    std::vector<unsigned>x(1);
    for(NumericVector::iterator i=v.begin();i!=v.end();++i)
    {
        if(*i>=Min&&*i<=Max)
        {
            x.front()=i-v.begin()+1;
            result.push_back(x);
        }
        else if(*i>Max)break;
    }
    return;
}
if(SK.front().UBI.size()==0&&SK_end-SK.begin()<2)return;
unsigned boo;
std::clock_t timeend=std::clock()+Duration*(double)CLOCKS_PER_SEC;
std::vector<unsigned>common(LEN);
std::vector<unsigned>::iterator xi;
while(1){
*SK_end=*(SK_end-1);
boo=giveBirth(SK_end,v,ME,M);
if(boo==1)
{
    ++SK_end;
    continue;
}
if(boo==3)
{
    xi=common.begin();
    unsigned i=SK_end->LBI.back()-v.begin()+1, end=SK_end->UBI.back()-v.begin()+1, tmp;
    if(i>end){tmp=i;i=end;end=tmp;}
    for(std::vector<PAT>::iterator SKi=SK.begin()+1;SKi!=SK_end;++SKi,++xi)*xi=SKi->s-v.begin()+1;
    for(;i<=end;i++)
    {
        common.back()=i;
        result.push_back(common);
    }
}
else if(boo==2)
{
    xi=common.begin();
    for(std::vector<PAT>::iterator SKi=SK.begin()+1;SKi!=SK_end;++SKi,++xi)*xi=SKi->s-v.begin()+1;
    std::vector<NumericVector::iterator>::iterator i=SK_end->UBI.begin();
    for(;i!=SK_end->UBI.end();++i,++xi)*xi=*i-v.begin()+1;
    result.push_back(common);
}
while(update(SK_end-1)==0)
{
    --SK_end;
    if(SK_end-SK.begin()<2)return;
}
if((sizeNeeded!=0&&result.size()>=sizeNeeded)||(Duration!=-1&&std::clock()>timeend))break;
}
}

List FLSSS_SK_throw(unsigned len, NumericVector v, double target, double ME, 
IntegerVector LB, IntegerVector UB, unsigned sizeNeeded, double Duration){
std::vector<std::vector<unsigned> >result;
if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
else result.reserve(1024);
std::vector<PAT>SK(len+6);
NumericVector::iterator vbegin=v.begin();
PAT&SKbegin=SK.front();
SKbegin.LBI.resize(len);
SKbegin.UBI.resize(len);
for(unsigned i=0;i<len;++i)
{
    SKbegin.LBI[i]=vbegin+LB[i]-1;
    SKbegin.UBI[i]=vbegin+UB[i]-1;
}
SKbegin.sumLBI=itersum(SKbegin.LBI.begin(),SKbegin.LBI.end());
SKbegin.sumUBI=itersum(SKbegin.UBI.begin(),SKbegin.UBI.end());
SKbegin.target=target;
std::vector<std::vector<double> >M(len);
theMatrix(v,len,M);
std::vector<PAT>::iterator SK_end=SK.begin()+1;
TTT_stack_1_1(len,v,ME,result,sizeNeeded,M,SK,SK_end,Duration);
if(result.size()==0)return List(0);
List lis(result.size());
for(unsigned i=0;i<result.size();i++)lis[i]=result[i];
std::vector<PAT>::iterator pati=SK.begin()+1;
std::vector<unsigned>I(3*(SK_end-SK.begin()-1));
std::vector<double>J(I.size());
std::vector<std::vector<unsigned> >K(I.size());
std::vector<unsigned>::iterator Ii=I.begin();
std::vector<double>::iterator Ji=J.begin();
std::vector<std::vector<unsigned> >::iterator Ki=K.begin();
for(;pati!=SK_end;++pati)
{
    *Ii=pati->position;++Ii;
    *Ii=pati->s-vbegin;++Ii;
    *Ii=pati->send-vbegin;++Ii;
    *Ji=pati->target;++Ji;
    *Ji=pati->sumLBI;++Ji;
    *Ji=pati->sumUBI;++Ji;
    to_num(pati->LBI,Ki,vbegin);++Ki;
    to_num(pati->UBI,Ki,vbegin);++Ki;
    to_num(pati->UBILeftReserve,Ki,vbegin);++Ki;
}
return List::create(Named("roots",lis),Named("node",List::create(I,J,K,M)));
}

List FLSSS_SK_catch(unsigned len, NumericVector v, double ME,
IntegerVector I, NumericVector J, List LK, List LM,unsigned sizeNeeded,double Duration){
std::vector<std::vector<unsigned> >K=as<std::vector<std::vector<unsigned> > >(LK);
std::vector<std::vector<double> >M=as<std::vector<std::vector<double> > >(LM);    
NumericVector::iterator vbegin=v.begin();
std::vector<PAT>SK(len+6);
std::vector<PAT>::iterator SK_end=SK.begin()+I.size()/3+1, pati=SK.begin()+1;
IntegerVector::iterator Ii=I.begin();
NumericVector::iterator Ji=J.begin();
std::vector<std::vector<unsigned> >::iterator Ki=K.begin();
for(;pati!=SK_end;++pati)
{
    pati->position=*Ii;++Ii;
    pati->s=vbegin+*Ii;++Ii;
    pati->send=vbegin+*Ii;++Ii;
    pati->target=*Ji;++Ji;
    pati->sumLBI=*Ji;++Ji;
    pati->sumUBI=*Ji;++Ji;
    to_iter(pati->LBI,Ki,vbegin);++Ki;
    to_iter(pati->UBI,Ki,vbegin);++Ki;
    to_iter(pati->UBILeftReserve,Ki,vbegin);++Ki;
}
std::vector<std::vector<unsigned> >result;
if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
else result.reserve(1024);
TTT_stack_1_1(len,v,ME,result,sizeNeeded,M,SK,SK_end,Duration);
if(result.size()==0)return List(0);
List lis(result.size());
for(unsigned i=0;i<result.size();i++)lis[i]=result[i];
return lis;
}

List FLSSS_SK_catch_throw(unsigned len, NumericVector v, double ME,
IntegerVector I, NumericVector J, List LK, List LM,unsigned sizeNeeded,double Duration){
std::vector<std::vector<unsigned> >K=as<std::vector<std::vector<unsigned> > >(LK);
std::vector<std::vector<double> >M=as<std::vector<std::vector<double> > >(LM);    
NumericVector::iterator vbegin=v.begin();
std::vector<PAT>SK(len+6);
std::vector<PAT>::iterator SK_end=SK.begin()+I.size()/3+1, pati=SK.begin()+1;
IntegerVector::iterator Ii=I.begin();
NumericVector::iterator Ji=J.begin();
std::vector<std::vector<unsigned> >::iterator Ki=K.begin();
for(;pati!=SK_end;++pati)
{
    pati->position=*Ii;++Ii;
    pati->s=vbegin+*Ii;++Ii;
    pati->send=vbegin+*Ii;++Ii;
    pati->target=*Ji;++Ji;
    pati->sumLBI=*Ji;++Ji;
    pati->sumUBI=*Ji;++Ji;
    to_iter(pati->LBI,Ki,vbegin);++Ki;
    to_iter(pati->UBI,Ki,vbegin);++Ki;
    to_iter(pati->UBILeftReserve,Ki,vbegin);++Ki;
}
std::vector<std::vector<unsigned> >result;
if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
else result.reserve(1024);
TTT_stack_1_1(len,v,ME,result,sizeNeeded,M,SK,SK_end,Duration);
if(result.size()==0)return List(0);
List lis(result.size());
for(unsigned i=0;i<result.size();i++)lis[i]=result[i];
pati=SK.begin()+1;
std::vector<unsigned>II(3*(SK_end-SK.begin()-1));
std::vector<double>JJ(II.size());
std::vector<std::vector<unsigned> >KK(II.size());
std::vector<unsigned>::iterator IIi=II.begin();
std::vector<double>::iterator JJi=JJ.begin();
std::vector<std::vector<unsigned> >::iterator KKi=KK.begin();
for(;pati!=SK_end;++pati)
{
    *IIi=pati->position;++IIi;
    *IIi=pati->s-vbegin;++IIi;
    *IIi=pati->send-vbegin;++IIi;
    *JJi=pati->target;++JJi;
    *JJi=pati->sumLBI;++JJi;
    *JJi=pati->sumUBI;++JJi;
    to_num(pati->LBI,KKi,vbegin);++KKi;
    to_num(pati->UBI,KKi,vbegin);++KKi;
    to_num(pati->UBILeftReserve,KKi,vbegin);++KKi;
}
return List::create(Named("roots",lis),Named("node",List::create(II,JJ,KK,LM)));
}

List FLSSS_SK(unsigned len, NumericVector v, double target, double ME, 
IntegerVector LB, IntegerVector UB, unsigned sizeNeeded, double Duration){
std::vector<std::vector<unsigned> >result;
if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
else result.reserve(1024);
std::vector<PAT>SK(len+6);
NumericVector::iterator vbegin=v.begin();
PAT&SKbegin=SK.front();
SKbegin.LBI.resize(len);
SKbegin.UBI.resize(len);
for(unsigned i=0;i<len;++i)
{
    SKbegin.LBI[i]=vbegin+LB[i]-1;
    SKbegin.UBI[i]=vbegin+UB[i]-1;
}
SKbegin.sumLBI=itersum(SKbegin.LBI.begin(),SKbegin.LBI.end());
SKbegin.sumUBI=itersum(SKbegin.UBI.begin(),SKbegin.UBI.end());
SKbegin.target=target;
std::vector<std::vector<double> >M(len);
M.reserve(len);
theMatrix(v,len,M);
std::vector<PAT>::iterator SK_end=SK.begin()+1;
TTT_stack_1_1(len,v,ME,result,sizeNeeded,M,SK,SK_end,Duration);
if(result.size()==0)return List(0);
List lis(result.size());
for(unsigned i=0;i<result.size();i++)lis[i]=result[i];
return lis;
}

// FLSSS_SK_throw
List FLSSS_SK_throw(unsigned len, NumericVector v, double target, double ME, IntegerVector LB, IntegerVector UB, unsigned sizeNeeded, double Duration);
RcppExport SEXP FLSSS_FLSSS_SK_throw(SEXP lenSEXP, SEXP vSEXP, SEXP targetSEXP, SEXP MESEXP, SEXP LBSEXP, SEXP UBSEXP, SEXP sizeNeededSEXP, SEXP DurationSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< unsigned >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type target(targetSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type LB(LBSEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type UB(UBSEXP );
        Rcpp::traits::input_parameter< unsigned >::type sizeNeeded(sizeNeededSEXP );
        Rcpp::traits::input_parameter< double >::type Duration(DurationSEXP );
        List __result = FLSSS_SK_throw(len, v, target, ME, LB, UB, sizeNeeded, Duration);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// FLSSS_SK_catch
List FLSSS_SK_catch(unsigned len, NumericVector v, double ME, IntegerVector I, NumericVector J, List LK, List LM, unsigned sizeNeeded, double Duration);
RcppExport SEXP FLSSS_FLSSS_SK_catch(SEXP lenSEXP, SEXP vSEXP, SEXP MESEXP, SEXP ISEXP, SEXP JSEXP, SEXP LKSEXP, SEXP LMSEXP, SEXP sizeNeededSEXP, SEXP DurationSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< unsigned >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type I(ISEXP );
        Rcpp::traits::input_parameter< NumericVector >::type J(JSEXP );
        Rcpp::traits::input_parameter< List >::type LK(LKSEXP );
        Rcpp::traits::input_parameter< List >::type LM(LMSEXP );
        Rcpp::traits::input_parameter< unsigned >::type sizeNeeded(sizeNeededSEXP );
        Rcpp::traits::input_parameter< double >::type Duration(DurationSEXP );
        List __result = FLSSS_SK_catch(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// FLSSS_SK_catch_throw
List FLSSS_SK_catch_throw(unsigned len, NumericVector v, double ME, IntegerVector I, NumericVector J, List LK, List LM, unsigned sizeNeeded, double Duration);
RcppExport SEXP FLSSS_FLSSS_SK_catch_throw(SEXP lenSEXP, SEXP vSEXP, SEXP MESEXP, SEXP ISEXP, SEXP JSEXP, SEXP LKSEXP, SEXP LMSEXP, SEXP sizeNeededSEXP, SEXP DurationSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< unsigned >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type I(ISEXP );
        Rcpp::traits::input_parameter< NumericVector >::type J(JSEXP );
        Rcpp::traits::input_parameter< List >::type LK(LKSEXP );
        Rcpp::traits::input_parameter< List >::type LM(LMSEXP );
        Rcpp::traits::input_parameter< unsigned >::type sizeNeeded(sizeNeededSEXP );
        Rcpp::traits::input_parameter< double >::type Duration(DurationSEXP );
        List __result = FLSSS_SK_catch_throw(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// FLSSS_SK
List FLSSS_SK(unsigned len, NumericVector v, double target, double ME, IntegerVector LB, IntegerVector UB, unsigned sizeNeeded, double Duration);
RcppExport SEXP FLSSS_FLSSS_SK(SEXP lenSEXP, SEXP vSEXP, SEXP targetSEXP, SEXP MESEXP, SEXP LBSEXP, SEXP UBSEXP, SEXP sizeNeededSEXP, SEXP DurationSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< unsigned >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type target(targetSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type LB(LBSEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type UB(UBSEXP );
        Rcpp::traits::input_parameter< unsigned >::type sizeNeeded(sizeNeededSEXP );
        Rcpp::traits::input_parameter< double >::type Duration(DurationSEXP );
        List __result = FLSSS_SK(len, v, target, ME, LB, UB, sizeNeeded, Duration);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
