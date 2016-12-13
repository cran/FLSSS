#include <Rcpp.h>
#include <ctime>
using namespace Rcpp;

# define eps 1e-11


// double itersum(std::vector<NumericVector::iterator>::iterator start,
// std::vector<NumericVector::iterator>::iterator end){
// double S=0;
// for(std::vector<NumericVector::iterator>::iterator i=start;i!=end;i++)S=S+**i;
// return S;
// }

template<typename valtype, typename indtype>
valtype itersum(indtype*BI, indtype BIsize, valtype*v){
valtype S=0;
for(indtype i=0;i!=BIsize;++i)S+=v[BI[i]];
return S;
}


// template<typename indtype>
// void to_num(std::vector<indtype>&BI, std::vector<std::vector<int> >::iterator x)
// {
//     x->resize(BI.size());
//     std::vector<int>::iterator xi=x->begin(),end=x->end();
//     indtype*j=&*BI.begin();
//     for(;xi!=end;++xi,++j)*xi=*j;
// }



// void to_iter(std::vector<NumericVector::iterator>&BI,
// std::vector<std::vector<int> >::iterator&x, NumericVector::iterator vbegin)
// {
//     BI.resize(x->size());
//     std::vector<int>::iterator xi=x->begin(),end=x->end();
//     std::vector<NumericVector::iterator>::iterator j=BI.begin();
//     for(;xi!=end;++xi,++j)*j=*xi+vbegin;
// }








template<typename valtype>
void theMatrix(NumericVector v, int len, std::vector<std::vector<valtype> >&M){
std::vector<valtype>*Mi=&*M.begin(), *Miend;
//std::vector<valtype>::iterator tmpi, tmpiend;
valtype*tmpi, *tmpiend;
NumericVector::iterator vi=v.begin(), vii;
int i=v.size();
Mi->resize(i);
for(tmpi=&*Mi->begin();vi!=v.end();++tmpi,++vi)*tmpi=*vi;
++Mi;
--i;
//std::vector<valtype>::iterator prior;
valtype*prior;
vi=v.begin()+1;
Miend=&*M.end();
for(;Mi!=Miend;++Mi,--i,++vi)
{
  vii=vi;
  Mi->resize(i);
  tmpi=&*Mi->begin();
  prior=&*(Mi-1)->begin();
  tmpiend=&*Mi->end();
  for(;tmpi!=tmpiend;++tmpi,++prior,++vii)*tmpi=*prior+*vii;
}
}
















// int FindBoundsCpp9_1_1(int&len,NumericVector&v,double& x,double&ME,
//                         std::vector<NumericVector::iterator>&LBI,double& sumLBI,
//                         std::vector<NumericVector::iterator>&UBI,double& sumUBI,
//                         std::vector<std::vector<double> >&M){
// NumericVector::iterator the, sup;
// std::vector<double>::iterator Mi_begin, Mi_last, mid;
// std::vector<std::vector<double> >::iterator Mi;
// int i, fb_len;
// std::vector<NumericVector::iterator>::iterator LBi, UBi, tmp;
// double Max=x+ME, Min=x-ME, sumnew;
// if(sumUBI<Min||sumLBI>Max)return 0;
// if(sumUBI==sumLBI)return 2;
// double fb_sum_target;
// bool boo=0;
// while(1){
// i=0;
// LBi=LBI.begin();
// UBi=UBI.begin();
//
// fb_len=0;
// fb_sum_target=Min-sumUBI;
// sumnew=0;
//
// for(;LBi!=LBI.end();i++,LBi++,UBi++,++fb_len){
//
// fb_sum_target=fb_sum_target+**UBi;
// if(i>0)
// {
//     the=*(LBi-1)+1;
//     if(*LBi<the)*LBi=the;
//     else the=*LBi;
//     sup=the-fb_len;
//     tmp=UBi-fb_len;
// }
// else {sup=*LBi;tmp=UBi;}
// Mi=M.begin()+fb_len;
// while(1)
// {
//     Mi_last=Mi->begin()+(*tmp-v.begin());
//     if(*Mi_last<fb_sum_target||*tmp+fb_len<*LBi)
//     {
//         if(Mi==M.begin()&& *Mi_last<fb_sum_target)return 0;
//         fb_sum_target=fb_sum_target-**tmp;
//         --Mi;
//         --fb_len;
//         ++tmp;
//     }
//     else break;
// }
// sup=*LBi-fb_len;
//
// Mi_begin=Mi->begin()+(sup-v.begin());
//
//
// if(*Mi_begin>=fb_sum_target)
// {
//     sumnew=sumnew+**LBi;
//     continue;
// }
//
// while(1)
// {
//     mid=Mi_begin+int((Mi_last-Mi_begin)/2);
//     if(*mid<fb_sum_target)
//     {
//         if(mid==Mi_begin)
//         {
//             *LBi=v.begin()+(Mi_last-Mi->begin())+fb_len;
//             break;
//         }
//         Mi_begin=mid;
//     }
//     else if(*(mid-1)>=fb_sum_target)Mi_last=mid;
//     else
//     {
//         *LBi=v.begin()+(mid-Mi->begin())+fb_len;
//         break;
//     }
// }
//
// sumnew=sumnew+**LBi;
// }
//
// if(!boo)boo=1;
// else
// {
//     if(sumLBI==sumnew)
//     {
//         if(sumLBI==sumUBI)return 2;
//         break;
//     }
// }
//
// sumLBI=sumnew;
// fb_sum_target=Max-sumLBI;
//
// i=len-1;
// LBi=LBI.end()-1;
// UBi=UBI.end()-1;
//
// fb_len=0;
// sumnew=0;
//
// for(;UBi>=UBI.begin();--i,--LBi,--UBi,++fb_len){
// fb_sum_target=fb_sum_target+**LBi;
// if(i<len-1)
// {
//     the=*(UBi+1)-1;
//     if(*UBi>the)*UBi=the;
//     else the=*UBi;
//     sup=the+fb_len;
//     tmp=LBi+fb_len;
// }
// else {sup=*UBi;tmp=LBi;}
// Mi=M.begin()+fb_len;
// while(1)
// {
//     Mi_last=Mi->begin()+(*tmp-fb_len-v.begin());
//     if(*Mi_last>fb_sum_target||*tmp-fb_len>*UBi)
//     {
//         if(Mi==M.begin()&& *Mi_last>fb_sum_target)return 0;
//         fb_sum_target=fb_sum_target-**tmp;
//         --Mi;
//         --tmp;
//         --fb_len;
//     }
//     else break;
// }
// sup=*UBi+fb_len;
// Mi_begin=Mi->begin()+(sup-fb_len-v.begin());
//
// if(*Mi_begin<=fb_sum_target)
// {
//     sumnew=sumnew+**UBi;
//     continue;
// }
//
// while(1)
// {
//     mid=Mi_begin-int((Mi_begin-Mi_last)/2);
//     if(*mid>fb_sum_target)
//     {
//         if(mid==Mi_begin)
//         {
//             *UBi=v.begin()+(Mi_last-Mi->begin());
//             break;
//         }
//         Mi_begin=mid;
//     }
//     else if(*(mid+1)<=fb_sum_target)Mi_last=mid;
//     else
//     {
//         *UBi=v.begin()+(mid-Mi->begin());
//         break;
//     }
// }
// sumnew=sumnew+**UBi;
// }
//
// if(sumnew==sumUBI)
// {
//     if(sumUBI==sumLBI)return 2;
//     break;
// }
// sumUBI=sumnew;
// }
// return 1;
// }

















template<typename valtype, typename indtype>
indtype FindBoundsCpp10(indtype len,
                        valtype*v,
                        valtype x, valtype ME,
                        indtype*LB,
                        valtype&sumLB,
                        indtype*UB,
                        valtype&sumUB,
                        std::vector<std::vector<valtype> >&M
                        //,std::ofstream*myfile=NULL
                          ){

//std::cout<<sizeof(len)<<std::endl;

indtype Bi, fb_len;
valtype Max=x+ME, Min=x-ME, sumnew;

if(sumUB<Min||sumLB>Max)return 0;
if(std::abs(sumUB-sumLB)<eps)return 2;
valtype fb_sum_target;
bool boo=0;
while(true){
Bi=0;
fb_len=1;
fb_sum_target=Min-sumUB;
sumnew=0;
//unsigned sumBiNew=0;
// deal with Bi=0 seperately
{
  fb_sum_target+=v[UB[Bi]];
  valtype*tmp=std::lower_bound(v+LB[Bi], v+UB[Bi]+1, fb_sum_target);
  if(tmp==v+UB[Bi]+1)return 0;
  LB[Bi]=tmp-v;
  sumnew+=*tmp;
  //sumBiNew+=tmp;
  ++Bi;
  ++fb_len;
}

for(;Bi!=len;++Bi,++fb_len){
fb_sum_target+=v[UB[Bi]];
if(LB[Bi-1]+1>LB[Bi])LB[Bi]=LB[Bi-1]+1;

//indtype fb_len_reserve=fb_len;

while(true)
{
  valtype Mi_last=M[fb_len-1][UB[Bi-fb_len+1]];
  if(Mi_last<fb_sum_target||LB[Bi]-fb_len+1>UB[Bi-fb_len+1])
  {
    if(fb_len==1&&Mi_last<fb_sum_target)return 0;
    fb_sum_target-=v[UB[Bi-fb_len+1]];
    --fb_len;
  }
  else break;
}

// binary search
{
  // indtype tmp;
  // if(fb_len==fb_len_reserve)tmp=LB[Bi]-fb_len+1;
  // else tmp=std::max(UB[Bi-fb_len+1-1]+1+1, LB[Bi]-fb_len+1);

  //std::cout<<int(UB[Bi-fb_len+1]+1)<<" high and low "<<(int)tmp<<std::endl;

  // LB[Bi]=std::lower_bound(&M[fb_len-1][tmp], &M[fb_len-1][UB[Bi-fb_len+1]]+1, fb_sum_target)
  //    -&M[fb_len-1][0]+fb_len-1;
  LB[Bi]=std::lower_bound(&M[fb_len-1][LB[Bi]-fb_len+1], &M[fb_len-1][UB[Bi-fb_len+1]]+1, fb_sum_target)
    -&M[fb_len-1][0]+fb_len-1;
}

//sumBiNew+=LB[Bi];
sumnew+=v[LB[Bi]];
}

//*myfile<<"in squeezing, after working on LB, sumnew=="<<sumnew<<" "<<"sumUB=="<<sumUB<<" sumLB=="
//  <<sumLB<<std::endl;

if(!boo)boo=1;
else
{
  if(std::abs(sumLB-sumnew)<eps)
  {
    if(std::abs(sumLB-sumUB)<eps)return 2;
    break;
  }
}

//sumLBi=sumBiNew;
sumLB=sumnew;


fb_sum_target=Max-sumLB;
Bi=len-1;
fb_len=1;
sumnew=0;
//sumBiNew=0;

// deal with Bi=len-1
{
  fb_sum_target+=v[LB[Bi]];
  valtype*tmp=std::upper_bound(v+LB[Bi], v+UB[Bi]+1, fb_sum_target)-1;
  if(tmp<v+LB[Bi])return 0;
  UB[Bi]=tmp-v;
  //*myfile<<"\nIn sequezzing, UB[Bi]="<<(int)UB[Bi]<<std::endl;
  sumnew+=*tmp;
  //sumBiNew+=(tmp-1);
}

for(;;){
if(Bi==0)break;
--Bi;
++fb_len;
fb_sum_target+=v[LB[Bi]];
if(UB[Bi]>UB[Bi+1]-1)UB[Bi]=UB[Bi+1]-1;

//if(Bi==0)*myfile<<"\n--UB[0]=="<<(int)UB[0]<<"---\n";

//indtype fb_len_reserve=fb_len;
// for supervising if the free vector is changed. Will be used in the binary search.s

while(true)
{
  valtype Mi_last=M[fb_len-1][LB[Bi+fb_len-1]-fb_len+1];
  if(Mi_last>fb_sum_target||UB[Bi]<LB[Bi+fb_len-1]-fb_len+1)
  {
     if(fb_len==1&&Mi_last>fb_sum_target)return 0;
     fb_sum_target-=v[LB[Bi+fb_len-1]];
     --fb_len;
  }
  else break;
}


//if(Bi==0)*myfile<<"\n--fb_len after free seq updated=="<<(int)fb_len<<"---\n";
//if(Bi==0)*myfile<<"\n--UB[0] after free seq updated=="<<(int)UB[0]<<"---\n";
//if(Bi==0)*myfile<<"\n--fb_len_reserve after free seq updated=="<<(int)fb_len_reserve<<"---\n";

// *myfile<<"just after LB updated"
// for(indtype i=0,iend=subsetSize)


//binary serach
{
  // indtype tmp;
  // if(fb_len_reserve==fb_len)tmp=UB[Bi];
  // else tmp=std::min((indtype)(LB[Bi+fb_len-1+1]-fb_len+1-1-1), UB[Bi]);

  // std::cout<<int(tmp)<<" high and low "<<
  // (int)(LB[Bi+fb_len-1]-fb_len+1)<<std::endl;


  //if(Bi==0)*myfile<<"\n--tmp after free seq updated=="<<(int)tmp<<"---\n";

  // if(Bi==0)*myfile<<int(tmp)<<" high and low "<<
  //   (int)(LB[Bi+fb_len-1]-fb_len+1)<<std::endl;

  // UB[Bi]=std::upper_bound(&M[fb_len-1][LB[Bi+fb_len-1]-fb_len+1], &M[fb_len-1][tmp]+1,
  //                         fb_sum_target)-1-&M[fb_len-1][0];

  UB[Bi]=std::upper_bound(&M[fb_len-1][LB[Bi+fb_len-1]-fb_len+1], &M[fb_len-1][UB[Bi]]+1,
                          fb_sum_target)-1-&M[fb_len-1][0];
}

//if(Bi==0)*myfile<<"\n--UB[0] after binary search=="<<(int)UB[0]<<"---\n";

sumnew+=v[UB[Bi]];
}

//*myfile<<"in squeezing, after working on UB, sumnew=="<<sumnew<<" "<<"sumUB=="<<sumUB<<" sumLB=="
// <<sumLB<<std::endl;

if(std::abs(sumnew-sumUB)<eps)
{
  if(std::abs(sumUB-sumLB)<eps)
  {
    //*myfile<<"what's going on here? didn't enter?\n";
    return 2;
  }
  break;
}
sumUB=sumnew;
//sumUBi=sumBiNew;
}
return 1;
}






// // [[Rcpp::export]]
// List findBound(int len, NumericVector v, double target, double ME, IntegerVector initialLB=-1,
//               IntegerVector initialUB=-1){
// std::vector<std::vector<double> >M(len);
// M.reserve(len);
// theMatrix(v, len, M);
//
// IntegerVector LB(len), UB(len);
//
// if(initialLB[0]<0)
// {
//   for(int i=0;i!=len;++i)
//   {
//     LB[i]=i;
//     UB[i]=v.size()-len+i;
//   }
// }
// else
// {
//   LB=initialLB-1;
//   UB=initialUB-1;
// }
//
// double sumLB=0, sumUB=0;
// for(int i=0,iend=LB.size();i!=iend;++i)
// {
//   sumLB+=v[LB[i]];
//   sumUB+=v[UB[i]];
// }
//
// FindBoundsCpp10(len, &*v.begin(), target, ME, &*LB.begin(), sumLB, &*UB.begin(), sumUB, M);
// return List::create(LB+1, UB+1);
// }
















template<typename valtype, typename indtype>
struct PAT{
//int position;
indtype position;
//NumericVector::iterator s,send;
indtype s, send;
//double target,sumLBI,sumUBI;
valtype target, sumLBI, sumUBI;
//std::vector<NumericVector::iterator>LBI,UBI,UBILeftReserve;
std::vector<indtype>LBI, UBI, UBILeftReserve;
};


template<typename valtype, typename indtype>
//int update(std::vector<PAT>::iterator P){
indtype update(PAT<valtype, indtype>*P, valtype*v, std::vector<std::vector<valtype> >&M){
if(P->s==P->send)return 0;
P->target=P->target+v[P->s];
if(P->position==0)
{
    ++P->s;
    P->target-=v[P->s];
    //std::vector<NumericVector::iterator>::iterator bound=P->LBI.begin();
    indtype*bound=&*P->LBI.begin();
    //NumericVector::iterator iter=P->s+1, tmp=*bound;
    indtype iter=P->s+1, tmp=*bound;
    //for(;bound!=P->LBI.end();bound++,iter++)
    for(indtype*boundEnd=&*P->LBI.end();bound!=boundEnd;++bound,++iter)
    {
        if(iter>*bound)*bound=iter;
        else
        {
            if(iter<*bound)P->sumLBI=P->sumLBI-v[tmp]+v[iter-1];
            break;
        }
    }
}
//else if(P->position==P->UBI.size())
else if(P->position==(indtype)P->UBI.size())
{
    --P->s;
    P->target=P->target-v[P->s];
    //std::vector<NumericVector::iterator>::iterator bound=P->UBI.end()-1;
    indtype*bound=&*(P->UBI.end()-1);
    //NumericVector::iterator iter=P->s-1, tmp=*bound;
    indtype iter=P->s-1, tmp=*bound;
    //for(;bound>=P->UBI.begin();bound--,iter--)
    for(indtype*boundEnd=&*P->UBI.begin(); bound>=boundEnd; bound--,iter--)
    {
        if(iter<*bound)*bound=iter;
        else
        {
            if(iter>*bound)P->sumUBI=P->sumUBI-v[tmp]+v[iter+1];
            break;
        }
    }
}
else
{
    ++P->s;
    //P->target-=*(P->s);
    P->target-=v[P->s];
    //std::vector<NumericVector::iterator>::iterator Lp_right=P->LBI.begin()+P->position, Up_left=P->UBI.begin()+P->position-1;
    indtype*Lp_right=&*(P->LBI.begin()+P->position), *Up_left=&*(P->UBI.begin()+P->position-1);
    //std::vector<NumericVector::iterator>::iterator ULRi=P->UBILeftReserve.end()-1;
    indtype*ULRi=&*P->UBILeftReserve.end()-1;
    //NumericVector::iterator iter=P->s-1, tmp;
    indtype iter=P->s-1, tmp;// need to look into
    //std::vector<NumericVector::iterator>::iterator bound=Up_left;
    indtype*bound=Up_left;
    //double sumUBIright=P->sumUBI-itersum(P->UBI.begin(),Up_left+1);
    for(indtype*boundEnd=&*P->UBI.begin();bound>=boundEnd;bound--,iter--,ULRi--)
    {
        if(iter<=*ULRi)*bound=iter;
        else break;
    }
    //P->sumUBI=itersum(P->UBI.begin(),Up_left+1)+sumUBIright;

    if(bound!=Up_left)P->sumUBI=P->sumUBI-M[Up_left-bound-1][iter]+M[Up_left-bound-1][iter+1];

    iter=P->s+1;
    bound=Lp_right;
    tmp=*bound;
    for(indtype*boundEnd=&*P->LBI.end(); bound!=boundEnd; bound++,iter++)
    {
        if(iter>*bound)*bound=iter;
        else
        {
            if(iter<*bound)P->sumLBI=P->sumLBI-v[tmp]+v[iter-1];
            break;
        }
    }
}
return 1;
}




template<typename valtype, typename indtype>
//int giveBirth(std::vector<PAT>::iterator&child, NumericVector&v, double&ME,std::vector<std::vector<double> >&M){
indtype giveBirth(PAT<valtype, indtype>*child, valtype*v, valtype ME, std::vector<std::vector<valtype> >&M){
//int len=child->UBI.size();
indtype len=(indtype)child->UBI.size();
//int boo=FindBoundsCpp9_1_1(len,v,child->target,ME,child->LBI,child->sumLBI,child->UBI,child->sumUBI,M);
indtype boo=FindBoundsCpp10(len, v, child->target, ME, &child->LBI.front(),
                            child->sumLBI, &child->UBI.front(), child->sumUBI, M);

if(boo==0)return 0;
if(len==1)return 3;
if(boo==2)return 2;

//std::vector<NumericVector::iterator>::iterator LBi=child->LBI.begin(), UBi=child->UBI.begin();
indtype*LBi=&*child->LBI.begin(), *UBi=&*child->UBI.begin();
child->position=0;
indtype Min=*UBi-*LBi;
for(indtype i=1;i<len;++i)
{
  indtype tmp=UBi[i]-LBi[i];
  if(Min>tmp)
  {
    Min=tmp;
    child->position=i;
  }
}

//std::cout<<"child->position="<<(int)child->position<<" ";

if(child->position==0)
{
    child->s=*child->LBI.begin();
    child->send=*child->UBI.begin();
    child->sumLBI-=v[child->s];
    child->target-=v[child->s];
    child->sumUBI-=v[child->send];
    child->LBI.erase(child->LBI.begin());
    child->UBI.erase(child->UBI.begin());
}
else if(child->position==len-1)
{
    child->s=*(child->UBI.end()-1);
    child->send=*(child->LBI.end()-1);
    child->sumUBI-=v[child->s];
    child->sumLBI-=v[child->send];
    child->target-=v[child->s];
    child->LBI.pop_back();
    child->UBI.pop_back();
}
else
{
  //std::cout<<"select in the middle\n";
    //std::vector<NumericVector::iterator>::iterator Lp=child->LBI.begin()+child->position, Up=child->UBI.begin()+child->position;
    indtype*Lp=&*(child->LBI.begin()+child->position), *Up=&*(child->UBI.begin()+child->position);
    child->s=*Lp;
    //child->target=child->target-*(child->s);
    child->target-=v[child->s];
    child->send=*Up;
    //child->sumLBI=child->sumLBI-**Lp;
    child->sumLBI-=v[*Lp];
    //child->sumUBI=child->sumUBI-**Up;
    child->sumUBI-=v[*Up];
    child->LBI.erase(Lp-&child->LBI.front()+child->LBI.begin());
    child->UBI.erase(Up-&child->UBI.front()+child->UBI.begin());
    child->UBILeftReserve.assign(child->UBI.begin(), child->UBI.begin()+child->position);
    //std::vector<NumericVector::iterator>::iterator Up_left=child->UBI.begin()+child->position-1, ULRi=child->UBILeftReserve.end()-1;
    indtype*Up_left=&*(child->UBI.begin()+child->position-1), *ULRi=&*(child->UBILeftReserve.end()-1);
    //NumericVector::iterator iter=child->s-1;
    indtype iter=child->s-1;
    //std::vector<NumericVector::iterator>::iterator bound=Up_left;
    indtype*bound=Up_left;
    //double sumUBIright=child->sumUBI-itersum(child->UBILeftReserve.begin(),child->UBILeftReserve.end());
    for(indtype*boundEnd=&*child->UBI.begin(); bound>=boundEnd; bound--,iter--,ULRi--)
    {
        //if(iter<=*ULRi)*bound=iter;
        if(iter<*ULRi)*bound=iter;
        else break;
    }
    //child->sumUBI=itersum(child->UBI.begin(),Up_left+1)+sumUBIright;
    if(bound!=Up_left)child->sumUBI=child->sumUBI-M[Up_left-bound-1][iter]+M[Up_left-bound-1][iter+1];
}
return 1;
};









// void TTT_stack_1_1(int&LEN,NumericVector&v,double&ME,
//                 std::vector<std::vector<int> >&result, int&sizeNeeded,
//                 std::vector<std::vector<double> >&M, std::vector<PAT>&SK,
//                 std::vector<PAT>::iterator&SK_end, double&Duration){

template<typename valtype, typename indtype>
void TTT_stack_1_1(indtype LEN, valtype*v, indtype vsize, valtype ME,
                std::vector<std::vector<int> >&result, int sizeNeeded,
                std::vector<std::vector<valtype> >&M, PAT<valtype, indtype>*SK,
                PAT<valtype, indtype>*&SK_end, double Duration){// look into SK_end

if(LEN==1)
{
    //double Max=SK.front().target+ME, Min=Max-2*ME;
    valtype Max=SK->target+ME, Min=Max-2*ME;
    std::vector<int>x(1);
    //for(NumericVector::iterator i=v.begin();i!=v.end();++i)
    for(indtype i=0;i!=vsize;++i)
    {
        if(v[i]>=Min&&v[i]<=Max)
        {
            //x.front()=i-v.begin()+1;
            x.front()=i+1;
            result.push_back(x);
        }
        else if(v[i]>Max)break;
    }
    return;
}
//if(SK.front().UBI.size()==0&&SK_end-SK.begin()<2)return;
if(SK->UBI.size()==0&&SK_end-SK<2)return;
//int boo;
indtype boo;
std::clock_t timeend=std::clock()+Duration*(double)CLOCKS_PER_SEC;
//std::vector<int>common(LEN);
std::vector<int>common(LEN);
//std::vector<int>::iterator xi;
int*xi;
while(true){
*SK_end=*(SK_end-1);
boo=giveBirth(SK_end,v,ME,M);
if(boo==1)
{
    ++SK_end;
    continue;
}
if(boo==3)
{
    //xi=common.begin();
    xi=&*common.begin();
    //int i=SK_end->LBI.back()-v.begin()+1, end=SK_end->UBI.back()-v.begin()+1, tmp;
    indtype i=SK_end->LBI.back()+1, end=SK_end->UBI.back()+1;
    //if(i>end){tmp=i;i=end;end=tmp;}
    if(i>end)std::swap(i,end);//?? why this line?
    //for(std::vector<PAT>::iterator SKi=SK.begin()+1;SKi!=SK_end;++SKi,++xi)*xi=SKi->s-v.begin()+1;
    for(PAT<valtype,indtype>*SKi=SK+1;SKi!=SK_end;++SKi,++xi)*xi=SKi->s+1;
    for(;i<=end;i++)
    {
        common.back()=i;
        result.push_back(common);
    }
}
else if(boo==2)
{
    //xi=common.begin();
    xi=&*common.begin();
    //for(std::vector<PAT>::iterator SKi=SK.begin()+1;SKi!=SK_end;++SKi,++xi)*xi=SKi->s-v.begin()+1;
    for(PAT<valtype, indtype>*SKi=SK+1;SKi!=SK_end;++SKi,++xi)*xi=SKi->s+1;
    //std::vector<NumericVector::iterator>::iterator i=SK_end->UBI.begin();
    indtype*i=&*SK_end->UBI.begin();
    //for(;i!=SK_end->UBI.end();++i,++xi)*xi=*i-v.begin()+1;
    for(;i!=&*SK_end->UBI.end();++i,++xi)*xi=*i+1;
    result.push_back(common);
}
while(update(SK_end-1, v, M)==0)
{
    --SK_end;
    //if(SK_end-SK.begin()<2)return;
    if(SK_end-SK<2)return;
}
if((sizeNeeded!=0&&result.size()>=(unsigned)sizeNeeded)||(Duration!=-1&&std::clock()>timeend))break;
}
}















//----------------------------------------------------




template<typename valtype, typename indtype>
List FLSSS_SK_throw_template(int len, NumericVector v, double target, double ME,
    IntegerVector LB, IntegerVector UB, int sizeNeeded, double Duration){
std::vector<std::vector<int> >result;
if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
else result.reserve(1024);
//std::vector<PAT>SK(len+6);
std::vector<PAT<valtype, indtype> >SK(len+6);
//NumericVector::iterator vbegin=v.begin();
indtype vbegin=0;
//PAT&SKbegin=SK.front();
PAT<valtype, indtype>&SKbegin=SK.front();
SKbegin.LBI.resize(len);
SKbegin.UBI.resize(len);
for(int i=0;i<len;++i)
{
    SKbegin.LBI[i]=vbegin+LB[i]-1;
    SKbegin.UBI[i]=vbegin+UB[i]-1;
}


//indtype*BI, indtype BIsize, valtype*v

//SKbegin.sumLBI=itersum(SKbegin.LBI.begin(),SKbegin.LBI.end());
SKbegin.sumLBI=itersum(&*(SKbegin.LBI.begin()), (indtype)(SKbegin.LBI.size()), (valtype*)(&*v.begin()));
//SKbegin.sumUBI=itersum(SKbegin.UBI.begin(),SKbegin.UBI.end());
SKbegin.sumUBI=itersum(&*(SKbegin.UBI.begin()), (indtype)(SKbegin.UBI.size()), (valtype*)(&*v.begin()));
SKbegin.target=(valtype)target;

//std::vector<std::vector<double> >M(len);
std::vector<std::vector<valtype> >M(len);
theMatrix(v, len, M);
//std::vector<PAT>::iterator SK_end=SK.begin()+1;
PAT<valtype, indtype>*SK_end=&*SK.begin()+1;

TTT_stack_1_1<valtype, indtype>((indtype)len, &M[0][0], (indtype)v.size(),
                                (valtype)ME, result, sizeNeeded, M, &SK.front(), SK_end, Duration);

if(result.size()==0)return List(0);
List lis(result.size());
for(unsigned i=0;i<result.size();i++)lis[i]=result[i];
//std::vector<PAT>::iterator pati=SK.begin()+1;
PAT<valtype, indtype>*pati=&*SK.begin()+1;

std::vector<int>I(3*(SK_end-&*SK.begin()-1));
std::vector<double>J(I.size());
std::vector<std::vector<int> >K(I.size());
std::vector<int>::iterator Ii=I.begin();
std::vector<double>::iterator Ji=J.begin();
std::vector<std::vector<int> >::iterator Ki=K.begin();
for(;pati!=SK_end;++pati)
{
    *Ii=pati->position;++Ii;
    *Ii=pati->s-vbegin;++Ii;
    *Ii=pati->send-vbegin;++Ii;
    *Ji=pati->target;++Ji;
    *Ji=pati->sumLBI;++Ji;
    *Ji=pati->sumUBI;++Ji;
    //to_num<indtype>(pati->LBI,Ki,vbegin);++Ki;
    Ki->assign(pati->LBI.begin(), pati->LBI.end());++Ki;
    //to_num<indtype>(pati->UBI,Ki,vbegin);++Ki;
    Ki->assign(pati->UBI.begin(), pati->UBI.end());++Ki;
    //to_num<indtype>(pati->UBILeftReserve,Ki,vbegin);++Ki;
    //std::cout<<pati->UBILeftReserve.size()<<" ";
    Ki->assign(pati->UBILeftReserve.begin(), pati->UBILeftReserve.end());++Ki;
}
return List::create(Named("roots",lis),Named("node",List::create(I,J,K,M)));
}



List FLSSS_SK_throw(int len, NumericVector v, double target, double ME,
  IntegerVector LB, IntegerVector UB, int sizeNeeded, double Duration, bool useFloat){
if(v.size()<256)
{
  if(useFloat)return FLSSS_SK_throw_template<float, unsigned char>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
  else return FLSSS_SK_throw_template<double, unsigned char>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
}
if(v.size()<65536)
{
  if(useFloat)return FLSSS_SK_throw_template<float, unsigned short>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
  else return FLSSS_SK_throw_template<double, unsigned short>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
}
if(useFloat)return FLSSS_SK_throw_template<float, unsigned>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
else return FLSSS_SK_throw_template<double, unsigned>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
}





//----------------------------------------------------




template<typename valtype, typename indtype>
List FLSSS_SK_catch_template(int len, NumericVector v, double ME,
                  IntegerVector I, NumericVector J, List LK, List LM, int sizeNeeded, double Duration){

std::vector<std::vector<int> >K=as<std::vector<std::vector<int> > >(LK);
//std::vector<std::vector<double> >M=as<std::vector<std::vector<double> > >(LM);
std::vector<std::vector<valtype> >M=as<std::vector<std::vector<valtype> > >(LM);
//NumericVector::iterator vbegin=v.begin();
//std::vector<PAT>SK(len+6);

std::vector<PAT<valtype, indtype> >SK(len+6);
//std::vector<PAT>::iterator SK_end=SK.begin()+I.size()/3+1, pati=SK.begin()+1;
PAT<valtype, indtype>*SK_end=&*SK.begin()+I.size()/3+1, *pati=&*SK.begin()+1;

IntegerVector::iterator Ii=I.begin();
NumericVector::iterator Ji=J.begin();

std::vector<std::vector<int> >::iterator Ki=K.begin();
for(;pati!=SK_end;++pati)
{
    pati->position=*Ii;++Ii;
    //pati->s=vbegin+*Ii;++Ii;
    pati->s=*Ii;++Ii;
    //pati->send=vbegin+*Ii;++Ii;
    pati->send=*Ii;++Ii;
    pati->target=*Ji;++Ji;
    pati->sumLBI=*Ji;++Ji;
    pati->sumUBI=*Ji;++Ji;
    //to_iter(pati->LBI,Ki,vbegin);++Ki;
    pati->LBI.assign(Ki->begin(), Ki->end());++Ki;
    //to_iter(pati->UBI,Ki,vbegin);++Ki;
    pati->UBI.assign(Ki->begin(), Ki->end());++Ki;
    //to_iter(pati->UBILeftReserve,Ki,vbegin);++Ki;
    //std::cout<<Ki->size()<<" ";
    pati->UBILeftReserve.assign(Ki->begin(), Ki->end());++Ki;
    //std::cout<<pati->UBILeftReserve.size()<<"\n";
}
std::vector<std::vector<int> >result;
if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
else result.reserve(1024);
TTT_stack_1_1((indtype)len, &M[0][0], (indtype)v.size(), (valtype)ME, result, sizeNeeded, M,
              &SK.front(), SK_end, Duration);
if(result.size()==0)return List(0);
List lis(result.size());
for(unsigned i=0;i<result.size();i++)lis[i]=result[i];
return lis;
}



List FLSSS_SK_catch(int len, NumericVector v, double ME,
                  IntegerVector I, NumericVector J, List LK, List LM, int sizeNeeded,
                  double Duration, bool useFloat){
if(v.size()<256)
{
  if(useFloat)return FLSSS_SK_catch_template<float, unsigned char>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
  else return FLSSS_SK_catch_template<float, unsigned char>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
}
if(v.size()<65536)
{
  if(useFloat)return FLSSS_SK_catch_template<float, unsigned short>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
  else return FLSSS_SK_catch_template<double, unsigned short>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
}
if(useFloat)return FLSSS_SK_catch_template<float, unsigned>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
else return FLSSS_SK_catch_template<double, unsigned>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
}





//----------------------------------------------------






template<typename valtype, typename indtype>
List FLSSS_SK_catch_throw_template(int len, NumericVector v, double ME,
    IntegerVector I, NumericVector J, List LK, List LM, int sizeNeeded, double Duration){
std::vector<std::vector<int> >K=as<std::vector<std::vector<int> > >(LK);
std::vector<std::vector<valtype> >M=as<std::vector<std::vector<valtype> > >(LM);
//NumericVector::iterator vbegin=v.begin();
std::vector<PAT<valtype, indtype> >SK(len+6);
PAT<valtype, indtype>*SK_end=&*SK.begin()+I.size()/3+1, *pati=&*SK.begin()+1;
IntegerVector::iterator Ii=I.begin();
NumericVector::iterator Ji=J.begin();
std::vector<std::vector<int> >::iterator Ki=K.begin();
for(;pati!=SK_end;++pati)
{
    pati->position=*Ii;++Ii;
    pati->s=*Ii;++Ii;
    pati->send=*Ii;++Ii;
    pati->target=*Ji;++Ji;
    pati->sumLBI=*Ji;++Ji;
    pati->sumUBI=*Ji;++Ji;
    //to_iter(pati->LBI,Ki);++Ki;
    pati->LBI.assign(Ki->begin(), Ki->end());++Ki;
    //to_iter(pati->UBI,Ki);++Ki;
    pati->UBI.assign(Ki->begin(), Ki->end());++Ki;
    //to_iter(pati->UBILeftReserve,Ki);++Ki;
    pati->UBILeftReserve.assign(Ki->begin(), Ki->end());++Ki;
}
std::vector<std::vector<int> >result;
if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
else result.reserve(1024);
TTT_stack_1_1((indtype)len, &M[0][0], (indtype)v.size(), (valtype)ME, result, sizeNeeded,
              M, &SK.front(), SK_end, Duration);
//std::cout<<"1.1\n";
if(result.size()==0)return List(0);
List lis(result.size());
for(unsigned i=0;i<result.size();i++)lis[i]=result[i];
pati=&*SK.begin()+1;
std::vector<int>II(3*(SK_end-&*SK.begin()-1));
std::vector<double>JJ(II.size());
std::vector<std::vector<int> >KK(II.size());
std::vector<int>::iterator IIi=II.begin();
std::vector<double>::iterator JJi=JJ.begin();
std::vector<std::vector<int> >::iterator KKi=KK.begin();
for(;pati!=SK_end;++pati)
{
    *IIi=pati->position;++IIi;
    *IIi=pati->s;++IIi;
    *IIi=pati->send;++IIi;
    *JJi=pati->target;++JJi;
    *JJi=pati->sumLBI;++JJi;
    *JJi=pati->sumUBI;++JJi;
    //to_num(pati->LBI,KKi);++KKi;
    KKi->assign(pati->LBI.begin(), pati->LBI.end());++KKi;
    //to_num(pati->UBI,KKi);++KKi;
    KKi->assign(pati->UBI.begin(), pati->UBI.end());++KKi;
    //to_num(pati->UBILeftReserve,KKi);++KKi;
    KKi->assign(pati->UBILeftReserve.begin(), pati->UBILeftReserve.end());++KKi;
}
return List::create(Named("roots",lis),Named("node",List::create(II,JJ,KK,LM)));
}




List FLSSS_SK_catch_throw(int len, NumericVector v, double ME,
    IntegerVector I, NumericVector J, List LK, List LM, int sizeNeeded, double Duration, bool useFloat){
if(v.size()<256)
{
  if(useFloat)return FLSSS_SK_catch_throw_template<float, unsigned char>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
  else return FLSSS_SK_catch_throw_template<double, unsigned char>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
}
if(v.size()<65536)
{
  if(useFloat)return FLSSS_SK_catch_throw_template<float, unsigned short>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
  else return FLSSS_SK_catch_throw_template<double, unsigned short>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
}
if(useFloat)return FLSSS_SK_catch_throw_template<float, unsigned>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
else return FLSSS_SK_catch_throw_template<double, unsigned>(len, v, ME, I, J, LK, LM, sizeNeeded, Duration);
}







//----------------------------------------------------









template<typename valtype, typename indtype>
List FLSSS_SK_template(int len, NumericVector v, double target, double ME,
    IntegerVector LB, IntegerVector UB, int sizeNeeded, double Duration){
std::vector<std::vector<int> >result;
if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
else result.reserve(1024);
std::vector<PAT<valtype, indtype> >SK(len+6);
//NumericVector::iterator vbegin=v.begin();
PAT<valtype, indtype>&SKbegin=SK.front();
SKbegin.LBI.resize(len);
SKbegin.UBI.resize(len);
for(int i=0;i<len;++i)
{
    SKbegin.LBI[i]=LB[i]-1;
    SKbegin.UBI[i]=UB[i]-1;
}
SKbegin.sumLBI=itersum(&*(SKbegin.LBI.begin()), (indtype)(SKbegin.LBI.size()), (valtype*)(&*v.begin()));
SKbegin.sumUBI=itersum(&*(SKbegin.UBI.begin()), (indtype)(SKbegin.UBI.size()), (valtype*)(&*v.begin()));
SKbegin.target=(valtype)target;
std::vector<std::vector<valtype> >M(len);
M.reserve(len);
theMatrix(v, len, M);
PAT<valtype, indtype>*SK_end=&*SK.begin()+1;
TTT_stack_1_1((indtype)len, &M[0][0], indtype(v.size()), (valtype)ME, result, sizeNeeded, M, &SK.front(), SK_end, Duration);
if(result.size()==0)return List(0);
List lis(result.size());
for(unsigned i=0;i<result.size();i++)lis[i]=result[i];
return lis;
}




List FLSSS_SK(int len, NumericVector v, double target, double ME,
    IntegerVector LB, IntegerVector UB, int sizeNeeded, double Duration, bool useFloat){
if(v.size()<256)
{
  if(useFloat)return FLSSS_SK_template<float, unsigned char>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
  else return FLSSS_SK_template<double, unsigned char>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
}
if(v.size()<65536)
{
  if(useFloat)return  FLSSS_SK_template<float, unsigned short>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
  else return FLSSS_SK_template<double, unsigned short>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
}
if(useFloat)return FLSSS_SK_template<float, unsigned>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
else return FLSSS_SK_template<float, unsigned>(len, v, target, ME, LB, UB, sizeNeeded, Duration);
}















//-------------------------------------------Rcpp

// FLSSS_SK_throw
List FLSSS_SK_throw(int len, NumericVector v, double target, double ME, IntegerVector LB, IntegerVector UB, int sizeNeeded, double Duration, bool useFloat);
RcppExport SEXP FLSSS_FLSSS_SK_throw(SEXP lenSEXP, SEXP vSEXP, SEXP targetSEXP, SEXP MESEXP, SEXP LBSEXP, SEXP UBSEXP, SEXP sizeNeededSEXP, SEXP DurationSEXP, SEXP useFloatSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< int >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type target(targetSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type LB(LBSEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type UB(UBSEXP );
        Rcpp::traits::input_parameter< int >::type sizeNeeded(sizeNeededSEXP );
        Rcpp::traits::input_parameter< double >::type Duration(DurationSEXP );
        Rcpp::traits::input_parameter< bool >::type useFloat(useFloatSEXP );
        List __result = FLSSS_SK_throw(len, v, target, ME, LB, UB, sizeNeeded, Duration, useFloat);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}



// FLSSS_SK_catch
List FLSSS_SK_catch(int len, NumericVector v, double ME, IntegerVector I, NumericVector J, List LK, List LM, int sizeNeeded, double Duration, bool useFloat);
RcppExport SEXP FLSSS_FLSSS_SK_catch(SEXP lenSEXP, SEXP vSEXP, SEXP MESEXP, SEXP ISEXP, SEXP JSEXP, SEXP LKSEXP, SEXP LMSEXP, SEXP sizeNeededSEXP, SEXP DurationSEXP, SEXP useFloatSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< int >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type I(ISEXP );
        Rcpp::traits::input_parameter< NumericVector >::type J(JSEXP );
        Rcpp::traits::input_parameter< List >::type LK(LKSEXP );
        Rcpp::traits::input_parameter< List >::type LM(LMSEXP );
        Rcpp::traits::input_parameter< int >::type sizeNeeded(sizeNeededSEXP );
        Rcpp::traits::input_parameter< double >::type Duration(DurationSEXP );
        Rcpp::traits::input_parameter< bool >::type useFloat(useFloatSEXP );
        List __result = FLSSS_SK_catch(len, v, ME, I, J, LK, LM, sizeNeeded, Duration, useFloat);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}




// FLSSS_SK_catch_throw
List FLSSS_SK_catch_throw(int len, NumericVector v, double ME, IntegerVector I, NumericVector J, List LK, List LM, int sizeNeeded, double Duration, bool useFloat);
RcppExport SEXP FLSSS_FLSSS_SK_catch_throw(SEXP lenSEXP, SEXP vSEXP, SEXP MESEXP, SEXP ISEXP, SEXP JSEXP, SEXP LKSEXP, SEXP LMSEXP, SEXP sizeNeededSEXP, SEXP DurationSEXP, SEXP useFloatSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< int >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type I(ISEXP );
        Rcpp::traits::input_parameter< NumericVector >::type J(JSEXP );
        Rcpp::traits::input_parameter< List >::type LK(LKSEXP );
        Rcpp::traits::input_parameter< List >::type LM(LMSEXP );
        Rcpp::traits::input_parameter< int >::type sizeNeeded(sizeNeededSEXP );
        Rcpp::traits::input_parameter< double >::type Duration(DurationSEXP );
        Rcpp::traits::input_parameter< bool >::type useFloat(useFloatSEXP );
        List __result = FLSSS_SK_catch_throw(len, v, ME, I, J, LK, LM, sizeNeeded, Duration, useFloat);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// FLSSS_SK
List FLSSS_SK(int len, NumericVector v, double target, double ME, IntegerVector LB, IntegerVector UB, int sizeNeeded, double Duration, bool useFloat);
RcppExport SEXP FLSSS_FLSSS_SK(SEXP lenSEXP, SEXP vSEXP, SEXP targetSEXP, SEXP MESEXP, SEXP LBSEXP, SEXP UBSEXP, SEXP sizeNeededSEXP, SEXP DurationSEXP, SEXP useFloatSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< int >::type len(lenSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP );
        Rcpp::traits::input_parameter< double >::type target(targetSEXP );
        Rcpp::traits::input_parameter< double >::type ME(MESEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type LB(LBSEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type UB(UBSEXP );
        Rcpp::traits::input_parameter< int >::type sizeNeeded(sizeNeededSEXP );
        Rcpp::traits::input_parameter< double >::type Duration(DurationSEXP );
        Rcpp::traits::input_parameter< bool >::type useFloat(useFloatSEXP );
        List __result = FLSSS_SK(len, v, target, ME, LB, UB, sizeNeeded, Duration, useFloat);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
