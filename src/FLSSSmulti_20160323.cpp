#include <ctime>
#include <fstream>
// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
using namespace Rcpp;
using namespace RcppParallel;

#define mdouble std::vector<double>

template<typename T>
struct vec{
T*begin, *end, *head, *tail;
vec(){begin=NULL;end=NULL;head=NULL;tail=NULL;}
int size(){return end-begin;}
T&front(){return *begin;}
T&back(){return *(end-1);}
void pop_front(){++begin;}
void pop_back(){--end;}
void erase(T*i)
{
  if(i-begin+1>end-i)
  {
    for(++i;i!=end;++i)*(i-1)=*i;
    --end;
  }
  else
  {
    for(;i>=begin;--i)*i=*(i-1);
    ++begin;
  }
}
void newSize(int n)
{
  //head=(T*)malloc(sizeof(T)*n);
  head=new T[n];
  tail=head+n;
  begin=head;
  end=tail;
}
void assign(T*s, T*t)
{
  int n=t-s, capacity=tail-head;
  if(capacity<n)
  {
    //free(head);
    delete[]head;
    //head=(T*)malloc(sizeof(T)*n);
    head=new T[n];
    tail=head+n;
    for(T*k=head;s!=t;++k,++s)*k=*s;
    begin=head;
    end=tail;
  }
  else
  {
    for(T*k=head;s!=t;++k,++s)*k=*s;
    begin=head;
    end=begin+n;
  }
}
~vec()
{
  delete[]head;
}
T operator [](int i) const {return *(begin+i);}
T & operator [](int i) {return *(begin+i);}
};

//every double is std::vector<T> now

// vector rst=x+y
void plus(mdouble&rst,mdouble&x,mdouble&y){
for(int i=0,iend=x.size();i!=iend;++i)rst[i]=x[i]+y[i];
}

// vector rst=x-y
void minus(mdouble&rst,mdouble&x,mdouble&y){
for(int i=0,iend=x.size();i!=iend;++i)rst[i]=x[i]-y[i];
}

// vector rst=rst+x-y
void plusMinus(mdouble&rst,mdouble&x,mdouble&y){
for(int i=0,iend=x.size();i!=iend;++i)rst[i]=rst[i]+x[i]-y[i];
}

// all(x>y)
bool greater(mdouble&x,mdouble&y){
for(int i=0,iend=x.size();i!=iend;++i){if(x[i]<=y[i])return 0;}
return 1;
}

bool less(mdouble&x,mdouble&y){
for(int i=0,iend=x.size();i!=iend;++i){if(x[i]>=y[i])return 0;}
return 1;
}

bool greaterEqual(mdouble&x,mdouble&y){
for(int i=0,iend=x.size();i!=iend;++i){if(x[i]<y[i])return 0;}
return 1;
}

bool lessEqual(mdouble&x,mdouble&y){
for(int i=0,iend=x.size();i!=iend;++i){if(x[i]>y[i])return 0;}
return 1;
}

bool equal(mdouble&x,mdouble&y){
for(int i=0,iend=x.size();i!=iend;++i){if(x[i]!=y[i])return 0;}
return 1;
}

bool unequal(mdouble&x,mdouble&y){
for(int i=0,iend=x.size();i!=iend;++i){if(x[i]==y[i])return 0;}
return 1;
}


void mdoubleS(mdouble&rst, std::vector<mdouble>::iterator start,
              std::vector<mdouble>::iterator finish){
for(;start!=finish;++start)plus(rst,rst,*start);
}




// // MDOUBLE is a vector of doubles now
// template<typename IT, typename MDOUBLE>
// //double itersum_m(IT start, IT end){
// MDOUBLE itersum(IT start, IT end){
// //double S=0;
// MDOUBLE S=0;
// for(;start!=end;++start)S+=**start;
// return S;
// }
//
//
// void itersum(mdouble&S,
//   std::vector<std::vector<mdouble>::iterator>::iterator start,
//   std::vector<std::vector<mdouble>::iterator>::iterator end){
// // for(;start!=end;start++)S+=**start;
// // return S;
// for(;start!=end;++start)plus(S,S,**start);
// }

template<typename IT>
void itersum(mdouble&S, IT start, IT end)
{
  for(;start!=end;++start)plus(S,S,**start);
}



//void theMatrix(NumericVector&v,std::vector<std::vector<double> >&M){
void mtheMatrix(std::vector<mdouble>&v, std::vector<std::vector<mdouble> >&M){
int _d=v.front().size();
//std::vector<std::vector<double> >::iterator Mi=M.begin(), Miend;
std::vector<std::vector<mdouble> >::iterator Mi=M.begin(), Miend;
//std::vector<double>::iterator tmpi, tmpiend;
std::vector<mdouble>::iterator tmpi, tmpiend;
//NumericVector::iterator vi=v.begin(), vii;
std::vector<mdouble>::iterator vi=v.begin(), vii;
int i=v.size();
Mi->resize(i);
for(tmpi=Mi->begin();vi!=v.end();++tmpi,++vi)*tmpi=*vi;
++Mi;
--i;
//std::vector<double>::iterator prior;
//std::cout<<"1.01\n";

std::vector<mdouble>::iterator prior;
vi=v.begin()+1;
Miend=M.end();
for(;Mi!=Miend;++Mi,--i,++vi)
{
  vii=vi;
  Mi->resize(i);
  tmpi=Mi->begin();
  prior=(Mi-1)->begin();
  tmpiend=Mi->end();
  //for(;tmpi!=tmpiend;++tmpi,++prior,++vii)*tmpi=*prior+*vii;
  for(;tmpi!=tmpiend;++tmpi,++prior,++vii)
  {
    tmpi->resize(_d);
    plus(*tmpi,*prior,*vii);
  }
}
}









/*
template<typename IT>
void to_num(IT BIbegin, IT BIend, std::vector<std::vector<int> >::iterator x,
            NumericVector::iterator vbegin)
{
    x->resize(BIend-BIbegin);
    std::vector<int>::iterator xi=x->begin(),end=x->end();
    IT j=BIbegin;
    for(;xi!=end;++xi,++j)*xi=*j-vbegin;
}




void to_iter(vec<NumericVector::iterator>&BI, std::vector<std::vector<int> >::iterator&x,
             NumericVector::iterator vbegin)
{
    BI.newSize(x->size());
    std::vector<int>::iterator xi=x->begin(), end=x->end();
    //std::vector<NumericVector::iterator>::iterator j=BI.begin;
    NumericVector::iterator* j=BI.begin;
    for(;xi!=end;++xi,++j)*j=*xi+vbegin;
}


void to_iter_stl(std::vector<NumericVector::iterator>&BI, std::vector<std::vector<int> >::iterator&x,
             NumericVector::iterator vbegin)
{
    BI.resize(x->size());
    std::vector<int>::iterator xi=x->begin(), end=x->end();
    //std::vector<NumericVector::iterator>::iterator j=BI.begin();
    std::vector<NumericVector::iterator>::iterator j=BI.begin();
    for(;xi!=end;++xi,++j)*j=*xi+vbegin;
}


void printBI(vec<std::vector<mdouble>::iterator>&x, std::vector<mdouble>&v, std::ofstream&myfile)
{
  for(int i=0;i!=x.size();++i)
    myfile<<x[i]-v.begin()+1<<" ";
  myfile<<"\n";
}

void printv(vec<std::vector<mdouble>::iterator>&x, std::vector<mdouble>&v, std::ofstream&myfile)
{
  for(int i=0,iend=x.size();i!=iend;++i)
    myfile<<x[i]-v.begin()+1<<" ";
  myfile<<"\n";
}

void printBI(vec<std::vector<mdouble>::iterator>&x, std::vector<mdouble>&v)
{
  for(int i=0;i!=x.size();++i)
    std::cout<<x[i]-v.begin()+1<<" ";
  std::cout<<"\n";
}

void printv(vec<std::vector<mdouble>::iterator>&x, std::vector<mdouble>&v)
{
  for(int i=0,iend=x.size();i!=iend;++i)
    std::cout<<x[i]-v.begin()+1<<" ";
  std::cout<<"\n";
}

void mdoublePrint(mdouble&x)
{
  for(int i=0,iend=x.size();i!=iend;++i)std::cout<<x[i]<<" ";
}

void mdoublePrint(mdouble&x, std::ofstream&myfile)
{
  for(int i=0,iend=x.size();i!=iend;++i)myfile<<x[i]<<" ";
}
*/

// int FindBoundsCpp9_1_1(int&len,NumericVector&v,double& x,double&ME,
//                         vec<NumericVector::iterator>&LBI,
//                         double&sumLBI,
//                         vec<NumericVector::iterator>&UBI,
//                         double&sumUBI,
//                         std::vector<std::vector<double> >&M){





int mFindBoundsCpp9_1_1(int&len,
                       std::vector<mdouble>&v,
                       mdouble&x, mdouble&ME,
                       vec<std::vector<mdouble>::iterator>&LBI,
                       mdouble&sumLBI,
                       vec<std::vector<mdouble>::iterator>&UBI,
                       mdouble&sumUBI,
                       std::vector<std::vector<mdouble> >&M){

// std::ofstream myfile;
// myfile.precision(3);
// myfile.setf(std::ios::fixed);
// myfile.setf(std::ios::showpoint);
//
// myfile.open("C:/Users/Charles Liu/Desktop/experiment/error.csv", std::ios_base::app);
// myfile<<"in findBound"<<std::endl;
// printBI(LBI, v, myfile);
// printBI(UBI, v, myfile);
// myfile<<"target==";mdoublePrint(x,myfile);myfile<<std::endl;

// std::cout<<"sumLBI==";mdoublePrint(sumLBI);std::cout<<"\n";
// std::cout<<"target==";mdoublePrint(x);std::cout<<"\n";
// std::cout<<"sumUBI==";mdoublePrint(sumUBI);std::cout<<"\n";

int _d=x.size();

//NumericVector::iterator the, sup;
std::vector<mdouble>::iterator the, sup;
//std::vector<double>::iterator Mi_begin, Mi_last, mid;
std::vector<mdouble>::iterator Mi_begin, Mi_last, mid;
//std::vector<std::vector<double> >::iterator Mi;
std::vector<std::vector<mdouble> >::iterator Mi;
int i, fb_len;
//NumericVector::iterator* LBi, *UBi, *tmp;
std::vector<mdouble>::iterator *LBi, *UBi, *tmp;
//double Max=x+ME, Min=x-ME, sumnew;
mdouble Max(_d), Min(_d), sumnew(_d);
plus(Max, x, ME);
minus(Min, x, ME);



// mdoublePrint(sumUBI);std::cout<<"\n";
// mdoublePrint(Min);std::cout<<"\n";
// mdoublePrint(sumLBI);std::cout<<"\n";
// mdoublePrint(Max);std::cout<<"\n";





//if(sumUBI<Min||sumLBI>Max)return 0;
//if(!(sumUBI>=Min&&sumLBI<=Max))return 0;
if(!(greaterEqual(sumUBI,Min)&&lessEqual(sumLBI,Max))){/*myfile<<"out findBound==0(1)"<<std::endl;myfile.close();*/return 0;}




//if(sumUBI==sumLBI)return 2;
if(equal(sumUBI,sumLBI)){/*myfile<<"out findBound==2"<<std::endl;myfile.close();*/return 2;}

//double fb_sum_target;
mdouble fb_sum_target(_d, 0);

bool boo=0;

while(1){
i=0;
//LBi=LBI.begin();
LBi=LBI.begin;
//UBi=UBI.begin();
UBi=UBI.begin;

fb_len=0;
//fb_sum_target=Min-sumUBI;
minus(fb_sum_target, Min, sumUBI);
//sumnew=0;

std::fill(sumnew.begin(), sumnew.end(), 0);

for(;LBi!=LBI.end;i++,LBi++,UBi++,++fb_len){
//fb_sum_target+=**UBi;
plus(fb_sum_target, fb_sum_target, **UBi);

if(i!=0)
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
  //bool tmpBool=(*Mi_last<fb_sum_target); // find the first one that satisfies
  //bool tmpBool=less(*Mi_last, fb_sum_target);
  bool tmpBool= !greaterEqual(*Mi_last, fb_sum_target);
  if(tmpBool||*tmp+fb_len<*LBi)
  {
    if(Mi==M.begin()&&tmpBool){/*myfile<<"out findBound==0(2)"<<std::endl;myfile.close();*/return 0;}
    //fb_sum_target-=**tmp;
    minus(fb_sum_target, fb_sum_target, **tmp);
    --Mi;
    --fb_len;
    ++tmp;
  }
  else break;
}

sup=*LBi-fb_len;

Mi_begin=Mi->begin()+(sup-v.begin());

//if(*Mi_begin>=fb_sum_target)
if(greaterEqual(*Mi_begin,fb_sum_target))
{
  //sumnew+=**LBi;
  plus(sumnew,sumnew,**LBi);
  continue;
}

while(1)
{
  mid=Mi_begin+int((Mi_last-Mi_begin)/2);
  //if(*mid<fb_sum_target)
  //if(less(*mid,fb_sum_target))
  if(!greaterEqual(*mid,fb_sum_target))// if somebody(element) is less then it's not OK
  {
    if(mid==Mi_begin)
    {
      *LBi=v.begin()+(Mi_last-Mi->begin())+fb_len;
      break;
    }
    Mi_begin=mid;
  }
  //else if(*(mid-1)>=fb_sum_target)Mi_last=mid;
  else if(greaterEqual(*(mid-1),fb_sum_target))Mi_last=mid;// if everybody in mid-1 >= then it's not OK
  //else if(!less(*(mid-1),fb_sum_target))Mi_last=mid;// if somebody is >= then it's not OK
  //Anyway, the key is, break if this->everybody >=... and !( (this-1)->everybody>=...)
  else
  {
    *LBi=v.begin()+(mid-Mi->begin())+fb_len;
    break;
  }
}
//sumnew+=**LBi;
plus(sumnew,sumnew,**LBi);
}

if(!boo)boo=1;
else
{
  //if(sumLBI==sumnew)
  if(equal(sumLBI,sumnew))
  {
    //if(sumLBI==sumUBI)return 2;
    if(equal(sumLBI,sumUBI)){/*myfile<<"out findBound==2"<<std::endl;myfile.close();*/return 2;}
    break;
  }
}

//sumLBI=sumnew;
std::swap(sumLBI,sumnew);
//fb_sum_target=Max-sumLBI;
minus(fb_sum_target,Max,sumLBI);

i=len-1;
//LBi=LBI.end()-1;
LBi=LBI.end-1;
//UBi=UBI.end()-1;
UBi=UBI.end-1;

fb_len=0;
//sumnew=0;
std::fill(sumnew.begin(),sumnew.end(),0);

for(;UBi>=UBI.begin;--i,--LBi,--UBi,++fb_len){
//fb_sum_target+=**LBi;
plus(fb_sum_target, fb_sum_target, **LBi);
if(i!=len-1)
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
  //bool tmpBool=(*Mi_last>fb_sum_target);
  //bool tmpBool=greater(*Mi_last, fb_sum_target);
  bool tmpBool= !lessEqual(*Mi_last,fb_sum_target);
  if(tmpBool||*tmp-fb_len>*UBi)
  {
    if(Mi==M.begin()&&tmpBool)
    {
//       myfile<<"Mi-M.begin()=="<<Mi-M.begin()<<std::endl;
//       myfile<<"Mi_last-Mi->begin()=="<<Mi_last-Mi->begin()<<std::endl;
//       myfile<<"UBi-UBI.begin=="<<UBi-UBI.begin<<std::endl;
//       myfile<<"LBi-LBI.begin=="<<LBi-LBI.begin<<std::endl;
//       myfile<<"UBi-UBI.begin=="<<UBi-UBI.begin<<std::endl;
//       myfile<<"out findBound==0(3)"<<std::endl;myfile.close();
      return 0;
    }
    //fb_sum_target-=**tmp;
    minus(fb_sum_target,fb_sum_target,**tmp);
    --Mi;
    --tmp;
    --fb_len;
  }
  else break;
}

sup=*UBi+fb_len;
Mi_begin=Mi->begin()+(sup-fb_len-v.begin());

//if(*Mi_begin<=fb_sum_target)
if(lessEqual(*Mi_begin,fb_sum_target))
{
  //sumnew+=**UBi;
  plus(sumnew,sumnew,**UBi);
  continue;
}

while(1)
{
  mid=Mi_begin-int((Mi_begin-Mi_last)/2);
  //if(*mid>fb_sum_target)
  if(!lessEqual(*mid,fb_sum_target))
  {
    if(mid==Mi_begin)
    {
      *UBi=v.begin()+(Mi_last-Mi->begin());
      break;
    }
    Mi_begin=mid;
  }
  //else if(*(mid+1)<=fb_sum_target)Mi_last=mid;
  else if(lessEqual(*(mid+1),fb_sum_target))Mi_last=mid;
  else
  {
    *UBi=v.begin()+(mid-Mi->begin());
    break;
  }
}
//sumnew+=**UBi;
plus(sumnew,sumnew,**UBi);
}
//if(sumnew==sumUBI)
if(equal(sumnew,sumUBI))
{
  //if(sumUBI==sumLBI)return 2;
  if(equal(sumUBI,sumLBI)){/*myfile<<"out findBound==2"<<std::endl;myfile.close();*/return 2;}
  break;
}
//sumUBI=sumnew;
std::swap(sumUBI,sumnew);
}

//myfile<<"out findBound==1"<<std::endl;myfile.close();
return 1;
}





/*
// don't delete this function damn it! just try to hide it from publishing!!
// [[Rcpp::export]]
List testFindBound(int len, List vr, NumericVector targetr, NumericVector MEr,
IntegerVector LB, IntegerVector UB){


std::vector<mdouble>v(vr.size());
for(int i=0,iend=v.size();i!=iend;++i)
{
  NumericVector tmp=vr[i];
  v[i].assign(tmp.begin(),tmp.end());
}

int _d=v.front().size();

std::vector<mdouble>::iterator vbegin=v.begin();

vec<std::vector<mdouble>::iterator>LBI,UBI;
LBI.newSize(len);
UBI.newSize(len);

for(int i=0;i!=len;++i)
{
    LBI[i]=vbegin+LB[i]-1;
    UBI[i]=vbegin+UB[i]-1;
}

mdouble target(targetr.begin(), targetr.end());
mdouble ME(MEr.begin(), MEr.end());

//std::vector<std::vector<double> >M(len);
std::vector<std::vector<mdouble> >M(len);

theMatrix(v,M);

mdouble sumLBI(_d), sumUBI(_d);
itersum(sumLBI, LBI.begin, LBI.end);
itersum(sumUBI, UBI.begin, UBI.end);

std::cout<<"findBound result=="<<FindBoundsCpp9_1_1(len, v, target, ME, LBI, sumLBI, UBI, sumUBI, M)
  <<std::endl;

IntegerVector rstLBI(len), rstUBI(len);
for(int i=0,iend=LBI.size();i!=iend;++i)
{
  rstLBI[i]=LBI[i]-vbegin+1;
  rstUBI[i]=UBI[i]-vbegin+1;
}

return List::create(Named("LB")=rstLBI,Named("UB")=rstUBI);
}
*/










struct mPAT{
int position;
std::vector<mdouble>::iterator s,send;
mdouble target, sumLBI, sumUBI;
vec<std::vector<mdouble>::iterator>LBI, UBI;
std::vector<std::vector<mdouble>::iterator>UBILeftReserve;
void copy(mPAT&x)
{
  position=x.position;
  s=x.s;
  send=x.send;
  target=x.target;
  sumLBI=x.sumLBI;
  sumUBI=x.sumUBI;
  LBI.assign(x.LBI.begin, x.LBI.end);
  UBI.assign(x.UBI.begin, x.UBI.end);
  UBILeftReserve.assign(x.UBILeftReserve.begin(), x.UBILeftReserve.end());
}
};









int mupdate(std::vector<mPAT>::iterator P
           //,std::vector<mdouble>&v
          ){

int _d=P->sumUBI.size();
if(P->s==P->send)return 0;
// P->target=P->target+*(P->s);
plus(P->target,P->target,*(P->s));

if(P->position==0)
{
    ++P->s;
    //P->target=P->target-*(P->s);
    minus(P->target,P->target,*(P->s));

    std::vector<mdouble>::iterator*bound=P->LBI.begin;

    std::vector<mdouble>::iterator iter=P->s+1, tmp=*bound;
    for(;bound!=P->LBI.end;bound++,iter++)
    {
        if(iter>*bound)*bound=iter;
        else
        {
          //if(iter<*bound)P->sumLBI=P->sumLBI-*tmp+*(iter-1);
          if(iter<*bound)plusMinus(P->sumLBI,*(iter-1),*tmp);
          break;
        }
    }
}
else if(P->position==P->UBI.size())
{
    --P->s;
    //P->target=P->target-*(P->s);
    minus(P->target,P->target,*(P->s));

    //NumericVector::iterator* bound=P->UBI.end-1;
    std::vector<mdouble>::iterator*bound=P->UBI.end-1;
    //NumericVector::iterator iter=P->s-1, tmp=*bound;
    std::vector<mdouble>::iterator iter=P->s-1, tmp=*bound;
    for(;bound>=P->UBI.begin;bound--,iter--)
    {
        if(iter<*bound)*bound=iter;
        else
        {
          //if(iter>*bound)P->sumUBI=P->sumUBI-*tmp+*(iter+1);
          if(iter>*bound)plusMinus(P->sumUBI,*(iter+1),*tmp);
          break;
        }
    }
}
else
{
    ++P->s;
    //P->target=P->target-*(P->s);
    minus(P->target,P->target,*(P->s));

    //NumericVector::iterator* Lp_right=P->LBI.begin+P->position, *Up_left=P->UBI.begin+P->position-1;
    std::vector<mdouble>::iterator*Lp_right=P->LBI.begin+P->position, *Up_left=P->UBI.begin+P->position-1;
    //std::vector<NumericVector::iterator>::iterator ULRi=P->UBILeftReserve.end()-1;
    std::vector<std::vector<mdouble>::iterator>::iterator ULRi=P->UBILeftReserve.end()-1;
    //NumericVector::iterator iter=P->s-1, tmp;
    std::vector<mdouble>::iterator iter=P->s-1, tmp;
    //NumericVector::iterator*bound=Up_left;
    std::vector<mdouble>::iterator*bound=Up_left;
    //double sumUBIright=P->sumUBI-itersum(P->UBI.begin, Up_left+1);
    mdouble sumUBIright(_d,0);
    itersum<std::vector<mdouble>::iterator*>(sumUBIright,P->UBI.begin,Up_left+1);
    minus(sumUBIright,P->sumUBI,sumUBIright);

    for(;bound>=P->UBI.begin;bound--,iter--,ULRi--)
    {
        if(iter<=*ULRi)*bound=iter;
        else break;
    }
    //P->sumUBI=itersum(P->UBI.begin, Up_left+1)+sumUBIright;
    std::fill(P->sumUBI.begin(),P->sumUBI.end(),0);
    itersum<std::vector<mdouble>::iterator*>(P->sumUBI,P->UBI.begin,Up_left+1);
    plus(P->sumUBI,P->sumUBI,sumUBIright);

    iter=P->s+1;
    bound=Lp_right;

    tmp=*bound;
    for(;bound!=P->LBI.end;bound++,iter++)
    {
        if(iter>*bound)*bound=iter;
        else
        {
          //if(iter<*bound)P->sumLBI=P->sumLBI-*tmp+*(iter-1);
          if(iter<*bound)plusMinus(P->sumLBI,*(iter-1),*tmp);
          break;
        }
    }
}
return 1;
}











int mgiveBirth(std::vector<mPAT>::iterator&child, std::vector<mdouble>&v,
              mdouble&ME, std::vector<std::vector<mdouble> >&M){


int len=child->UBI.size();

//std::cout<<"in giveBirth, getting into findBound\n";
int boo=mFindBoundsCpp9_1_1(len,v,child->target,ME,child->LBI,child->sumLBI,child->UBI,child->sumUBI,M);
//std::cout<<"in giveBirth, getting out findBound\n";
//std::cout<<"just out of find bound function, boo="<<boo<<std::endl;

// std::ofstream myfile;
// myfile.open("C:/Users/Charles Liu/Desktop/experiment/error.csv", std::ios_base::app);
// myfile<<"just out of findBound and boo=="<<boo<<std::endl;
// printBI(child->LBI, v, myfile);
// printBI(child->UBI, v, myfile);
// myfile.close();

// std::cout<<"sumLBI=="<<child->sumLBI<<"\n";
// std::cout<<"sumUBI=="<<child->sumUBI<<"\n";
// std::cout<<"boo=="<<boo<<"\n";

if(boo==0)return 0;// out of bound!
if(len==1)return 3;
if(boo==2)return 2;// a solution found!

//std::vector<NumericVector::iterator>::iterator LBi=child->LBI.begin, UBi=child->UBI.begin;
std::vector<mdouble>::iterator *LBi=child->LBI.begin, *UBi=child->UBI.begin;

int Min=*UBi-*LBi, temp=Min, i=1;
child->position=0;
LBi++;
UBi++;
for(;i!=len;i++,LBi++,UBi++)
{
  temp=*UBi-*LBi;
  if(Min>temp){Min=temp;child->position=i;}
}
if(child->position==0)
{
    //child->s=*child->LBI.begin();
    child->s=*child->LBI.begin;
    //child->send=*child->UBI.begin();
    child->send=*child->UBI.begin;
    //child->sumLBI=child->sumLBI-*child->s;
    minus(child->sumLBI,child->sumLBI,*child->s);
    //child->target=child->target-*child->s;
    minus(child->target,child->target,*child->s);
    //child->sumUBI=child->sumUBI-*child->send;
    minus(child->sumUBI,child->sumUBI,*child->send);
    //child->LBI.erase(child->LBI.begin());
    child->LBI.pop_front();
    //child->UBI.erase(child->UBI.begin());
    child->UBI.pop_front();
}
else if(child->position==len-1)
{
    //child->s=*(child->UBI.end()-1);
    child->s=*(child->UBI.end-1);
    //child->send=*(child->LBI.end()-1);
    child->send=*(child->LBI.end-1);
    //child->sumUBI=child->sumUBI-*child->s;
    minus(child->sumUBI,child->sumUBI,*child->s);
    //child->sumLBI=child->sumLBI-*child->send;
    minus(child->sumLBI,child->sumLBI,*child->send);
    //child->target=child->target-*child->s;
    minus(child->target,child->target,*child->s);
    child->LBI.pop_back();
    child->UBI.pop_back();
}
else
{
//     std::vector<NumericVector::iterator>::iterator
//       Lp=child->LBI.begin+child->position, Up=child->UBI.begin+child->position;
    std::vector<mdouble>::iterator
      *Lp=child->LBI.begin+child->position, *Up=child->UBI.begin+child->position;

    child->s=*Lp;
    //child->target=child->target-*(child->s);
    minus(child->target,child->target,*(child->s));
    child->send=*Up;
    //child->sumLBI-=**Lp;
    minus(child->sumLBI,child->sumLBI,**Lp);
    //child->sumUBI-=**Up;
    minus(child->sumUBI,child->sumUBI,**Up);
    child->LBI.erase(Lp);
    child->UBI.erase(Up);

//     std::cout<<"just after erase\n";
//     printBI(child->LBI,v);
//     printBI(child->UBI,v);

    //child->UBILeftReserve.assign(child->UBI.begin(), child->UBI.begin()+child->position);
    child->UBILeftReserve.assign(child->UBI.begin, child->UBI.begin+child->position);

//     std::cout<<"print UBILeftReserve\n";
//     printv(child->UBILeftReserve,v);

//     std::vector<NumericVector::iterator>::iterator Up_left=child->UBI.begin+child->position-1,
//       ULRi=child->UBILeftReserve.end()-1;

    std::vector<mdouble>::iterator*Up_left=child->UBI.begin+child->position-1;
    std::vector<std::vector<mdouble>::iterator>::iterator ULRi=child->UBILeftReserve.end()-1;

    std::vector<mdouble>::iterator iter=child->s-1;
    //std::vector<NumericVector::iterator>::iterator bound=Up_left;
    std::vector<mdouble>::iterator *bound=Up_left;
    //double sumUBIright=child->sumUBI-itersum(child->UBILeftReserve.begin(), child->UBILeftReserve.end());
    mdouble sumUBIright(child->sumUBI.size(),0);
    itersum(sumUBIright, child->UBILeftReserve.begin(), child->UBILeftReserve.end());
    minus(sumUBIright, child->sumUBI, sumUBIright);

    for(;bound>=child->UBI.begin;bound--,iter--,ULRi--)
    {
      if(iter<=*ULRi)*bound=iter;
      else break;
    }
    //child->sumUBI=itersum(child->UBI.begin(),Up_left+1)+sumUBIright;
    std::fill(child->sumUBI.begin(),child->sumUBI.end(),0);
    itersum(child->sumUBI, child->UBI.begin, Up_left+1);
    plus(child->sumUBI,child->sumUBI,sumUBIright);
}
return 1;
};













void mTTT_stack_1_1(int LEN, std::vector<mdouble>&v, mdouble&ME,
                std::vector<std::vector<int> >&result, int&sizeNeeded,
                std::vector<std::vector<mdouble> >&M, std::vector<mPAT>&SK,
                std::vector<mPAT>::iterator&SK_end, double&Duration){

// std::ofstream myfile;
// myfile.precision(3);
// myfile.setf(std::ios::fixed);
// myfile.setf(std::ios::showpoint);
// myfile.open("C:/Users/Charles Liu/Desktop/experiment/error.csv", std::ios_base::app);


int _d=SK.front().target.size();
if(LEN==1)
{
  //double Max=SK.front().target+ME, Min=Max-2*ME;
  mdouble Max(_d), Min(_d);
  plus(Max, SK.front().target, ME);
  minus(Min, SK.front().target, ME);

  std::vector<int>x(1);
  //for(NumericVector::iterator i=v.begin();i!=v.end();++i)
  for(std::vector<mdouble>::iterator i=v.begin();i!=v.end();++i)
  {
    //if(*i>=Min&&*i<=Max)
    if(greaterEqual(*i,Min)&&lessEqual(*i,Max))
    {
      x.front()=i-v.begin()+1;
      result.push_back(x);
    }
    //else if(*i>Max)break;
    else if(!lessEqual(*i,Max))break;
  }
  return;
}

if(SK.front().UBI.size()==0&&SK_end-SK.begin()<2)return;
int boo;
std::clock_t timeend=std::clock()+Duration*(double)CLOCKS_PER_SEC;
std::vector<int>common(LEN);
std::vector<int>::iterator xi;

//std::cout<<"1.2\n";

while(1){
//*SK_end=*(SK_end-1);
SK_end->copy(*(SK_end-1));

// myfile<<"just copied SK_end-1 to SK_end------------------------\n";
// myfile<<"address of (SK_end-1)->LBI.begin=="<<&*(SK_end-1)->LBI.begin<<std::endl;
// myfile<<"contents of (SK_end-1)->LBI==";printBI((SK_end-1)->LBI,v,myfile);
// myfile<<"contents of (SK_end-1)->UBI==";printBI((SK_end-1)->UBI,v,myfile);
// myfile<<"(SK_end-1)->position=="<<(SK_end-1)->position<<std::endl;
// myfile<<"(SK_end-1)->s=="<<(SK_end-1)->s-v.begin()+1<<std::endl;
// myfile<<"(SK_end-1)->send=="<<(SK_end-1)->send-v.begin()+1<<std::endl;
//
// myfile<<"address of (SK_end)->LBI.begin=="<<&*(SK_end)->LBI.begin<<std::endl;
// myfile<<"contents of (SK_end)->LBI==";printBI((SK_end)->LBI,v,myfile);
// myfile<<"contents of (SK_end)->UBI==";printBI((SK_end)->UBI,v,myfile);
// myfile<<"(SK_end)->position=="<<(SK_end)->position<<std::endl;
// myfile<<"(SK_end)->s=="<<(SK_end)->s-v.begin()+1<<std::endl;
// myfile<<"(SK_end)->send=="<<(SK_end)->send-v.begin()+1<<std::endl;
// myfile<<"------------------------\n\n";

boo=mgiveBirth(SK_end,v,ME,M);

// myfile<<"just ran giveBith------------------------\n";
// myfile<<"address of (SK_end-1)->LBI.begin=="<<&*(SK_end-1)->LBI.begin<<std::endl;
// myfile<<"contents of (SK_end-1)->LBI==";printBI((SK_end-1)->LBI,v,myfile);
// myfile<<"contents of (SK_end-1)->UBI==";printBI((SK_end-1)->UBI,v,myfile);
// myfile<<"(SK_end-1)->position=="<<(SK_end-1)->position<<std::endl;
// myfile<<"(SK_end-1)->s=="<<(SK_end-1)->s-v.begin()+1<<std::endl;
// myfile<<"(SK_end-1)->send=="<<(SK_end-1)->send-v.begin()+1<<std::endl;
//
// myfile<<"address of (SK_end)->LBI.begin=="<<&*(SK_end)->LBI.begin<<std::endl;
// myfile<<"contents of (SK_end)->LBI==";printBI((SK_end)->LBI,v,myfile);
// myfile<<"contents of (SK_end)->UBI==";printBI((SK_end)->UBI,v,myfile);
// myfile<<"(SK_end)->position=="<<(SK_end)->position<<std::endl;
// myfile<<"(SK_end)->s=="<<(SK_end)->s-v.begin()+1<<std::endl;
// myfile<<"(SK_end)->send=="<<(SK_end)->send-v.begin()+1<<std::endl;
// myfile<<"------------------------\n\n";



if(boo==1)
{
    ++SK_end;
    continue;
}
if(boo==3)
{
    xi=common.begin();
    int i=SK_end->LBI.back()-v.begin()+1, end=SK_end->UBI.back()-v.begin()+1, tmp;
    if(i>end){tmp=i;i=end;end=tmp;}
    for(std::vector<mPAT>::iterator SKi=SK.begin()+1;SKi!=SK_end;++SKi,++xi)*xi=SKi->s-v.begin()+1;
    for(;i<=end;i++)
    {
        common.back()=i;
        result.push_back(common);
    }
}
else if(boo==2)
{
    xi=common.begin();
    for(std::vector<mPAT>::iterator SKi=SK.begin()+1;SKi!=SK_end;++SKi,++xi)*xi=SKi->s-v.begin()+1;

    std::vector<mdouble>::iterator*i=SK_end->UBI.begin;
    for(;i!=SK_end->UBI.end;++i,++xi)*xi=*i-v.begin()+1;
    result.push_back(common);
}


//while(update(SK_end-1, v)==0)
while(mupdate(SK_end-1)==0)
{

//   myfile<<"right after update before --SK_end------------------------\n";
//   myfile<<"address of (SK_end-1)->LBI.begin=="<<&*(SK_end-1)->LBI.begin<<"\n";
//   myfile<<"contents of (SK_end-1)->LBI==";printBI((SK_end-1)->LBI,v,myfile);
//   myfile<<"contents of (SK_end-1)->UBI==";printBI((SK_end-1)->UBI,v,myfile);
//   myfile<<"(SK_end-1)->position=="<<(SK_end-1)->position<<"\n";
//   myfile<<"(SK_end-1)->s=="<<(SK_end-1)->s-v.begin()+1<<"\n";
//   myfile<<"(SK_end-1)->send=="<<(SK_end-1)->send-v.begin()+1<<"\n";
//
//   myfile<<"address of (SK_end)->LBI.begin=="<<&*(SK_end)->LBI.begin<<"\n";
//   myfile<<"contents of (SK_end)->LBI==";printBI((SK_end)->LBI,v,myfile);
//   myfile<<"contents of (SK_end)->UBI==";printBI((SK_end)->UBI,v,myfile);
//   myfile<<"(SK_end)->position=="<<(SK_end)->position<<"\n";
//   myfile<<"(SK_end)->s=="<<(SK_end)->s-v.begin()+1<<"\n";
//   myfile<<"(SK_end)->send=="<<(SK_end)->send-v.begin()+1<<"\n";
//   myfile<<"------------------------\n\n";
//
    --SK_end;
    if(SK_end-SK.begin()<2)return;
}
if((sizeNeeded!=0&&int(result.size())>=sizeNeeded)||(Duration!=-1&&std::clock()>timeend))break;
}
//myfile.close();
//std::cout<<result.size()<<"\n";
}





// // [[Rcpp::export]]
// List mFLSSS_SK0(int len, int vlen, DataFrame vr, NumericVector targetr, NumericVector MEr,
//                IntegerVector LB, IntegerVector UB, int sizeNeeded, double Duration){
//
// std::vector<std::vector<int> >result;
// if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
// else result.reserve(1024);
// std::vector<mPAT>SK(len+6);
//
// int _d=vr.size();
//
// std::vector<mdouble>v(vlen,mdouble(_d));
// for(int i=0,iend=vr.size();i!=iend;++i)
// {
//   NumericVector tmp=vr[i];
//   for(int j=0,jend=vlen;j!=jend;++j)
//   {
//     v[j][i]=tmp[j];
//   }
// }
//
// std::vector<std::vector<mdouble> >M(len);
// mtheMatrix(v,M);
//
//
//
//
//
// // parallel for starts here I believe:
//
//
//
//
//
// std::vector<mdouble>::iterator vbegin=v.begin();
//
// mPAT&SKbegin=SK.front();
// SKbegin.LBI.newSize(len);
// SKbegin.UBI.newSize(len);
//
// for(int i=0;i!=len;++i)
// {
//     SKbegin.LBI[i]=vbegin+LB[i]-1;
//     SKbegin.UBI[i]=vbegin+UB[i]-1;
// }
//
// //SKbegin.sumLBI=itersum(SKbegin.LBI.begin, SKbegin.LBI.end);
// SKbegin.sumLBI.resize(_d,0);
// itersum(SKbegin.sumLBI, SKbegin.LBI.begin, SKbegin.LBI.end);
//
// //SKbegin.sumUBI=itersum(SKbegin.UBI.begin, SKbegin.UBI.end);
// SKbegin.sumUBI.resize(_d,0);
// itersum(SKbegin.sumUBI, SKbegin.UBI.begin, SKbegin.UBI.end);
//
// // std::cout<<"here we print mdoubles\n";
// // mdoublePrint(SKbegin.sumLBI);std::cout<<"\n";
// // mdoublePrint(SKbegin.sumUBI);std::cout<<"\n";
//
// //SKbegin.target=target;
// SKbegin.target.assign(targetr.begin(), targetr.end());
//
// mdouble ME(MEr.begin(), MEr.end());
//
// //std::vector<std::vector<double> >M(len);
//
//
// //std::cout<<"1.0\n";
//
//
// std::vector<mPAT>::iterator SK_end=SK.begin()+1;
//
// mTTT_stack_1_1(len,v,ME,result,sizeNeeded,M,SK,SK_end,Duration);
//
// if(result.size()==0)return List(0);
//
// List lis(result.size());
//
// for(int i=0,iend=result.size();i!=iend;i++)lis[i]=result[i];
//
// return lis;
// }














void mTTT_stack_1_1_par(int LEN, std::vector<mdouble>&v, mdouble&ME,
                std::vector<std::vector<int> >&result, int&sizeNeeded,
                std::vector<std::vector<mdouble> >&M, std::vector<mPAT>&SK,
                std::vector<mPAT>::iterator&SK_end, std::clock_t endTime,
                volatile int&entireResultSize,
                int entireResultNeeded){

int _d=SK.front().target.size();
int resultOriginalSize=result.size();

if(LEN==1)
{
  mdouble Max(_d), Min(_d);
  plus(Max, SK.front().target, ME);
  minus(Min, SK.front().target, ME);

  std::vector<int>x(1);
  for(std::vector<mdouble>::iterator i=v.begin();i!=v.end();++i)
  {
    if(greaterEqual(*i,Min)&&lessEqual(*i,Max))
    {
      x.front()=i-v.begin()+1;
      result.push_back(x);
    }
    else if(!lessEqual(*i,Max))
    {
      int gap=result.size()-resultOriginalSize;
      if(gap!=0)entireResultSize+=gap;
      break;
    }
  }
  return;
}

if(SK.front().UBI.size()==0&&SK_end-SK.begin()<2)
{
  // int gap=result.size()-resultOriginalSize;
  // if(gap!=0)entireResultSize+=gap;
  return;
}

int boo;

std::vector<int>common(LEN);
std::vector<int>::iterator xi;

while(1){
SK_end->copy(*(SK_end-1));
boo=mgiveBirth(SK_end,v,ME,M);
if(boo==1)
{
    ++SK_end;
    continue;
}
if(boo==3)
{
    xi=common.begin();
    int i=SK_end->LBI.back()-v.begin()+1, end=SK_end->UBI.back()-v.begin()+1, tmp;
    if(i>end){tmp=i;i=end;end=tmp;}
    for(std::vector<mPAT>::iterator SKi=SK.begin()+1;SKi!=SK_end;++SKi,++xi)*xi=SKi->s-v.begin()+1;
    for(;i<=end;i++)
    {
        common.back()=i;
        result.push_back(common);
        //++entireResultSize;
    }
}
else if(boo==2)
{
    xi=common.begin();
    for(std::vector<mPAT>::iterator SKi=SK.begin()+1;SKi!=SK_end;++SKi,++xi)*xi=SKi->s-v.begin()+1;
    std::vector<mdouble>::iterator*i=SK_end->UBI.begin;
    for(;i!=SK_end->UBI.end;++i,++xi)*xi=*i-v.begin()+1;
    result.push_back(common);
    //++entireResultSize;
}

while(mupdate(SK_end-1)==0)
{
    --SK_end;
    if(SK_end-SK.begin()<2)
    {
      int gap=result.size()-resultOriginalSize;
      if(gap!=0){entireResultSize+=gap;}
      return;
    }
}


if(entireResultSize>=entireResultNeeded||int(result.size())>=sizeNeeded||
   std::clock()>endTime)break;
}

int gap=result.size()-resultOriginalSize;
if(gap!=0)entireResultSize+=gap;
}









//{

// // [[Rcpp::export]]
// List mFLSSS_SK_throw(int len, List vr,
//                     NumericVector targetr, NumericVector MEr,
// IntegerVector LB, IntegerVector UB, int sizeNeeded, double Duration){
//
// std::vector<std::vector<int> >result;
// if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
// else result.reserve(1024);
// std::vector<PAT>SK(len+6);
//
// //NumericVector::iterator vbegin=v.begin();
//
// std::vector<mdouble>v(vr.size());
// for(int i=0,iend=v.size();i!=iend;++i)
// {
//   NumericVector tmp=vr[i];
//   v[i].assign(tmp.begin(),tmp.end());
// }
//
// std::vector<mdouble>::iterator vbegin=v.begin();
//
// PAT&SKbegin=SK.front();
// SKbegin.LBI.newSize(len);
// SKbegin.UBI.newSize(len);
//
// for(int i=0;i!=len;++i)
// {
//     SKbegin.LBI[i]=vbegin+LB[i]-1;
//     SKbegin.UBI[i]=vbegin+UB[i]-1;
// }
//
// //SKbegin.sumLBI=itersum(SKbegin.LBI.begin, SKbegin.LBI.end);
// itersum(SKbegin.sumLBI,SKbegin.LBI.begin, SKbegin.LBI.end);
//
// //SKbegin.sumUBI=itersum(SKbegin.UBI.begin, SKbegin.UBI.end);
// itersum(SKbegin.sumUBI, SKbegin.UBI.begin, SKbegin.UBI.end);
//
// //SKbegin.target=target;
// SKbegin.target.assign(targetr.begin(),targetr.end());
//
// mdouble ME(MEr.begin(), MEr.end());
//
// //std::vector<std::vector<double> >M(len);
// std::vector<std::vector<mdouble> >M(len);
//
// theMatrix(v,M);
//
// std::vector<PAT>::iterator SK_end=SK.begin()+1;
// TTT_stack_1_1(len,v,ME,result,sizeNeeded,M,SK,SK_end,Duration);
// if(result.size()==0)return List(0);
//
//
// List lis(result.size());
// for(int i=0,iend=result.size();i!=iend;i++)lis[i]=result[i];
// std::vector<PAT>::iterator pati=SK.begin()+1;
// std::vector<int>I(3*(SK_end-SK.begin()-1));
// std::vector<double>J(I.size());
// std::vector<std::vector<int> >K(I.size());
// std::vector<int>::iterator Ii=I.begin();
// std::vector<double>::iterator Ji=J.begin();
// std::vector<std::vector<int> >::iterator Ki=K.begin();
// for(;pati!=SK_end;++pati)
// {
//     *Ii=pati->position;++Ii;
//     *Ii=pati->s-vbegin;++Ii;
//     *Ii=pati->send-vbegin;++Ii;
//     *Ji=pati->target;++Ji;
//     *Ji=pati->sumLBI;++Ji;
//     *Ji=pati->sumUBI;++Ji;
//     to_num(pati->LBI.begin, pati->LBI.end, Ki, vbegin);++Ki;
//     to_num(pati->UBI.begin, pati->UBI.end, Ki, vbegin);++Ki;
//     to_num(pati->UBILeftReserve.begin(), pati->UBILeftReserve.end(), Ki,vbegin);++Ki;
// }
// return List::create(Named("roots",lis),Named("node",List::create(I,J,K,M)));
// }
//
//
// // [[Rcpp::export]]
// List mFLSSS_SK_catch(int len, NumericVector v, double ME,
// IntegerVector I, NumericVector J, List LK, List LM,int sizeNeeded,double Duration){
// std::vector<std::vector<int> >K=as<std::vector<std::vector<int> > >(LK);
// std::vector<std::vector<double> >M=as<std::vector<std::vector<double> > >(LM);
// NumericVector::iterator vbegin=v.begin();
// std::vector<PAT>SK(len+6);
// std::vector<PAT>::iterator SK_end=SK.begin()+I.size()/3+1, pati=SK.begin()+1;
// IntegerVector::iterator Ii=I.begin();
// NumericVector::iterator Ji=J.begin();
// std::vector<std::vector<int> >::iterator Ki=K.begin();
// for(;pati!=SK_end;++pati)
// {
//     pati->position=*Ii;++Ii;
//     pati->s=vbegin+*Ii;++Ii;
//     pati->send=vbegin+*Ii;++Ii;
//     pati->target=*Ji;++Ji;
//     pati->sumLBI=*Ji;++Ji;
//     pati->sumUBI=*Ji;++Ji;
//     to_iter(pati->LBI,Ki,vbegin);++Ki;
//     to_iter(pati->UBI,Ki,vbegin);++Ki;
//     to_iter_stl(pati->UBILeftReserve,Ki,vbegin);++Ki;
// }
// std::vector<std::vector<int> >result;
// if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
// else result.reserve(1024);
// TTT_stack_1_1(len,v,ME,result,sizeNeeded,M,SK,SK_end,Duration);
// if(result.size()==0)return List(0);
// List lis(result.size());
// for(int i=0,iend=result.size();i!=iend;i++)lis[i]=result[i];
// return lis;
// }
//
//
//
// // [[Rcpp::export]]
// List mFLSSS_SK_catch_throw(int len, NumericVector v, double ME,
// IntegerVector I, NumericVector J, List LK, List LM,int sizeNeeded,double Duration){
// std::vector<std::vector<int> >K=as<std::vector<std::vector<int> > >(LK);
// std::vector<std::vector<double> >M=as<std::vector<std::vector<double> > >(LM);
// NumericVector::iterator vbegin=v.begin();
// std::vector<PAT>SK(len+6);
// std::vector<PAT>::iterator SK_end=SK.begin()+I.size()/3+1, pati=SK.begin()+1;
// IntegerVector::iterator Ii=I.begin();
// NumericVector::iterator Ji=J.begin();
// std::vector<std::vector<int> >::iterator Ki=K.begin();
// for(;pati!=SK_end;++pati)
// {
//     pati->position=*Ii;++Ii;
//     pati->s=vbegin+*Ii;++Ii;
//     pati->send=vbegin+*Ii;++Ii;
//     pati->target=*Ji;++Ji;
//     pati->sumLBI=*Ji;++Ji;
//     pati->sumUBI=*Ji;++Ji;
//     to_iter(pati->LBI,Ki,vbegin);++Ki;
//     to_iter(pati->UBI,Ki,vbegin);++Ki;
//     to_iter_stl(pati->UBILeftReserve,Ki,vbegin);++Ki;
// }
// std::vector<std::vector<int> >result;
// if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
// else result.reserve(1024);
// TTT_stack_1_1(len,v,ME,result,sizeNeeded,M,SK,SK_end,Duration);
// if(result.size()==0)return List(0);
// List lis(result.size());
// for(int i=0,iend=result.size();i!=iend;i++)lis[i]=result[i];
// pati=SK.begin()+1;
// std::vector<int>II(3*(SK_end-SK.begin()-1));
// std::vector<double>JJ(II.size());
// std::vector<std::vector<int> >KK(II.size());
// std::vector<int>::iterator IIi=II.begin();
// std::vector<double>::iterator JJi=JJ.begin();
// std::vector<std::vector<int> >::iterator KKi=KK.begin();
// for(;pati!=SK_end;++pati)
// {
//     *IIi=pati->position;++IIi;
//     *IIi=pati->s-vbegin;++IIi;
//     *IIi=pati->send-vbegin;++IIi;
//     *JJi=pati->target;++JJi;
//     *JJi=pati->sumLBI;++JJi;
//     *JJi=pati->sumUBI;++JJi;
//     to_num(pati->LBI.begin, pati->LBI.end, KKi,vbegin);++KKi;
//     to_num(pati->UBI.begin, pati->UBI.end,KKi,vbegin);++KKi;
//     to_num(pati->UBILeftReserve.begin(),pati->UBILeftReserve.end(),KKi,vbegin);++KKi;
// }
// return List::create(Named("roots",lis),Named("node",List::create(II,JJ,KK,LM)));
// }

//}




















struct lowHigh{int low, high;};



struct para: public Worker{
std::vector<mdouble>&v;
std::vector<std::vector<mdouble> >&M;
mdouble&mTarget;
mdouble&mME;
int&len;
std::clock_t&tlimit;
//std::vector<int>&LB;
//std::vector<int>&UB;
std::vector<std::vector<mdouble>::iterator>&commonLBI;
std::vector<std::vector<mdouble>::iterator>&commonUBI;
mdouble&commonSumLBI;
mdouble&commonSumUBI;
int&sizeNeeded;
int&sizeNeededForAll;
std::vector<std::vector<std::vector<int> > >&rst;
mdouble&thek;
std::vector<double>&keyTarget;
std::vector<lowHigh>&paraInd;
volatile int&solutionsObtained;

para(std::vector<mdouble>&v,
     std::vector<std::vector<mdouble> >&M,
     mdouble&mTarget,
     mdouble&mME,
     int&len,
     std::clock_t&tlimit,
     std::vector<std::vector<mdouble>::iterator>&commonLBI,
     std::vector<std::vector<mdouble>::iterator>&commonUBI,
     mdouble&commonSumLBI,
     mdouble&commonSumUBI,
     int&sizeNeeded,
     int&sizeNeededForAll,
     std::vector<std::vector<std::vector<int> > >&rst,
     mdouble&thek,
     std::vector<double>&keyTarget,
     std::vector<lowHigh>&paraInd,
     volatile int&solutionsObtained):
  v(v), M(M), mTarget(mTarget), mME(mME), len(len), tlimit(tlimit),
  //LB(LB), UB(UB),
  commonLBI(commonLBI),
  commonUBI(commonUBI),
  commonSumLBI(commonSumLBI),
  commonSumUBI(commonSumUBI),
  sizeNeeded(sizeNeeded), sizeNeededForAll(sizeNeededForAll),
  rst(rst), thek(thek), keyTarget(keyTarget),
  paraInd(paraInd),solutionsObtained(solutionsObtained){}

void operator()(std::size_t st, std::size_t end){
for(;st!=end;++st)
{
  int _d=mME.size();
  rst[st].reserve(sizeNeededForAll+6);
  // //int i=0,iend=0;
  // std::ofstream myfile;
  // if(st==0)myfile.open("C:/Users/Charles Liu/Desktop/test/i_0.csv");
  // else if(st==1)myfile.open("C:/Users/Charles Liu/Desktop/test/i_1.csv");
  // else if(st==2)myfile.open("C:/Users/Charles Liu/Desktop/test/i_2.csv");

  for(int i=paraInd[st].low,iend=paraInd[st].high;;++i)
  {
    //myfile<<i<<","<<solutionsObtained<<"\n";

    if(i==iend||solutionsObtained>=sizeNeededForAll){break;}
    std::vector<mPAT>SK(len+6);
    //std::vector<mdouble>::iterator vbegin=v.begin();
    mPAT&SKbegin=SK.front();

    SKbegin.LBI.assign(&*commonLBI.begin(),&*commonLBI.end());
    SKbegin.UBI.assign(&*commonUBI.begin(),&*commonUBI.end());

    SKbegin.sumLBI.assign(commonSumLBI.begin(),commonSumLBI.end());
    SKbegin.sumUBI.assign(commonSumUBI.begin(),commonSumUBI.end());

    SKbegin.target.resize(_d);
    SKbegin.target.front()=keyTarget[i];

    for(int j=1,jend=_d;j!=jend;++j)
      SKbegin.target[j]=thek[j-1]*keyTarget[i]+mTarget[j-1];


    std::vector<mPAT>::iterator SK_end=SK.begin()+1;

    //std::cout<<"start mTTT, solutionObtained="<<solutionsObtained<<std::endl;

    mTTT_stack_1_1_par(len,v,mME,rst[st],sizeNeeded,M,SK,SK_end,
                       tlimit,solutionsObtained,sizeNeededForAll);

  }
}
}
};






// // [[Rcpp::export]]
List mFLSSS_SK_par(int maxCore, int len, int vlen, DataFrame vr, NumericVector keyTargetr,
                   NumericVector targetr, NumericVector thekr,
                   NumericVector MEr, IntegerVector LBr, IntegerVector UBr,
                   int sizeNeededForAll, int sizeNeeded, double Duration){

std::vector<std::vector<std::vector<int> > >result(maxCore);
// if(sizeNeeded!=0)result.reserve(sizeNeeded+6);
// else result.reserve(1024);
// std::vector<mPAT>SK(len+6);

int _d=vr.size();

std::vector<mdouble>v(vlen,mdouble(_d));
for(int i=0,iend=vr.size();i!=iend;++i)
{
  NumericVector tmp=vr[i];
  for(int j=0,jend=vlen;j!=jend;++j)
  {
    v[j][i]=tmp[j];
  }
}

std::vector<std::vector<mdouble> >M(len);
mtheMatrix(v,M);


std::clock_t tlimit=std::clock()+Duration*(double)CLOCKS_PER_SEC;
mdouble mTarget(targetr.begin(),targetr.end());
mdouble mME(MEr.begin(),MEr.end());
std::vector<int>LB(LBr.begin(),LBr.end()),UB(UBr.begin(),UBr.end());
std::vector<double>keyTarget(keyTargetr.begin(),keyTargetr.end());
std::vector<lowHigh>paraInd(maxCore);
int blockSize=keyTarget.size()/maxCore;
for(int i=0;i!=maxCore;++i)
{
  paraInd[i].low=i*blockSize;
  paraInd[i].high=paraInd[i].low+blockSize;
}
paraInd.back().high=keyTarget.size();
mdouble thek(thekr.begin(),thekr.end());
volatile int solutionsObtained=0;


std::vector<std::vector<mdouble>::iterator>commonLBI(len);
std::vector<std::vector<mdouble>::iterator>commonUBI(len);
mdouble commonSumLBI(_d, 0);
mdouble commonSumUBI(_d, 0);



for(int j=0;j!=len;++j)
{
  commonLBI[j]=v.begin()+LB[j]-1;
  commonUBI[j]=v.begin()+UB[j]-1;
}

itersum(commonSumLBI, commonLBI.begin(), commonLBI.end());
itersum(commonSumUBI, commonUBI.begin(), commonUBI.end());

//std::cout<<"start para\n";
para T(v,M,mTarget,mME,len,tlimit,
       commonLBI, commonUBI, commonSumLBI, commonSumUBI,
       sizeNeeded,sizeNeededForAll,result,
       thek,keyTarget,paraInd,solutionsObtained);

parallelFor(0, maxCore, T);
//std::cout<<"para ended\n";

int NofSolution=0;
for(std::vector<std::vector<std::vector<int> > >::iterator j=result.begin();j!=result.end();++j)
  NofSolution+=j->size();

if(NofSolution==0)return List::create();

List lis(NofSolution);
for(int i=0,j=0,iend=result.size();i!=iend;++i)
{
  for(int k=0,kend=result[i].size();k!=kend;++k)
  {
    lis[j]=result[i][k];
    ++j;
  }
}

return lis;
}















// // [[Rcpp::export]]
List mFLSSS_SK(int len, int vlen, DataFrame vr, NumericVector keyTargetr,
                   NumericVector targetr, NumericVector thekr,
                   NumericVector MEr, IntegerVector LBr, IntegerVector UBr,
                   int sizeNeededForAll, int sizeNeeded, double Duration){

int _d=vr.size();

std::vector<mdouble>v(vlen,mdouble(_d));
for(int i=0,iend=vr.size();i!=iend;++i)
{
  NumericVector tmp=vr[i];
  for(int j=0,jend=vlen;j!=jend;++j)
  {
    v[j][i]=tmp[j];
  }
}

std::vector<std::vector<mdouble> >M(len);
mtheMatrix(v,M);


std::clock_t tlimit=std::clock()+Duration*(double)CLOCKS_PER_SEC;
mdouble mTarget(targetr.begin(),targetr.end());
mdouble mME(MEr.begin(),MEr.end());
std::vector<int>LB(LBr.begin(),LBr.end()),UB(UBr.begin(),UBr.end());
std::vector<double>keyTarget(keyTargetr.begin(),keyTargetr.end());


mdouble thek(thekr.begin(),thekr.end());
volatile int solutionsObtained=0;

std::vector<std::vector<mdouble>::iterator>commonLBI(len);
std::vector<std::vector<mdouble>::iterator>commonUBI(len);
mdouble commonSumLBI(_d, 0);
mdouble commonSumUBI(_d, 0);

for(int j=0;j!=len;++j)
{
  commonLBI[j]=v.begin()+LB[j]-1;
  commonUBI[j]=v.begin()+UB[j]-1;
}

itersum(commonSumLBI, commonLBI.begin(), commonLBI.end());
itersum(commonSumUBI, commonUBI.begin(), commonUBI.end());

std::vector<std::vector<int> >rst;
rst.reserve(sizeNeededForAll+6);

for(int i=0,iend=keyTarget.size();;++i)
{
  if(i==iend||solutionsObtained>=sizeNeededForAll){break;}
  std::vector<mPAT>SK(len+6);
  mPAT&SKbegin=SK.front();

  SKbegin.LBI.assign(&*commonLBI.begin(),&*commonLBI.end());
  SKbegin.UBI.assign(&*commonUBI.begin(),&*commonUBI.end());

  SKbegin.sumLBI.assign(commonSumLBI.begin(),commonSumLBI.end());
  SKbegin.sumUBI.assign(commonSumUBI.begin(),commonSumUBI.end());

  SKbegin.target.resize(_d);
  SKbegin.target.front()=keyTarget[i];

  for(int j=1,jend=_d;j!=jend;++j)
  SKbegin.target[j]=thek[j-1]*keyTarget[i]+mTarget[j-1];

  std::vector<mPAT>::iterator SK_end=SK.begin()+1;
  mTTT_stack_1_1_par(len,v,mME,rst,sizeNeeded,M,SK,SK_end,
                       tlimit,solutionsObtained,sizeNeededForAll);
}

if(rst.size()==0)return List::create();

List lis(rst.size());
for(int i=0,iend=rst.size();i!=iend;++i)
  lis[i]=rst[i];
return lis;
}






// // [[Rcpp::export]]
List mFLSSS_SK_como(int len, int vlen, DataFrame vr,
                    NumericVector targetr,
                    NumericVector MEr, IntegerVector LBr, IntegerVector UBr,
                    int sizeNeededForAll,
                    double Duration){

int _d=vr.size();

std::vector<mdouble>v(vlen,mdouble(_d));
for(int i=0,iend=vr.size();i!=iend;++i)
{
  NumericVector tmp=vr[i];
  for(int j=0,jend=vlen;j!=jend;++j)
  {
    v[j][i]=tmp[j];
  }
}

std::vector<std::vector<mdouble> >M(len);
mtheMatrix(v,M);


//mdouble mTarget(targetr.begin(),targetr.end());
mdouble mME(MEr.begin(),MEr.end());
std::vector<int>LB(LBr.begin(),LBr.end()),UB(UBr.begin(),UBr.end());

std::vector<std::vector<int> >rst;
rst.reserve(sizeNeededForAll+6);

std::vector<mPAT>SK(len+6);
mPAT&SKbegin=SK.front();

SKbegin.LBI.newSize(len);
SKbegin.UBI.newSize(len);
SKbegin.sumLBI.resize(_d,0);
SKbegin.sumUBI.resize(_d,0);

for(int j=0;j!=len;++j)
{
  SKbegin.LBI[j]=v.begin()+LB[j]-1;
  SKbegin.UBI[j]=v.begin()+UB[j]-1;
}

itersum(SKbegin.sumLBI, &*SKbegin.LBI.begin, &*SKbegin.LBI.end);
itersum(SKbegin.sumUBI, &*SKbegin.UBI.begin, &*SKbegin.UBI.end);

//SKbegin.target.resize(_d);
SKbegin.target.assign(targetr.begin(),targetr.end());

std::vector<mPAT>::iterator SK_end=SK.begin()+1;

mTTT_stack_1_1(len,v,mME,rst,sizeNeededForAll,M,SK,SK_end,Duration);
if(rst.size()==0)return List::create();
List lis(rst.size());
for(int i=0,iend=rst.size();i!=iend;++i)
  lis[i]=rst[i];
return lis;
}












// mFLSSS_SK_par
List mFLSSS_SK_par(int maxCore, int len, int vlen, DataFrame vr, NumericVector keyTargetr, NumericVector targetr, NumericVector thekr, NumericVector MEr, IntegerVector LBr, IntegerVector UBr, int sizeNeededForAll, int sizeNeeded, double Duration);
RcppExport SEXP FLSSS_mFLSSS_SK_par(SEXP maxCoreSEXP, SEXP lenSEXP, SEXP vlenSEXP, SEXP vrSEXP, SEXP keyTargetrSEXP, SEXP targetrSEXP, SEXP thekrSEXP, SEXP MErSEXP, SEXP LBrSEXP, SEXP UBrSEXP, SEXP sizeNeededForAllSEXP, SEXP sizeNeededSEXP, SEXP DurationSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type maxCore(maxCoreSEXP);
    Rcpp::traits::input_parameter< int >::type len(lenSEXP);
    Rcpp::traits::input_parameter< int >::type vlen(vlenSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type vr(vrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type keyTargetr(keyTargetrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type targetr(targetrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thekr(thekrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type MEr(MErSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type LBr(LBrSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type UBr(UBrSEXP);
    Rcpp::traits::input_parameter< int >::type sizeNeededForAll(sizeNeededForAllSEXP);
    Rcpp::traits::input_parameter< int >::type sizeNeeded(sizeNeededSEXP);
    Rcpp::traits::input_parameter< double >::type Duration(DurationSEXP);
    __result = Rcpp::wrap(mFLSSS_SK_par(maxCore, len, vlen, vr, keyTargetr, targetr, thekr, MEr, LBr, UBr, sizeNeededForAll, sizeNeeded, Duration));
    return __result;
END_RCPP
}
// mFLSSS_SK
List mFLSSS_SK(int len, int vlen, DataFrame vr, NumericVector keyTargetr, NumericVector targetr, NumericVector thekr, NumericVector MEr, IntegerVector LBr, IntegerVector UBr, int sizeNeededForAll, int sizeNeeded, double Duration);
RcppExport SEXP FLSSS_mFLSSS_SK(SEXP lenSEXP, SEXP vlenSEXP, SEXP vrSEXP, SEXP keyTargetrSEXP, SEXP targetrSEXP, SEXP thekrSEXP, SEXP MErSEXP, SEXP LBrSEXP, SEXP UBrSEXP, SEXP sizeNeededForAllSEXP, SEXP sizeNeededSEXP, SEXP DurationSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type len(lenSEXP);
    Rcpp::traits::input_parameter< int >::type vlen(vlenSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type vr(vrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type keyTargetr(keyTargetrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type targetr(targetrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thekr(thekrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type MEr(MErSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type LBr(LBrSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type UBr(UBrSEXP);
    Rcpp::traits::input_parameter< int >::type sizeNeededForAll(sizeNeededForAllSEXP);
    Rcpp::traits::input_parameter< int >::type sizeNeeded(sizeNeededSEXP);
    Rcpp::traits::input_parameter< double >::type Duration(DurationSEXP);
    __result = Rcpp::wrap(mFLSSS_SK(len, vlen, vr, keyTargetr, targetr, thekr, MEr, LBr, UBr, sizeNeededForAll, sizeNeeded, Duration));
    return __result;
END_RCPP
}





// mFLSSS_SK_como
List mFLSSS_SK_como(int len, int vlen, DataFrame vr, NumericVector targetr, NumericVector MEr, IntegerVector LBr, IntegerVector UBr, int sizeNeededForAll, double Duration);
RcppExport SEXP FLSSS_mFLSSS_SK_como(SEXP lenSEXP, SEXP vlenSEXP, SEXP vrSEXP, SEXP targetrSEXP, SEXP MErSEXP, SEXP LBrSEXP, SEXP UBrSEXP, SEXP sizeNeededForAllSEXP, SEXP DurationSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type len(lenSEXP);
    Rcpp::traits::input_parameter< int >::type vlen(vlenSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type vr(vrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type targetr(targetrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type MEr(MErSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type LBr(LBrSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type UBr(UBrSEXP);
    Rcpp::traits::input_parameter< int >::type sizeNeededForAll(sizeNeededForAllSEXP);
    Rcpp::traits::input_parameter< double >::type Duration(DurationSEXP);
    __result = Rcpp::wrap(mFLSSS_SK_como(len, vlen, vr, targetr, MEr, LBr, UBr, sizeNeededForAll, Duration));
    return __result;
END_RCPP
}



