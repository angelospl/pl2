#define MAXN 1000000
#define MAXV 2000000

/*@ predicate solexists{L}(int n,int res, int *a) =
  @     \exists integer i,j; 0<= i < j < n ==> \at(a[i],L) == \at(a[j],L) && res == j - i ;
  @*/

/*@ predicate noothersol{L}(int n,int res, int *a) =
  @     \forall integer i,j; 0<= i < j < n ==> a[i] == a[j] ==> res >= j - i ;
  @
  @*/

/*@ predicate validres{L}(int n, int res, int *a) =
  @     res == 0 ? solexists {L} (n,res,a) : noothersol {L} (n,res,a);
  @*/


/*@ requires 2 <= N < MAXN;
  @ requires \valid (x + (0..(N-1)));
  @ requires \forall integer i; 0 <= i < N ==> 1 <= x[i] < MAXV;
  @ assigns \nothing;
  @ ensures 0 <= \result <= MAXV;
  @ ensures validres(N,\result,x);
@*/
int samenum(int N,int *x) {
  int p[MAXV+1];
  /*@ loop assigns i, p[0..(MAXV)];
    @ loop invariant 0 <= i <= (MAXV + 1);
    @ loop invariant \forall integer k; 0 <= k < i ==> p[k] == -1;
    @ loop variant MAXV - i;
  @*/
  for (int i = 0;i <= MAXV; ++i) p[i] = -1;
  /*@ assigns best; @*/
  int best = 0;

  /*@ loop assigns i , p[0..(MAXV)],best;
    @ loop invariant 0 <= i < N;
    @ loop invariant \forall integer k; 0<= k < i ==> p[x[k]] > -1;
    @ loop invariant \forall integer k; 0<= k < i ==> best >= i - p[x[k]];
    @ loop variant N - i;
  @*/
  for (int i = 0; i < N; ++i)
    if (p[x[i]] == -1) p[x[i]]= i;
    else if (i-p[x[i]] > best) best = i - p[x[i]];
  return best;

}
