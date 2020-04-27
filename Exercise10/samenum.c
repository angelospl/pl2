#define MAXN 1000000
#define MAXV 2000000

/*@ predicate solexists{L}(int n,int res, int *a) =
  @     \exists integer i,j; 0<= i < j < n ==> \at(a[i],L) == \at(a[j],L) && res == j - i ;
  @*/

/*@ predicate noothersol{L}(int n,int res, int *a) =
  @     \forall integer i,j; 0<= i < j < n && \at(a[i],L) == \at(a[j],L) ==> res >= j - i ;
  @
  @*/

/*@ predicate validres{L}(int n, int res, int *a) =
  @     noothersol {L} (n,res,a) ? solexists {L} (n,res,a) : res == 0;
  @*/


/*@ requires 2 <= N < MAXN;
  @ requires \valid (x + (0..(N-1)));
  @ requires \forall integer i; 0 <= i < N ==> 1 <= x[i] < MAXV;
  @ assigns \nothing;
  @ ensures 0 <= \result < N;
  @ ensures validres(N,\result,x);
@*/
int samenum(int N,int *x) {
  int p[MAXV+1];
  /*@ loop assigns i, p[0..(MAXV)];
    @ loop invariant 0 <= i <= (MAXV+1);
    @ loop invariant \forall integer k; 0 <= k < i ==> p[k] == -1;
    @ loop variant MAXV - i;
  @*/
  for (int i = 0;i <= MAXV; ++i) p[i] = -1;
  int best = 0;

  /*@ loop assigns i , p[0..(MAXV)],best;
    @ loop invariant 0 <= i <= N;
    @ loop invariant \forall integer k; 0 <= k <= MAXV ==> -1 <= p[k] < N;
    @ loop invariant \forall integer k; 0<= k < i ==>
    @      \exists integer y; y < k && x[y] == x[k] ==> best == k - p[x[y]]; 
    @ loop variant N - i;
  @*/
  for (int i = 0; i < N; ++i)
    if (p[x[i]] == -1) {
      p[x[i]]= i;
    }
    else {
      if (i-p[x[i]] > best) best = i - p[x[i]];
    }
  return best;

}
