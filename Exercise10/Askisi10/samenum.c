#define MAXN 1000000
#define MAXV 2000000

//frama-c version Phosphorus-20170501



/*@ predicate validres{L}(int n, int res, int *a) =
  @     (\exists integer i,j; 0 <= i < j < n && \at(a[i],L) == \at(a[j],L) && res == j - i
              && (\forall integer k,l; 0<= k < l < n && (k != i || l != j) && \at(a[k],L) == \at(a[l],L) ==> res >= l - k)) ==> res != 0;
  @*/

/*@ requires 2 <= N < MAXN;
  @ requires \valid (x + (0..(N-1)));
  @ requires \forall integer i; 0 <= i < N ==> 1 <= x[i] < MAXV;
  @ assigns \nothing;
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
  //@ assert best == 0;
  /*@ loop assigns i , p[1..(MAXV)],best;
    @ loop invariant 0 <= i <= N;

    @ loop invariant \forall integer k; 0<= k < i ==> p[x[k]] != -1;

    @ loop invariant (\forall integer k; 0<= k < i ==> p[x[k]] == k) ==> best == 0;

    @ loop invariant !(\exists integer k,l; 0<= k < l < i && x[k] == x[l] && best == l- k) ==> (\forall integer k,l; 0<= l < k < i ==> x[l] != x[k] || best != l - k) ;

    @ loop invariant \forall integer k; 0<= k < i ==> (\exists integer l; 0<= l < k && p[x[k]] == l ==> x[k] == x[l]);

    @ loop invariant \forall integer k; 0<= k < i && k - p[x[k]] > 0 ==> best >= k - p[x[k]];

    @ loop invariant \exists integer k,l; 0 <= k < l < i && x[k] == x[l] ==> best != \at(best,Pre) && best == l - k;

    @ loop invariant \forall integer k; 0<= k < i ==> (p[x[k]] == k ==> \forall integer l; 0<= l < k ==> x[l] != x[k]);

    @ loop invariant \forall integer k; 0 <= k < i ==> \forall integer l; 0 <= l < p[x[k]] ==> x[l] != x[k];

    @ loop invariant best == \at(best,Pre) ==> (\forall integer k,l; 0<= k < l < i ==> x[l] != x[k]);

    @ loop invariant best != \at(best,Pre) ==> (\forall integer k; 0<= k < i && k != p[x[k]] ==> best >= k-p[x[k]]);

    @ loop invariant \forall integer k; 0<= k < i ==> p[x[k]] != k ==> (\exists integer l; 0<= l < k < i && p[x[k]] == l ==> x[k] == x[l]);

    @ loop invariant \forall integer k; i <= k < N && p[x[k]] != -1 ==>
                          \exists integer y; 0<= y < i && p[x[k]] == y;

    @ loop variant N - i;
  @*/
  for (int i = 0; i < N; ++i)
    if (p[x[i]] == -1) {
      p[x[i]]= i;
      //@ assert p[x[i]] == i;
      //@ assert \forall integer k; 0<= k < i ==> x[k] != x[i];
    }
    else {
      //@ assert p[x[i]] < i && p[x[i]] != -1;
      if (i-p[x[i]] > best) {
      best = i - p[x[i]];
      //@ assert \exists integer k; 0<= k < i && p[x[i]] == k && best == i - k;
    }
  }
  return best;

}
