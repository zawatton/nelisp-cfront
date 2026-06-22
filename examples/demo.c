/* nelisp-cfront demo: compiled C → native via the nelisp-cc grammar */
long fact(long n){ if (n <= 1) return 1; return n * fact(n - 1); }
long sum_evens(long n){
  long s = 0;
  for (long i = 0; i < n; i = i + 1)
    if ((i & 1) == 0) s = s + i;
  return s;
}
