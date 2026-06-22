/* nelisp-cfront libc subset (M3) — written in C, compiled by nelisp-cfront
 * itself (dogfooding the integer-C + pointer/array support from M2).
 * Names are nlcf_-prefixed to avoid clashing with the host libc when the
 * object is linked into a program that also includes <string.h>. */

void *nlcf_memcpy(char *d, char *s, long n) {
  for (long i = 0; i < n; i = i + 1) d[i] = s[i];
  return d;
}

void *nlcf_memset(char *d, long c, long n) {
  for (long i = 0; i < n; i = i + 1) d[i] = c;
  return d;
}

long nlcf_strlen(char *s) {
  long n = 0;
  while (s[n] != 0) n = n + 1;
  return n;
}

int nlcf_strcmp(char *a, char *b) {
  long i = 0;
  while (a[i] != 0 && a[i] == b[i]) i = i + 1;
  return a[i] - b[i];
}

char *nlcf_strcpy(char *d, char *s) {
  long i = 0;
  while (s[i] != 0) { d[i] = s[i]; i = i + 1; }
  d[i] = 0;
  return d;
}

/* Structured (no early-return-in-loop, which is an M4 feature): walk until
 * a mismatch or the end, then decide after the loop. */
int nlcf_memcmp(char *a, char *b, long n) {
  long i = 0;
  while (i < n && a[i] == b[i]) i = i + 1;
  if (i == n) return 0;
  return a[i] - b[i];
}
