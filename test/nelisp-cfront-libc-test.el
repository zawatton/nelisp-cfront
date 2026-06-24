;;; nelisp-cfront-libc-test.el --- M3 libc-in-C compiled by cfront -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M3 — compile examples/libc.c (a libc subset written in C) with
;; nelisp-cfront itself, link it with a C driver, and verify the
;; functions behave like libc.  Dogfoods M2 (loops/pointers/arrays).
;; Skips when the backend or cc are unavailable.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)
(require 'nelisp-cfront-e2e-test)        ; reuse the run helper

(defconst nelisp-cfront-libc-test--dir
  (file-name-directory (or load-file-name buffer-file-name
                           (expand-file-name "test/x")))
  "Directory of this test file, captured at load time.")

(defun nelisp-cfront-libc-test--file ()
  (expand-file-name "../examples/libc.c" nelisp-cfront-libc-test--dir))

(ert-deftest nelisp-cfront-libc-string-mem ()
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc (with-temp-buffer
                 (insert-file-contents (nelisp-cfront-libc-test--file))
                 (buffer-string)))
         (drv "
#include <stdio.h>
extern void *nlcf_memcpy(char*, char*, long);
extern void *nlcf_memset(char*, long, long);
extern long  nlcf_strlen(char*);
extern int   nlcf_strcmp(char*, char*);
extern char *nlcf_strcpy(char*, char*);
extern int   nlcf_memcmp(char*, char*, long);
int main(void){
  char buf[16]; char buf2[16];
  nlcf_memset(buf, 'A', 5); buf[5]=0;
  nlcf_memcpy(buf2, buf, 6);
  long l = nlcf_strlen(buf);
  int c1 = nlcf_strcmp(\"abc\",\"abc\");
  int c2 = nlcf_strcmp(\"abc\",\"abd\");
  char dst[8]; nlcf_strcpy(dst, \"hi\");
  int m = nlcf_memcmp(buf, buf2, 6);
  printf(\"%s %s %ld %d %d %s %d\\n\", buf, buf2, l, c1, (c2<0)?-1:1, dst, m);
  return (l==5 && c1==0 && c2<0 && m==0
          && buf[0]=='A' && buf2[4]=='A' && dst[0]=='h' && dst[1]=='i' && dst[2]==0)
         ? 0 : 1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "AAAAA AAAAA 5 0 -1 hi 0" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-libc-extern-call-e2e ()
  "A cfront-compiled function may CALL the real libc: a name that is
declared (a prototype) or implicitly declared but not defined in the unit
lowers to a PLT `extern-call', which the linker resolves against libc.
Here `mydup' calls strlen+malloc+memcpy, and the verbatim real-SQLite
`sqlite3Strlen30' calls strlen — both run correctly."
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
typedef unsigned long u64;
extern u64 strlen(const char *);
extern void *memcpy(void *, const void *, u64);
extern void *malloc(u64);
char *mydup(const char *s){ u64 n = strlen(s)+1; char *p=(char*)malloc(n); memcpy(p,s,n); return p; }
int sqlite3Strlen30(const char *z){ if( z==0 ) return 0; return 0x3fffffff & (int)strlen(z); }
")
         (drv "
#include <stdio.h>
#include <string.h>
extern char *mydup(const char *);
extern int sqlite3Strlen30(const char *);
int main(void){
  char *d = mydup(\"hello, libc!\");
  int l = sqlite3Strlen30(\"SELECT * FROM t;\");
  int z = sqlite3Strlen30(0);
  int ok = (strcmp(d,\"hello, libc!\")==0) && (l==16) && (z==0);
  printf(\"%s %d %d %s\\n\", d, l, z, ok?\"OK\":\"FAIL\");
  return ok?0:1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "hello, libc! 16 0 OK" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-multi-fn-module-intra-and-extern-e2e ()
  "Integration guard for whole-module compilation: several functions in ONE
cfront-compiled object, mixing same-unit DIRECT calls between defined
functions (`encode'->`putv', `decode'->`getv') with a PLT `extern-call' to
libc (`tag'->strlen).  Mirrors the real-SQLite varint cluster shape
(PutVarint->putVarint64, GetVarint32->GetVarint) at small scale; the whole
.o links against libc and runs."
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
typedef unsigned char u8;
typedef unsigned long u64;
extern u64 strlen(const char *);
/* leaf, called by encode/decode (same-unit direct calls); LEB128 codec */
int putv(u8 *p, u64 v){ int n=0; do { u8 c=v&0x7f; v>>=7; if(v) c|=0x80; p[n++]=c; } while(v); return n; }
u64 getv(const u8 *p){ u64 v=0; int sh=0,i=0; for(;;){ u8 c=p[i++]; v|=(u64)(c&0x7f)<<sh; sh+=7; if(!(c&0x80)) break; } return v; }
int encode(u8 *p, u64 v){ return putv(p, v); }     /* -> putv */
u64 decode(const u8 *p){ return getv(p); }         /* -> getv */
int tag(const char *s){ return (int)strlen(s); }   /* -> libc strlen (extern) */
")
         (drv "
#include <stdio.h>
typedef unsigned char u8;
typedef unsigned long u64;
extern int encode(u8*, u64);
extern u64 decode(const u8*);
extern int tag(const char*);
int main(void){
  u8 b[12]; u64 vals[]={0,1,127,128,300,0xfedcba9876543210UL};
  int ok=1;
  for(int i=0;i<6;i++){ int n=encode(b,vals[i]); u64 back=decode(b); if(back!=vals[i]||n<1) ok=0; }
  int t = tag(\"hello\");      /* 5 */
  ok = ok && (t==5);
  printf(\"tag=%d %s\\n\", t, ok?\"OK\":\"FAIL\");
  return ok?0:1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "tag=5 OK" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-builtin-bswap-e2e ()
  "`__builtin_bswap16/32/64' lower to inline byte-reversal (no external
symbol), so real code that uses them links and runs.  Includes the verbatim
real-SQLite big-endian 4-byte serializers `sqlite3Get4byte'/`sqlite3Put4byte'
(bswap32 + memcpy)."
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
typedef unsigned char u8;
typedef unsigned int u32;
typedef unsigned long u64;
extern void *memcpy(void *, const void *, u64);
u64 b16(u64 x){ return __builtin_bswap16((unsigned short)x); }
u64 b32(u64 x){ return __builtin_bswap32((u32)x); }
u64 b64(u64 x){ return __builtin_bswap64(x); }
u32 sqlite3Get4byte(const u8 *p){ u32 x; memcpy(&x,p,4); return __builtin_bswap32(x); }
void sqlite3Put4byte(unsigned char *p, u32 v){ u32 x = __builtin_bswap32(v); memcpy(p,&x,4); }
")
         (drv "
#include <stdio.h>
typedef unsigned char u8;
typedef unsigned int u32;
extern unsigned long b16(unsigned long), b32(unsigned long), b64(unsigned long);
extern u32 sqlite3Get4byte(const u8 *); extern void sqlite3Put4byte(unsigned char *, u32);
int main(void){
  unsigned char buf[4]; sqlite3Put4byte(buf, 0x12345678u);
  int ok = (b16(0xABCD)==0xCDAB) && (b32(0x11223344)==0x44332211)
        && (b64(0x0102030405060708UL)==0x0807060504030201UL)
        && buf[0]==0x12 && buf[1]==0x34 && buf[2]==0x56 && buf[3]==0x78
        && (sqlite3Get4byte(buf)==0x12345678u);
  printf(\"%lx %lx %lx %x %s\\n\", b16(0xABCD), b32(0x11223344),
         b64(0x0102030405060708UL), sqlite3Get4byte(buf), ok?\"OK\":\"FAIL\");
  return ok?0:1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "cdab 44332211 807060504030201 12345678 OK" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-variadic-extern-call-e2e ()
  "A cfront-compiled function may call a VARIADIC libc function: the args
past the fixed parameters are marked `:varargs' so the back-end sets the
SysV AL register (without it the call faults).  Exercises integer + string
varargs via `snprintf' (deterministic buffer output)."
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
extern int snprintf(char *, unsigned long, const char *, ...);
int fmt2(char *b, unsigned long n, int a, int c){ return snprintf(b, n, \"%d+%d=%d\", a, c, a+c); }
int fmts(char *b, unsigned long n, const char *s, int k){ return snprintf(b, n, \"[%s:%d]\", s, k); }
")
         (drv "
#include <stdio.h>
#include <string.h>
extern int fmt2(char*, unsigned long, int, int);
extern int fmts(char*, unsigned long, const char*, int);
int main(void){
  char a[32], b[32];
  int na = fmt2(a, sizeof a, 3, 4);          /* \"3+4=7\", 5 */
  int nb = fmts(b, sizeof b, \"id\", 42);      /* \"[id:42]\", 7 */
  int ok = (strcmp(a,\"3+4=7\")==0) && (na==5)
        && (strcmp(b,\"[id:42]\")==0) && (nb==7);
  printf(\"%s|%s|%d|%d|%s\\n\", a, b, na, nb, ok?\"OK\":\"FAIL\");
  return ok?0:1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "3+4=7|[id:42]|5|7|OK" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-defined-variadic-forward-e2e ()
  "A cfront-compiled function may DEFINE its own variadic `...': the
SysV AMD64 prologue lays down a 176-byte register-save-area (six GP regs
+ xmm0-7) below the param/let slots, and `__builtin_va_start' fills a
24-byte `__va_list_tag' (gp_offset / fp_offset / overflow_arg_area /
reg_save_area) so the list can be forwarded to a `v*printf'-style
callee.  This is the dominant real-world shape (= `xmlStrPrintf' ->
`vsnprintf').  Exercises the register path (3 varargs), the
overflow_arg_area path (5 varargs, 2 spilled to the stack), and a lone
string vararg — all compared byte-for-byte against glibc `vsnprintf'."
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
typedef __builtin_va_list va_list;
extern int vsnprintf(char *, unsigned long, const char *, va_list);
int myfmt(char *buf, unsigned long n, const char *fmt, ...){
  va_list ap; __builtin_va_start(ap, fmt);
  int r = vsnprintf(buf, n, fmt, ap);
  __builtin_va_end(ap);
  return r;
}
")
         (drv "
#include <stdio.h>
#include <string.h>
extern int myfmt(char *, unsigned long, const char *, ...);
int main(void){
  char b[64], b2[64], b3[64];
  int r  = myfmt(b,  sizeof b,  \"%d-%s-%d\", 7, \"ok\", 99);      /* reg path */
  int r2 = myfmt(b2, sizeof b2, \"%d,%d,%d,%d,%d\", 1,2,3,4,5);  /* overflow */
  int r3 = myfmt(b3, sizeof b3, \"<%s>\", \"hello\");
  int ok = (strcmp(b,\"7-ok-99\")==0) && (r==7)
        && (strcmp(b2,\"1,2,3,4,5\")==0) && (r2==9)
        && (strcmp(b3,\"<hello>\")==0) && (r3==7);
  printf(\"%s|%d|%s|%d|%s|%d|%s\\n\", b, r, b2, r2, b3, r3, ok?\"OK\":\"FAIL\");
  return ok?0:1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "7-ok-99|7|1,2,3,4,5|9|<hello>|7|OK" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-defined-variadic-va-arg-e2e ()
  "A cfront-compiled variadic function may WALK its own `...' with
`__builtin_va_arg' (GP class): the lowering reads `gp_offset' from the
`va_list', loads from `reg_save_area + gp_offset' (register path) or
`overflow_arg_area' (stack path), and advances the cursor.  Covers the
register path, the overflow path (> 5 GP varargs), signed-`int' sign
extension of negatives, 8-byte `long', and `char *' pointer args."
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
typedef __builtin_va_list va_list;
extern unsigned long strlen(const char *);
long sum_ints(int count, ...){
  va_list ap; __builtin_va_start(ap, count);
  long s = 0;
  for (int i = 0; i < count; i++) s += __builtin_va_arg(ap, int);
  __builtin_va_end(ap);
  return s;
}
long sum_longs(int count, ...){
  va_list ap; __builtin_va_start(ap, count);
  long s = 0;
  for (int i = 0; i < count; i++) s += __builtin_va_arg(ap, long);
  __builtin_va_end(ap);
  return s;
}
long total_len(int count, ...){
  va_list ap; __builtin_va_start(ap, count);
  long t = 0;
  for (int i = 0; i < count; i++){ const char *s = __builtin_va_arg(ap, char *); t += (long)strlen(s); }
  __builtin_va_end(ap);
  return t;
}
")
         (drv "
#include <stdio.h>
extern long sum_ints(int, ...);
extern long sum_longs(int, ...);
extern long total_len(int, ...);
int main(void){
  long a = sum_ints(5, 10,20,30,40,50);                 /* reg path  = 150 */
  long b = sum_ints(7, 1,2,3,4,5,6,7);                  /* overflow  = 28  */
  long c = sum_ints(3, -5, 10, -2);                     /* negatives = 3   */
  long d = sum_longs(4, 1000000000000L, 2L, 3L, 4L);    /* 8-byte    = 1000000000009 */
  long e = total_len(3, \"ab\", \"cde\", \"f\");             /* ptr walk  = 6   */
  int ok = (a==150)&&(b==28)&&(c==3)&&(d==1000000000009L)&&(e==6);
  printf(\"%ld|%ld|%ld|%ld|%ld|%s\\n\", a,b,c,d,e, ok?\"OK\":\"FAIL\");
  return ok?0:1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "150|28|3|1000000000009|6|OK" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-odd-arity-stack-arg-extern-align-e2e ()
  "Guard for the SysV odd-arity stack-alignment fix (nelisp AOT): an
ODD-arity caller (3 params) calls the SSE-heavy variadic `snprintf' with
10 total GP args, so 4 land on the stack and the computed `base+k' args
take the extern-call general-stack-spill path.  Before the fix, the
`needs-align' / `spill-needs-align' formulas added the enclosing defun
arity on top of a prologue that already aligned rsp to 0 mod 16, so the
call landed at rsp = 8 mod 16 and `snprintf' SIGSEGV'd on aligned SSE.
The output must match glibc."
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
extern int snprintf(char *, unsigned long, const char *, ...);
int many(char *b, unsigned long n, int base){
  return snprintf(b, n, \"%d %d %d %d %d %d %d\",
                  base+1, base+2, base+3, base+4, base+5, base+6, base+7);
}
")
         (drv "
#include <stdio.h>
#include <string.h>
extern int many(char *, unsigned long, int);
int main(void){
  char b[64];
  int r = many(b, sizeof b, 10);
  int ok = (strcmp(b,\"11 12 13 14 15 16 17\")==0) && (r==20);
  printf(\"%s|%d|%s\\n\", b, r, ok?\"OK\":\"FAIL\");
  return ok?0:1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "11 12 13 14 15 16 17|20|OK" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-libc-test)

;;; nelisp-cfront-libc-test.el ends here
