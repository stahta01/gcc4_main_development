/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */
/* Make sure we can inline a varargs function whose variable arguments
   are not used.  See PR32493.  */
static inline __attribute__((always_inline)) void __check_printsym_format(const
char *fmt, ...)
{
}
static inline __attribute__((always_inline)) void print_symbol(const char *fmt,
unsigned long addr)
{
 __check_printsym_format(fmt, "");
}
void do_initcalls(void **call)
{
   print_symbol(": %s()", (unsigned long) *call);
}
