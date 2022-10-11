/* PR tree-optimization/20076 */
/* { dg-options "-O2 -Wmissing-noreturn" } */
/* { dg-options "-O2 -mno-mmx" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-do run } */

#ifdef __LMP__
#define STACK_ARG_SIZE 4
#else
#define STACK_ARG_SIZE 16
#endif

extern void abort (void);

double
foo (int arg)
{
  if (arg != 116)
    abort();
  return arg + 1;
}

inline double
bar (int arg)
{
  foo (arg);
  __builtin_return (__builtin_apply ((void (*) ()) foo,
				     __builtin_apply_args (), STACK_ARG_SIZE));
}

int
main (int argc, char **argv)
{
  if (bar (116) != 117.0)
    abort ();

  return 0;
}
