/* PR middle-end/12210 */
/* Origin: Ossadchy Yury A. <waspcoder@mail.ru> */

/* This used to fail on i686 because the argument was not copied
   to the right location by __builtin_apply after the direct call.  */

/* { dg-do run } */

#ifdef __LMP__
#define STACK_ARG_SIZE 4
#else
#define STACK_ARG_SIZE 16
#endif

#define INTEGER_ARG  5

extern void abort(void);

void foo(int arg)
{
  if (arg != INTEGER_ARG)
    abort();
}

void bar(int arg)
{
  foo(arg);
  __builtin_apply(foo, __builtin_apply_args(), STACK_ARG_SIZE);
}

int main(void)
{
  bar(INTEGER_ARG);

  return 0;
}
