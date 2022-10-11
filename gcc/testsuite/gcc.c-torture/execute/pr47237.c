#ifdef __LMP__
#define STACK_ARG_SIZE 4
#else
#define STACK_ARG_SIZE 16
#endif

#define INTEGER_ARG  5

extern void abort(void);

static void foo(int arg)
{
  if (arg != INTEGER_ARG)
    abort();
}

static void bar(int arg)
{
  foo(arg);
  __builtin_apply(foo, __builtin_apply_args(), STACK_ARG_SIZE);
}

int main(void)
{
  bar(INTEGER_ARG);

  return 0;
}
