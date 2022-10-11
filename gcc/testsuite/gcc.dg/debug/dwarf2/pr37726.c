/* PR debug/37726 */
/* { dg-do compile } */
/* { dg-options "-g -O0 -dA -fno-merge-debug-strings" } */

int foo (int parm)
{
  int var = 0;
  int bar (void)
  {
    return parm + var;
  }
  parm++;
  var++;
  return bar ();
}

int
main (void)
{
  return foo (4) - 6;
}
