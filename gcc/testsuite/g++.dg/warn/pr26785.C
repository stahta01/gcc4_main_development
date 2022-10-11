// PR 26785
// { dg-do compile }
// { dg-options "-fshow-column" }

class foo {
  foo::foo // { dg-error "8:extra qualification" }
  (int a, 
   int b,
   int c);
};
