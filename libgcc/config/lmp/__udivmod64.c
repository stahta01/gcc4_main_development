/*
 * Filename:
 *
 *   __udivmod64.c
 *
 * Synopsis:
 *
 *   long long __udivmod64 (long long, long long, long long *);
 *
 * Description:
 *
 *   Function to divide two 64-bit unsigned integers giving an unsigned
 *   64-bit quotient and unsigned 64-bit remainder.
 *
 * Copyright (c) 1997, Free Software Foundation, Inc.
 *
 * This file is part of the GNU C Library. The GNU C Library is free
 * software; you can redistribute it and/or modify it under the terms of
 * the GNU Library General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your
 * option) any later version. The GNU C Library is distributed in the
 * hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE. See the GNU Library General Public License for more details.
 * You should have received a copy of the GNU Library General Public
 * License along with the GNU C Library; see the file COPYING.LIB. If
 * not, write to the Free Software Foundation, Inc., 675 Mass Ave,
 * Cambridge, MA 02139, USA.
 *
 * ChangeLog:
 *
 *   $Log: __udivmod64.c,v $
 *   Revision 1.5  1999/03/12 21:38:15  cvs
 *   Update for HOLD V Ada Version 1.0
 *
 *   Revision 1.4  1999/03/11 17:09:59  cvs
 *   TJF's update
 *
 *   Revision 1.3  1999/03/05 19:44:28  cvs
 *   Changed HI to SI.
 *
 *   Revision 1.2  1999/02/26 12:18:48  cvs
 *   HOLDV Base Level 4
 *
 *   Revision 1.1.1.1  1999/02/23 12:23:36  cvs
 *   Import of xgc runtimes
 *
 */

typedef          int HItype     __attribute__ ((mode (HI)));
typedef unsigned int UHItype    __attribute__ ((mode (HI)));
typedef          int SItype     __attribute__ ((mode (SI)));
typedef unsigned int USItype    __attribute__ ((mode (SI)));
typedef          int DItype     __attribute__ ((mode (DI)));
typedef unsigned int UDItype    __attribute__ ((mode (DI)));

UDItype __udivmod64 (UDItype, UDItype, UDItype *);

#define HI_TYPE_SIZE 16
#define SI_TYPE_SIZE 32
#define DI_TYPE_SIZE 64

struct DIstruct {SItype high, low;};
typedef union
{
  struct DIstruct s;
  DItype ll;
} DIunion;


/* sub_ddmmss(high_difference, low_difference, high_minuend, low_minuend,
high_subtrahend, low_subtrahend) subtracts two 2-word unsigned integers,
composed by HIGH_MINUEND and LOW_MINUEND, and HIGH_SUBTRAHEND and
LOW_SUBTRAHEND respectively.  The result is placed in HIGH_DIFFERENCE and
LOW_DIFFERENCE.  Overflow (i.e. carry out) is lost.  */

#define sub_ddmmss(sh, sl, ah, al, bh, bl)          \
  __asm__ ("sub.l   %0,%2,%4\n\t"                   \
           "subc.l  %1,%3,%5"                       \
           : "=&r" (sl), "=r" (sh)                  \
           : "r" (al), "r" (ah), "r" (bl), "r" (bh) \
           : "cc")


/* div_qqrvvd(high_quotient,low_quotient,remainder,high_dividend,low_dividend,
divisor) divides the 2-word unsigned integer composed of HIGH_DIVIDEND and
LOW_DIVIDEND by the unsigned word integer DIVISOR.  The quotient is placed
in HIGH_QUOTIENT and LOW_QUOTIENT and the remainder in REMAINDER.  */

#define div_qqrvvd(sh, sl, t, ah, al, b)    \
  __asm__ ("writemd %3,%4\n\t"              \
           "divdu   %5\n\t"                 \
           "readmda %0\n\t"                 \
           "readmdb %1\n\t"                 \
           "readmdc %2"                     \
           : "=r" (sl), "=r" (sh), "=r" (t) \
           : "r" (al), "r" (ah), "r" (b)    \
           : "mdb", "mdc")


UDItype
__udivmod64 (UDItype n, UDItype d, UDItype *rp)
{
  DIunion ww;
  DIunion nn, dd;
  DIunion rr;
  USItype d0, d1, n0, n1, n2;
  USItype q0, q1;
  USItype b;
  int bm;

  nn.ll = n;
  dd.ll = d;

  d0 = dd.s.low;
  d1 = dd.s.high;
  n0 = nn.s.low;
  n1 = nn.s.high;

  if (d1 == 0)
    {
      /* 64/32 bit division will do */

      if (d0 == 0)
        d0 = 1 / (int)d0;        /* Divide intentionally by zero.  */

      div_qqrvvd (q1, q0, n0, n1, n0, d0);

      if (rp != 0)
        {
          rr.s.low = n0;
          rr.s.high = 0;
          *rp = rr.ll;
        }
    }
  else
    {
      if (d1 > n1)
        {
          /* 00 = nn / DD */

          q0 = 0;
          q1 = 0;

          /* Remainder in n1n0.  */
          if (rp != 0)
            {
              rr.s.low = n0;
              rr.s.high = n1;
              *rp = rr.ll;
            }
        }
      else
        {
          /* 0q = NN / dd */

          bm = 0;
          while ((d1 & 0x80000000) == 0)
            {
              bm++;
              d1 <<= 1;
            }

          if (bm == 0)
            {
              /* From (n1 >= d1) /\ (the most significant bit of d1 is set),
                 conclude (the most significant bit of n1 is set) /\ (the
                 quotient digit q0 = 0 or 1).

                 This special case is necessary, not an optimization.  */

              /* The condition on the next line takes advantage of that
                 n1 >= d1 (true due to program flow).  */
              if (n1 > d1 || n0 >= d0)
                {
                  q0 = 1;
                  sub_ddmmss (n1, n0, n1, n0, d1, d0);
                }
              else
                q0 = 0;

              q1 = 0;

              if (rp != 0)
                {
                  rr.s.low = n0;
                  rr.s.high = n1;
                  *rp = rr.ll;
                }
            }
          else
            {
              USItype m1, m0;
              DIunion m1m0;

              /* Normalize.  */

              b = SI_TYPE_SIZE - bm;

              d1 = d1 | (d0 >> b);
              d0 = d0 << bm;
              n2 = n1 >> b;
              n1 = (n1 << bm) | (n0 >> b);
              n0 = n0 << bm;

              div_qqrvvd (q1, q0, n1, n2, n1, d1);

              m1m0.ll = (unsigned long long)q0 * (unsigned long long)d0;
              m1 = m1m0.s.high;
              m0 = m1m0.s.low;

              if (m1 > n1 || (m1 == n1 && m0 > n0))
                {
                  q0--;
                  sub_ddmmss (m1, m0, m1, m0, d1, d0);
                }

              /* Remainder in (n1n0 - m1m0) >> bm.  */
              if (rp != 0)
                {
                  sub_ddmmss (n1, n0, n1, n0, m1, m0);
                  rr.s.low = (n1 << b) | (n0 >> bm);
                  rr.s.high = n1 >> bm;
                  *rp = rr.ll;
                }
            }
        }
    }

  ww.s.low = q0;
  ww.s.high = q1;
  return ww.ll;
}
