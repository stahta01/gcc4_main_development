/*
 * Filename:
 *
 *   __divdi3.c
 *
 * Synopsis:
 *
 *   long long __divdi3 (long long, long long);
 *
 * Description:
 *
 *   Function to divide two 64-bit signed integers giving a signed
 *   64-bit quotient.
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
 *   $Log: __divdi3.c,v $
 *   Revision 1.2  1999/02/26 12:18:47  cvs
 *   HOLDV Base Level 4
 *
 *   Revision 1.1.1.1  1999/02/23 12:23:36  cvs
 *   Import of xgc runtimes
 *
 */

typedef int DItype __attribute__ ((mode (DI)));

extern unsigned long long
  __udivmod64 (unsigned long long, unsigned long long, unsigned long long *);

DItype __div64 (DItype, DItype);

struct DIstruct
  {
    long high, low;
  };

typedef union
  {
    struct DIstruct s;
    DItype ll;
  }
DIunion;

DItype
__div64 (DItype u, DItype v)
{
  int c = 0;
  DIunion uu, vv;
  DItype w;

  uu.ll = u;
  vv.ll = v;

  if (uu.s.high < 0)
    c = ~c,
      uu.ll = -uu.ll;
  if (vv.s.high < 0)
    c = ~c,
      vv.ll = -vv.ll;

  w = __udivmod64 (uu.ll, vv.ll, (DItype *) 0);
  if (c)
    w = -w;

  return w;
}
