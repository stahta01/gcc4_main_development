/*
 * Filename:
 *
 *   __udivdi3.c
 *
 * Synopsis:
 *
 *   unsigned long long __udivdi3 (unsigned long long, unsigned long long);
 *
 * Description:
 *
 *   Function to divide two 64-bit unsigned integers giving a unsigned
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
 *   $Log: __udivdi3.c,v $
 *   Revision 1.1  1999/03/03 18:12:10  cvs
 *   Added Files:
 *    src/__moddi3.c src/__udivdi3.c src/__umoddi3.c
 *
 *
 */

typedef unsigned int UDItype __attribute__ ((mode (DI)));

UDItype __udiv64 (UDItype, UDItype);
extern UDItype __udivmod64 (UDItype, UDItype, UDItype *);


UDItype
__udiv64 (UDItype u, UDItype v)
{
  return __udivmod64 (u, v, (UDItype *) 0);
}
