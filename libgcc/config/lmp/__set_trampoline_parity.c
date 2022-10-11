/*
 * Filename:
 *
 *   __set_trampoline_parity
 *
 * Synopsis:
 *
 *   void __set_trampoline_parity (int *)
 *
 * Description:
 *
 *   Sets the correct parity on the instructions in a trampoline.
 *   A trampoline is a fragment of code installed in the stack to load
 *   the static chain register and transfer control to a nested function.
 *   A difficulty in installing a trampoline is setting the correct parity
 *   in the instructions. This function does the job.
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
 */

void __set_trampoline_parity (int *);
static int check_parity (unsigned char *addr);

void
__set_trampoline_parity (int *addr)
{
  int i;

  for (i = 0; i < 5; i++)
    {
      if (check_parity ((unsigned char *) (addr + i)))
        addr[i] |= 0x80000000;
    }
}

static int
check_parity (unsigned char *addr)
{
  unsigned int p1 = addr[0] ^ addr[1] ^ addr[2] ^ addr[3];
  unsigned int p2 = 0;
  int i;

  for (i = 0; i < 8; i++)
    {
      p2 += p1 & 1;
      p1 >>= 1;
    }

  return (p2 & 1);
}
