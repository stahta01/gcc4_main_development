/* EH unwinding support for IA64 HPUX.
   Copyright (C) 2012 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <signal.h>
#include <sys/ucontext.h>
#include <sys/uc_access.h>

#define MD_HANDLE_UNWABI ia64_handle_unwabi

#define ABI_MARKER_HPUX_SIGTRAMP	((1 << 8) | 1)

static void
ia64_handle_unwabi (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  ucontext_t *uc;
  unsigned int nat;
  int status;
  int i;
  uint64_t bsp;
  uint64_t bspstore;
  uint64_t rbs_size;

  /* We handle only signal frames.  */
  if (fs->unwabi != ABI_MARKER_HPUX_SIGTRAMP)
    return;

  /* The ucontext structure address is stored in r32.  */
  uc = (ucontext_t *)(uintptr_t)_Unwind_GetGR (context, 32);

  status = __uc_get_grs (uc, 1, 1, (uint64_t *)&context->gp, &nat);
  status = __uc_get_grs (uc, 2, 30, (uint64_t *)&context->sigctxt.ireg, &nat);
  if (status == 0)
    {
      context->pri_unat_loc = &context->initial_unat;
      context->initial_unat = 0;
      for (i = 0; i < 30; i++)
	{
	  unw_word *loc = &context->sigctxt.ireg[i];
	  context->ireg[i].loc = loc;
	  context->ireg[i].nat.type = UNW_NAT_MEMSTK;
	  context->ireg[i].nat.off = context->pri_unat_loc - loc;
	  context->initial_unat |= 
	    ((nat >> (i + 1)) & 1) << (((uintptr_t)loc & 0x178) / 8);
	}
    }
  context->psp = context->sigctxt.ireg[12 - 2];

  status = __uc_get_frs (uc, 2, 30, (fp_regval_t *)context->sigctxt.fr);
  if (status == 0)
    for (i = 0; i < 30; i++)
      context->fr_loc[i] = &context->sigctxt.fr[i].w;

  status = __uc_get_brs (uc, 0, 8, (uint64_t *)context->sigctxt.br);
  if (status == 0)
    for (i = 0; i < 8; i++)
      context->br_loc[i] = &context->sigctxt.br[i];

  status = __uc_get_prs (uc, (uint64_t *)&context->pr);
  status = __uc_get_ip (uc, (uint64_t *)&context->rp);
  status = __uc_get_cfm (uc, (uint64_t *)&context->sigctxt.pfs);
  if (status == 0)
    context->pfs_loc = &context->sigctxt.pfs;
  status = __uc_get_ar_pfs (uc, (pfs_t *)&context->sigctxt.signal_pfs);
  if (status == 0)
    context->signal_pfs_loc = &context->sigctxt.signal_pfs;
  status = __uc_get_ar_lc (uc, (uint64_t *)&context->sigctxt.lc);
  if (status == 0)
    context->lc_loc = &context->sigctxt.lc;
  status = __uc_get_ar_fpsr (uc, (fpsr_t *)&context->sigctxt.fpsr);
  if (status == 0)
    context->fpsr_loc = &context->sigctxt.fpsr;
  status = __uc_get_ar_unat (uc, (uint64_t *)&context->sigctxt.unat);
  if (status == 0)
    context->unat_loc = &context->sigctxt.unat;

  /* Signal frame doesn't have an associated reg. stack frame
     other than what we adjust for below.	  */
  fs->no_reg_stack_frame = 1;

  /* Things look simpler on HPUX: we don't know the address of the alternate
     RBS, and it looks like NAT words are automatically adjusted.  */
  status = __uc_get_ar_bsp (uc, &bsp);
  status = __uc_get_ar_bspstore (uc, &bspstore);
  status = __uc_get_rsebs (uc, (uint64_t *)bspstore, (bsp - bspstore) / 8,
			   (uint64_t *)bspstore);
  status = __uc_get_rsebs (uc, (uint64_t *)(bsp | 0x1ff), 1,
			   (uint64_t *)&context->rnat);
  context->regstk_top = bspstore & ~0x1ffULL;

  /* Adjust BSP using SOF (to revert the cover effect).  */
  context->bsp = ia64_rse_skip_regs
    ((uint64_t *)bsp, -(context->sigctxt.pfs & 0x7f));

  fs->curr.reg[UNW_REG_RP].when = UNW_WHEN_NEVER;
}
