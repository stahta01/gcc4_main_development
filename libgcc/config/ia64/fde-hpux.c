/* Copyright (C) 2012 Free Software Foundation, Inc.
   Contributed by Tristan Gingold <gingold@adacore.com>.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include <stddef.h>
#include <stdlib.h>
#include "unwind-ia64.h"
#include <dlfcn.h>

/* Be careful: this unit is compiled both in ILP32 and LP64 modes.  */

/* Return a pointer to the unwind table entry for the function
   containing PC.  */

struct unw_table_entry *
_Unwind_FindTableEntry (void *pc, unw_word *segment_base,
                        unw_word *gp,
                        struct unw_table_entry *ent ATTRIBUTE_UNUSED)
{
  uint64_t hand;
  struct load_module_desc desc;
  struct unw_table_entry *ute_start;
  struct unw_table_entry *ute_end;
  uint64_t *uw;
  size_t lo, hi;

  hand = dlmodinfo ((uintptr_t)pc, &desc, sizeof (desc), NULL, 0, 0);
  if (hand == 0)
    return NULL;

  *gp = desc.linkage_ptr;
  *segment_base = desc.text_base;

  /* There is an HPUX specific header in front of the unwind table.
     Always 64 bits.  */
  uw = (uint64_t *)(uintptr_t)desc.unwind_base;
  ute_start = (struct unw_table_entry *)(uintptr_t)(desc.text_base + uw[1]);
  ute_end = (struct unw_table_entry *)(uintptr_t)(desc.text_base + uw[2]);

  lo = 0;
  hi = ute_end - ute_start;

  while (lo < hi)
    {
      struct unw_table_entry *e;
      size_t mid = lo + (hi - lo) / 2;

      e = ute_start + mid;
      if ((unw_word)pc < e->start_offset + desc.text_base)
	hi = mid;
      else if ((unw_word)pc >= e->end_offset + desc.text_base)
	lo = mid + 1;
      else
	return e;
    }
  return NULL;
}
