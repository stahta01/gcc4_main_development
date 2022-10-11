/* Prototypes of target machine for LMP.
   Copyright (C) 2002-2013 Free Software Foundation, Inc.
   Contributed by C.Nettleton,J.P.Parkes and P.Garbett.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_LMP_PROTOS_H
#define GCC_LMP_PROTOS_H

extern unsigned int lmp_data_alignment (tree, unsigned int);
extern void prepare_move_operands (rtx *, enum machine_mode);
extern int empty_delay_slot (rtx);
extern int gr5_hazard_bypass_p (rtx, rtx);
extern int lmp_interrupt_function_p (void);
extern void lmp_expand_prologue (void);
extern void lmp_expand_epilogue (void);
extern int lmp_epilogue_uses (int);
extern int lmp_hard_regno_rename_ok (unsigned int, unsigned int);
extern void lmp_initialize_trampoline (rtx, rtx, rtx);
extern bool lmp_can_use_return_insn_p (void);
extern void lmp_profile_hook (void);
extern rtx lmp_return_addr_rtx (int, rtx);
extern rtx lmp_dynamic_chain_address (rtx);
extern void lmp_init_expanders (void);
extern rtx lmp_legitimize_reload_address (rtx, enum machine_mode, int, int,
					  int);
extern int lmp_initial_elimination_offset (int from, int to);
extern void notice_update_cc (rtx, rtx);
extern const char *output_fp_compare (rtx, rtx, rtx);
extern const char *output_ubranch (rtx, rtx);
extern const char *output_cbranch (rtx, rtx, int, rtx);
extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern rtx lmp_eh_return_handler_rtx (void);
extern void split_double_move (rtx *, enum machine_mode);
extern void lmp_expand_copysign (rtx *, enum machine_mode);
extern void lmp_expand_int_cstore (rtx *, enum machine_mode);
extern void lmp_expand_fp_cstore (rtx *, enum machine_mode);
extern int lmp_expand_block_move (rtx *);
extern int lmp_expand_block_set (rtx *);
extern unsigned int reg_or_subreg_regno (rtx);

/* wcet-related items */
extern void lmp_inspect_instruction_stream (void);

#endif /* ! GCC_LMP_PROTOS_H */
