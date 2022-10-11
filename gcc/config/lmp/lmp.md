;; Machine description for LMP.
;;  Copyright (C) 2002-2013 Free Software Foundation, Inc.
;;
;;  Contributed by C.Nettleton, J.P.Parkes and P.Garbett.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Extra register constraints are:
;;   'b'   EAM register mdb
;;   'c'   EAM register mdc
;;   'f'   Floating-point register
;;   'k'   Register that can be used as the target of a sibcall, i.e. call-used
;;         general register not clobbered in the epilogue: r1-r8 and r10
;;   'l'   Low general register, i.e. general register accessible in user mode
;;         on the GR6 and, consequently, that can be used as the target of a
;;         branch with prediction: r1-r28
;;   't'   Register r1
;;   'u'   Register r2
;;   'v'   Register r3
;;
;; Immediate integer operand constraints are:
;;   'I'  65535          (16-bit low mask)
;;   'J'  0 .. 65535     (16-bit immediate)
;;   'K'  1 .. 31        (5-bit immediate)
;;   'L'  -1 .. -65535   (16-bit negative immediate)
;;   'M'  -1             (minus one)
;;   'N'  -65536         (16-bit high mask)
;;   'O'  0              (integer zero)
;;   'P'  32             (thirty two)
;;
;; Immediate FP operand constraints are:
;;   'G'  0.0            (floating-point zero)
;;
;; Operand substitution characters are:
;;   %b   LS 8 bits of immediate operand
;;   %w   LS 16 bits of immediate operand
;;   %u   MS 16 bits of immediate operand
;;   %d   second register in a pair
;;   %r   register or zero (r0)
;;   %R   second register in a pair or zero (r0)
;;   %#   delay slot follows, if empty, fill with NOP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Registers by name.
(define_constants [
  (R_R1          1)
  (R_R2          2)
  (R_R3          3)
  (R_R4          4)
  (R_R5          5)
  (R_R6          6)
  (R_LINK	21)
  (R_FP		22)
  (R_SP		23)
  (R_MDB	32)
  (R_MDC	33)
])

;; UNSPEC usage.
(define_c_enum "unspec" [
  UNSPEC_MDBHI
  UNSPEC_FLOAD
  UNSPEC_FSTORE
  UNSPEC_ITOF
  UNSPEC_FTOI
  UNSPEC_NOP
])

;; UNSPEC_VOLATILE usage.
(define_c_enum "unspecv" [
  UNSPECV_BLOCKAGE
  UNSPECV_DSI
])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Attributes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; Instruction type.
;
;imm_reg       Move of immediate value to register.
;mem_reg       Move from memory to register.
;eam_reg       Move from EAM to register.
;fp_reg        Move from FPU to register.
;reg_mem       Move from register to memory.
;reg_eam       Move from register to EAM.
;reg_fp        Move from register to FPU.
;arith         Arithmetic operation, result in register, sets overflow.
;arith2        Two successive arithmetic operations.
;logic         Logical operation, result in register, does not set overflow.
;abs_branch    Absolute branch.
;branch        Branch.
;bmi           Block move.
;call          Call to subprogram.
;ret           Return from subprogram.
;rfi           Return from interrupt.
;dsi           Disable interrupts.
;cmp           Compare or test.
;div           EAM 32/32 division.
;divd          EAM 64/32 division.
;mul           EAM 32 * 32 -> 64 multiplication.
;shiftdi       EAM 64 bit shift.
;fdiv          Floating point divide.
;fsqrt         Floating point square root.
;ftoi          Fix float to integer.
;itof          Float integer.
;fmove         Floating point move w/ or w/o change of sign: fmove, fabs, fneg.
;fcmp          Floating point compare or test.
;fp            Other floating point operations.
;nop           No operation.
;multi         Multiple instructions which split.
;asm           User asm instructions.

(define_attr "type"
"imm_reg,mem_reg,eam_reg,fp_reg,reg_mem,reg_eam,reg_fp,arith,arith2,logic,abs_branch,branch,bmi,call,ret,rfi,dsi,cmp,div,divd,mul,shiftdi,fdiv,fsqrt,ftoi,itof,fmove,fcmp,fp,nop,multi,asm" (const_string "logic"))

; Iterator definitions.

(define_mode_iterator I [QI HI SI])
(define_mode_attr s [(QI ".b") (HI ".w") (SI ".l")])

; This code iterator allows signed and unsigned widening multiplications
; to use the same template.
(define_code_iterator any_extend [sign_extend zero_extend])

; <u> expands to an empty string when doing a signed operation and
; "u" when doing an unsigned operation.
(define_code_attr u [(sign_extend "") (zero_extend "u")])

; <su> is like <u>, but the signed form expands to "s" rather than "".
(define_code_attr su [(sign_extend "s") (zero_extend "u")])

; This code iterator allows returns and simple returns to use the same template.
(define_code_iterator any_return [return simple_return])
(define_code_attr return_pred [(return "lmp_can_use_return_insn_p ()")
			       (simple_return "!lmp_interrupt_function_p ()")])
(define_code_attr return_str [(return "") (simple_return "simple_")])

; This code iterator allows integer and FP cstores to use the same template.
(define_code_iterator any_scc [ltu lt])
(define_code_attr scc_str [( ltu "sltu") (lt "slt")])

; Condition codes.
(define_attr "cc"
  "clobber,unchanged,set,set_noov,compare,compare_fp,change0"
  (cond [(eq_attr "type" "imm_reg,mem_reg,eam_reg,fp_reg") (const_string "change0")
         (eq_attr "type" "reg_mem,reg_eam") (const_string "unchanged")
         (eq_attr "type" "reg_fp") (const_string "change0")
         (eq_attr "type" "arith") (const_string "set_noov")
         (eq_attr "type" "logic") (const_string "set")
         (eq_attr "type" "abs_branch,branch") (const_string "unchanged")
         (eq_attr "type" "cmp") (const_string "compare")
         (eq_attr "type" "div,divd,mul,shiftdi") (const_string "change0")
         (eq_attr "type" "fdiv,fsqrt,ftoi,itof,fp,fmove") (const_string "change0")
         (eq_attr "type" "fcmp") (const_string "compare_fp")
         (eq_attr "type" "nop") (const_string "unchanged")]
         (const_string "clobber")))

; Those insns that occupy 4 bytes.
(define_attr "single_insn" "no,yes"
  (if_then_else (eq_attr "type" "arith2,rfi,multi")
                (const_string "no")
                (const_string "yes")))

;; True if branch or call will be emitting a nop into its delay slot.
(define_attr "empty_delay_slot" "false,true"
  (symbol_ref "(empty_delay_slot (insn)
		? EMPTY_DELAY_SLOT_TRUE : EMPTY_DELAY_SLOT_FALSE)"))

; Length in bytes.
(define_attr "length" ""
  (cond [(eq_attr "type" "abs_branch,call,ret")
           (if_then_else (eq_attr "empty_delay_slot" "true")
                         (const_int 8)
                         (const_int 4))
         (eq_attr "type" "branch")
           (if_then_else (leu (plus (minus (match_dup 0) (pc))
                                    (const_int 131068))
                              (const_int 262136))
                         (if_then_else (eq_attr "empty_delay_slot" "true")
                                       (const_int 8)
                                       (const_int 4))
                         (const_int 20))
         (eq_attr "single_insn" "no")
           (const_int 8)] (const_int 4)))

(define_asm_attributes [(set_attr "type" "asm")])

; Delay slots.
(define_delay (eq_attr "type" "abs_branch,branch,call,ret")
  [(and (eq_attr "type" "!abs_branch,branch,call,ret,rfi,bmi,mul,div,divd,fdiv,fsqrt,asm")
        (eq_attr "single_insn" "yes"))
    (nil) (nil)])

(include "predicates.md")
(include "constraints.md")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Processor pipeline description.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; Attribute for cpu type.
; These must match the values for enum processor_type in lmp-opts.h.
(define_attr "cpu" "gr5,gr6" (const (symbol_ref "lmp_cpu_attr")))

(include "gr5.md")
(include "gr6.md")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RTL pro/epilogue support.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; Expand prologue in RTL
(define_expand "prologue"
  [(const_int 0)]
  ""
{
  lmp_expand_prologue ();
  DONE;
})

; Expand epilogue in RTL
(define_expand "epilogue"
  [(return)]
  ""
{
  lmp_expand_epilogue ();
})

; Expand epilogue without a final jump in RTL
(define_expand "sibcall_epilogue"
  [(return)]
  ""
{
  lmp_expand_epilogue ();
  DONE;
})

; The artificial dependency on the link register is to prevent the
; frame instruction from being put in a call delay slot, which can
; confuse the CFI machinery.

(define_insn "stack_save"
  [(set (reg:SI R_FP) (reg:SI R_SP))
   (use (reg:SI R_LINK))]
  ""
  "move.l  fp,sp		;stack_save"
  [(set_attr "type" "logic")
   (set_attr "cc" "clobber")])

; The construct (mem:BLK (scratch)) is considered to alias all other
; memory accesses.  Thus it can be used as a memory barrier in stack
; deallocation patterns.

(define_insn "stack_restore"
  [(set (reg:SI R_SP) (reg:SI R_FP))
   (clobber (mem:BLK (scratch)))]
  ""
  "move.l  sp,fp		;stack_restore"
  [(set_attr "type" "logic")
   (set_attr "cc" "clobber")])

(define_insn "stack_pop"
  [(set (reg:SI R_SP)
        (plus:SI (reg:SI R_SP) (match_operand:SI 0 "add_operand" "J,r")))
   (clobber (mem:BLK (scratch)))]
  ""
  "@
    addi    sp,%0		;stack pop
    add.l   sp,sp,%0		;stack pop"
  [(set_attr "type" "arith")
   (set_attr "cc" "clobber")])

(define_expand "<return_str>return"
  [(any_return)]
  "<return_pred>"
  "")

(define_insn "*<return_str>return_internal"
  [(any_return)]
  "!lmp_interrupt_function_p ()"
{
  return output_ubranch (pc_rtx, insn);
}
  [(set_attr "type" "ret")])

(define_insn "*return_internal_interrupt"
  [(return)]
  "lmp_interrupt_function_p ()"
  "rfi\n\t nop				;return from interrupt"
  [(set_attr "type" "rfi")])

(define_insn "dsi"
  [(unspec_volatile [(const_int 0)] UNSPECV_DSI)]
  ""
  "dsi"
  [(set_attr "type" "dsi")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; QImode moves
;;
;; For moving among registers we use the move.b instruction. This is
;; actually an OR instruction using an alias. For moving between register
;; and memory we need the address of the memory location in a register.
;; However, we can accept an expression (reg + offset) where offset is in
;; the range 0 .. 31.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, QImode);
})

(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r, m,?b,?c, r, r,r,r")
        (match_operand:QI 1 "general_operand"      " r,rO, r, r,?b,?c,i,m"))]
  "register_operand (operands[0], QImode)
   || reg_or_0_operand (operands[1], QImode)"
  "@
    move.b  %0,%1
    write.b %0,%r1
    writemd %1,r0		;movqi ?b r
    writemdc %1		;movqi ?c r
    readmda %0		;movqi r ?b
    readmdc %0		;movqi r ?c
    moviq   %0,%b1		;movqi  r  i
    read.b  %0,%1"
  [(set_attr "type" "logic,reg_mem,reg_eam,reg_eam,eam_reg,eam_reg,imm_reg,mem_reg")])

(define_insn "movstrictqi"
  [(set (strict_low_part (match_operand:QI 0 "register_operand" "+r,r"))
	(match_operand:QI 1 "general_operand"                   "rO,m"))]
  ""
  "@
    move.b  %0,%r1
    read.b  %0,%1"
  [(set_attr "type" "logic,mem_reg")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HImode moves
;;
;; For moving among registers we use the move.w instruction. This is
;; actually an OR instruction using an alias. For moving between register
;; and memory we need the address of the memory location in a register.
;; However, we can accept an expression (reg + offset) where offset is in
;; the range 0 .. 62 and is shifted right one place in the assembled 
;; instruction.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
        (match_operand:HI 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, HImode);
})

(define_insn "*movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r, m,?b,?c, r, r,r,r")
        (match_operand:HI 1 "general_operand"      " r,rO, r, r,?b,?c,i,m"))]
  "register_operand (operands[0], HImode)
   || reg_or_0_operand (operands[1], HImode)"
  "@
    move.w  %0,%1
    write.w %0,%r1
    writemd %1,r0		;movhi ?b r
    writemdc %1		;movhi ?c r
    readmda %0		;movhi r ?b
    readmdc %0		;movhi r ?c
    moviq   %0,%w1		;movhi  r  i
    read.w  %0,%1"
  [(set_attr "type" "logic,reg_mem,reg_eam,reg_eam,eam_reg,eam_reg,imm_reg,mem_reg")])

(define_insn "movstricthi"
  [(set (strict_low_part (match_operand:HI 0 "register_operand" "+r,r,r"))
	(match_operand:HI 1 "general_operand"                   " r,i,m"))]
  ""
  "@
    move.w  %0,%1
    movil   %0,%w1
    read.w  %0,%1"
  [(set_attr "type" "logic,imm_reg,mem_reg")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SImode moves
;;
;; For moving among registers we use the move.l instruction. This is
;; actually an OR instruction using an alias. For moving between register
;; and memory we need the address of the memory location in a register.
;; However, we can accept an expression (reg + offset) where offset is in
;; the range 0 .. 124 and is shifted right two places in the assembled 
;; instruction.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, SImode);
})

(define_insn "*movsi_high"
  [(set (match_operand:SI 0 "gpc_reg_operand" "=r,r") 
        (high:SI (match_operand:SI 1 "immediate_operand" "n,i")) )]
  ""
  "@
    moviu   %0,%u1
    moviu   %0,%%u %a1"
  [(set_attr "type" "imm_reg")])

; We only care about the lower 16 bits of the constant 
; being inserted into the upper 16 bits of the register.
(define_insn "*moviu"
  [(set (zero_extract:SI (match_operand:SI 0 "gpc_reg_operand" "+r")
                         (const_int 16)
                         (const_int 0))
        (match_operand:SI 1 "const_int_operand" "n"))]
  ""
  "moviu   %0,%w1"
  [(set_attr "type" "imm_reg")])

(define_insn "*movsi_losum"
  [(set (match_operand:SI 0 "gpc_reg_operand" "=r,r")
        (lo_sum:SI (match_operand:SI 1 "gpc_reg_operand" "0,0")
                   (match_operand:SI 2 "immediate_operand" "n,i")))]
  ""
  "@
    movil   %0,%w2
    movil   %0,%%l %a2"
  [(set_attr "type" "imm_reg")])

(define_insn "*movil"
  [(set (zero_extract:SI (match_operand:SI 0 "gpc_reg_operand" "+r")
                         (const_int 16)
                         (const_int 16))
        (match_operand:SI 1 "const_int_operand" "n"))]
  ""
  "movil   %0,%w1"
  [(set_attr "type" "imm_reg")])

(define_insn "*movsi_insn_no_ieee"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r, m,?b,?c, r, r,r,r,r,r, r,!f")
        (match_operand:SI 1 "general_operand"      " r,rO, r, r,?b,?c,J,M,i,m,!f, r"))]
  "!TARGET_FPU_IEEE
   && (register_operand (operands[0], SImode)
       || reg_or_0_operand (operands[1], SImode))"
  "@
    move.l  %0,%1
    write.l %0,%r1
    writemd %1,r0		;movsi  ?b  r
    writemdc %1		;movsi  ?c  r
    readmda %0		;movsi  r  ?b
    readmdc %0		;movsi  r  ?c
    moviq   %0,%1		;movsi  r  J
    not.l   %0,r0		;movsi  r  M
    #			;movsi  r  i
    read.l  %0,%1
    fstore  %0,%1
    fload   %0,%1"
  [(set_attr "type" "logic,reg_mem,reg_eam,reg_eam,eam_reg,eam_reg,imm_reg,logic,multi,mem_reg,fp_reg,reg_fp")])

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r, m,?b,?c, r, r,r,r,r,r, r,?f,f")
        (match_operand:SI 1 "general_operand"      " r,rO, r, r,?b,?c,J,M,i,m,?f, r,f"))]
  "TARGET_FPU_IEEE
   && (register_operand (operands[0], SImode)
       || reg_or_0_operand (operands[1], SImode))"
  "@
    move.l  %0,%1
    write.l %0,%r1
    writemd %1,r0		;movsi  ?b  r
    writemdc %1		;movsi  ?c  r
    readmda %0		;movsi  r  ?b
    readmdc %0		;movsi  r  ?c
    moviq   %0,%1		;movsi  r  J
    not.l   %0,r0		;movsi  r  M
    #			;movsi  r  i
    read.l  %0,%1
    fstore  %0,%1
    fload   %0,%1
    fmove   %0,%1"
  [(set_attr "type" "logic,reg_mem,reg_eam,reg_eam,eam_reg,eam_reg,imm_reg,logic,multi,mem_reg,fp_reg,reg_fp,fmove")])

(define_insn "*movsi_mdbhi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(reg:DI R_MDB)] UNSPEC_MDBHI))]
  ""
  "readmdb %0"
  [(set_attr "type" "eam_reg")])

(define_split
  [(set (match_operand:SI 0 "gpc_reg_operand" "")
        (match_operand:SI 1 "long_immed_operand" ""))]
  "reload_completed"
  [(set (match_dup 0)
        (high:SI (match_dup 1)) )
   (set (match_dup 0)
        (lo_sum:SI (match_dup 0) (match_dup 1)))]
  "")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DImode moves
;;
;; Where the destination is the EAM register MDB, then we use
;; the writemd instruction. In all other cases we split the 64-bit move
;; into two separate 32-bit moves.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
        (match_operand:DI 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, DImode);
})

(define_insn "*movdi_insn"
  [(set (match_operand:DI 0 "nonimmediate_operand" "= r, m, r,??b")
        (match_operand:DI 1 "general_operand"      "rim,rO,?b,  r"))]
  "register_operand (operands[0], DImode)
   || reg_or_0_operand (operands[1], DImode)"
  "@
    #
    #
    #
    writemd %d1,%1		;movdi  ?b r"
  [(set_attr "type" "multi,multi,multi,reg_eam")])

(define_split
  [(set (match_operand:DI 0 "gpc_reg_operand" "") (reg:DI R_MDB))]
  "reload_completed"
  [(set (match_dup 1) (unspec:SI [(reg:DI R_MDB)] UNSPEC_MDBHI))
   (set (match_dup 2) (reg:SI R_MDB))]
{
  operands[1] = operand_subword (operands[0], 0, 1, DImode);
  operands[2] = operand_subword (operands[0], 1, 1, DImode);
})

(define_split
  [(set (match_operand:DI 0 "non_eam_dst_operand" "")
        (match_operand:DI 1 "non_eam_src_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  split_double_move (operands, DImode);
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SFmode moves
;;
;; Constants are constructed in a GP register and moved to the FP register.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
        (match_operand:SF 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, SFmode);
})

(define_insn "*movsf_insn"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,f,r,r, m,r,r,r")
        (match_operand:SF 1 "general_operand"      " f,G,r,f,r,rG,G,F,m"))]
  "register_operand (operands[0], SFmode)
   || reg_or_0_operand (operands[1], SFmode)"
  "@
    fmove   %0,%1
    fmove   %0,f0
    fload   %0,%1
    fstore  %0,%1
    move.l  %0,%1
    write.l %0,%r1
    moviq   %0,0
    #
    read.l  %0,%1"
  [(set_attr "type" "fmove,fmove,reg_fp,fp_reg,logic,reg_mem,imm_reg,multi,mem_reg")])

(define_split
  [(set (match_operand:SF 0 "gpc_reg_operand" "")
        (match_operand:SF 1 "const_double_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))]
{
  long l;
  REAL_VALUE_TYPE rv;

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_SINGLE (rv, l);

  operands[2] = operand_subword (operands[0], 0, 0, SFmode);
  operands[3] = GEN_INT (trunc_int_for_mode (l, SImode));
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DFmode moves
;;
;; We always split a DFmode move into two SImode moves.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
        (match_operand:DF 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, DFmode);
})

(define_insn "*movdf_insn"
  [(set (match_operand:DF 0 "nonimmediate_operand" "= r, m")
        (match_operand:DF 1 "general_operand"      "rFm,rG"))]
  "register_operand (operands[0], DFmode)
   || reg_or_0_operand (operands[1], DFmode)"
  "#"
  [(set_attr "type" "multi")])

(define_split
  [(set (match_operand:DF 0 "non_eam_dst_operand" "")
        (match_operand:DF 1 "general_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  split_double_move (operands, DFmode);
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Add
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "addqi3"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (plus:QI (match_operand:QI 1 "register_operand" "%r")
                 (match_operand:QI 2 "register_operand" "r")))]
  ""
  "add.b   %0,%1,%2"
  [(set_attr "type" "arith")])

(define_insn "addhi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (plus:HI (match_operand:HI 1 "register_operand" "%r")
                 (match_operand:HI 2 "register_operand" "r")))]
  ""
  "add.w   %0,%1,%2"
  [(set_attr "type" "arith")])

;
; Favour the addition of small negative constants, since they are
; expensive to load into a register.
;

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r,r,r")
        (plus:SI (match_operand:SI 1 "register_operand" "%0,r,0")
                 (match_operand:SI 2 "add_operand"      " L,r,J")))]
  ""
  "@
    subi    %0,%n2
    add.l   %0,%1,%2
    addi    %0,%2"
  [(set_attr "type" "arith")])

;
; Disfavour the use of add.l because of the early clobber.
;

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand"          "=r,r,&r")
        (plus:DI (match_operand:DI 1 "register_operand" "%0,0, r")
                 (match_operand:DI 2 "add_operand"      " J,L, r")))]
  ""
  "@
    addi    %d0,%2\n\tadc.l   %0,%0,r0
    subi    %d0,%n2\n\tsubc.l  %0,%0,r0
    add.l   %d0,%d1,%d2\n\tadc.l   %0,%1,%2"
  [(set_attr "type" "arith2")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Add with Carry
;;
;; Only SI mode is supported as slt[u] for the sake of cstore.  Note that we
;; cannot have combined PLUS patterns before reload because an instruction
;; referencing cc0 cannot have input reloads.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*<scc_str>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(any_scc:SI (cc0) (const_int 0)))]
  ""
  "adc.l   %0,r0,r0"
  [(set_attr "type" "arith")])

(define_insn "*plus_<scc_str>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_operand:SI 1 "register_operand" "r")
		 (any_scc:SI (cc0) (const_int 0))))]
  "reload_completed"
  "adc.l   %0,%1,r0"
  [(set_attr "type" "arith")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Subtract
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "subqi3"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (minus:QI (match_operand:QI 1 "reg_or_0_operand" "rO")
                  (match_operand:QI 2 "register_operand" "r")))]
  ""
  "sub.b   %0,%r1,%2"
  [(set_attr "type" "arith")])

(define_insn "subhi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (minus:HI (match_operand:HI 1 "reg_or_0_operand" "rO")
                 (match_operand:HI 2 "register_operand" "r")))]
  ""
  "sub.w   %0,%r1,%2"
  [(set_attr "type" "arith")])

;
; Favour the subtraction of small negative constants, since they are
; expensive to load into a register.
;

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand"           "=r,r, r")
        (minus:SI (match_operand:SI 1 "reg_or_0_operand" " 0,rO,0")
                  (match_operand:SI 2 "add_operand"      " L,r, J")))]
  ""
  "@
    addi    %0,%n2
    sub.l   %0,%r1,%2
    subi    %0,%2"
  [(set_attr "type" "arith")])

;
; Disfavour the use of the sub.l because of the early clobber.
;

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand"           "=r,r,&r")
        (minus:DI (match_operand:DI 1 "register_operand" " 0,0, r")
                  (match_operand:DI 2 "add_operand"      " J,L, r")))]
  ""
  "@
    subi    %d0,%2\n\tsubc.l  %0,%0,r0
    addi    %d0,%n2\n\tadc.l   %0,%0,r0
    sub.l   %d0,%d1,%d2\n\tsubc.l  %0,%1,%2"
  [(set_attr "type" "arith2")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Subtract with Carry
;;
;; Only SI mode is supported as neg<slt[u]> for the sake of cstore.  Note that
;; we cannot have combined MINUS patterns before reload because an instruction
;; referencing cc0 cannot have input reloads.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*neg_<scc_str>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (any_scc:SI (cc0) (const_int 0))))]
  ""
  "subc.l  %0,r0,r0"
  [(set_attr "type" "arith")])

(define_insn "*minus_<scc_str>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (any_scc:SI (cc0) (const_int 0))))]
  "reload_completed"
  "subc.l  %0,%1,r0"
  [(set_attr "type" "arith")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Multiply (non-widening and widening, signed and unsigned)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; The mults and multu instructions clear MDC but we only pretend that they
;; clobber it to keep things relatively simple.

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=b")
         (mult:SI (match_operand:SI 1 "register_operand" "%r")
                  (match_operand:SI 2 "register_operand" "r")))
   (clobber (reg:SI R_MDC))]
  ""
  "mults   %1,%2"
  [(set_attr "type" "mul")])

;; The names are mulsidi3 and umulsidi3 here.

(define_insn "<u>mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=b")
        (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "%r"))
                 (any_extend:DI (match_operand:SI 2 "register_operand" "r"))))
   (clobber (reg:SI R_MDC))]
  ""
  "mult<su>   %1,%2"
  [(set_attr "type" "mul")])

;; But they are smulsi3_highpart and umulsi3_highpart here.

(define_insn_and_split "<su>mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (truncate:SI
          (ashiftrt:DI
            (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "%r"))
                     (any_extend:DI (match_operand:SI 2 "register_operand" "r")))
            (const_int 32))))
   (clobber (reg:DI R_MDB))
   (clobber (reg:SI R_MDC))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (reg:DI R_MDB)
                   (mult:DI (any_extend:DI (match_dup 1))
                            (any_extend:DI (match_dup 2))))
              (clobber (reg:SI R_MDC))])
   (set (match_dup 0) (unspec:SI [(reg:DI R_MDB)] UNSPEC_MDBHI))]
  ""
  [(set_attr "type" "multi")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Signed and unsigned integer divide giving quotient and remainder
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*divmodsi4_insn"
  [(set (match_operand:SI 0 "register_operand" "=b")
        (div:SI (match_operand:SI 1 "register_operand" "0")
                (match_operand:SI 2 "register_operand" "r")))
   (set (reg:SI R_MDC) (mod:SI (match_dup 1) (match_dup 2)))]
  ""
  "divs    %2"
  [(set_attr "type" "div")])

(define_insn_and_split "divmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=b")
        (div:SI (match_operand:SI 1 "register_operand" "0")
                (match_operand:SI 2 "register_operand" "r")))
   (set (match_operand:SI 3 "register_operand" "=r")
        (mod:SI (match_dup 1) (match_dup 2)))
   (clobber (reg:SI R_MDC))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (div:SI (match_dup 1) (match_dup 2)))
              (set (reg:SI R_MDC) (mod:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (reg:SI R_MDC))]
  ""
  [(set_attr "type" "multi")])

(define_insn "*udivmodsi4_insn"
  [(set (match_operand:SI 0 "register_operand" "=b")
        (udiv:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "register_operand" "r")))
   (set (reg:SI R_MDC) (umod:SI (match_dup 1) (match_dup 2)))]
  ""
  "divu    %2"
  [(set_attr "type" "div")])

(define_insn_and_split "udivmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=b")
        (udiv:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "register_operand" "r")))
   (set (match_operand:SI 3 "register_operand" "=r")
        (umod:SI (match_dup 1) (match_dup 2)))
   (clobber (reg:SI R_MDC))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (udiv:SI (match_dup 1) (match_dup 2)))
              (set (reg:SI R_MDC) (umod:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (reg:SI R_MDC))]
  ""
  [(set_attr "type" "multi")])

; FIXME. How do we persuade the compiler to use 64/32 bit divides directly ?
; Currently this can't be done PG

(define_insn "*divds"
  [(set (reg:DI R_MDB)
        (div:DI (reg:DI R_MDB) (sign_extend:DI (match_operand:SI 0 "register_operand" "r"))))
   (set (reg:SI R_MDC) (truncate:SI (mod:DI (reg:DI R_MDB) (sign_extend:DI (match_dup 0)))))]
  ""
  "divds   %0"
  [(set_attr "type" "divd")])

(define_insn "*divdu"
  [(set (reg:DI R_MDB)
        (udiv:DI (reg:DI R_MDB) (zero_extend:DI (match_operand:SI 0 "register_operand" "r"))))
   (set (reg:SI R_MDC) (truncate:SI (umod:DI (reg:DI R_MDB) (zero_extend:DI (match_dup 0)))))]
  ""
  "divdu   %0"
  [(set_attr "type" "divd")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bitwise Logical AND
;;
;; Modes QI, HI and SI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "andqi3"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (and:QI  (match_operand:QI 1 "register_operand" "%r")
                 (match_operand:QI 2 "register_operand" "r")))]
  ""
  "and.b   %0,%1,%2"
  [(set_attr "type" "logic")])

(define_insn "andhi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (and:HI  (match_operand:HI 1 "register_operand" "%r")
                 (match_operand:HI 2 "register_operand" "r")))]
  ""
  "and.w   %0,%1,%2"
  [(set_attr "type" "logic")])

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (and:SI  (match_operand:SI 1 "register_operand" "%r,0,0")
                 (match_operand:SI 2 "reg_or_mask_operand" "r,I,N")))]
  ""
  "@
    and.l   %0,%1,%2
    moviu   %0,%u2
    movil   %0,%w2"
  [(set_attr "type" "logic,imm_reg,imm_reg")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bitwise inclusive logical OR
;;
;; Modes QI, HI and SI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (ior:QI  (match_operand:QI 1 "register_operand" "%r")
                 (match_operand:QI 2 "register_operand" "r")))]
  ""
  "or.b    %0,%1,%2"
  [(set_attr "type" "logic")])

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (ior:HI  (match_operand:HI 1 "register_operand" "%r")
                 (match_operand:HI 2 "register_operand" "r")))]
  ""
  "or.w    %0,%1,%2"
  [(set_attr "type" "logic")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (ior:SI  (match_operand:SI 1 "register_operand" "%r,0,0")
                 (match_operand:SI 2 "reg_or_mask_operand" "r,I,N")))]
  ""
  "@
    or.l    %0,%1,%2
    movil   %0,%w2
    moviu   %0,%u2"
  [(set_attr "type" "logic,imm_reg,imm_reg")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bitwise exclusive Logical OR
;;
;; Modes QI, HI and SI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "xor<mode>3"
  [(set (match_operand:I 0 "register_operand" "=r")
        (xor:I  (match_operand:I 1 "register_operand" "%r")
                (match_operand:I 2 "register_operand" "r")))]
  ""
  "xor<s>   %0,%1,%2"
  [(set_attr "type" "logic")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Arithmetic Shift Left
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "ashl<mode>3"
  [(set (match_operand:I 0 "register_operand" "=r,r")
        (ashift:I (match_operand:I  1 "register_operand"     "r,r")
                  (match_operand:QI 2 "reg_or_shift_operand" "r,K")))]
  ""
  "asl<s>   %0,%1,%2"
  [(set_attr "type" "arith")])

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "=b,r")
        (ashift:DI (match_operand:DI 1 "register_operand" "0,r")
                   (match_operand:QI 2 "reg_or_32_operand" "r,P")))
   (clobber (reg:SI R_MDC))]
  ""
  "@
    asld    %2
    #"
  [(set_attr "type" "shiftdi,multi")])

(define_split
  [(set (match_operand:DI 0 "gpc_reg_operand" "")
        (ashift:DI (match_operand:DI 1 "gpc_reg_operand" "")
                   (const_int 32)))
   (clobber (reg:SI R_MDC))]
  "reload_completed"
  [(set (subreg:SI (match_dup 0) 0) (subreg:SI (match_dup 1) 4))
   (set (subreg:SI (match_dup 0) 4) (const_int 0))]
  "")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Arithmetic Shift Right
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "ashr<mode>3"
  [(set (match_operand:I 0 "register_operand" "=r,r")
        (ashiftrt:I (match_operand:I  1 "register_operand"     "r,r")
                    (match_operand:QI 2 "reg_or_shift_operand" "r,K")))]
  ""
  "asr<s>   %0,%1,%2"
  [(set_attr "type" "logic")])

(define_insn "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "=b,r")
        (ashiftrt:DI (match_operand:DI 1 "register_operand" "0,r")
                     (match_operand:QI 2 "reg_or_32_operand" "r,P")))
   (clobber (reg:SI R_MDC))]
  ""
  "@
    asrd    %2
    #"
  [(set_attr "type" "shiftdi,multi")])

(define_split
  [(set (match_operand:DI 0 "gpc_reg_operand" "")
        (ashiftrt:DI (match_operand:DI 1 "gpc_reg_operand" "")
                     (const_int 32)))
   (clobber (reg:SI R_MDC))]
  "reload_completed"
  [(set (subreg:SI (match_dup 0) 4) (subreg:SI (match_dup 1) 0))
   (set (subreg:SI (match_dup 0) 0)
        (ashiftrt:SI (subreg:SI (match_dup 1) 0) (const_int 31)))]
  "")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Logical Shift Right
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "lshr<mode>3"
  [(set (match_operand:I 0 "register_operand" "=r,r")
        (lshiftrt:I (match_operand:I  1 "register_operand"     "r,r")
                    (match_operand:QI 2 "reg_or_shift_operand" "r,K")))]
  ""
  "lsr<s>   %0,%1,%2"
  [(set_attr "type" "logic")])

(define_insn "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=b,r")
        (lshiftrt:DI (match_operand:DI 1 "register_operand" "0,r")
                     (match_operand:QI 2 "reg_or_32_operand" "r,P")))
   (clobber (reg:SI R_MDC))]
  ""
  "@
    lsrd    %2
    #"
  [(set_attr "type" "shiftdi,multi")])

(define_split
  [(set (match_operand:DI 0 "gpc_reg_operand" "")
        (lshiftrt:DI (match_operand:DI 1 "gpc_reg_operand" "")
                     (const_int 32)))
   (clobber (reg:SI R_MDC))]
  "reload_completed"
  [(set (subreg:SI (match_dup 0) 4) (subreg:SI (match_dup 1) 0))
   (set (subreg:SI (match_dup 0) 0) (const_int 0))]
  "")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bit Test
;;
;; Only SI mode is supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; BITS_BIG_ENDIAN is defined to 1 so operand #1 counts from the MSB.

(define_insn "*btst"
  [(set (cc0)
	(compare
	 (zero_extract:SI
	  (match_operand:SI 0 "register_operand" "r")
	  (const_int 1)
	  (match_operand:QI 1 "const_shift_operand" "K"))
	 (const_int 0)))]
  ""
  "lsr.l   r0,%0,32-%1"
  [(set_attr "type" "cmp")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Negate
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "neg<mode>2"
  [(set (match_operand:I 0 "register_operand" "=r")
        (neg:I (match_operand:I 1 "register_operand" "r")))]
  ""
  "sub<s>   %0,r0,%1"
  [(set_attr "type" "arith")])

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=&r")
        (neg:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "sub.l   %d0,r0,%d1\n\tsubc.l  %0,r0,%1"
  [(set_attr "type" "arith2")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Not
;;
;; Modes QI, HI and SI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:I 0 "register_operand" "=r")
        (not:I (match_operand:I 1 "reg_or_0_operand" "rO")))]
  ""
  "not<s>   %0,%r1"
  [(set_attr "type" "logic")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer comparisons
;;
;; Modes QI, HI and SI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "cmp<mode>"
  [(set (cc0)
        (compare (match_operand:I 0 "register_operand" "r")
                 (match_operand:I 1 "reg_or_0_operand" "rO")))]
  ""
  "cmp<s>   %0,%r1"
  [(set_attr "type" "cmp")])

(define_insn "*cmp_sne<mode>"
  [(set (cc0)
        (compare (not:I (match_operand:I 0 "register_operand" "r"))
                 (const_int -1)))]
  ""
  "cmp<s>   r0,%0"
  [(set_attr "type" "cmp")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Single float operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "addsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (plus:SF (match_operand:SF 1 "fp_reg_operand" "%f")
                 (match_operand:SF 2 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fadd    %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (minus:SF (match_operand:SF 1 "fp_reg_operand" "f")
                  (match_operand:SF 2 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fsub    %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (mult:SF (match_operand:SF 1 "fp_reg_operand" "%f")
                 (match_operand:SF 2 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fmult   %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (div:SF (match_operand:SF 1 "fp_reg_operand" "f")
                (match_operand:SF 2 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fdiv    %0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (sqrt:SF (match_operand:SF 1 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fsqrt   %0,%1"
  [(set_attr "type" "fsqrt")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (neg:SF (match_operand:SF 1 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fneg    %0,%1"
  [(set_attr "type" "fmove")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (abs:SF (match_operand:SF 1 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fabs    %0,%1"
  [(set_attr "type" "fmove")])

(define_expand "copysignsf3"
  [(match_operand:SF 0 "register_operand" "")
   (match_operand:SF 1 "nonmemory_operand" "")
   (match_operand:SF 2 "register_operand" "")]
  "TARGET_FPU && !TARGET_FPU_IEEE"
{
  lmp_expand_copysign (operands, SFmode);
  DONE;
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Single float <-> single integer conversions for !TARGET_FPU_IEEE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;  An FMOVE instruction converts a signalling NaN (zero high order bit of the
;  mantissa) to a quiet NaN (-1). This is acceptable when the data to be
;  moved is in fact a floating-point number, but to avoid nasty surprises
;  integers must in general be kept out of the floating-point registers.
;  HARD_REGNO_MODE_OK thus only allows SFmode in these registers.
;  However, since FTOI and ITOF use floating-point registers for both their
;  inputs and outputs, to use these instructions integers must transiently
;  occupy such registers. To disguise this from the compiler, UNSPECs are
;  used for floating-point operations on integers and floating from general
;  register to floating-point register and fixing in the reverse direction
;  are only split into the individual UNSPEC operations after reload.

(define_insn "*fload_no_ieee"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (unspec:SF [(match_operand:SI 1 "register_operand" "r")] UNSPEC_FLOAD))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "fload   %0,%1"
  [(set_attr "type" "reg_fp")])

(define_insn "*itof_no_ieee"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (unspec:SF [(match_operand:SF 1 "fp_reg_operand" "f")] UNSPEC_ITOF))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "itof    %0,%1"
  [(set_attr "type" "itof")])

(define_insn_and_split "*floatsisf2_no_ieee"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (float:SF (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "#"
  "&& reload_completed"
  [(set (match_dup 0)
        (unspec:SF [(match_dup 1)] UNSPEC_FLOAD))
   (set (match_dup 0)
        (unspec:SF [(match_dup 0)] UNSPEC_ITOF))]
  ""
  [(set_attr "type" "multi")])

(define_insn "*ftoi_no_ieee"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (unspec:SF [(match_operand:SF 1 "fp_reg_operand" "f")] UNSPEC_FTOI))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "ftoi    %0,%1"
  [(set_attr "type" "ftoi")])

(define_insn "*fstore_no_ieee"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SF 1 "fp_reg_operand" "f")] UNSPEC_FSTORE))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "fstore  %0,%1"
  [(set_attr "type" "fp_reg")])

(define_insn_and_split "*fix_truncsfsi2_no_ieee"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(fix:SI (fix:SF (match_operand:SF 1 "fp_reg_operand" "f"))))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "#"
  "&& reload_completed"
  [(set (match_dup 1)
        (unspec:SF [(match_dup 1)] UNSPEC_FTOI))
   (set (match_dup 0)
        (unspec:SI [(match_dup 1)] UNSPEC_FSTORE))]
  ""
  [(set_attr "type" "multi")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Single float <-> single integer conversions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*itof"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (float:SF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU_IEEE"
  "itof    %0,%1"
  [(set_attr "type" "itof")])

(define_expand "floatsisf2"
  [(set (match_operand:SF 0 "fp_reg_operand" "")
        (float:SF (match_operand:SI 1 "register_operand" "")))]
  "TARGET_FPU"
  "")

(define_insn "*ftoi"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:SF (match_operand:SF 1 "fp_reg_operand" "f"))))]
  "TARGET_FPU_IEEE"
  "ftoi    %0,%1"
  [(set_attr "type" "ftoi")])

(define_expand "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(fix:SI (fix:SF (match_operand:SF 1 "fp_reg_operand" ""))))]
  "TARGET_FPU"
  "")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Single float comparisons
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "cmpsf"
  [(set (cc0)
        (compare (match_operand:SF 0 "fp_reg_or_0_operand" "fG")
                 (match_operand:SF 1 "fp_reg_or_0_operand" "fG")))]
  "TARGET_FPU"
{
  return output_fp_compare (operands[0], operands[1], insn);
}
  [(set_attr "type" "fcmp")])

(define_insn "tstsf"
  [(set (cc0)
        (match_operand:SF 0 "fp_reg_operand" "f"))]
  "TARGET_FPU"
{
  return output_fp_compare (operands[0], CONST0_RTX (SFmode), insn);
}
  [(set_attr "type" "fcmp")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Branch unconditionally
;;
;; The unconditinal branch instruction always has a delay slot. The #
;; character in the output statement indicates that the slot should be
;; filled either with a delayed instruction or a NOP.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
{
  return output_ubranch (operands[0], insn);
}
  [(set_attr "type" "branch")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Conditional branch instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "cbranch<mode>4"
  [(set (cc0) (compare
               (match_operand:I 1 "register_operand")
               (match_operand:I 2 "reg_or_0_operand")))
   (set (pc)
        (if_then_else (match_operator 0 "lmp_cbranch_operator"
                       [(cc0) (const_int 0)])
                      (label_ref (match_operand 3 ""))
                      (pc)))]
  ""
  "")

(define_expand "cbranchsf4"
  [(set (cc0) (compare
               (match_operand:SF 1 "fp_reg_operand")
               (match_operand:SF 2 "fp_reg_or_0_operand")))
   (set (pc)
        (if_then_else (match_operator 0 "lmp_cbranch_operator"
                       [(cc0) (const_int 0)])
                      (label_ref (match_operand 3 ""))
                      (pc)))]
  "TARGET_FPU"
  "")

; Now match both normal and inverted branches.

(define_insn "*normal_branch"
  [(set (pc)
        (if_then_else (match_operator 1 "lmp_cbranch_operator"
                       [(cc0) (const_int 0)])
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  return output_cbranch (operands[0], operands[1], 0, insn);
}
  [(set_attr "type" "branch")])

(define_insn "*inverted_branch"
  [(set (pc)
        (if_then_else (match_operator 1 "lmp_cbranch_operator"
                       [(cc0) (const_int 0)])
                      (pc)
                      (label_ref (match_operand 0 "" ""))))]
  ""
{
  return output_cbranch (operands[0], operands[1], 1, insn);
}
  [(set_attr "type" "branch")])

; And then match both normal and inverted returns.

(define_insn "*cond_<return_str>return"
  [(set (pc)
        (if_then_else (match_operator 0 "lmp_cbranch_operator"
                       [(cc0) (const_int 0)])
                      (any_return)
                      (pc)))]
  "<return_pred>"
{
  return output_cbranch (pc_rtx, operands[0], 0, insn);
}
  [(set_attr "type" "ret")])

(define_insn "*inverted_cond_<return_str>return"
  [(set (pc)
        (if_then_else (match_operator 0 "lmp_cbranch_operator"
                       [(cc0) (const_int 0)])
                      (pc)
                      (any_return)))]
  "<return_pred>"
{
  return output_cbranch (pc_rtx, operands[0], 1, insn);
}
  [(set_attr "type" "ret")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Conditional store instructions
;;
;; Modes QI, HI, SI and SF are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0)
	(match_operator:SI 1 "lmp_int_cstore_operator"
	 [(match_operand:I 2 "register_operand")
	  (match_operand:I 3 "reg_or_0_operand")]))]
  ""
{
  lmp_expand_int_cstore (operands, <MODE>mode);
  DONE;
})

(define_expand "cstoresf4"
  [(set (match_operand:SI 0)
	(match_operator:SI 1 "lmp_fp_cstore_operator"
	 [(match_operand:SF 2 "fp_reg_operand")
	  (match_operand:SF 3 "fp_reg_or_0_operand")]))]
  ""
{
  lmp_expand_fp_cstore (operands, SFmode);
  DONE;
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Truncate
;;
;; Truncations among modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (truncate:QI
         (match_operand:HI 1 "register_operand" "r")))]
  ""
  "move.b  %0,%1"
  [(set_attr "type" "logic")])

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (truncate:HI
         (match_operand:SI 1 "register_operand" "r")))]
  ""
  "move.w  %0,%1"
  [(set_attr "type" "logic")])

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (truncate:SI
         (match_operand:DI 1 "register_operand" "r")))]
  ""
  "move.l  %0,%d1		;; truncdisi2"
  [(set_attr "type" "logic")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sign-extension
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (sign_extend:HI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "extb.w  %0,%1"
  [(set_attr "type" "logic")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "extb.l  %0,%1"
  [(set_attr "type" "logic")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "extw.l  %0,%1"
  [(set_attr "type" "logic")])

(define_insn_and_split "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (sign_extend:DI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 2) (ashiftrt:SI (match_dup 1) (const_int 31)))]
{
  operands[2] = operand_subword (operands[0], 0, 0, DImode);
  operands[3] = operand_subword (operands[0], 1, 0, DImode);
}
  [(set_attr "type" "multi")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Zero-extension
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; QI is zero-extended to wider modes by shifting left and then performing
; a logical shift right to insert the zeroes. This avoids the need to use
; another register.

(define_insn_and_split "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (zero_extend:HI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 0) (ashift:HI (match_dup 2) (const_int 8)))
   (set (match_dup 0) (lshiftrt:HI (match_dup 0) (const_int 8)))]
{
  operands[2] = gen_rtx_SUBREG (HImode, operands[1], 0);
}
  [(set_attr "type" "multi")])

(define_insn_and_split "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 0) (ashift:SI (match_dup 2) (const_int 24)))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (const_int 24)))]
{
  operands[2] = gen_rtx_SUBREG (SImode, operands[1], 0);
}
  [(set_attr "type" "multi")])

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "moviu   %0,0"
  [(set_attr "type" "imm_reg")])

(define_insn_and_split "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 2) (const_int 0))]
{
  operands[2] = operand_subword (operands[0], 0, 0, DImode);
  operands[3] = operand_subword (operands[0], 1, 0, DImode);
}
  [(set_attr "type" "multi")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Call subprogram
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Subroutine call instruction returning no value.  Operand 0 is the function
;; to call; operand 1 is the number of bytes of arguments pushed (in mode
;; 'SImode', except it is normally a 'const_int'); operand 2 is the number of
;; registers used as operands.

(define_expand "call"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand 1 "" ""))
	      (use (match_operand 2 "" ""))
	      (clobber (match_dup 3))])]
  ""
{
  if (GET_CODE (XEXP (operands[0], 0)) != REG)
    XEXP (operands[0], 0) = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));

  if (!operands[2])
    operands[2] =  const0_rtx;

  operands[3] = gen_rtx_REG (Pmode, R_LINK);
})

(define_insn "*call_internal"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "l,!r"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (match_operand 3 "" ""))]
  "!SIBLING_CALL_P (insn)"
  "bra     tr,%0,%3%#		;call"
  [(set_attr "type" "call")])

;; Subroutine call instruction returning a value.  Operand 0 is the hard
;; register in which the value is returned.  There are three more operands, the
;; same as the three operands of the 'call' instruction (but with numbers
;; increased by one).

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "register_operand" "")
		   (call (match_operand 1 "" "")
			 (match_operand 2 "" "")))
	      (use (match_operand 3 "" ""))
	      (clobber (match_dup 4))])]
  ""
{
  if (GET_CODE (XEXP (operands[1], 0)) != REG)
    XEXP (operands[1], 0) = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));

  if (!operands[3])
    operands[3] = const0_rtx;

  operands[4] = gen_rtx_REG (Pmode, R_LINK);
})

(define_insn "*call_value_internal"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:SI (match_operand:SI 1 "register_operand" "l,!r"))
	      (match_operand 2 "" "")))
	(use (match_operand 3 "" ""))
	(clobber (match_operand 4 "" ""))]
  "!SIBLING_CALL_P (insn)"
  "bra     tr,%1,%4%#		;call value"
  [(set_attr "type" "call")])

;; Tail calls are similar, except that the link register is not used.  But
;; we don't use r0 as the destination register of the branch because we want
;; the Branch Pre-decode Logic of the GR6 to use the Address Load Array to
;; predict the branch target.

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand 1 "" ""))
	      (use (match_operand 2 "" ""))
	      (clobber (match_dup 3))])]
  ""
{
  if (GET_CODE (XEXP (operands[0], 0)) != REG)
    XEXP (operands[0], 0) = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));

  if (!operands[2])
    operands[2] = const0_rtx;

  operands[3] = gen_rtx_SCRATCH (SImode);
})

(define_insn "*sibcall_internal"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "k"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (match_scratch:SI 3 "=0"))]
  "SIBLING_CALL_P (insn)"
  "bra     tr,%0,%0%#		;sibcall"
  [(set_attr "type" "call")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "register_operand" "")
		   (call (match_operand 1 "" "")
			 (match_operand 2 "" "")))
	      (use (match_operand 3 "" ""))
	      (clobber (match_dup 4))])]
  ""
{
  if (GET_CODE (XEXP (operands[1], 0)) != REG)
    XEXP (operands[1], 0) = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));

  if (!operands[3])
    operands[3] = const0_rtx;

  operands[4] = gen_rtx_SCRATCH (SImode);
})

(define_insn "*sibcall_value_internal"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:SI (match_operand:SI 1 "register_operand" "k"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (match_scratch:SI 4 "=1"))]
  "SIBLING_CALL_P (insn)"
  "bra     tr,%1,%1%#		;sibcall value"
  [(set_attr "type" "call")])

;; Call subroutine returning any type.
(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
{
  int i;

  emit_call_insn (GEN_CALL (operands[0], const0_rtx, NULL, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

  DONE;
})

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "nop")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOP (no-op instruction).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop			;generated nop"
  [(set_attr "type" "nop")])


(define_insn "hazard_nop"
  [(unspec_volatile [(const_int 0)] UNSPEC_NOP)]
  ""
  "nop			;hazard avoidance nop"
  [(set_attr "type" "nop")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Indirect jump
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "bra     tr,%0,r0%#		;indirect jump"
  [(set_attr "type" "abs_branch")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Table jump
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "tablejump"
  [(set (pc)
        (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "bra     tr,%0,r0%#		;tablejump"
  [(set_attr "type" "abs_branch")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; String/block operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; String/block move insn.
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment

(define_expand "movmemsi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
		   (match_operand:BLK 1 "memory_operand" ""))
	      (use (match_operand:SI  2 "general_operand" ""))
	      (use (match_operand:SI  3 "const_int_operand" ""))])]
  ""
{
  if (lmp_expand_block_move (operands))
    DONE;
  else
    FAIL;
})

(define_insn "*bmd"
  [(set (mem:BLK (reg:SI R_R1))
        (mem:BLK (reg:SI R_R2)))
   (use (reg:SI R_R3))
   (clobber (reg:SI R_R1))
   (clobber (reg:SI R_R2))
   (clobber (reg:SI R_R3))
   (clobber (reg:SI R_R4))
   (clobber (reg:SI R_R5))
   (clobber (reg:SI R_R6))]
  "TARGET_BMI"
  "bmd     r1,r2,r3"
  [(set_attr "type" "bmi")])

;; String/block set insn.
;; Argument 0 is the destination
;; Argument 1 is the length
;; Argument 2 is the value
;; Argument 3 is the alignment

(define_expand "setmemsi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
		   (match_operand 2 "nonmemory_operand" ""))
	      (use (match_operand:SI  1 "general_operand" ""))
	      (use (match_operand:SI  3 "const_int_operand" ""))])]
  ""
{
  if (lmp_expand_block_set (operands))
    DONE;
  else
    FAIL;
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Peepholes for Integer Add and Subtract with Carry
;;
;; They are needed because we cannot combine cc0 patterns before reload.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(any_scc:SI (cc0) (const_int 0)))
   (set (match_operand:SI 1 "register_operand" "")
	(plus:SI (match_operand:SI 2 "register_operand" "")
		 (match_dup:SI 0)))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (match_dup:SI 1)
	(plus:SI (match_dup:SI 2)
		 (any_scc:SI (cc0) (const_int 0))))]
  "")

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(any_scc:SI (cc0) (const_int 0)))
   (set (match_operand:SI 1 "register_operand" "")
	(minus:SI (match_operand:SI 2 "register_operand" "")
		  (match_dup:SI 0)))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (match_dup:SI 1)
	(minus:SI (match_dup:SI 2)
		  (any_scc:SI (cc0) (const_int 0))))]
  "")
