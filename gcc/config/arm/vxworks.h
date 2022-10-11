/* Definitions of target machine for GCC,
   for ARM with targetting the VXWorks run time environment. 
   Copyright (C) 1999, 2000, 2003, 2004, 2007, 2008, 2009, 2010, 2011, 2013
   Free Software Foundation, Inc.

   Contributed by: Mike Stump <mrs@wrs.com>
   Brought up to date by CodeSourcery, LLC.
   
This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#define TARGET_OS_CPP_BUILTINS()		\
  do {						\
    if (TARGET_BIG_END)				\
      builtin_define ("ARMEB");			\
    else					\
      builtin_define ("ARMEL");			\
						\
    if (arm_arch7)				\
      {						\
	if (thumb_code)				\
	  builtin_define ("CPU=ARMARCH7_T2");	\
	else					\
	  builtin_define ("CPU=ARMARCH7");	\
      }						\
    else if (arm_arch_xscale)			\
      builtin_define ("CPU=XSCALE");		\
    else if (arm_arch5)				\
      builtin_define ("CPU=ARMARCH5");		\
    else if (arm_arch4)				\
      {						\
	if (thumb_code)				\
	  builtin_define ("CPU=ARMARCH4_T");	\
	else					\
	  builtin_define ("CPU=ARMARCH4");	\
      }						\
    VXWORKS_OS_CPP_BUILTINS ();			\
  } while (0)

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS VXWORKS_OVERRIDE_OPTIONS

/* Refined from ../vxworks.h.  */
#undef VXWORKS_ADDITIONAL_CPP_SPEC
#define VXWORKS_ADDITIONAL_CPP_SPEC			\
 "%{!nostdinc:						\
    %{isystem*}						\
    %{mrtp: -idirafter %:getenv(WIND_USR /h)		\
            -idirafter %:getenv(WIND_USR /h/wrn/coreip) \
      ;:    -idirafter %:getenv(WIND_BASE /target/h)	\
            -idirafter %:getenv(WIND_BASE /target/h/wrn/coreip)}}"

/* Subsume the arm/elf.h definition, and add RTP hooks.  */
#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "-D__ELF__" VXWORKS_ADDITIONAL_CPP_SPEC

#undef  CC1_SPEC
#define CC1_SPEC							\
"%{tstrongarm:-mlittle-endian -mcpu=strongarm ;				\
   t4:        -mlittle-endian -march=armv4 ;				\
   t4be:      -mbig-endian -march=armv4 ;				\
   t4t:       -mthumb -mthumb-interwork -mlittle-endian -march=armv4t ;	\
   t4tbe:     -mthumb -mthumb-interwork -mbig-endian -march=armv4t ;	\
   t5:        -mlittle-endian -march=armv5 ;				\
   t5be:      -mbig-endian -march=armv5 ;				\
   t5t:       -mthumb -mthumb-interwork -mlittle-endian -march=armv5 ;	\
   t5tbe:     -mthumb -mthumb-interwork -mbig-endian -march=armv5 ;	\
   txscale:   -mlittle-endian -mcpu=xscale ;				\
   txscalebe: -mbig-endian -mcpu=xscale ;				\
   t7:        -mlittle-endian -march=armv7-a ;				\
   t7be:      -mbig-endian -march=armv7-a ;				\
   t7t2:      -mthumb -mlittle-endian -march=armv7-a ;			\
   t7t2be:    -mthumb -mbig-endian -march=armv7-a ;			\
        :     -march=armv7-a}"

/* Pass -EB for big-endian targets.  */
#define VXWORKS_ENDIAN_SPEC \
  "%{mbig-endian|t4be|t4tbe|t5be|t5tbe|txscalebe:-EB}"

#undef SUBTARGET_EXTRA_ASM_SPEC
#define SUBTARGET_EXTRA_ASM_SPEC VXWORKS_ENDIAN_SPEC

#undef LINK_SPEC
#define LINK_SPEC VXWORKS_LINK_SPEC " " VXWORKS_ENDIAN_SPEC

#undef LIB_SPEC
#define LIB_SPEC VXWORKS_LIB_SPEC \
 "%{mrtp:%{!shared: \
    -L%:getenv(WIND_BASE /target/lib_smp/usr/lib/arm/ARMARCH7/common) \
 }}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC VXWORKS_STARTFILE_SPEC

#undef ENDFILE_SPEC
#define ENDFILE_SPEC VXWORKS_ENDFILE_SPEC

/* Setup to produce unwind info for DW2 eh and to
   configure crtstuff accordingly.  */

#undef  DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 1

#define HAS_INIT_SECTION
#undef  INIT_SECTION_ASM_OP
#undef  FINI_SECTION_ASM_OP

#define USE_EH_FRAME_REGISTRY
#define USE_TM_CLONE_REGISTRY 0
#define TARGET_USE_JCR_SECTION 0

/* There is no default multilib.  */
#undef MULTILIB_DEFAULTS

#define FPUTYPE_DEFAULT "vfp"

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER VXWORKS_FUNCTION_PROFILER

/* We want to be compatible with a version of "2.96" at one point in
   the past before this macro was changed.  */
#undef DEFAULT_STRUCTURE_SIZE_BOUNDARY
#define DEFAULT_STRUCTURE_SIZE_BOUNDARY 8

/* The kernel loader does not allow relocations to overflow, so we
   cannot allow arbitrary relocation addends in kernel modules or RTP
   executables.  Also, the dynamic loader uses the resolved relocation
   value to distinguish references to the text and data segments, so we
   cannot allow arbitrary offsets for shared libraries either.  */
#undef ARM_OFFSETS_MUST_BE_WITHIN_SECTIONS_P
#define ARM_OFFSETS_MUST_BE_WITHIN_SECTIONS_P 1

#undef TARGET_DEFAULT_WORD_RELOCATIONS
#define TARGET_DEFAULT_WORD_RELOCATIONS 1

/* Clear the instruction cache from `beg' to `end'.  This is
   implemented in lib1funcs.S, so ensure an error if this definition
   is used.  */
#undef  CLEAR_INSN_CACHE
#define CLEAR_INSN_CACHE(BEG, END) not_used

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

/* This platform supports the probing method of stack checking (RTP mode)
   and the ZCX mechanism. 8K is reserved in the stack to propagate
   exceptions reliably in case of stack overflow. */
#define STACK_CHECK_PROTECT 8192
