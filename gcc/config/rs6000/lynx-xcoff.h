/* Target independent definitions for LynxOS XCOFF on PowerPC.
   Copyright (C) 1993, 1994, 1995, 1996, 1999, 2000, 2002, 2003, 2004, 2012
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Use the AIX ABI and borrow other bits of the AIX configuration.  */
 
#undef  TARGET_AIX
#define TARGET_AIX 1

/* Use the AIX ABI and borrow other bits of the AIX configuration.  */

#define DEFAULT_ABI ABI_AIX

#define TARGET_NO_TOC 0
#define TARGET_TOC 1

#define FIXED_R2 1
#define FIXED_R13 0

/* Then override generic settings according to the lynx-xcoff
   specificities.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
 "%{!mthreads: \
    %{p:%{pg:%e-p and -pg options are incompatible}} \
    %{p:pinit1.o%s} %{pg:gpinit1.o%s} \
    %{!p:%{!pg:%:getenv(ENV_PREFIX /lib/init1.o)%s}}} \
  %{mthreads: \
    %{p:%{pg:%e-p and -pg options are incompatible}}\
    %{p:pinit1.o%s} %{pg:gpinit1.o%s}\
    %{!p:%{!pg:%:getenv(ENV_PREFIX /lib/thread/init1.o)%s}}}"

#undef ENDFILE_SPEC

#undef LINK_SPEC
#define LINK_SPEC \
  "%{mthreads: -L %:getenv(ENV_PREFIX /lib/thread)} \
   -L %:getenv(ENV_PREFIX /lib) -Ttext 0x10001000 -Tdata 0x20000000"

#undef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 1

#undef RS6000_DEFAULT_LONG_DOUBLE_SIZE
#define RS6000_DEFAULT_LONG_DOUBLE_SIZE 64

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE XCOFF_DEBUG

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS)

/* Define cutoff for using external functions to save floating point and
   general purpose registers.  Neither LynxOS-178 nor our own libgcc.a
   provide them so we disable them altogether.  */

#undef FP_SAVE_INLINE
#define FP_SAVE_INLINE(FIRST_REG) ((FIRST_REG) < 64)

#undef GP_SAVE_INLINE
#define GP_SAVE_INLINE(FIRST_REG) ((FIRST_REG) < 32)

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(file, profile_label_no)

/* Enable dwarf-2 support, and make it the default.  */
#define DWARF2_DEBUGGING_INFO 1
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Indicate that jump tables go in the text section.  Putting them in RO csect
   isn't supported by GAS.  */

#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Just like AIX, Lynx178 already uses descriptors for its standard
   calling sequence.  */
#undef USE_RUNTIME_DESCRIPTORS
#define USE_RUNTIME_DESCRIPTORS 0

/* Disconnect the routines not present in the certified libgcc.  */
#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS rs6000_lynxos_init_libfuncs
