/* IA32 VxWorks target definitions for GNU compiler.
   Copyright (C) 2003, 2004, 2005, 2007, 2010, 2011
   Free Software Foundation, Inc.
   Updated by CodeSourcery, LLC.

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

#undef  ASM_SPEC
#define ASM_SPEC ""

#define TARGET_OS_CPP_BUILTINS()			\
  do							\
    {							\
      VXWORKS_OS_CPP_BUILTINS ();			\
      if (TARGET_386)					\
        builtin_define ("CPU=I80386");			\
      else if (TARGET_486)				\
        builtin_define ("CPU=I80486");			\
      else if (TARGET_PENTIUM)				\
        {						\
          builtin_define ("CPU=PENTIUM");		\
          builtin_define ("CPU_VARIANT=PENTIUM");	\
        }						\
      else if (TARGET_PENTIUMPRO)			\
        {						\
          builtin_define ("CPU=PENTIUM2");		\
          builtin_define ("CPU_VARIANT=PENTIUMPRO");	\
        }						\
      else if (TARGET_PENTIUM4)				\
        {						\
          builtin_define ("CPU=PENTIUM4");		\
          builtin_define ("CPU_VARIANT=PENTIUM4");	\
        }						\
    }							\
  while (0)

#undef  CPP_SPEC
#define CPP_SPEC VXWORKS_ADDITIONAL_CPP_SPEC

/* For -mrtp !-shared which has -l:crt0.o, augment LIB_SPEC to include the
   -L options designating the directories where crt0.o could be found.  */
#undef  LIB_SPEC
#define LIB_SPEC VXWORKS_LIB_SPEC \
 "%{mrtp:%{!shared: \
    %{vxsim: \
      -L%:getenv(WIND_BASE /target/usr/lib/simpentium/SIMPENTIUM/common) \
      -L%:getenv(WIND_BASE /target/lib/usr/lib/simpentium/SIMPENTIUM/common)} \
    %{!vxsim: \
      -L%:getenv(WIND_BASE /target/usr/lib/pentium/PENTIUM/common) \
      -L%:getenv(WIND_BASE /target/lib/usr/lib/pentium/PENTIUM/common)}}}"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC VXWORKS_STARTFILE_SPEC
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC VXWORKS_ENDFILE_SPEC
#undef  LINK_SPEC
#define LINK_SPEC VXWORKS_LINK_SPEC

#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES EXTRA_SUBTARGET_SWITCHES

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS VXWORKS_OVERRIDE_OPTIONS

/* No _mcount profiling on VxWorks.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE,LABELNO) VXWORKS_FUNCTION_PROFILER(FILE,LABELNO)

/* We cannot use PC-relative accesses for VxWorks PIC because there is no
   fixed gap between segments.  */
#undef ASM_PREFERRED_EH_DATA_FORMAT

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

/* This platform supports the probing method of stack checking (RTP mode)
   and the ZCX mechanism. 8K is reserved in the stack to propagate
   exceptions reliably in case of stack overflow. */
#define STACK_CHECK_PROTECT 8192

/* Support frame-pointer based unwinding.  */
#define USE_IX86_FRAME_POINTER 1
