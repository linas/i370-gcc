/* Definitions of target machine for GNU compiler.  System/370 version.
   Copyright (C) 1989, 1993, 1995, 1996, 1997 Free Software Foundation, Inc.
   Contributed by Jan Stein (jan@cd.chalmers.se).
   Modified for CMS by Paul Edwards.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Specify that we're generating code for MVS.  */

#define TARGET_MVS 1
#define TARGET_HLASM 1
#define TARGET_EBCDIC 1

/* Specify that we're using the GCC macros */

#define TARGET_PDPMAC 1
#define STARTFILE_SPEC ""

/* Specify that we're using macro prolog/epilog.  */

#define TARGET_MACROS 1

/* Options for the preprocessor for this target machine.  */

#define CPP_SPEC "-trigraphs"

/* Target OS preprocessor built-ins.  */
#define TARGET_OS_CPP_BUILTINS()               \
    do {                                       \
       builtin_define ("__CMS__");             \
       builtin_assert ("system=mvs");          \
    } while (0)

/* PDPCLIB uses "char" for "wchar_t" */
#undef WCHAR_TYPE
#define WCHAR_TYPE "char"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_UNIT

#ifdef PUREISO
#include "pureiso.h"
#endif
