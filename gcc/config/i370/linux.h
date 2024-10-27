/* Definitions of target machine for GNU compiler.  System/370 version.
   Copyright (C) 1989, 1993, 1995, 1996, 1997, 2003
   Free Software Foundation, Inc.
   Contributed by Jan Stein (jan@cd.chalmers.se).
   Modified for Linux/390 by Linas Vepstas (linas@linas.org)

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#define TARGET_VERSION fprintf (stderr, " (i370 GNU/Linux with ELF)");

/* Specify that we're generating code for a Linux port to System/370 */

#define TARGET_ELF_ABI

/* Target OS preprocessor built-ins.  */
#define TARGET_OS_CPP_BUILTINS() LINUX_TARGET_OS_CPP_BUILTINS()

/* Options for this target machine.  */

#define LIBGCC_SPEC "libgcc.a%s"

/* ======================================================== */
/* TARGET_EXTRA_SPECS for correct linking on Linux */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "\
%{!shared: crt1.o%s} crti.o%s \
%{!shared: crtbegin.o%s} %{shared: crtbeginS.o%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "\
%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"

#define LINK_START_SPEC "-Ttext 0x10000"

#define CPP_OS_SPEC "-D__unix__ -D__gnu_linux__ -D__linux__ \
%{!ansi: -Dunix -Dlinux } \
-Asystem=unix -Asystem=linux"

/* Define any extra SPECS that the compiler needs to generate.  */
#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS                               \
  { "startfile",          STARTFILE_SPEC },                 \
  { "endfile",            ENDFILE_SPEC },                   \
  { "link_start",         LINK_START_SPEC },                \
  { "cpp_os",             CPP_OS_SPEC },                    \

/* ======================================================== */
