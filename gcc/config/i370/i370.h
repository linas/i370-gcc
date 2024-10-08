/* Definitions of target machine for GNU compiler.  System/370 version.
   Copyright (C) 1989, 1993, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
   2003 Free Software Foundation, Inc.
   Contributed by Jan Stein (jan@cd.chalmers.se).
   Modified for OS/390 LanguageEnvironment C by Dave Pitts (dpitts@cozx.com)
   Modified for Linux-ELF/390 by Linas Vepstas (linas@linas.org)

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

#ifndef GCC_I370_H
#define GCC_I370_H

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("GCC");		\
      builtin_define_std ("gcc");		\
      builtin_assert ("machine=i370");		\
      builtin_assert ("cpu=i370");		\
    }						\
  while (0)

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* The sizes of the code and literals on the current page.  */

extern int mvs_page_code, mvs_page_lit;

/* The current page number and the base page number for the function.  */

extern int mvs_page_num, function_base_page;

/* The name of the current function.  */

extern char *mvs_function_name;

/* The length of the function name malloc'd area.  */

extern size_t mvs_function_name_length;

/* Compile using char instructions (mvc, nc, oc, xc).  On 4341 use this since
   these are more than twice as fast as load-op-store.
   On 3090 don't use this since load-op-store is much faster.
   On Hercules it seems that the char instructions make a
   module slightly faster.  */


#define TARGET_CHAR_INSTRUCTIONS (target_flags & 1)

/* Compile experimental position independent code */
#define TARGET_PIC (target_flags & 2)
extern int i370_enable_pic;

/* Default target switches */
/* This appears to be what switches
   target char instructions on by default */

#define TARGET_DEFAULT 1

/* The desired CSECT name */
extern char *mvs_csect_name;

#ifdef TARGET_HLASM
/* HLASM requires #pragma map.  */
#define REGISTER_TARGET_PRAGMAS() \
  do { \
  cpp_register_pragma (PFILE, 0, "map", i370_pr_map); \
  cpp_register_pragma (PFILE, 0, "nomargins", i370_pr_skipit); \
  cpp_register_pragma (PFILE, 0, "nosequence", i370_pr_skipit); \
  cpp_register_pragma (PFILE, 0, "checkout", i370_pr_checkout); \
  cpp_register_pragma (PFILE, 0, "linkage", i370_pr_linkage); \
  } while(0)
#endif /* TARGET_HLASM */

#define SUBTARGET_OVERRIDE_OPTIONS i370_override_options()

/* To use IBM supplied macro function prologue and epilogue, define the
   following to 1.  Should only be needed if IBM changes the definition
   of their prologue and epilogue.  */

#define MACROPROLOGUE 0
#define MACROEPILOGUE 0

/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered in instructions
   that operate on numbered bit-fields.  */

#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */

#define BYTES_BIG_ENDIAN 1

/* Define this if MS word of a multiword is the lowest numbered.  */

#define WORDS_BIG_ENDIAN 1

/* Width of a word, in units (bytes).  */

#define UNITS_PER_WORD 4

/* Allocation boundary (in *bits*) for storing pointers in memory.  */

#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */

#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */

#define STACK_BOUNDARY 64

/* Allocation boundary (in *bits*) for the code of a function.  */

#define FUNCTION_BOUNDARY 32

/* There is no point aligning anything to a rounder boundary than this.  */

#define BIGGEST_ALIGNMENT 64

/* Alignment of field after `int : 0' in a structure.  */

#define EMPTY_FIELD_BOUNDARY 32

/* Define this if move instructions will actually fail to work when given
   unaligned data.  */

#define STRICT_ALIGNMENT 0

/* Define target floating point format.  */

#define TARGET_FLOAT_FORMAT IBM_FLOAT_FORMAT

/* #define REAL_ARITHMETIC */

/* Define character mapping for cross-compiling.  */
/* but only define it if really needed, since otherwise it will break builds */

#ifdef TARGET_EBCDIC
#ifdef HOST_EBCDIC
#define MAP_CHARACTER(c) ((char)(c))
#else
#define MAP_CHARACTER(c) ((char)mvs_map_char (c))
#endif
#endif

/* Define maximum length of page minus page escape overhead.  */

#define MAX_MVS_PAGE_LENGTH 4060

/* Define special register allocation order desired.
   Don't fiddle with this.  I did, and I got all sorts of register
   spill errors when compiling even relatively simple programs...
   I have no clue why ...
   E.g. this one is bad:
   { 0, 1, 2, 9, 8, 7, 6, 5, 10, 15, 14, 12, 3, 4, 16, 17, 18, 19, 11, 13 }
 */

#define REG_ALLOC_ORDER							\
   { 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 10, 15, 14, 12, 16, 17, 18, 19, 11, 13 }

/* Standard register usage.  */

/* Number of actual hardware registers.  The hardware registers are
   assigned numbers for the compiler from 0 to just below
   FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.
   For the 370, we give the data registers numbers 0-15,
   and the floating point registers numbers 16-19.  */

#define FIRST_PSEUDO_REGISTER 20

#if defined(TARGET_HLASM) || defined(TARGET_PDOSGB)

/* 1 for registers that have pervasive standard uses and are not available
   for the register allocator.  These are registers that must have fixed,
   valid values stored in them for the entire length of the subroutine call,
   and must not in any way be moved around, jiggered with, etc. That is,
   they must never be clobbered, and, if clobbered, the register allocator
   will never restore them back.

   For the LE/370 mode, we use five registers in this special way:
   -- R3 which is used as the base register
   -- R4 the page origin table pointer used to load R3,
   -- R11 the arg pointer.
   -- R12 the TCA pointer
   -- R13 the stack (DSA) pointer

   For TARGET_DIGNUS or TARGET_PDPMAC mode:
   -- R10 the page origin table pointer used to load R3,
   -- R11 the arg pointer.
   -- R12 the base register.
   -- R13 the stack pointer

   A fifth register is also exceptional: R14 is used in many branch
   instructions to hold the target of the branch.  Technically, this
   does not qualify R14 as a register with a long-term meaning; it should
   be enough, theoretically, to note that these instructions clobber
   R14, and let the compiler deal with that.  In practice, however,
   the "clobber" directive acts as a barrier to optimization, and the
   optimizer appears to be unable to perform optimizations around branches.
   Thus, a much better strategy appears to give R14 a pervasive use;
   this eliminates it from the register pool without hurting optimization.

   There are other registers which have special meanings, but its OK
   for them to get clobbered, since other allocator config below will
   make sure that they always have the right value.  These are for
   example:
   -- R1 the returned structure pointer.
   -- R10 the static chain reg.
   -- R15 holds the value a subroutine returns.

   Notice that it is *almost* safe to mark R11 as available to the allocator.
   By marking it as a call_used_register, in most cases, the compiler
   can handle it being clobbered.  However, there are a few rare
   circumstances where the register allocator will allocate r11 and
   also try to use it as the arg pointer ... thus it must be marked fixed.
   I think this is a bug, but I can't track it down...
 */

/* Define base and page registers.  */
#define BASE_REGISTER 3
#define PAGE_REGISTER 4

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 13

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 13

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be
   accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define TARGET_FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 11

#if defined(TARGET_DIGNUS) || defined(TARGET_PDPMAC) \
    || defined(TARGET_PDOSGB)
#undef PAGE_REGISTER
#undef BASE_REGISTER
#define PAGE_REGISTER 10
#define BASE_REGISTER 12

#ifdef TARGET_DIGNUS
#define FIXED_REGISTERS 						\
{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0 }
/*0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19*/
#endif

#if defined(TARGET_PDPMAC) || defined(TARGET_PDOSGB)
/* made register 1 fixed because it is used
   for parameter passing, otherwise we DO
   have a problem! */
/* also made register 0 fixed, because I am using that
   for the struct, instead of 1.  Why would anyone
   choose 1 for the struct when it is being used
   already for the parameters? */
#define FIXED_REGISTERS 						\
{ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0 }
/*0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19*/
#endif

/* 1 for registers not available across function calls.  These must include
   the FIXED_REGISTERS and also any registers that can be used without being
   saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   NOTE: all floating registers are undefined across calls.
*/

#define CALL_USED_REGISTERS 						\
{ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }
/*0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19*/
#endif

#ifdef TARGET_LE
#define FIXED_REGISTERS 						\
{ 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 }
/*0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19*/

/* 1 for registers not available across function calls.  These must include
   the FIXED_REGISTERS and also any registers that can be used without being
   saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   NOTE: all floating registers are undefined across calls.
*/

#define CALL_USED_REGISTERS 						\
{ 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1 }
/*0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19*/
#endif /* TARGET_LE */

#endif /* TARGET_HLASM */

/* ================= */
#ifdef TARGET_ELF_ABI

/* The Linux/ELF ABI uses a register layout similar to
   the MVS/OE version, yet with various changes:
   -- r3 is used as the base register
   -- r4 is not used; its role is taken by 0(r13)
      This is the page-origin; every now and then,
      r3 is reloaded from it.
   -- r12 is used as a base pointer into the data section
      but only if i370_enable_pic is true; otherwise we can
      free up this register. It implements a kind-of-like
      PLT, but per-function, instead of being grouped into
      one location..

   -- r13 is used as a combined argument & frame pointer
   -- r11 is used to point to the top of the stack. Dynamic
      allocations such as alloca() happen above r11.

   The stack grow upwards, so r11 is always larger than r13.
   Some prototyping was done for a downwards-growing stack,
   but it is an unfinished folly.

   Note that the ELF calling convention is radically different
   than the MVS/OE convention.  In particular, r11 always points
   to the top of the stack, and r13 always points to the bottom
   of the stack.  Thus, r13 can be used as a dual arg & frame
   pointer for all occasions, whereas r11 can be used for alloca
   and other stack-dynamic allocations.

   XXX Future enhancement possible: When a function doesn't have
   any args, and doesn't use alloca(), then r11 is not really needed.
 */

/* Define base and page registers.  */
#define BASE_REGISTER 3
#define PIC_BASE_REGISTER 12

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 11

/* Base register for access to local variables of the function.
   A separate stack and frame pointer is required for any function
   that calls alloca() or does other pushing onto the stack. */
#define FRAME_POINTER_REGNUM 13

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be
   accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define TARGET_FRAME_POINTER_REQUIRED 1

/* Function epilogue uses the frame pointer to restore the context */
#define EXIT_IGNORE_STACK 1

/* Base register for access to arguments of the function.
   We will use the frame pointer as the arg pointer. */
#define ARG_POINTER_REGNUM 13


#define FIXED_REGISTERS 						\
{ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 }
/*0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19*/

#define CALL_USED_REGISTERS 						\
{ 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1 }
/*0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19*/

#endif /* TARGET_ELF_ABI */
/* ================= */


/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.
   Note that DCmode (complex double) needs two regs.  */

#define TARGET_HARD_REGNO_NREGS(REGNO, MODE) 					\
  ((REGNO) > 15 ? 							\
   ((GET_MODE_SIZE (MODE) + 2*UNITS_PER_WORD - 1) / (2*UNITS_PER_WORD)) :	\
   (GET_MODE_SIZE(MODE)+UNITS_PER_WORD-1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the 370, the cpu registers can hold QI, HI, SI, SF and DF.  The
   even registers can hold DI.  The floating point registers can hold
   either SF, DF, SC or DC.  */

#define TARGET_HARD_REGNO_MODE_OK(REGNO, MODE)					\
  ((REGNO) < 16 ? (((REGNO) & 1) == 0 || 				\
		  (((MODE) != DImode) && ((MODE) != DFmode)))		\
		: ((MODE) == SFmode || (MODE) == DFmode) ||		\
                   (MODE) == SCmode || (MODE) == DCmode)

/* Value is 1 if it is a good idea to tie two pseudo registers when one has
   mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define TARGET_MODES_TIEABLE_P(MODE1, MODE2)					\
  (((MODE1) == SFmode || (MODE1) == DFmode)				\
   == ((MODE2) == SFmode || (MODE2) == DFmode))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* 370 PC isn't overloaded on a register.  */

/* #define PC_REGNUM */

/* ------------------------------------------------------------------- */

/* R10 is register in which static-chain is passed to a function.
   Static-chaining is done when a nested function references as a global
   a stack variable of its parent: e.g.
        int parent_func (int arg) {
             int x;                            // x is in parents stack
             void child_func (void) { x++: }   // child references x as global var
             ...
        }
 */

#define STATIC_CHAIN_REGNUM 10

/* R1 is register in which address to store a structure value is passed to
   a function.  This is used only when returning 64-bit long-long in a 32-bit arch
   and when calling functions that return structs by value. e.g.
        typedef struct A_s { int a,b,c; } A_t;
        A_t fun_returns_value (void) {
            A_t a; a.a=1; a.b=2 a.c=3;
            return a;
        }
   In the above, the storage for the return value is in the callers stack, and
   the R1 points at that mem location.
 */

#if defined(TARGET_PDPMAC) || defined(TARGET_PDOSGB)
#define I370_STRUCT_VALUE_REGNUM 0
#else
#define I370_STRUCT_VALUE_REGNUM 1
#endif

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

enum reg_class
  {
    NO_REGS, ADDR_REGS, DATA_REGS,
    FP_REGS, ALL_REGS, LIM_REG_CLASSES
  };

#define GENERAL_REGS DATA_REGS
#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES 						\
{ "NO_REGS", "ADDR_REGS", "DATA_REGS", "FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.  This is an initializer for
   a vector of HARD_REG_SET of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {{0}, {0x0fffe}, {0x0ffff}, {0xf0000}, {0xfffff}}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) 						\
  ((REGNO) >= 16 ? FP_REGS : (REGNO) != 0 ? ADDR_REGS : DATA_REGS)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS ADDR_REGS
#define BASE_REG_CLASS ADDR_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C)					\
  ((C) == 'a' ? ADDR_REGS :						\
  ((C) == 'd' ? DATA_REGS :						\
  ((C) == 'f' ? FP_REGS   : NO_REGS)))

/* The letters I, J, K, L and M in a register constraint string can be used
   to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)					\
  ((C) == 'I' ? (unsigned) (VALUE) < 256 :				\
   (C) == 'J' ? (unsigned) (VALUE) < 4096 :				\
   (C) == 'K' ? (VALUE) >= -32768 && (VALUE) < 32768 : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  1

/* see recog.c for details */
#define EXTRA_CONSTRAINT(OP,C)						\
   ((C) == 'R' ? r_or_s_operand (OP, GET_MODE(OP)) :			\
    (C) == 'S' ? s_operand (OP, GET_MODE(OP)) :	0)			\

/* Given an rtx X being reloaded into a reg required to be in class CLASS,
   return the class of reg to actually use.  In general this is just CLASS;
   but on some machines in some cases it is preferable to use a more
   restrictive class.

   XXX We reload CONST_INT's into ADDR not DATA regs because on certain
   rare occasions when lots of egisters are spilled, reload() will try
   to put a const int into r0 and then use r0 as an index register.
*/

#define PREFERRED_RELOAD_CLASS(X, CLASS)				\
    (GET_CODE(X) == CONST_DOUBLE ? FP_REGS :				\
     GET_CODE(X) == CONST_INT ? (reload_in_progress ? ADDR_REGS : DATA_REGS) :	\
     GET_CODE(X) == LABEL_REF ||					\
     GET_CODE(X) == SYMBOL_REF ||					\
     GET_CODE(X) == CONST ? ADDR_REGS : (CLASS))

/* Return the maximum number of consecutive registers needed to represent
   mode MODE in a register of class CLASS.
   Note that DCmode (complex double) needs two regs.
*/

#define CLASS_MAX_NREGS(CLASS, MODE)					\
  ((CLASS) == FP_REGS ? 						\
   ((GET_MODE_SIZE (MODE) + 2*UNITS_PER_WORD - 1) / (2*UNITS_PER_WORD)) :	\
   (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)


/* ------------------------------------------------------------------- */
/* Stack layout; function entry, exit and calling.  */
/* ================= */
#if defined(TARGET_HLASM) || defined(TARGET_PDOSGB)

/* Define offset from stack pointer, to location where a parm can be
   pushed.  */

#if defined(TARGET_DIGNUS) || defined(TARGET_PDPMAC) \
    || defined(TARGET_PDOSGB)
  #define STACK_POINTER_OFFSET 88
  #define STACK_FRAME_BASE 88
#elif defined(TARGET_LE)
  #define STACK_POINTER_OFFSET 148
  #define STACK_FRAME_BASE 28
#else
  #define STACK_POINTER_OFFSET 148
  #define STACK_FRAME_BASE 88
#endif


/* used in i370.md for temp scratch area */
#if defined(TARGET_DIGNUS) || defined(TARGET_PDPMAC) \
    || defined(TARGET_PDOSGB)
#define CONVLO "80"
#define CONVHI "84"
#else
#define CONVLO "140"
#define CONVHI "144"
#endif

/* Offset within stack frame to start allocating local variables at.
   It is the offset to the BEGINNING of the first local allocated.  */

#define STARTING_FRAME_OFFSET  						\
     (STACK_POINTER_OFFSET + current_function_outgoing_args_size)

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = STARTING_FRAME_OFFSET

/* If we generate an insn to push BYTES bytes, this says how many the stack
   pointer really advances by.  On the 370, we have no push instruction.  */

#define FIRST_PARM_OFFSET(FNDECL) 0

/* Accumulate the outgoing argument count so we can request the right
   DSA size and determine stack offset.  */

#define ACCUMULATE_OUTGOING_ARGS 1

#ifdef TARGET_PDOSGB
#define ASM_COMMENT_START "*"
#define ASM_APP_OFF ""
#define ASM_APP_ON ""

/* stop generating call to __main */
#define HAS_INIT_SECTION

#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)			\
  sprintf (LABEL, ".%s%d", PREFIX, NUM)

#define MAX_CHUNK 32767

#define ASM_OUTPUT_SKIP(FILE, SIZE)  					\
{									\
  int s, k;								\
  for (s = (SIZE); s > 0; s -= MAX_CHUNK)				\
    {									\
      if (s > MAX_CHUNK)						\
	k = MAX_CHUNK;							\
      else								\
	k = s;								\
      fprintf (FILE, ".rept %d\n.byte 0\n.endr\n", k);			\
    }									\
}

#endif

#endif /* TARGET_HLASM */

/* ================= */
#ifdef TARGET_ELF_ABI
/*
   The ELF target has the stackframe growing upward, and thus
   ... #define STACK_GROWS_DOWNWARD
   ... #define FRAME_GROWS_DOWNWARD
   are never set. There's some experimentation with having the stack
   grow the other way, in the i370.c file, but it is incomplete.
   Note that STARTING_FRAME_OFFSET would have to be fixed.

   Here's the stack layout as currently designed:

   r11 -- top of stack aka stack pointer
   -4(r11) -- last local (stack) variable)
   ...          ...
   88+4*nargs(r13) -- first local (stack) variable.
   ...          ...
   92(r13) -- second incoming (callee) argument
   88(r13) -- first incoming (callee) argument
   84(r13) -- volatile scratch area
   80(r13) -- volatile scratch area
   76(r13) -- not used (frame size)
   72(r13) -- not used
   68(r13) -- saved callers r12
   64(r13) -- saved callers r11
   ...          ...
   28(r13) -- saved callers r2
   24(r13) -- saved callers r1
   20(r13) -- saved callers r0
   16(r13) -- saved callers r15
   12(r13) -- saved callers r14
   8(r13)  -- saved callers r13
   4(r13)  -- not used
   0(r13)  -- code page table pointer
   r13 -- bottom of stack aka frame pointer aka arg pointer

   Note that this bears superficial similarity to the MVS/OE stack layout,
   but in fact it is very very different.  In particular, under MVS/OE
   the roles of r11 and r13 are quite different.

   Note that the use of varargs/stdarg is limited to 512 bytes of
   of arguments.  This is the price that is paid for freeing up a
   register and having a more efficient function return.
*/

/* Define size of the calling convention register save area.
   This includes room for the 16 GPR's, a saved frame size, and
   a (floating point math) scratch area */
#define I370_SAVE_AREA_SIZE 88

/* Define the size of the amount of room reserved for varargs */
#define I370_VARARGS_AREA_SIZE 512

/* Used in i370.md for temp scratch area. Must be that last two words
   of the I370_SAVE_AREA. */
#define CONVLO "80"
#define CONVHI "84"

/* Define offset from stack pointer, to location where a parm can be
   pushed.  */

#define STACK_POINTER_OFFSET I370_SAVE_AREA_SIZE

#define STACK_DYNAMIC_OFFSET(FNDECL) 0

/* Offset within frame to start allocating local variables at.
   It is the offset to the BEGINNING of the first local allocated.  */

#define STARTING_FRAME_OFFSET                  \
     (current_function_stdarg ?    \
     (I370_SAVE_AREA_SIZE + I370_VARARGS_AREA_SIZE):       \
     (I370_SAVE_AREA_SIZE + current_function_args_size))

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = STARTING_FRAME_OFFSET

/* Offset of first incoming parameter from the arg ptr register value.  */

#define FIRST_PARM_OFFSET(FNDECL) I370_SAVE_AREA_SIZE

/* The ACCUMULATE_OUTGOING_ARGS flag seems to have some funny side effects
   that we need.  Specifically, if it is set, then the stack pointer is
   not bumped when args are placed on the stack, which is just how we want
   it. */
#define ACCUMULATE_OUTGOING_ARGS 1

#endif /* TARGET_ELF_ABI */
/* ================= */

/* If we generate an insn to push BYTES bytes, this says how many the stack
   pointer really advances by.  On the 370, we have no push instruction.  */

/* #define PUSH_ROUNDING(BYTES) */

/* 1 if N is a possible register number for function argument passing.
   On the 370, no registers are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list during
   the scan of that argument list.  This data type should hold all
   necessary information about the function itself and about the args
   processed so far, enough to enable macros such as FUNCTION_ARG to
   determine where the next arg should go.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS for a call to
   a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  ((CUM) = 0)

/* Update the data in CUM to advance over an argument of mode MODE and
   data type TYPE.  (TYPE is null for libcalls where that information
   may not be available.) */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
 ((CUM) += ((MODE) == DFmode || (MODE) == SFmode			\
	    ? 256							\
	    : (MODE) != BLKmode                 			\
	    ? (GET_MODE_SIZE (MODE) + 3) / 4 				\
	    : (int_size_in_bytes (TYPE) + 3) / 4))

/* Define where to put the arguments to a function.  Value is zero to push
   the argument on the stack, or a hard register in which to store the
   argument.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) 0

/* For an arg passed partly in registers and partly in memory, this is the
   number of registers used.  For args passed entirely in registers or
   entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* Define if returning from a function call automatically pops the
   arguments described by the number-of-args field in the call.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* The FUNCTION_VALUE macro defines how to find the value returned by a
   function.  VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is NULL.

   On the 370 the return value is in R15 or R16.  However,
   DImode (64-bit ints) scalars need to get returned on the stack,
   with r15 pointing to the location.  To accomplish this, we define
   the RETURN_IN_MEMORY macro to be true for both blockmode (structures)
   and the DImode scalars.
 */

#define RET_REG(MODE)	\
    (((MODE) == DCmode || (MODE) == SCmode \
      || (MODE) == DFmode || (MODE) == SFmode) ? 16 : 15)

#define FUNCTION_VALUE(VALTYPE, FUNC)  					\
  gen_rtx_REG (TYPE_MODE (VALTYPE), RET_REG (TYPE_MODE (VALTYPE)))

#define RETURN_IN_MEMORY(VALTYPE)  \
  ((DImode == TYPE_MODE (VALTYPE)) || (BLKmode == TYPE_MODE (VALTYPE)))

/* Define how to find the value returned by a library function assuming
   the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)  gen_rtx_REG (MODE, RET_REG (MODE))

/* 1 if N is a possible register number for a function value.
   On the 370 under C/370, R15 and R16 are thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 15 || (N) == 16)

/* This macro definition sets up a default value for `main' to return.  */

#define DEFAULT_MAIN_RETURN  c_expand_return (integer_zero_node)


/* Output assembler code for a block containing the constant parts of a
   trampoline, leaving space for the variable parts.

   On the 370, the trampoline contains these instructions:

        BALR  14,0
        USING *,14
        L     STATIC_CHAIN_REGISTER,X
        L     15,Y
        BR    15
   X    DS    0F
   Y    DS    0F  */
/*
   I am confused as to why this emitting raw binary, instead of instructions ...
   see for example, rs6000/rs000.c for an example of a different way to
   do this ... especially since BASR should probably be substituted for BALR.
 */

#define TRAMPOLINE_TEMPLATE(FILE)					\
{									\
  assemble_aligned_integer (2, GEN_INT (0x05E0));			\
  assemble_aligned_integer (2, GEN_INT (0x5800 | STATIC_CHAIN_REGNUM << 4)); \
  assemble_aligned_integer (2, GEN_INT (0xE00A));			\
  assemble_aligned_integer (2, GEN_INT (0x58F0)); 			\
  assemble_aligned_integer (2, GEN_INT (0xE00E));			\
  assemble_aligned_integer (2, GEN_INT (0x07FF));			\
  assemble_aligned_integer (2, const0_rtx);				\
  assemble_aligned_integer (2, const0_rtx);				\
  assemble_aligned_integer (2, const0_rtx);				\
  assemble_aligned_integer (2, const0_rtx);				\
}

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 20

/* Emit RTL insns to initialize the variable parts of a trampoline.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 12)), CXT); \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 16)), FNADDR); \
}

/* Define EXIT_IGNORE_STACK if, when returning from a function, the stack
   pointer does not matter (provided there is a frame pointer).  */

#define EXIT_IGNORE_STACK	1

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* These assume that REGNO is a hard or pseudo reg number.  They give
   nonzero only if REGNO is a hard reg of the suitable class or a pseudo
   reg currently allocated to a suitable hard reg.
   These definitions are NOT overridden anywhere.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) 					\
  (((REGNO) > 0 && (REGNO) < 16)					\
    || (reg_renumber[REGNO] > 0 && reg_renumber[REGNO] < 16))

#define REGNO_OK_FOR_BASE_P(REGNO) REGNO_OK_FOR_INDEX_P(REGNO)

#define REGNO_OK_FOR_DATA_P(REGNO) 					\
  ((REGNO) < 16 || (unsigned) reg_renumber[REGNO] < 16)

#define REGNO_OK_FOR_FP_P(REGNO) 					\
  ((unsigned) ((REGNO) - 16) < 4 || (unsigned) (reg_renumber[REGNO] - 16) < 4)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.  */

/* 1 if X is a data register.  */

#define DATA_REG_P(X) (REG_P (X) && REGNO_OK_FOR_DATA_P (REGNO (X)))

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* 1 if X is an address register.  */

#define ADDRESS_REG_P(X) (REG_P (X) && REGNO_OK_FOR_BASE_P (REGNO (X)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#ifdef TARGET_HLASM
/* HLASM passes through i370_encode_section_info() for symbols */
#define CONSTANT_ADDRESS_P(X)						\
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
  || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST_DOUBLE		\
  || (GET_CODE (X) == CONST						\
	  && GET_CODE (XEXP (XEXP (X, 0), 0)) == LABEL_REF)		\
  || (GET_CODE (X) == CONST						\
	  && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF		\
	  && !SYMBOL_REF_FLAG (XEXP (XEXP (X, 0), 0))))
#endif

#ifdef TARGET_ELF_ABI
#define CONSTANT_ADDRESS_P(X)						\
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
  || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST_DOUBLE		\
  || (GET_CODE (X) == CONST						\
	  && GET_CODE (XEXP (XEXP (X, 0), 0)) == LABEL_REF)		\
  || (GET_CODE (X) == CONST						\
	  && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF		\
	  && !SYMBOL_REF_EXTERNAL_P (XEXP (XEXP (X, 0), 0))))
#endif

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) 1

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx and check
   its validity for a certain class.  We have two alternate definitions
   for each of them.  The usual definition accepts all pseudo regs; the
   other rejects them all.  The symbol REG_OK_STRICT causes the latter
   definition to be used.

   Most source files want to accept pseudo regs in the hope that they will
   get allocated to the class that the insn wants them to be in.
   Some source files that are used after register allocation
   need to be strict.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index or if it is
  a pseudo reg.  */

#define REG_OK_FOR_INDEX_P(X)						\
  ((REGNO(X) > 0 && REGNO(X) < 16) || REGNO(X) >= 20)

/* Nonzero if X is a hard reg that can be used as a base reg or if it is
   a pseudo reg.  */

#define REG_OK_FOR_BASE_P(X)	REG_OK_FOR_INDEX_P(X)

#else /* REG_OK_STRICT */

/* Nonzero if X is a hard reg that can be used as an index.  */

#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P(REGNO(X))

/* Nonzero if X is a hard reg that can be used as a base reg.  */

#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P(REGNO(X))

#endif /* REG_OK_STRICT */

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression that is a
   valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is actually machine-independent.
*/

#define COUNT_REGS(X, REGS, FAIL)					\
 if (REG_P (X)) {							\
   if (REG_OK_FOR_BASE_P (X)) REGS += 1;				\
   else goto FAIL;							\
 }									\
 else if (GET_CODE (X) != CONST_INT || (unsigned) INTVAL (X) >= 4096)	\
   goto FAIL;

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{									\
  if (REG_P (X) && REG_OK_FOR_BASE_P (X))				\
    goto ADDR;								\
  if (GET_CODE (X) == PLUS)						\
    {									\
      int regs = 0;							\
      rtx x0 = XEXP (X, 0);						\
      rtx x1 = XEXP (X, 1);						\
      if (GET_CODE (x0) == PLUS)					\
	{								\
	  COUNT_REGS (XEXP (x0, 0), regs, FAIL);			\
	  COUNT_REGS (XEXP (x0, 1), regs, FAIL);			\
	  COUNT_REGS (x1, regs, FAIL);					\
	  if (regs == 2)						\
	    goto ADDR;							\
	}								\
      else if (GET_CODE (x1) == PLUS)					\
	{								\
	  COUNT_REGS (x0, regs, FAIL);					\
	  COUNT_REGS (XEXP (x1, 0), regs, FAIL);			\
	  COUNT_REGS (XEXP (x1, 1), regs, FAIL);			\
	  if (regs == 2)						\
	    goto ADDR;							\
	}								\
      else								\
	{								\
	  COUNT_REGS (x0, regs, FAIL);					\
	  COUNT_REGS (x1, regs, FAIL);					\
	  if (regs != 0)						\
	    goto ADDR;							\
	}								\
    }									\
  FAIL: ;								\
}

/* The 370 has no mode dependent addresses.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)

/* Macro: LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)
   Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   Several comments:
   (1) It's not obvious that this macro results in better code
       than its omission does. For historical reasons we leave it in.

   (2) This macro may be (???) implicated in the accidental promotion
       or RS operand to RX operands, which bombs out any RS, SI, SS
       instruction that was expecting a simple address.  Note that
       this occurs fairly rarely ...

   (3) There is a bug somewhere that causes either r4 to be spilled,
       or causes r0 to be used as a base register.  Changing the macro
       below will make the bug move around, but will not make it go away
       ... Note that this is a rare bug ...

 */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)				\
{									\
  if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 1)))		\
    (X) = gen_rtx_PLUS (SImode, XEXP (X, 0),				\
			copy_to_mode_reg (SImode, XEXP (X, 1)));	\
  if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 0)))		\
    (X) = gen_rtx_PLUS (SImode, XEXP (X, 1),				\
			copy_to_mode_reg (SImode, XEXP (X, 0)));	\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == MULT)		\
    (X) = gen_rtx_PLUS (SImode, XEXP (X, 1),				\
			force_operand (XEXP (X, 0), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == MULT)		\
    (X) = gen_rtx_PLUS (SImode, XEXP (X, 0),				\
			force_operand (XEXP (X, 1), 0));		\
  if (memory_address_p (MODE, X))					\
    goto WIN;								\
}

/* Specify the machine mode that this machine uses for the index in the
   tablejump instruction.  */

#define CASE_VECTOR_MODE SImode

/* Define this if the tablejump instruction expects the table to contain
   offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */

/* #define CASE_VECTOR_PC_RELATIVE */

/* Define this if fixuns_trunc is the same as fix_trunc.  */

#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* We use "unsigned char" as default.  */

#define DEFAULT_SIGNED_CHAR 0

/* Max number of bytes we can move from memory to memory in one reasonably
   fast instruction.  */

#define MOVE_MAX 256

/* Nonzero if access to memory by bytes is slow and undesirable.  */

#define SLOW_BYTE_ACCESS 1

/* Define if shifts truncate the shift count which implies one can omit
   a sign-extension or zero-extension of a shift count.  */

/* #define SHIFT_COUNT_TRUNCATED */

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC)	(OUTPREC != 16)

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

/* #define STORE_FLAG_VALUE -1 */

/* When a prototype says `char' or `short', really pass an `int'.  */

#define PROMOTE_PROTOTYPES 1

/* Don't perform CSE on function addresses.  */

#define NO_FUNCTION_CSE

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */

/* Theoretically using DImode instead of SImode should stop
   the negative indexes being generated, which are a problem
   if we run a 32-bit executable while in AMODE64, but there
   seems to be something else broken in the i370 target preventing
   that from working */
/* #define Pmode DImode */
#define Pmode SImode

/* A function address in a call instruction is a byte address (for
   indexing purposes) so give the MEM rtx a byte's mode.  */

#define FUNCTION_MODE QImode

/*   A C statement (sans semicolon) to update the integer variable COST
     based on the relationship between INSN that is dependent on
     DEP_INSN through the dependence LINK.  The default is to make no
     adjustment to COST.  This can be used for example to specify to
     the scheduler that an output- or anti-dependence does not incur
     the same cost as a data-dependence.

     We will want to use this to indicate that there is a cost associated
     with the loading, followed by use of base registers ...
#define ADJUST_COST (INSN, LINK, DEP_INSN, COST)
 */

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* Store in cc_status the expressions that the condition codes will
   describe after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.

   On the 370, load insns do not alter the cc's.  However, in some
   cases these instructions can make it possibly invalid to use the
   saved cc's.  In those cases we clear out some or all of the saved
   cc's so they won't be used.

   Note that only some arith instructions set the CC.  These include
   add, subtract, complement, various shifts.  Note that multiply
   and divide do *not* set set the CC.  Therefore, in the code below,
   don't set the status for MUL, DIV, etc.

   Note that the bitwise ops set the condition code, but not in a
   way that we can make use of it. So we treat these as clobbering,
   rather than setting the CC.  These are clobbered in the individual
   instruction patterns that use them.  Use CC_STATUS_INIT to clobber.
*/

#define NOTICE_UPDATE_CC(EXP, INSN)					\
{									\
  rtx xexp = (EXP);							\
  if (GET_CODE (xexp) == PARALLEL) /* Check this */			\
    xexp = XVECEXP (xexp, 0, 0);						\
  if (GET_CODE (xexp) != SET)						\
    CC_STATUS_INIT;							\
  else									\
    {									\
      if (XEXP (xexp, 0) == cc0_rtx)					\
	{								\
	  cc_status.value1 = XEXP (xexp, 0);				\
	  cc_status.value2 = XEXP (xexp, 1);				\
	  cc_status.flags = 0;						\
	}								\
      else								\
	{								\
	  if (cc_status.value1						\
	      && reg_mentioned_p (XEXP (xexp, 0), cc_status.value1))	\
	    cc_status.value1 = 0;					\
	  if (cc_status.value2						\
	      && reg_mentioned_p (XEXP (xexp, 0), cc_status.value2))	\
	    cc_status.value2 = 0;					\
	  switch (GET_CODE (XEXP (xexp, 1)))				\
	    {								\
	      case PLUS:     case MINUS: case NEG:    			\
	      case NOT:	 case ABS:					\
		CC_STATUS_SET (XEXP (xexp, 0), XEXP (xexp, 1));		\
									\
              /* mult and div don't set any cc codes !! */		\
	      case MULT:  /* case UMULT: */ case DIV:      case UDIV: 	\
              /* and, or and xor set the cc's the wrong way !! */	\
	      case AND:   case IOR:    case XOR:  			\
              /* some shifts set the CC some don't.  */			\
              case ASHIFT: 	 case ASHIFTRT:  			\
                 do {} while (0);					\
              default:							\
                break;							\
	    }								\
	}								\
    }									\
}


#define CC_STATUS_SET(V1, V2)						\
{									\
  cc_status.flags = 0;							\
  cc_status.value1 = (V1);						\
  cc_status.value2 = (V2);						\
  if (cc_status.value1							\
      && reg_mentioned_p (cc_status.value1, cc_status.value2))		\
    cc_status.value2 = 0;						\
}

#define OUTPUT_JUMP(NORMAL, FLOAT, NO_OV) 				\
{ if (cc_status.flags & CC_NO_OVERFLOW)	return NO_OV; return NORMAL; }

/* ------------------------------------------ */
/* Control the assembler format that we output.  */

/* Define standard character escape sequences for non-ASCII targets
   only.  */

#ifdef TARGET_EBCDIC
#define TARGET_ESC	39
#define TARGET_BELL	47
#define TARGET_BS	22
#define TARGET_TAB	5
#define TARGET_NEWLINE	21
#define TARGET_VT	11
#define TARGET_FF	12
#define TARGET_CR	13
#endif

/* ======================================================== */

#ifdef TARGET_HLASM

#define TEXT_SECTION_ASM_OP "* Program text area"
#define DATA_SECTION_ASM_OP "* Program data area"
#define INIT_SECTION_ASM_OP "* Program initialization area"
#define SHARED_SECTION_ASM_OP "* Program shared data"
#define CTOR_LIST_BEGIN		/* NO OP */
#define CTOR_LIST_END		/* NO OP */
#define MAX_MVS_LABEL_SIZE 8

/* How to refer to registers in assembler output.  This sequence is
   indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES							\
{ "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",			\
  "8",  "9", "10", "11", "12", "13", "14", "15",			\
  "0",  "2",  "4",  "6"							\
}

#define PRINT_OPERAND(FILE, XV, CODE) i370_print_operand(FILE, XV, CODE)
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) i370_print_operand_address(FILE, ADDR)

#define ASM_COMMENT_START "*"
#define ASM_APP_OFF ""
#define ASM_APP_ON ""

#define ASM_OUTPUT_LABEL(FILE, NAME) 					\
{ assemble_name (FILE, NAME); fputs ("\tEQU\t*\n", FILE); }

#define ASM_OUTPUT_FUNCTION_PREFIX(FILE, NAME)				\
  mvs_need_to_globalize = 0;						\
  mvs_need_entry = 0

#if defined(TARGET_DIGNUS) || defined(TARGET_PDPMAC)
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)
#endif

#ifdef TARGET_LE
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)				\
{									\
  char temp[MAX_MVS_LABEL_SIZE + 1];					\
  if (mvs_check_alias (NAME, temp) == 2)				\
    {									\
      fprintf (FILE, "%s\tALIAS\tC'%s'\n", temp, NAME);			\
    }									\
}
#endif

/* MVS externals are limited to 8 characters, upper case only.
   The '_' is mapped to '@', except for MVS functions, then '#'.  */

#ifdef TARGET_ALIASES

#if defined(TARGET_DIGNUS) || defined(TARGET_PDPMAC)
#define ASM_OUTPUT_LABELREF(FILE, NAME)					\
{									\
  char *bp, ch, temp[MAX_MVS_LABEL_SIZE + 1];				\
  if (!mvs_get_alias (NAME, temp))					\
    strcpy (temp, NAME);						\
  fprintf (FILE, "%s", temp);						\
}
#endif

#ifdef TARGET_LE
#define ASM_OUTPUT_LABELREF(FILE, NAME)					\
{									\
  char *bp, ch, temp[MAX_MVS_LABEL_SIZE + 1];				\
  if (!mvs_get_alias (NAME, temp))					\
    strcpy (temp, NAME);						\
  if (!strcmp (temp,"main"))						\
    strcpy (temp,"gccmain");						\
  if (mvs_function_check (temp))					\
    ch = '#';								\
  else									\
    ch = '@';								\
  for (bp = temp; *bp; bp++)						\
    *bp = (*bp == '_' ? ch : TOUPPER (*bp));				\
  fprintf (FILE, "%s", temp);						\
}
#endif

#else /* !TARGET_ALIASES */

#if defined(TARGET_DIGNUS) || defined(TARGET_PDPMAC)
#define ASM_OUTPUT_LABELREF(FILE, NAME)					\
{									\
  char *bp, ch, temp[MAX_MVS_LABEL_SIZE + 1];				\
  if (!mvs_get_alias (NAME, temp))					\
    strcpy (temp, NAME);						\
  ch = '@';								\
  for (bp = temp; *bp; bp++)						\
    *bp = (*bp == '_' ? ch : TOUPPER (*bp));				\
  fprintf (FILE, "%s", temp);						\
}
#endif

#ifdef TARGET_LE
#define ASM_OUTPUT_LABELREF(FILE, NAME)					\
{									\
  char *bp, ch, temp[MAX_MVS_LABEL_SIZE + 1];				\
  if (!mvs_get_alias (NAME, temp))					\
    strcpy (temp, NAME);						\
  if (!strcmp (temp,"main"))						\
    strcpy (temp,"gccmain");						\
  if (mvs_function_check (temp))					\
    ch = '#';								\
  else									\
    ch = '@';								\
  for (bp = temp; *bp; bp++)						\
    *bp = (*bp == '_' ? ch : TOUPPER (*bp));				\
  fprintf (FILE, "%s", temp);						\
}
#endif

#endif /* TARGET_ALIASES */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)			\
  sprintf (LABEL, "*@@%s%d", PREFIX, NUM)

/* Generate case label.  For HLASM we can change to the data CSECT
   and put the vectors out of the code body. The assembler just
   concatenates CSECTs with the same name.  */

#ifdef TARGET_ALIASES
#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, TABLE)			\
  fprintf (FILE, "\tDS\t0F\n");                                         \
  fprintf (FILE,"@DATA\tCSECT\n");                                      \
  fprintf (FILE, "%s%d\tEQU\t*\n", PREFIX, NUM)
#else /* !TARGET_ALIASES */

#ifdef TARGET_PDPMAC
#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, TABLE)			\
  fprintf (FILE, "\tLTORG\n");                                          \
  fprintf (FILE, "\tDS\t0F\n");                                         \
  mvs_case_code = 0;							\
  fprintf (FILE, "@@%s%d\tEQU\t*\n", PREFIX, NUM)
#else

#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, TABLE)			\
  fprintf (FILE, "\tDS\t0F\n");                                         \
  fprintf (FILE,"$%s\tCSECT\n", mvs_module);                            \
  fprintf (FILE, "%s%d\tEQU\t*\n", PREFIX, NUM)
#endif
#endif /* TARGET_ALIASES */

/* Put the CSECT back to the code body */

#ifdef TARGET_ALIASES
#define ASM_OUTPUT_CASE_END(FILE, NUM, TABLE)                           \
  fputs ("@CODE\tCSECT\n", FILE);
#else /* !TARGET_ALIASES */

#ifdef TARGET_PDPMAC
#define ASM_OUTPUT_CASE_END(FILE, NUM, TABLE)                           \
  mvs_page_code += mvs_case_code;					\
  mvs_check_page (FILE, 0, 0);						\
  mvs_case_code = 0;
#else

#define ASM_OUTPUT_CASE_END(FILE, NUM, TABLE)                           \
  fprintf (FILE, "@%s\tCSECT\n", mvs_module);
#endif
#endif /* TARGET_ALIASES */

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  				\
  mvs_case_code += 4;							\
  fprintf (FILE, "\tDC\tA(@@L%d)\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) 		\
  fprintf (FILE, "\tDC\tA(@@L%d-@@L%d)\n", VALUE, REL)

/* This is how to output an insn to push a register on the stack.
    It need not be very fast code.
   Right now, PUSH & POP are used only when profiling is enabled,
   and then, only to push the static chain reg and the function struct
   value reg, and only if those are used.  Since profiling is not
   supported anyway, punt on this.  */

#define ASM_OUTPUT_REG_PUSH(FILE, REGNO)				\
  mvs_check_page (FILE, 8, 4);						\
  fprintf (FILE, "\tS\t13,=F'4'\n\tST\t%s,%d(13)\n",			\
     reg_names[REGNO], STACK_POINTER_OFFSET)

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE, REGNO)					\
  mvs_check_page (FILE, 8, 0);						\
  fprintf (FILE, "\tL\t%s,%d(13)\n\tLA\t13,4(13)\n",			\
     reg_names[REGNO], STACK_POINTER_OFFSET)

/* This outputs a text string.  The string are chopped up to fit into
   an 80 byte record.  Also, control and special characters, interpreted
   by the IBM assembler, are output numerically.  */

#define MVS_ASCII_TEXT_LENGTH 48

#define ASM_OUTPUT_ASCII(FILE, PTR, LEN)				\
{									\
  size_t i, limit = (LEN);						\
  int j;								\
  for (j = 0, i = 0; i < limit; j++, i++)				\
    {									\
      int c = (PTR)[i];							\
      c = MAP_INCHAR(c);							\
      if (!IS_ISOBASIC (c) || ISCNTRL(c) || c == '&')			\
	{								\
	  if (j % MVS_ASCII_TEXT_LENGTH != 0 )				\
	    fprintf (FILE, "'\n");					\
	  j = -1;							\
	  c = MAP_OUTCHAR (c);						\
	  fprintf (FILE, "\tDC\tX'%X'\n", c );				\
	}								\
      else								\
	{								\
	  if (j % MVS_ASCII_TEXT_LENGTH == 0)				\
            fprintf (FILE, "\tDC\tC'");					\
          if ( c == '\'' )                                       	\
	    {	/* we are going to print 2 chars - is there space */	\
	  if( j % MVS_ASCII_TEXT_LENGTH == MVS_ASCII_TEXT_LENGTH - 1)   \
          { /* not enough space */					\
	    fprintf (FILE, "'\n" );					\
            fprintf (FILE, "\tDC\tC'");	++j;}				\
               fprintf (FILE, "%c%c", c, c);++j;}                   	\
	  else                                                   	\
	    fprintf (FILE, "%c", c);                             	\
	  if (j % MVS_ASCII_TEXT_LENGTH == MVS_ASCII_TEXT_LENGTH - 1)	\
	    fprintf (FILE, "'\n" );					\
	}								\
    }									\
  if (j % MVS_ASCII_TEXT_LENGTH != 0)					\
    fprintf (FILE, "'\n");						\
}

/* This is how to output an assembler line that says to advance the
   location counter to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE, LOG)					\
  if (LOG)								\
    {									\
      if ((LOG) == 1)							\
        fprintf (FILE, "\tDS\t0H\n" );					\
      else 								\
        fprintf (FILE, "\tDS\t0F\n" );					\
    }									\

/* The maximum length of memory that the IBM assembler will allow in one
   DS operation.  */

#define MAX_CHUNK 32767

/* A C statement to output to the stdio stream FILE an assembler
   instruction to advance the location counter by SIZE bytes. Those
   bytes should be zero when loaded.  */

#ifdef TARGET_PDPMAC
#define ASM_OUTPUT_SKIP(FILE, SIZE)  					\
{									\
  int s, k;								\
  for (s = (SIZE); s > 0; s -= MAX_CHUNK)				\
    {									\
      if (s > MAX_CHUNK)						\
	k = MAX_CHUNK;							\
      else								\
	k = s;								\
      fprintf (FILE, "\tDC\t%dX'00'\n", k);				\
    }									\
}
#else
#define ASM_OUTPUT_SKIP(FILE, SIZE)  					\
{									\
  int s, k;								\
  for (s = (SIZE); s > 0; s -= MAX_CHUNK)				\
    {									\
      if (s > MAX_CHUNK)						\
	k = MAX_CHUNK;							\
      else								\
	k = s;								\
      fprintf (FILE, "\tDS\tXL%d\n", k);				\
    }									\
}
#endif

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of a common-label named NAME whose
   size is SIZE bytes.  The variable ROUNDED is the size rounded up
   to whatever alignment the caller wants.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED) 			\
{									\
  char temp[MAX_MVS_LABEL_SIZE + 1];					\
  if (mvs_check_alias(NAME, temp) == 2)					\
    {									\
      fprintf (FILE, "%s\tALIAS\tC'%s'\n", temp, NAME);			\
    }									\
  fprintf(FILE, "* X-var %s\n", NAME); \
  fputs ("\tENTRY\t", FILE);						\
  assemble_name (FILE, NAME);						\
  fputs ("\n", FILE);							\
  fprintf (FILE, "\tDS\t0F\n");						\
  ASM_OUTPUT_LABEL (FILE,NAME);						\
  ASM_OUTPUT_SKIP (FILE,SIZE);						\
}

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of a local-common-label named NAME
   whose size is SIZE bytes.  The variable ROUNDED is the size
   rounded up to whatever alignment the caller wants.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED) 			\
{									\
  fprintf (FILE, "\tDS\t0F\n");						\
  ASM_OUTPUT_LABEL (FILE,NAME);						\
  ASM_OUTPUT_SKIP (FILE,SIZE);						\
}

/* Store in OUTPUT a string (made with alloca) containing an
   assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#ifdef TARGET_PDPMAC
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)  		\
{									\
  (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10);			\
  sprintf ((OUTPUT), "__%d", (LABELNO));				\
}
#else
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)  		\
{									\
  (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10);			\
  sprintf ((OUTPUT), "%s%d", (NAME), (LABELNO));			\
}
#endif

#ifdef TARGET_LE
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
{									\
  /* Save a copy of the function name. We need it later */		\
  if (strlen (NAME) + 1 > mvs_function_name_length)			\
    {									\
      if (mvs_function_name)						\
	free (mvs_function_name);					\
      mvs_function_name = 0;						\
    }									\
  if (!mvs_function_name)						\
    {									\
      mvs_function_name_length = strlen (NAME) * 2 + 1;			\
      mvs_function_name = (char *) xmalloc (mvs_function_name_length);	\
    }									\
  if (!strcmp (NAME, "main"))						\
    strcpy (mvs_function_name, "gccmain");				\
  else									\
    strcpy (mvs_function_name, NAME);					\
}
#endif

#if defined(TARGET_DIGNUS) || defined(TARGET_PDPMAC)
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
{									\
  if (strlen (NAME) + 1 > mvs_function_name_length)			\
    {									\
      if (mvs_function_name)						\
	free (mvs_function_name);					\
      mvs_function_name = 0;						\
    }									\
  if (!mvs_function_name)						\
    {									\
      mvs_function_name_length = strlen (NAME) * 2 + 1;			\
      mvs_function_name = (char *) xmalloc (mvs_function_name_length);	\
    }									\
  strcpy (mvs_function_name, NAME);					\
  mvs_need_to_globalize = 1;						\
}
#endif

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO) 				\
  fprintf (FILE, "Error: No profiling available.\n")

#endif /* TARGET_HLASM */

/* ======================================================== */

#ifdef TARGET_ELF_ABI

/* How to refer to registers in assembler output.  This sequence is
   indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES							\
{ "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",		\
  "r8",  "r9", "r10", "r11", "r12", "r13", "r14", "r15",		\
  "f0",  "f2",  "f4",  "f6"						\
}

#define PRINT_OPERAND(FILE, XV, CODE) i370_print_operand(FILE, XV, CODE)
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) i370_print_operand_address(FILE, ADDR)

/* Copy from elfos.h, but capture the function name first. */

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
{									\
  /* Save a copy of the function name. We need it later */		\
  if (strlen (NAME) + 1 > mvs_function_name_length)			\
    {									\
      if (mvs_function_name)						\
	free (mvs_function_name);					\
      mvs_function_name = 0;						\
    }									\
  if (!mvs_function_name)						\
    {									\
      mvs_function_name_length = strlen (NAME) * 2 + 1;			\
      mvs_function_name = (char *) xmalloc (mvs_function_name_length);	\
    }									\
  strcpy (mvs_function_name, NAME);					\
									\
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "function");			\
  ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
}

/* Overload the default implementation, so that we can hop back to
 * the previous (text) segment, before printing .Letext0. This is
 * needed only because the PIC code dumping prologs in the .data
 * segment.  In general, the PIC design my be very broken, so this
 * might not be needed in the end. */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do									\
    {									\
      if (!flag_inhibit_size_directive)					\
        {								\
          ASM_OUTPUT_MEASURED_SIZE (FILE, FNAME);			\
	  if (i370_enable_pic)						\
	    fprintf(FILE, ".previous\n");				\
        }								\
    }									\
  while (0)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */
/* Make it a no-op for now, so we can at least compile glibc */
#define FUNCTION_PROFILER(FILE, LABELNO)  {				\
  mvs_check_page (FILE, 24, 4);						\
     fprintf (FILE, "\tSTM\tr1,r2,%d(sp)\n", STACK_POINTER_OFFSET-8);	\
     fprintf (FILE, "\tLA\tr1,1(0,0)\n"); 				\
     fprintf (FILE, "\tL\tr2,=A(.LP%d)\n", LABELNO);			\
     fprintf (FILE, "\tA\tr1,0(r2)\n");			 		\
     fprintf (FILE, "\tST\tr1,0(r2)\n");		 		\
     fprintf (FILE, "\tLM\tr1,r2,%d(sp)\n", STACK_POINTER_OFFSET-8);	\
}

/* Don't bother to output .extern pseudo-ops.  They are not needed by
   ELF assemblers.  */

#undef ASM_OUTPUT_EXTERNAL

#define ASM_DOUBLE "\t.double"

/* #define ASM_OUTPUT_LABELREF(FILE, NAME) */	/* use gas -- defaults.h */

/* let config/svr4.h define this ...
 * XXX FIXME: what if it branches off the page!??
 * See i370_internal_label
 *  #define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, TABLE)
 *    fprintf (FILE, "%s%d:\n", PREFIX, NUM)
 */

/* This is how to output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  				\
  mvs_check_page (FILE, 4, 0);						\
  fprintf (FILE, "\t.long\t.L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) 		\
  mvs_check_page (FILE, 4, 0);						\
  fprintf (FILE, "\t.long\t.L%d-.L%d\n", VALUE, REL)

/* Right now, PUSH & POP are used only when profiling is enabled,
   and then, only to push the static chain reg and the function struct
   value reg, and only if those are used by the function being profiled.
   We don't need this for profiling, so punt.  */
#define ASM_OUTPUT_REG_PUSH(FILE, REGNO)
#define ASM_OUTPUT_REG_POP(FILE, REGNO)	


/* Indicate that jump tables go in the text section.  This is
   necessary when compiling PIC code.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Define macro used to output shift-double opcodes when the shift
   count is in %cl.  Some assemblers require %cl as an argument;
   some don't.

   GAS requires the %cl argument, so override i386/unix.h.  */

#undef SHIFT_DOUBLE_OMITS_COUNT
#define SHIFT_DOUBLE_OMITS_COUNT 0

/* Implicit library calls should use memcpy, not bcopy, etc.  */
#define TARGET_MEM_FUNCTIONS

/* Output before read-only data.  */
#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable (initialized) data.  */
#define DATA_SECTION_ASM_OP "\t.data"

/* Output before writable (uninitialized) data.  */
#define BSS_SECTION_ASM_OP "\t.bss"

/* In the past there was confusion as to what the argument to .align was
   in GAS.  For the last several years the rule has been this: for a.out
   file formats that argument is LOG, and for all other file formats the
   argument is 1<<LOG.

   However, GAS now has .p2align and .balign pseudo-ops so to remove any
   doubt or guess work, and since this file is used for both a.out and other
   file formats, we use one of them.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG)!=0) fprintf ((FILE), "\t.balign %d\n", 1<<(LOG))

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP ".globl "

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),                     \
  assemble_name ((FILE), (NAME)),               \
  fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED "\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),                    \
  assemble_name ((FILE), (NAME)),               \
  fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED "\n", (ROUNDED)))

#endif /* TARGET_ELF_ABI */
#endif /* ! GCC_I370_H */
