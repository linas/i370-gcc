#
# This file just supplies function prologues for the .init and .fini
# sections.  It is linked in before crtbegin.o. It is supposed to
# provide a standard function prolog; users can put "anything" in
# here. _init is (supposed to be) called directly from _start
# (crt0.s or crt1.s provided by the C library). The _fini is
# called by atexit() from crt0.s/crt1.s.
#
# crti.s contains the prologues to these two sections, and crtn.s
# contains the epilogs. STARTFILE_SPEC should list crti.o before
# any other object files that might add code to .init or .fini
# sections, and ENDFILE_SPEC should list crtn.o after any such object
# files
#
# I have no clue if this is correct.

	.file   "crti.s"
	.ident  "GNU C crti.s"

	.section .init
	.globl	_init
	.type	_init,@function
_init:
	.using	.,r15
	B	.Linit
	.long	88	# stacksize
	.long	_initpgtable
	.drop	r15
	.balign	2
.Linit:
	STM	r13,r12,8(r11)
	LR	r13,r11
	A	r11,4(,r15)
	L	r4,8(,r15)
	BASR	r3,0
	.using	.,r3
_initpage:
	ST	r4,0(r13)


	.section .fini
	.globl	_fini
	.type	_fini,@function
_fini:
	.using	.,r15
	B	.Lfini
	.long	88	# stacksize
	.long	_finipgtable
	.drop	r15
	.balign	2
.Lfini:
	STM	r13,r12,8(r11)
	LR	r13,r11
	A	r11,4(,r15)
	L	r4,8(,r15)
	BASR	r3,0
	.using	.,r3
_finipage:
	ST	r4,0(r13)
