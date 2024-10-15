#
# This file just makes sure that the .fini and .init sections do in
# fact return.  Users may put any desired instructions in those sections.
# This file is the last thing linked into any executable.
# See crti.s for more info.

	.file   "crtn.s"
	.ident  "GNU C crtn.s"

	.section .init
	L	r14,12(,r13)
	LM	2,12,28(r13)
	L	r13,8(,r13)
	BASR	r1,r14
# Function page table
	.balign	4
_initpgtable:
	.long	0xf00f # Well, we didn't set up r4 anyway ...  _initpage
# Uhhh
#	.size _init, .-_init

	.section .fini
	L	r14,12(,r13)
	LM	2,12,28(r13)
	L	r13,8(,r13)
	BASR	r1,r14
# Function page table
	.balign	4
_finipgtable:
	.long	0x123d00de # Not today _finipage
# Uhhh
#	.size _fini, .-_fini

	.section .note.GNU-stack
	.balign	4
	.long	0xbaadf00d
