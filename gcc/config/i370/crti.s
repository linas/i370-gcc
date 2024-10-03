/* Bogus .init and .fini sections for i370-ibm-linux target.
   Just enough to allow basic builds. */

	.file   "crti.o"
	.ident  "GNU C crti.o"

	.section .init
	.globl	_init
	.type	_init,@function
_init:
	nopr	0

	.section .fini
	.globl	_fini
	.type	_fini,@function
_fini:
	nopr	0
