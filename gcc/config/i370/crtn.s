/* Bogus .init and .fini sections for i370-ibm-linux target.
   Just enough to allow basic builds. */

	.file   "crtn.o"
	.ident  "GNU C crtn.o"

	.section .init
	nopr	0

	.section .fini
	nopr	0
