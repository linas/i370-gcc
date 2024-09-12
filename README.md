README for i370-gcc
-------------------
This git repo contains several versions of GNU GCC adapted for the
IBM System/370 instruction set. The goal is to provide a compiler
suitable for use with the i370-binutils assembler.

Both this target, the i370, and the s390 target create binaries that can
run on the IBM System/390 mainframes. However, the generated assembly
language is quite different, as well as the ABI's. The i370 port is less
sophisticated and less modern than the s390 port; however, it should be
a lot easier to understand for conventional (old-school?) mainframe
programmers.

The original i370 port can be found on
[Linas' i370 website](https://linas.org/linux/i370/i370.html).

### HOWTO
Nothing works yet. Currently attempt to revive the old version 3.4.6
with is the last version that had i370 in it.
```
git checkout i370-gcc-3.4.6
mkdir build; cd build
../configure --target=i370-ibm-linux --enable-obsolete --enable-languages="c" --with-newlib
make
```

Original GNU README
===================
This directory contains the GNU Compiler Collection (GCC).

The GNU Compiler Collection is free software.  See the file COPYING
for copying permission.  The manuals, and some of the runtime
libraries, are under different terms; see the individual source files
for details.

The directory INSTALL contains copies of the installation information
as HTML and plain text.  The source of this information is
gcc/doc/install.texi.  The installation information includes details
of what is included in the GCC sources and what files GCC installs.

See the file gcc/doc/gcc.texi (together with other files that it
includes) for usage and porting information.  An online readable
version of the manual is in the files gcc/doc/gcc.info*.

See http://gcc.gnu.org/bugs.html for how to report bugs usefully.
