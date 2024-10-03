README for i370-gcc
-------------------
This git repo contains several versions of GNU GCC adapted for the
IBM System/370 instruction set. The goal is to provide a compiler
suitable for use with the i370-binutils assembler.

Both this target, the i370, and the s390 target create binaries that can
run on the IBM System/390 mainframes. However, the generated assembly
language is quite different, as well as the ABI's. The i370 port
generates assembly for both HLASM and GNU binutils (ELF). The HLASM
targets include CMS, VMS, Dignus, OpenEdition, MVS/Language
Environment (MVS/LE) and VSE.

The i370-ibm-linux backend is needed to compile the i370 port of the
Linux kernel.  This kernel can be found on github, at
[linas/i370-linux-2.2.1](https://github.com/linas/i370-linux-2.2.1).
General background is provided on
[Linas' i370 website](https://linas.org/linux/i370/i370.html).

### HOWTO
The last version of gcc with the i370 machine definition in it was
version 3.4.6. This is tagged in github as `releases/gcc-3.4.6`.
The i370 code was removed by `releases/gcc-4.0.0`.

The code here starts with gcc release 3.4.6 and applies a large number
of fixes that (a) were lost during the infighting between egcs and gcc,
(b) fix bugs that were discovered after gcc-4.0.0 came out, and thus,
the fixes were never upstreamed.  This includes fixes from Paul Edwards,
among others. (c) Extensions for a number of different HLASM OS targets.

To get the latest, either clone everything:
```
git clone https://github.com/linas/i370-gcc
```
or clone only one branch (this will save some time and bandwidth):
```
git clone -b i370-gcc-3.4.6 --single-branch https://github.com/linas/i370-gcc
```
Then build the version for binutils/ELF:
```
git checkout i370-gcc-3.4.6
mkdir build; cd build
../configure --target=i370-ibm-linux --enable-languages="c" --disable-threads
make -j12
sudo make install
```

The `sudo make install` will install `gcc` into two places, with two
different names. First, using the plain name `gcc`, in
`/usr/local/i370-ibm-linux/bin/gcc`. Since this conflicts with the host
gcc in a cross-compile environment, it is also installed to
`/usr/local/bin/i370-ibm-linux-gcc`.

Objects and libraries such as `crtbegin.o`, `libgcc_s.so.1` etc.
are installed into `/usr/local/lib/gcc/i370-ibm-linux/3.4.6`.

Other targets include:
```
--target=i370-ibm-cms
--target=i370-ibm-mvsle
--target=i370-ibm-mvsdignus
--target=i370-ibm-mvspdp
--target=i370-ibm-mvs38_dignus
--target=i370-ibm-opened
```
The configuration for these is defined in the `gcc/config.gcc` file.


Original GNU README
===================
This directory contains the GNU Compiler Collection (GCC).

The GNU Compiler Collection is free software.  See the files whose
names start with COPYING for copying permission.  The manuals, and
some of the runtime libraries, are under different terms; see the
individual source files for details.

The directory INSTALL contains copies of the installation information
as HTML and plain text.  The source of this information is
gcc/doc/install.texi.  The installation information includes details
of what is included in the GCC sources and what files GCC installs.

See the file gcc/doc/gcc.texi (together with other files that it
includes) for usage and porting information.  An online readable
version of the manual is in the files gcc/doc/gcc.info*.

See http://gcc.gnu.org/bugs/ for how to report bugs usefully.

Copyright years on GCC source files may be listed using range
notation, e.g., 1987-2012, indicating that every year in the range,
inclusive, is a copyrightable year that could otherwise be listed
individually.
