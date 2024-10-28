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

Other targets for other operating systems include:
```
--target=i370-ibm-cms
--target=i370-ibm-mvsle
--target=i370-ibm-mvsdignus
--target=i370-ibm-mvspdp
--target=i370-ibm-mvs38_dignus
--target=i370-ibm-opened
```
These differ from each-other in how subroutine calls work (argument
passing, stack management, return values). They all emit pure HLASM.
These differ from `i370-ibm-linux` in the type of assembly emitted:
the `i370-ibm-linux` target emits svr4-elf style assembly (lower-case
pseudo-ops, labels with a dot prefix, standard ELF section names, etc.)

Both types of emitted assembly can be assembled with the binutils
assembler, available here:
[github.com/linas/i370-binutils](https://github.com/linas/i370-binutils).
This assembler is explicitly HLASM-compatible. At this time, this
assembler emits only ELF file format binaries (it does not support
the [ESD/XSD/GOFF](https://en.wikipedia.org/wiki/GOFF) object format
used by MVS.) There is a linker/loader that can link together both
ELF and MVS binaries together; inquire with the PDOS maintainer.
(Thus, in principle, the ELF binaries created by binutils can be
transformed into executables that run on MVS. !? Thus, one has a
complete free & open source toolchain for MVS. !?)

The configuration for the different OS targets is defined in the
`gcc/config.gcc` file.


## Cross-host builds
Cross-host builds are a bit tricky. The goal here is to build a version
of gcc that will run on the i370. Assuming you have a C Library for the
i370, then the following should be enough:
```
mkdir build-libc
cd build-libc
export SYSROOT=/usr/local/i370-linux-uclibc
../configure --target=i370-ibm-linux --host=i370-ibm-linux --enable-languages="c" --disable-threads --prefix=$SYSROOT/usr
make
```
Here, `SYSROOT` provides the location of the C Library to link to.
Change as appropriate.

XXX Except the above does not actually work. I can't figure out how to
make it work. It keeps using the wrong compiler, or uses the wrong
include files, or uses the wrong assembler. Tried to hack around this
with the `xxbuild.sh` script, and it almost works. But not quite.

The trainwreck involves `genmodes`. This needs to execute on the
builder (lets assume x86_64, for example). And so it needs a version of
libiberty for x86_64, too. Great. The `genmodes` tool generates the
actual compiler from the machine description (in `i370.md`). Great!
The result should then be built with `i370-ibm-linux-gcc`, but it
isn't: the x86 version of `gcc` gets used instead. Argh! I can't get
it to switch over.

The above does result in a cross-compiler `xgcc` being built. Fine.
It should then go back and rebuild the compiler, but this time using
`xgcc`. It would neeed to also use the cross-include files, and the
cross-C-library.  But it doesn't; it gets tangled up. For example,
the below looks promising, but fails quite soon:
```
CC=gcc ../configure --target=i370-ibm-linux --build=i370-ibm-linux --enable-languages="c" --disable-threads --x-includes=$SYSROOT/usr/include --x-libraries=$SYSROOT/usr/lib
```
And of course, this doesn't work:
```
CPPFLAGS="-I$SYSROOT/usr/include" CFLAGS="$CPPFLAGS -B$SYSROOT/usr/lib -L$SYSROOT/usr/lib" $CFG --target=i370-ibm-linux --host=i370-ibm-linux --enable-languages="c" --disable-threads --prefix=$SYSROOT/usr
```
The hackery in `xxbuild.sh` seems promising, until one realizes that
the resulting `genmodes` is for i370, and can't be run on the builder.
I give up.

FYI, one confusing bug is worth mentioning:
* The double-crossed binutils, installs a file
  `$SYSROOT/usr/include/ansidecl.h` which clashes with the libiberty
  `ansidecl.h` and then (potentialy) screws up the build.

That's a typical example of the wrong headers getting used at the wrong
time. It's quite hard to debug.

How to get a C library is explained at
[github.com/linas/i370-bigfoot](https://github.com/linas/i370-bigfoot).
That demonstrates a working uClibc and also a working Busybox. It's
possible that PDPCLIB might work, but that remains unclear.



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
