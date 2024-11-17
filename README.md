README for i370-gcc
-------------------
This git repo contains several versions of GNU GCC, including GCCMVS,
adapted for the IBM System/370 instruction set. It produces code that
can be assembled either with HLASM or with the i370-binutils assembler.

(Do not be fooled by the ancient date-stamps on some of the files in
git: this version is current, as of November 2024.)

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
A working demo is in the
[i370-bigfoot Docker container](https://github.com/linas/i370-bigfoot).

### HOWTO
The last version of gcc with the i370 machine definition in it was
version 3.4.6. This is tagged in github as `releases/gcc-3.4.6`.
The i370 code was removed by `releases/gcc-4.0.0`.

The code here starts with gcc release 3.4.6 and applies a large number
of fixes that (a) were lost during the infighting between egcs and gcc,
(b) fix bugs that were discovered after gcc-4.0.0 came out, and thus,
the fixes were never upstreamed.  This includes fixes from Paul Edwards
and Dave Pitts, among others. (c) Extensions for a number of different
HLASM OS targets.

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
i370, then the following should have been enough:
```
mkdir build-libc
cd build-libc
export SYSROOT=/usr/local/i370-linux-uclibc
../configure --target=i370-ibm-linux --host=i370-ibm-linux --build=x86_64-unknown-linux-gnu --enable-languages="c" --disable-threads --prefix=$SYSROOT/usr
make
```
Here, `SYSROOT` provides the location of the C Library to link to.
Change as appropriate.

FYI: See the [cross-compiling reference](https://gcc.gnu.org/onlinedocs/gccint/Configure-Terms.html).

The above should have been enough. Yet it isn't; things break in strange
weird ways.  The following does seem to work (for me):
```
cd /usr/local
sudo cp -pr i370-linux-uclibc/usr/include i370-ibm-linux
sudo cp -pr i370-linux-uclibc/usr/lib/* i370-ibm-linux/lib

CC="i370-ibm-linux-gcc -B$SYSROOT/usr/lib -L$SYSROOT/usr/lib" \
CC_FOR_BUILD="gcc -I/usr/include -I/usr/include/x86_64-linux-gnu/ -D__x86_64__ -U__ILP32__" \
../configure --host=i370-ibm-linux \
             --target=i370-ibm-linux \
             --build=x86_64-unknown-linux-gnu \
             --prefix=$SYSROOT/usr \
             --enable-languages="c" --disable-threads
```
The two `sudo cp` put the i370 C library and header files where they
can be found. The C library must have been built and installed sometime
earlier. Despite this, the build will use the incorrect `crt1.o` if the
`-B` flag isn't given. The `-L` is there for good luck.

The `CC_FOR_BUILD` is needed because the build gcc seems to have trouble
finding header files, and then there's some weirdness with `<gnu/stubs.h>`
that has to be hacked around with the `-D` and `-U` flags. At least for me.
YMMV.

How to get a C library is explained at
[github.com/linas/i370-bigfoot](https://github.com/linas/i370-bigfoot).
That demonstrates a working uClibc and also a working BusyBox. It's
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
