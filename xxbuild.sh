#
# This is a shell-script hackathon that tries to use the
# i370-ibm-linux-gcc compiler to build a version of gcc that
# will run on the i370. It almost works. But the configure
# scripts keep switching back to the builder gcc instead of
# using i370-ibm-linux-gcc and everything gets borked.

export CFG=../configure
CPPFLAGS="-I$SYSROOT/usr/include" CFLAGS="$CPPFLAGS -B$SYSROOT/usr/lib -L$SYSROOT/usr/lib" $CFG --target=i370-ibm-linux --host=i370-ibm-linux --enable-languages="c" --disable-threads --prefix=$SYSROOT/usr

make configure-libiberty
export CFG=../../libiberty/configure
cd libiberty
rm -r *
CPPFLAGS="-I$SYSROOT/usr/include" CFLAGS="$CPPFLAGS -B$SYSROOT/usr/lib -L$SYSROOT/usr/lib" $CFG --target=i370-ibm-linux --host=i370-ibm-linux --enable-languages="c" --disable-threads --prefix=$SYSROOT/usr
make
cd ..

make configure-intl
cd intl
export CFG=../../intl/configure
rm -r *
CPPFLAGS="-I$SYSROOT/usr/include" CFLAGS="$CPPFLAGS -B$SYSROOT/usr/lib -L$SYSROOT/usr/lib" $CFG --target=i370-ibm-linux --host=i370-ibm-linux --enable-languages="c" --disable-threads --prefix=$SYSROOT/usr
make
cd ..

make configure-gcc
cd gcc
export CFG=../../gcc/configure
rm -r *
CPPFLAGS="-I$SYSROOT/usr/include" CFLAGS="$CPPFLAGS -B$SYSROOT/usr/lib -L$SYSROOT/usr/lib" $CFG --target=i370-ibm-linux --host=i370-ibm-linux --enable-languages="c" --disable-threads --prefix=$SYSROOT/usr
## make
cd ..


## make
