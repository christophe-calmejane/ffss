#!/bin/sh

echo
echo '############# BUILDING FFSS LIBRARY ###############'
echo
cd libffss-1.0.0
libdir=`pwd`
./configure --libdir=$libdir/src/.libs --enable-context --enable-debug --enable-malloc_trace
#./configure --libdir=$libdir/src/.libs --enable-context --enable-debug
make
cp $libdir/skyutils-1.16/libskyutils.a $libdir/src/.libs
cd ..
echo
echo -n "compile ffss client (to browse shares) ? (Y/n) "
read res
if [ "$res" != "n" ]
then
	echo
	echo '############# BUILDING FFSS CLIENTS ###############'
	echo
	cd appli/client/example
	make
        cd ../text
	./configure --with-ffss=$libdir --enable-debug --enable-malloc_trace
	make
        cd ../cgi
        make
        cd ../gtk
	./configure --with-ffss=$libdir --enable-debug --enable-malloc_trace
	make
        cd ../../..
fi
echo
echo -n "compile ffss server (to share directories) ? (Y/n) "
read res
if [ "$res" != "n" ]
then
	echo
	echo '############# BUILDING FFSS SERVER ###############'
	echo
	cd appli/server
	./configure --with-ffss=$libdir --enable-debug --enable-malloc_trace
	make
        cd ../..
fi
echo
echo -n "compile ffss master (to manage some servers on a domain) ? (y/N) "
read res
if [ "$res" == "y" ]
then
	echo
	echo '############# BUILDING FFSS MASTER ###############'
	echo
	cd appli/master
#	./configure --with-ffss=$libdir --enable-debug --enable-context --enable-malloc_trace
	./configure --with-ffss=$libdir --enable-debug --enable-context
	make
	cd ../..
fi
echo
echo
echo 'ALL DONE.'
echo
