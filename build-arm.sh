#!/bin/sh

CC=arm-linux-gcc

archdir=`pwd`
echo
echo '############# BUILDING SKYUTILS LIBRARY ###############'
echo
cd skyutils
./configure --libdir=$archdir/skyutils/src/.libs --enable-reentrant --host=arm-linux --target=arm-linux
make
cd ..
echo
echo '############# BUILDING FFSS LIBRARY ###############'
echo
cd libffss-1.0.0
libdir=`pwd`
./configure --libdir=$libdir/src/.libs --with-skyutils=$archdir/skyutils --host=arm-linux --target=arm-linux --disable-zlib
make
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
	./configure --with-ffss=$libdir --host=arm-linux --target=arm-linux
	make
        arm-linux-strip src/ffss-client
        cd ../cgi
        make
        cd ../gtk
	./configure --with-ffss=$libdir --host=arm-linux --target=arm-linux
	make
        arm-linux-strip src/ffss_client
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
	./configure --with-ffss=$libdir --host=arm-linux --target=arm-linux
	make
        arm-linux-strip src/ffss-server
        arm-linux-strip src/ffsswho
        cd ../..
fi
echo
echo
echo 'ALL DONE.'
echo
