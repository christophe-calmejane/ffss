#!/bin/sh

archdir=`pwd`
echo
echo '############# BUILDING SKYUTILS LIBRARY ###############'
echo
cd skyutils
./configure --includedir=$archdir/skyutils/src --libdir=$archdir/skyutils/src/.libs --enable-reentrant
make
cd ..
echo
echo '############# BUILDING FFSS LIBRARY ###############'
echo
cd libffss-1.0.0
libdir=`pwd`
./configure --includedir=$libdir/src --libdir=$libdir/src/.libs --enable-static --enable-context --with-skyutils=$archdir/skyutils
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
	./configure --with-ffss=$libdir --enable-static
	make
        cd ../cgi
        make
        cd ../gtk
	./configure --with-ffss=$libdir --enable-static
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
	./configure --with-ffss=$libdir --enable-static
	make
        strip src/ffss-server
        strip src/ffsswho
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
	./configure --with-ffss=$libdir --enable-static --enable-context
	make
        strip src/ffss-master
	cd ../..
fi
echo
echo
echo 'ALL DONE.'
echo
