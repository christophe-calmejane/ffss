#!/bin/sh
cd libffss-1.0.0
make distclean
cd ../appli/client/example
make clean
rm -f test_download
cd ../text
make distclean
cd ../cgi
make clean
cd ../gtk
make distclean
cd ../../server
make distclean
cd ../master
make distclean
cd ../../..
echo -n "Make archive ? (Y/n)"
read res
if [ "$res" != "n" ]
then
	tar cvzf ffss.tgz ffss
fi
