#!/bin/sh
cd skyutils
make distclean
cd ..
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
cd ../..