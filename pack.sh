#!/bin/sh
./clean.sh
cd ..
echo -n "Make archive ? (Y/n)"
read res
if [ "$res" != "n" ]
then
	tar cvzf ffss.tgz ffss
fi
