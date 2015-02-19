#!/bin/bash

if [ "${OSTYPE}" == "cygwin" ]; then
	cmake -H. -BBuild-32 -DFFSS_PROJECT_NAME=ffss-32
	sed -i 's/Express 2013 for Windows Desktop/Ultimate 2013/' Build-32/ffss-32.sln
	cmake -H. -BBuild-64 -G"Visual Studio 12 Win64" -DFFSS_PROJECT_NAME=ffss-64
	sed -i 's/Express 2013 for Windows Desktop/Ultimate 2013/' Build-64/ffss-64.sln
else
	cmake -H. -BBuild-linux
fi

