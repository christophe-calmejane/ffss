#!/bin/sh

rm -f Release/*
# Build all
 # Skyutils
cd skyutils/src/Windows/skyutils
nmake -f skyutils.mak CFG="skyutils - Win32 Release" clean
nmake -f skyutils.mak CFG="skyutils - Win32 Debug" clean
nmake -f skyutils.mak CFG="skyutils - Win32 Release"
nmake -f skyutils.mak CFG="skyutils - Win32 Debug"
cd ../../../../
 # LibFFSS
cd libffss-1.0.0/src/Windows/ffss
nmake -f ffss.mak CFG="ffss - Win32 Release" clean
nmake -f ffss.mak CFG="ffss - Win32 Debug" clean
nmake -f ffss.mak CFG="ffss - Win32 Release"
nmake -f ffss.mak CFG="ffss - Win32 Debug"
cd ../../../../
 # Server
cd appli/server/src/Windows/server
nmake -f server.mak CFG="server - Win32 Release" clean
nmake -f server.mak CFG="server - Win32 Release"
cd ../../../../..
 # Server console
cd appli/server/src/Windows/server_console
nmake -f server_console.mak CFG="server_console - Win32 Debug" clean
nmake -f server_console.mak CFG="server_console - Win32 Debug"
cd ../../../../..
 # ConfConn
cd appli/server/plugins/ConfConn/ConfConn
nmake -f ConfConn.mak CFG="ConfConn - Win32 Release" clean
nmake -f ConfConn.mak CFG="ConfConn - Win32 Release"
cd ../../../../..
# HideShare
cd appli/server/plugins/HideShare/HideShare
nmake -f HideShare.mak CFG="HideShare - Win32 Release" clean
nmake -f HideShare.mak CFG="HideShare - Win32 Release"
cd ../../../../..
# Log
cd appli/server/plugins/Log/Log
nmake -f Log.mak CFG="Log - Win32 Release" clean
nmake -f Log.mak CFG="Log - Win32 Release"
cd ../../../../..
# TrayConn
cd appli/server/plugins/TrayConn/TrayConn
nmake -f TrayConn.mak CFG="TrayConn - Win32 Release" clean
nmake -f TrayConn.mak CFG="TrayConn - Win32 Release"
cd ../../../../..
# ipfinstall
cd appli/server/plugins/ipfilter
nmake -f ipfinstall.mak CFG="ipfinstall - Win32 Release" clean
nmake -f ipfinstall.mak CFG="ipfinstall - Win32 Release"
cd ../../../../
# ipfgui
cd appli/server/plugins/ipfilter/ipfgui
nmake -f ipfgui.mak CFG="ipfgui - Win32 Release" clean
nmake -f ipfgui.mak CFG="ipfgui - Win32 Release"
cd ../../../../..
 # master
cd appli/master/src/Windows/master
nmake -f master.mak CFG="master - Win32 Debug" clean
nmake -f master.mak CFG="master - Win32 Debug"
cd ../../../../..
 # ffssdkiller
cd appli/Server-killer/ffssdkiller
nmake -f ffssdkiller.mak CFG="ffssdkiller - Win32 Release" clean
nmake -f ffssdkiller.mak CFG="ffssdkiller - Win32 Release"
cd ../../..
 # CheckForPatch
cd "appli/Install Setting/CheckForPatch/AutoPatch"
nmake -f AutoPatch.mak CFG="AutoPatch - Win32 Release" clean
nmake -f AutoPatch.mak CFG="AutoPatch - Win32 Release"
cd ../../../../
 # AutoCheck
cd "appli/Install Setting/CheckForUpgrade/AutoCheck"
nmake -f AutoCheck.mak CFG="AutoCheck - Win32 Release" clean
nmake -f AutoCheck.mak CFG="AutoCheck - Win32 Release"
cd ../../../../
 # ffssdaemon
cd "appli/Install Setting/ffssdaemon"
nmake -f ffssdaemon.mak CFG="ffssdaemon - Win32 Release" clean
nmake -f ffssdaemon.mak CFG="ffssdaemon - Win32 Release"
cd ../../..
# ffssupdater
cd "appli/Install Setting/ffssupdatercfg"
nmake -f ffssupdater.mak CFG="ffssupdater - Win32 Release" clean
nmake -f ffssupdater.mak CFG="ffssupdater - Win32 Release"
cd ../../..
# LangSelect
cd "appli/Install Setting/langselect"
nmake -f LangSelect.mak CFG="LangSelect - Win32 Release" clean
nmake -f LangSelect.mak CFG="LangSelect - Win32 Release"
cd ../../..
# PreInstall
cd "appli/Install Setting/preinstall"
nmake -f PreInstall.mak CFG="PreInstall - Win32 Release" clean
nmake -f PreInstall.mak CFG="PreInstall - Win32 Release"
cd ../../..

# Copy all files
cp appli/server/src/Windows/server/Release/server.exe Release/
cp appli/server/src/Windows/server_console/Debug/server_console.exe Release/
cp appli/server/plugins/ConfConn/ConfConn/Release/ConfConn.dll Release/
cp appli/server/plugins/HideShare/HideShare/Release/HideShare.dll Release/
cp appli/server/plugins/Log/Log/Release/Log.dll Release/
cp appli/server/plugins/TrayConn/TrayConn/Release/TrayConn.dll Release/
cp appli/server/plugins/ipfilter/Release/ipfinstall.dll Release/
cp appli/server/plugins/ipfilter/ipfgui/Release/ipfgui.dll Release/
cp appli/master/src/Windows/master/Debug/master.exe Release/
cp appli/Server-killer/ffssdkiller/Release/ffssdkiller.exe Release/
cp "appli/Install Setting/CheckForPatch/AutoPatch/Release/AutoPatch.exe" Release/
cp "appli/Install Setting/CheckForUpgrade/AutoCheck/Release/AutoCheck.exe" Release/
cp "appli/Install Setting/ffssdaemon/Release/ffssdaemon.exe" Release/
#ffssupdatercfg
cp "appli/Install Setting/langselect/Release/LangSelect.exe" Release/
#preinstall

# Compress all files
for i in Release/* ; do upx --best $i ; done;

# Done
echo "Done."
