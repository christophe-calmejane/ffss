# Microsoft Developer Studio Project File - Name="ffss" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=ffss - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "ffss.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ffss.mak" CFG="ffss - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ffss - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "ffss - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=xicl6.exe
RSC=rc.exe

!IF  "$(CFG)" == "ffss - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../../../../skyutils/src" /I "../../../../misc/windows/include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /D "_REENTRANT" /D "_WIN32" /D "FFSS_CONTEXT" /YX /FD /c
# ADD BASE RSC /l 0x40c /d "NDEBUG"
# ADD RSC /l 0x40c /i "../../../../skyutils/src/windows/skyutils/release" /i "../../../../misc/windows/lib" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "ffss - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "../../../../skyutils/src" /I "../../../../misc/windows/include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /D "_REENTRANT" /D "DEBUG" /D "_WIN32" /D "FFSS_CONTEXT" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x40c /d "_DEBUG"
# ADD RSC /l 0x40c /i "../../../../misc/windows/lib" /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "ffss - Win32 Release"
# Name "ffss - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\client.c
# End Source File
# Begin Source File

SOURCE=..\..\common.c
# End Source File
# Begin Source File

SOURCE=..\..\filter.c
# End Source File
# Begin Source File

SOURCE=..\..\master.c
# End Source File
# Begin Source File

SOURCE=..\..\message.c
# End Source File
# Begin Source File

SOURCE=..\..\server.c
# End Source File
# Begin Source File

SOURCE=..\..\transfer.c
# End Source File
# Begin Source File

SOURCE=..\..\utils.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\common.h
# End Source File
# Begin Source File

SOURCE=..\..\ffss.h
# End Source File
# Begin Source File

SOURCE=..\..\transfer.h
# End Source File
# Begin Source File

SOURCE=..\..\utils.h
# End Source File
# End Group
# Begin Group "Resources"

# PROP Default_Filter "lib"
# Begin Source File

SOURCE=..\..\..\..\misc\windows\lib\libbz2.lib
# End Source File
# Begin Source File

SOURCE=..\..\..\..\misc\windows\lib\libz.lib
# End Source File
# End Group
# Begin Group "docs"

# PROP Default_Filter ""
# Begin Source File

SOURCE="..\..\..\..\doc\protocol-en.txt"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\doc\protocol-fr.txt"
# End Source File
# End Group
# End Target
# End Project
