# Microsoft Developer Studio Project File - Name="kffss" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=kffss - Win32 NT4 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "kffss.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "kffss.mak" CFG="kffss - Win32 NT4 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "kffss - Win32 NT4 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "kffss - Win32 NT4 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "kffss - Win32 NT4 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "NT4 Release"
# PROP BASE Intermediate_Dir "NT4 Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /Gz /W4 /Oy /Gy /I "..\..\..\..\appli\driver\ffssfs\inc" /I "..\.." /I "..\..\..\..\skyutils\src" /I "$(BASEDIR)\inc\ddk" /I "$(BASEDIR)\inc" /I "$(DRIVERWORKS)\source" /I "$(DRIVERWORKS)\include" /I "$(DRIVERNETWORKS)\include\tdiclient" /I "$(DRIVERNETWORKS)\include" /I "$(BASEDIR)\src\network\wshsmple" /I "$(BASEDIR)\src\network\inc" /D FPO=1 /D "NDEBUG" /D DBG=0 /D _X86_=1 /D i386=1 /D "STD_CALL" /D CONDITION_HANDLING=1 /D NT_UP=1 /D NT_INST=0 /D WIN32=100 /D _NT1X_=100 /D WINNT=1 /D _WIN32_WINNT=0x0400 /D WIN32_LEAN_AND_MEAN=1 /D DEVL=1 /D NTVERSION=400 /D "TDI_WINDOWS_NT" /D "FFSS_DRIVER" /D "DISABLE_ZLIB" /D "DISABLE_BZLIB" /D "DISABLE_CHECKSUM" /FR /cbstring /GF /Oxs /QIfdiv- /QIf /Zel /c
# SUBTRACT CPP /Gf
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=Build Environment Variables
PostBuild_Cmds=echo BASEDIR $(BASEDIR)	echo DRIVERWORKS $(DRIVERWORKS)
# End Special Build Tool

!ELSEIF  "$(CFG)" == "kffss - Win32 NT4 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "NT4 Debug"
# PROP BASE Intermediate_Dir "NT4 Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /Gz /W4 /Z7 /Oi /Gy /I "..\..\..\..\appli\driver\ffssfs\inc" /I "..\.." /I "..\..\..\..\skyutils\src" /I "$(BASEDIR)\inc\ddk" /I "$(BASEDIR)\inc" /I "$(DRIVERWORKS)\source" /I "$(DRIVERWORKS)\include" /I "$(DRIVERNETWORKS)\include\tdiclient" /I "$(DRIVERNETWORKS)\include" /I "$(BASEDIR)\src\network\wshsmple" /I "$(BASEDIR)\src\network\inc" /D "DEBUG" /D DBG=1 /D FPO=0 /D _X86_=1 /D i386=1 /D "STD_CALL" /D CONDITION_HANDLING=1 /D NT_UP=1 /D NT_INST=0 /D WIN32=100 /D _NT1X_=100 /D WINNT=1 /D _WIN32_WINNT=0x0400 /D WIN32_LEAN_AND_MEAN=1 /D DEVL=1 /D NTVERSION=400 /D "TDI_WINDOWS_NT" /D "FFSS_DRIVER" /D "DISABLE_ZLIB" /D "DISABLE_BZLIB" /D "DISABLE_CHECKSUM" /FR /YX /FD /Zel -cbstring /QIfdiv- /QIf /GF /c
# SUBTRACT CPP /Gf
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=Build Environment Variables
PostBuild_Cmds=echo BASEDIR $(BASEDIR)	echo DRIVERWORKS $(DRIVERWORKS)
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "kffss - Win32 NT4 Release"
# Name "kffss - Win32 NT4 Debug"
# Begin Group "Source Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\driver.cpp

!IF  "$(CFG)" == "kffss - Win32 NT4 Release"

!ELSEIF  "$(CFG)" == "kffss - Win32 NT4 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\message.cpp

!IF  "$(CFG)" == "kffss - Win32 NT4 Release"

!ELSEIF  "$(CFG)" == "kffss - Win32 NT4 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\utils.cpp

!IF  "$(CFG)" == "kffss - Win32 NT4 Release"

!ELSEIF  "$(CFG)" == "kffss - Win32 NT4 Debug"

!ENDIF 

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter ".h"
# Begin Source File

SOURCE=..\..\ffss.h
# End Source File
# End Group
# End Target
# End Project
