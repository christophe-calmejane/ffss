# Microsoft Developer Studio Project File - Name="ffssfs" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=ffssfs - Win32 W2K Free
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "ffssfs.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ffssfs.mak" CFG="ffssfs - Win32 W2K Free"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ffssfs - Win32 NT4 Checked" (based on "Win32 (x86) External Target")
!MESSAGE "ffssfs - Win32 NT4 Free" (based on "Win32 (x86) External Target")
!MESSAGE "ffssfs - Win32 W2K Checked" (based on "Win32 (x86) External Target")
!MESSAGE "ffssfs - Win32 W2K Free" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "ffssfs - Win32 NT4 Checked"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "ffssfs___Win32_NT4_Checked"
# PROP BASE Intermediate_Dir "ffssfs___Win32_NT4_Checked"
# PROP BASE Cmd_Line ""
# PROP BASE Rebuild_Opt ""
# PROP BASE Target_File "..\bin\nt4\i386\checked\ffssfs.sys"
# PROP BASE Bsc_Name "..\bin\nt4\i386\checked\ffssfs.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "ffssfs___Win32_NT4_Checked"
# PROP Intermediate_Dir "ffssfs___Win32_NT4_Checked"
# PROP Cmd_Line "runbuild nt4 checked"
# PROP Rebuild_Opt "-c"
# PROP Target_File "..\bin\nt4\i386\checked\ffssfs.sys"
# PROP Bsc_Name "..\bin\nt4\i386\checked\ffssfs.bsc"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "ffssfs - Win32 NT4 Free"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "ffssfs___Win32_NT4_Free"
# PROP BASE Intermediate_Dir "ffssfs___Win32_NT4_Free"
# PROP BASE Cmd_Line ""
# PROP BASE Rebuild_Opt ""
# PROP BASE Target_File "..\bin\nt4\i386\checked\ffssfs.sys"
# PROP BASE Bsc_Name "..\bin\nt4\i386\checked\ffssfs.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "ffssfs___Win32_NT4_Free"
# PROP Intermediate_Dir "ffssfs___Win32_NT4_Free"
# PROP Cmd_Line "runbuild nt4 free"
# PROP Rebuild_Opt "-c"
# PROP Target_File "..\bin\nt4\i386\free\ffssfs.sys"
# PROP Bsc_Name "..\bin\nt4\i386\free\ffssfs.bsc"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "ffssfs - Win32 W2K Checked"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "ffssfs___Win32_W2K_Checked"
# PROP BASE Intermediate_Dir "ffssfs___Win32_W2K_Checked"
# PROP BASE Cmd_Line ""
# PROP BASE Rebuild_Opt ""
# PROP BASE Target_File "..\bin\nt4\i386\checked\ffssfs.sys"
# PROP BASE Bsc_Name "..\bin\nt4\i386\checked\ffssfs.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "ffssfs___Win32_W2K_Checked"
# PROP Intermediate_Dir "ffssfs___Win32_W2K_Checked"
# PROP Cmd_Line "runbuild w2k checked"
# PROP Rebuild_Opt "-c"
# PROP Target_File "..\bin\w2k\checked\i386\ffssfs.sys"
# PROP Bsc_Name "..\bin\w2k\checked\i386\ffssfs.bsc"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "ffssfs - Win32 W2K Free"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "ffssfs___Win32_W2K_Free"
# PROP BASE Intermediate_Dir "ffssfs___Win32_W2K_Free"
# PROP BASE Cmd_Line ""
# PROP BASE Rebuild_Opt ""
# PROP BASE Target_File "..\bin\nt4\i386\checked\ffssfs.sys"
# PROP BASE Bsc_Name "..\bin\nt4\i386\checked\ffssfs.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "ffssfs___Win32_W2K_Free"
# PROP Intermediate_Dir "ffssfs___Win32_W2K_Free"
# PROP Cmd_Line "runbuild w2k free"
# PROP Rebuild_Opt "-c"
# PROP Target_File "..\bin\w2k\free\i386\ffssfs.sys"
# PROP Bsc_Name "..\bin\w2k\free\i386\ffssfs.bsc"
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "ffssfs - Win32 NT4 Checked"
# Name "ffssfs - Win32 NT4 Free"
# Name "ffssfs - Win32 W2K Checked"
# Name "ffssfs - Win32 W2K Free"

!IF  "$(CFG)" == "ffssfs - Win32 NT4 Checked"

!ELSEIF  "$(CFG)" == "ffssfs - Win32 NT4 Free"

!ELSEIF  "$(CFG)" == "ffssfs - Win32 W2K Checked"

!ELSEIF  "$(CFG)" == "ffssfs - Win32 W2K Free"

!ENDIF 

# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\alloc.c
# End Source File
# Begin Source File

SOURCE=.\blockdev.c
# End Source File
# Begin Source File

SOURCE=.\char.c
# End Source File
# Begin Source File

SOURCE=.\cleanup.c
# End Source File
# Begin Source File

SOURCE=.\close.c
# End Source File
# Begin Source File

SOURCE=.\cmcb.c
# End Source File
# Begin Source File

SOURCE=.\create.c
# End Source File
# Begin Source File

SOURCE=.\debug.c
# End Source File
# Begin Source File

SOURCE=.\devctl.c
# End Source File
# Begin Source File

SOURCE=.\dirctl.c
# End Source File
# Begin Source File

SOURCE=.\driver.cpp
# End Source File
# Begin Source File

SOURCE=.\fastio.c
# End Source File
# Begin Source File

SOURCE=.\ffss_tdi.cpp
# End Source File
# Begin Source File

SOURCE=.\ffssfsrec.c
# End Source File
# Begin Source File

SOURCE=.\fileinfo.c
# End Source File
# Begin Source File

SOURCE=.\fsctl.c
# End Source File
# Begin Source File

SOURCE=.\fsd.c
# End Source File
# Begin Source File

SOURCE=.\init.c
# End Source File
# Begin Source File

SOURCE=.\lockctl.c
# End Source File
# Begin Source File

SOURCE=.\read.c
# End Source File
# Begin Source File

SOURCE=.\string.c
# End Source File
# Begin Source File

SOURCE=.\volinfo.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\inc\border.h
# End Source File
# Begin Source File

SOURCE=..\inc\ffss_fs.h
# End Source File
# Begin Source File

SOURCE=..\inc\ffss_tdi.h
# End Source File
# Begin Source File

SOURCE=..\inc\fsd.h
# End Source File
# Begin Source File

SOURCE=..\inc\ltypes.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\ffssfs.rc
# End Source File
# End Group
# Begin Source File

SOURCE=.\ddkver
# End Source File
# Begin Source File

SOURCE=..\..\readme.txt
# End Source File
# Begin Source File

SOURCE=.\runbuild.bat
# End Source File
# Begin Source File

SOURCE=.\Sources
# End Source File
# End Target
# End Project
