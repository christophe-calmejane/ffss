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
DEP_CPP_DRIVE=\
	"..\..\..\..\appli\driver\ffssfs\inc\ffss_tdi.h"\
	"..\..\..\..\skyutils\src\skyutils.h"\
	"..\..\ffss.h"\
	"..\..\kffss.h"\
	"c:\NTDDK\inc\alpharef.h"\
	"c:\ntddk\inc\basetsd.h"\
	"c:\ntddk\inc\bugcodes.h"\
	"c:\NTDDK\inc\ddk\afilter.h"\
	"c:\NTDDK\inc\ddk\efilter.h"\
	"c:\NTDDK\inc\ddk\ffilter.h"\
	"c:\ntddk\inc\ddk\ndis.h"\
	"c:\ntddk\inc\ddk\netpnp.h"\
	"c:\ntddk\inc\ddk\ntddk.h"\
	"c:\NTDDK\inc\ddk\tfilter.h"\
	"c:\ntddk\inc\guiddef.h"\
	"c:\NTDDK\inc\ia64reg.h"\
	"c:\ntddk\inc\netevent.h"\
	"c:\ntddk\inc\nettypes.h"\
	"c:\ntddk\inc\ntddndis.h"\
	"c:\ntddk\inc\ntddtdi.h"\
	"c:\ntddk\inc\ntdef.h"\
	"c:\ntddk\inc\ntiologc.h"\
	"c:\ntddk\inc\ntstatus.h"\
	"c:\ntddk\inc\packoff.h"\
	"c:\ntddk\inc\packon.h"\
	"c:\ntddk\inc\tdi.h"\
	"c:\ntddk\inc\tdikrnl.h"\
	"c:\NTDDK\inc\wdm.h"\
	"c:\NTDDK\inc\wmistr.h"\
	"c:\ntddk\src\network\inc\tdiinfo.h"\
	"c:\ntddk\src\network\wshsmple\smpletcp.h"\
	"c:\progra~1\numega\softic~1\common\include\stcinit.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kirppool.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\klistbrowser.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndis.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisadaptertraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisbuffer.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisbufferheap.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisbufferpool.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisBundle.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisChars.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisconfig.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\kndiscpp.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisevent.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterAdapter.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterAdapter51.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterBinding.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterPacketPool.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisheap.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisHeap9x.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisinterlocked.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisInterrupt.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndislist.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisLookahead.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisLookaside.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndismedium.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisMiniAdapter.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisMiniDriver.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisOid.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisoiddebug.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisOidTraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacket.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPacketCancelId.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacketlist.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacketpool.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacketstransfer.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispartialpacket.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPciInformation.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPeripheralAddress.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPhysAddr.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPnpMode.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisprotocolbinding.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisprotocoltraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisprotocolwrapper.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisReceiveArea.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisReceiveAreaTraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisrequest.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisResource.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisRxAreaAsyncMgr.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisRxAreaNoAsync.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisSharedMemory.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisSharedReceiveArea.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisspinlock.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisstats.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisstring.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisSystemReceiveArea.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndistimer.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndistrace.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisWrapper.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kobject.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\ipnumbers.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\kipintr.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\ktdiirp.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\routable.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdi9xiface.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdiclient.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdiconnobj.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdidef9x.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdintiface.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdipartialreceive.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdipnpcl.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdisclient.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdisocket.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdistatus.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdistrserver.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\w9xNdisExp.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\cpprt.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\cright.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\dwcontrl.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394async.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394Irb.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394isoc.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kadapter.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kaddress.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\karray.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kchecker.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kcontrol.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdevice.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdevque.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdispobj.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdmaxfer.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdmqex.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdmqueue.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdpc.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdriver.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kerrlog.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kevent.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kfifo.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kfile.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kfilter.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kgenlock.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kheap.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kicount.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kimgsect.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kintrupt.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kiocparm.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kirp.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\klist.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\klower.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kmemory.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kmutex.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kndisvdw.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kpcicfg.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kpnpdev.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kpnplow.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kquery.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kregkey.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kresreq.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks5920.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks5933.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks59xx.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks59xxrd.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ksemaphr.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ksfifo.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kspin.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kstdwmi.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ksysthrd.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ktimer.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ktrace.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kunitnam.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kustring.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kvxdintf.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\Kwdmfltr.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kwmi.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kwmiblock.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kwmistr.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kworkitm.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\shfifo.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\vdw.h"\
	"c:\progra~1\numega\softic~1\driverworks\source\function.h"\
	"c:\progra~1\numega\softic~1\driverworks\source\shfifo.cpp"\
	{$(INCLUDE)}"bzlib.h"\
	{$(INCLUDE)}"zconf.h"\
	{$(INCLUDE)}"zlib.h"\
	
NODEP_CPP_DRIVE=\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\netvxd.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\TDISTAT.H"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdivxd.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\vtdi.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\vxdntlib.h"\
	

!IF  "$(CFG)" == "kffss - Win32 NT4 Release"

!ELSEIF  "$(CFG)" == "kffss - Win32 NT4 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\message.cpp
DEP_CPP_MESSA=\
	"..\..\..\..\appli\driver\ffssfs\inc\ffss_tdi.h"\
	"..\..\..\..\skyutils\src\skyutils.h"\
	"..\..\common.h"\
	"..\..\ffss.h"\
	"..\..\kffss.h"\
	"..\..\transfer.h"\
	"..\..\utils.h"\
	"c:\NTDDK\inc\alpharef.h"\
	"c:\ntddk\inc\basetsd.h"\
	"c:\ntddk\inc\bugcodes.h"\
	"c:\NTDDK\inc\ddk\afilter.h"\
	"c:\NTDDK\inc\ddk\efilter.h"\
	"c:\NTDDK\inc\ddk\ffilter.h"\
	"c:\ntddk\inc\ddk\ndis.h"\
	"c:\ntddk\inc\ddk\netpnp.h"\
	"c:\ntddk\inc\ddk\ntddk.h"\
	"c:\NTDDK\inc\ddk\tfilter.h"\
	"c:\ntddk\inc\guiddef.h"\
	"c:\NTDDK\inc\ia64reg.h"\
	"c:\ntddk\inc\netevent.h"\
	"c:\ntddk\inc\nettypes.h"\
	"c:\ntddk\inc\ntddndis.h"\
	"c:\ntddk\inc\ntddtdi.h"\
	"c:\ntddk\inc\ntdef.h"\
	"c:\ntddk\inc\ntiologc.h"\
	"c:\ntddk\inc\ntstatus.h"\
	"c:\ntddk\inc\packoff.h"\
	"c:\ntddk\inc\packon.h"\
	"c:\ntddk\inc\tdi.h"\
	"c:\ntddk\inc\tdikrnl.h"\
	"c:\NTDDK\inc\wdm.h"\
	"c:\NTDDK\inc\wmistr.h"\
	"c:\ntddk\src\network\inc\tdiinfo.h"\
	"c:\ntddk\src\network\wshsmple\smpletcp.h"\
	"c:\progra~1\numega\softic~1\common\include\stcinit.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kirppool.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\klistbrowser.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndis.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisadaptertraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisbuffer.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisbufferheap.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisbufferpool.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisBundle.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisChars.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisconfig.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\kndiscpp.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisevent.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterAdapter.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterAdapter51.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterBinding.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterPacketPool.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisheap.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisHeap9x.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisinterlocked.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisInterrupt.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndislist.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisLookahead.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisLookaside.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndismedium.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisMiniAdapter.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisMiniDriver.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisOid.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisoiddebug.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisOidTraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacket.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPacketCancelId.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacketlist.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacketpool.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacketstransfer.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispartialpacket.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPciInformation.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPeripheralAddress.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPhysAddr.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPnpMode.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisprotocolbinding.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisprotocoltraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisprotocolwrapper.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisReceiveArea.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisReceiveAreaTraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisrequest.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisResource.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisRxAreaAsyncMgr.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisRxAreaNoAsync.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisSharedMemory.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisSharedReceiveArea.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisspinlock.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisstats.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisstring.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisSystemReceiveArea.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndistimer.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndistrace.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisWrapper.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kobject.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\ipnumbers.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\kipintr.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\ktdiirp.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\routable.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdi9xiface.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdiclient.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdiconnobj.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdidef9x.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdintiface.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdipartialreceive.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdipnpcl.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdisclient.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdisocket.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdistatus.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdistrserver.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\w9xNdisExp.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\cpprt.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\cright.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\dwcontrl.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394async.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394Irb.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394isoc.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kadapter.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kaddress.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\karray.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kchecker.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kcontrol.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdevice.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdevque.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdispobj.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdmaxfer.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdmqex.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdmqueue.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdpc.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdriver.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kerrlog.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kevent.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kfifo.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kfile.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kfilter.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kgenlock.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kheap.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kicount.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kimgsect.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kintrupt.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kiocparm.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kirp.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\klist.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\klower.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kmemory.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kmutex.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kndisvdw.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kpcicfg.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kpnpdev.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kpnplow.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kquery.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kregkey.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kresreq.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks5920.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks5933.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks59xx.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks59xxrd.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ksemaphr.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ksfifo.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kspin.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kstdwmi.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ksysthrd.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ktimer.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ktrace.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kunitnam.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kustring.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kvxdintf.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\Kwdmfltr.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kwmi.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kwmiblock.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kwmistr.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kworkitm.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\shfifo.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\vdw.h"\
	"c:\progra~1\numega\softic~1\driverworks\source\function.h"\
	"c:\progra~1\numega\softic~1\driverworks\source\shfifo.cpp"\
	{$(INCLUDE)}"bzlib.h"\
	{$(INCLUDE)}"zconf.h"\
	{$(INCLUDE)}"zlib.h"\
	
NODEP_CPP_MESSA=\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\netvxd.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\TDISTAT.H"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdivxd.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\vtdi.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\vxdntlib.h"\
	

!IF  "$(CFG)" == "kffss - Win32 NT4 Release"

!ELSEIF  "$(CFG)" == "kffss - Win32 NT4 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\utils.cpp
DEP_CPP_UTILS=\
	"..\..\..\..\appli\driver\ffssfs\inc\ffss_tdi.h"\
	"..\..\..\..\skyutils\src\skyutils.h"\
	"..\..\ffss.h"\
	"..\..\kffss.h"\
	"..\..\utils.h"\
	"c:\NTDDK\inc\alpharef.h"\
	"c:\ntddk\inc\basetsd.h"\
	"c:\ntddk\inc\bugcodes.h"\
	"c:\NTDDK\inc\ddk\afilter.h"\
	"c:\NTDDK\inc\ddk\efilter.h"\
	"c:\NTDDK\inc\ddk\ffilter.h"\
	"c:\ntddk\inc\ddk\ndis.h"\
	"c:\ntddk\inc\ddk\netpnp.h"\
	"c:\ntddk\inc\ddk\ntddk.h"\
	"c:\NTDDK\inc\ddk\tfilter.h"\
	"c:\ntddk\inc\guiddef.h"\
	"c:\NTDDK\inc\ia64reg.h"\
	"c:\ntddk\inc\netevent.h"\
	"c:\ntddk\inc\nettypes.h"\
	"c:\ntddk\inc\ntddndis.h"\
	"c:\ntddk\inc\ntddtdi.h"\
	"c:\ntddk\inc\ntdef.h"\
	"c:\ntddk\inc\ntiologc.h"\
	"c:\ntddk\inc\ntstatus.h"\
	"c:\ntddk\inc\packoff.h"\
	"c:\ntddk\inc\packon.h"\
	"c:\ntddk\inc\tdi.h"\
	"c:\ntddk\inc\tdikrnl.h"\
	"c:\NTDDK\inc\wdm.h"\
	"c:\NTDDK\inc\wmistr.h"\
	"c:\ntddk\src\network\inc\tdiinfo.h"\
	"c:\ntddk\src\network\wshsmple\smpletcp.h"\
	"c:\progra~1\numega\softic~1\common\include\stcinit.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kirppool.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\klistbrowser.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndis.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisadaptertraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisbuffer.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisbufferheap.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisbufferpool.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisBundle.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisChars.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisconfig.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\kndiscpp.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisevent.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterAdapter.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterAdapter51.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterBinding.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisFilterPacketPool.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisheap.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisHeap9x.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisinterlocked.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisInterrupt.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndislist.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisLookahead.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisLookaside.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndismedium.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisMiniAdapter.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisMiniDriver.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisOid.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisoiddebug.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisOidTraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacket.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPacketCancelId.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacketlist.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacketpool.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispacketstransfer.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndispartialpacket.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPciInformation.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPeripheralAddress.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPhysAddr.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisPnpMode.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisprotocolbinding.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisprotocoltraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisprotocolwrapper.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisReceiveArea.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisReceiveAreaTraits.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisrequest.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisResource.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisRxAreaAsyncMgr.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisRxAreaNoAsync.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisSharedMemory.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisSharedReceiveArea.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisspinlock.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisstats.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndisstring.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisSystemReceiveArea.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndistimer.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kndistrace.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverNetworks\include\KNdisWrapper.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\kobject.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\ipnumbers.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\kipintr.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\ktdiirp.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\routable.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdi9xiface.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdiclient.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdiconnobj.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdidef9x.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdintiface.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdipartialreceive.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdipnpcl.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdisclient.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdisocket.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdistatus.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdistrserver.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\w9xNdisExp.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\cpprt.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\cright.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\dwcontrl.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394async.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394Irb.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\k1394isoc.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kadapter.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kaddress.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\karray.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kchecker.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kcontrol.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdevice.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdevque.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdispobj.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdmaxfer.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdmqex.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdmqueue.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdpc.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kdriver.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kerrlog.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kevent.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kfifo.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kfile.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kfilter.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kgenlock.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kheap.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kicount.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kimgsect.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kintrupt.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kiocparm.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kirp.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\klist.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\klower.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kmemory.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kmutex.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kndisvdw.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kpcicfg.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kpnpdev.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kpnplow.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kquery.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kregkey.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kresreq.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks5920.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks5933.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks59xx.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ks59xxrd.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ksemaphr.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ksfifo.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kspin.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kstdwmi.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ksysthrd.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ktimer.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\ktrace.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kunitnam.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kustring.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kvxdintf.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\Kwdmfltr.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kwmi.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kwmiblock.h"\
	"C:\PROGRA~1\NuMega\SOFTIC~1\DriverWorks\include\kwmistr.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\kworkitm.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\shfifo.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\vdw.h"\
	"c:\progra~1\numega\softic~1\driverworks\source\function.h"\
	"c:\progra~1\numega\softic~1\driverworks\source\shfifo.cpp"\
	{$(INCLUDE)}"bzlib.h"\
	{$(INCLUDE)}"zconf.h"\
	{$(INCLUDE)}"zlib.h"\
	
NODEP_CPP_UTILS=\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\netvxd.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\TDISTAT.H"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\tdivxd.h"\
	"c:\progra~1\numega\softic~1\drivernetworks\include\tdiclient\vtdi.h"\
	"c:\progra~1\numega\softic~1\driverworks\include\vxdntlib.h"\
	

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
# Begin Source File

SOURCE=..\..\..\..\appli\driver\ffssfs\inc\ffss_tdi.h
# End Source File
# Begin Source File

SOURCE=..\..\kffss.h
# End Source File
# End Group
# End Target
# End Project
