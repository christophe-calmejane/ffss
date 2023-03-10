/*
    This is a ffssfs file system driver for Windows NT/2000/XP.
    Copyright (C) 2002 Christophe Calmejane
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "ntifs.h"
#include "fsd.h"
#include "ffss_fs.h"
#include "ffss_tdi.h"

struct ffss_super_block *FFSS_SuperBlock;

//
// The FSDs global data
//
FSD_GLOBAL_DATA FsdGlobalData;

#pragma code_seg(FSD_INIT_CODE)

NTSTATUS
DriverEntry (
    IN PDRIVER_OBJECT   DriverObject,
    IN PUNICODE_STRING  RegistryPath
    )
{
    PFAST_IO_DISPATCH           FastIoDispatch;
    PCACHE_MANAGER_CALLBACKS    CacheManagerCallbacks;
    UNICODE_STRING              DeviceName;
#if DBG
    UNICODE_STRING              DosDeviceName;
#endif
    NTSTATUS                    Status;

    //
    // Print some info about the driver
    //

    DbgPrint(DRIVER_NAME ": " __DATE__ " " __TIME__

#ifdef FSD_RO
    ", read-only"
#endif

#if DBG
    ", checked"
#endif

    ", _WIN32_WINNT=%#x, Christophe Calmejane\n", _WIN32_WINNT);

	// Initialize TDI
    Status = TDI_Init();
    if (!NT_SUCCESS(Status))
	{
		DbgPrint(DRIVER_NAME ": Cannot initialize TDI layer\n");
		return Status;
	}

    //
    // Initialize the global data
    //

    FFSS_SuperBlock = FsdAllocSuperBlock();

    RtlZeroMemory(&FsdGlobalData, sizeof(FSD_GLOBAL_DATA));

    FsdGlobalData.Identifier.Type = FGD;
    FsdGlobalData.Identifier.Size = sizeof(FSD_GLOBAL_DATA);

    FsdGlobalData.DriverObject = DriverObject;

    InitializeListHead(&FsdGlobalData.VcbList);

    //
    // Initialize the dispatch entry points
    //

    DriverObject->MajorFunction[IRP_MJ_CREATE] = FsdBuildRequest;
    DriverObject->MajorFunction[IRP_MJ_CLOSE] = FsdBuildRequest;
    DriverObject->MajorFunction[IRP_MJ_READ] = FsdBuildRequest;
    DriverObject->MajorFunction[IRP_MJ_QUERY_INFORMATION] = FsdBuildRequest;
    DriverObject->MajorFunction[IRP_MJ_SET_INFORMATION] = FsdBuildRequest;
    DriverObject->MajorFunction[IRP_MJ_QUERY_VOLUME_INFORMATION] = FsdBuildRequest;
    DriverObject->MajorFunction[IRP_MJ_DIRECTORY_CONTROL] = FsdBuildRequest;
    DriverObject->MajorFunction[IRP_MJ_FILE_SYSTEM_CONTROL] = FsdBuildRequest;
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = FsdBuildRequest;
    DriverObject->MajorFunction[IRP_MJ_LOCK_CONTROL] = FsdBuildRequest;
    DriverObject->MajorFunction[IRP_MJ_CLEANUP] = FsdBuildRequest;

    //
    // Initialize the fast I/O entry points
    //
/* TO DO FASTIO */
    /*FastIoDispatch = &FsdGlobalData.FastIoDispatch;

    FastIoDispatch->SizeOfFastIoDispatch = sizeof(FAST_IO_DISPATCH);
    FastIoDispatch->FastIoCheckIfPossible = FsdFastIoCheckIfPossible;
#if DBG
    FastIoDispatch->FastIoRead = FsdFastIoRead;
#else
    FastIoDispatch->FastIoRead = FsRtlCopyRead;
#endif
    FastIoDispatch->FastIoQueryBasicInfo = FsdFastIoQueryBasicInfo;
    FastIoDispatch->FastIoQueryStandardInfo = FsdFastIoQueryStandardInfo;
    FastIoDispatch->FastIoLock = FsdFastIoLock;
    FastIoDispatch->FastIoUnlockSingle = FsdFastIoUnlockSingle;
    FastIoDispatch->FastIoUnlockAll = FsdFastIoUnlockAll;
    FastIoDispatch->FastIoUnlockAllByKey = FsdFastIoUnlockAllByKey;
    FastIoDispatch->FastIoQueryNetworkOpenInfo = FsdFastIoQueryNetworkOpenInfo;

    DriverObject->FastIoDispatch = FastIoDispatch;*/

    //
    // Initialize the Cache Manager callbacks
    //

    CacheManagerCallbacks = &FsdGlobalData.CacheManagerCallbacks;

    CacheManagerCallbacks->AcquireForLazyWrite = FsdAcquireForLazyWrite;
    CacheManagerCallbacks->ReleaseFromLazyWrite = FsdReleaseFromLazyWrite;
    CacheManagerCallbacks->AcquireForReadAhead = FsdAcquireForReadAhead;
    CacheManagerCallbacks->ReleaseFromReadAhead = FsdReleaseFromReadAhead;

    //
    // Create the main device object
    //

    RtlInitUnicodeString(&DeviceName, DEVICE_NAME);

    Status = IoCreateDevice(
        DriverObject,
        0,
        &DeviceName,
        FILE_DEVICE_DISK_FILE_SYSTEM,
        0,
        FALSE,
        &FsdGlobalData.DeviceObject
        );

    if (NT_SUCCESS(Status))
    {
        ExInitializeResourceLite(&FsdGlobalData.Resource);
#if DBG
        RtlInitUnicodeString(&DosDeviceName, DOS_DEVICE_NAME);

        IoCreateSymbolicLink(&DosDeviceName, &DeviceName);

        ProcessNameOffset = FsdGetProcessNameOffset();
#endif
        IoRegisterFileSystem(FsdGlobalData.DeviceObject);
    }

    return Status;
}

#pragma code_seg(FSD_PAGED_CODE)

#if DBG

/*
 * An application must call IOCTL_PREPARE_TO_UNLOAD before the driver can be
 * unloaded.
 */

VOID
DriverUnload (
    IN PDRIVER_OBJECT DriverObject
    )
{
    UNICODE_STRING DosDeviceName;


    KdPrint((DRIVER_NAME ": Unloading driver\n"));

    FsdFreeSuperBlock(FFSS_SuperBlock);

    RtlInitUnicodeString(&DosDeviceName, DOS_DEVICE_NAME);

    IoDeleteSymbolicLink(&DosDeviceName);

    ExDeleteResourceLite(&FsdGlobalData.Resource);

    IoDeleteDevice(FsdGlobalData.DeviceObject);
}

#endif // DBG

#pragma code_seg() // end FSD_PAGED_CODE
