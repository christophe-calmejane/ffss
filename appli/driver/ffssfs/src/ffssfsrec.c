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
#include "border.h"

#pragma code_seg(FSD_PAGED_CODE)

NTSTATUS
FsdIsDeviceRomfs (
    IN PDEVICE_OBJECT DeviceObject
    )
{
    PUCHAR          Buffer;
    LARGE_INTEGER   Offset;
    NTSTATUS        Status;

    ASSERT(DeviceObject != NULL);

    Buffer = FsdAllocatePool(NonPagedPoolCacheAligned, 512, '1ceR');

    if (!Buffer)
    {
        return STATUS_INSUFFICIENT_RESOURCES;
    }

    Offset.QuadPart = 0;

    Status = FsdReadWriteBlockDevice(
        IRP_MJ_READ,
        DeviceObject,
        &Offset,
        512,
        TRUE,
        Buffer
        );

    if (!NT_SUCCESS(Status) || strncmp(Buffer, FFSSDISK_ID, sizeof(FFSSDISK_ID)))
    {
		KdPrint((DBGNAME "Not FFSS Volume"));
        Status = STATUS_UNRECOGNIZED_VOLUME;
    }
	else
	{
		KdPrint((DBGNAME "Found FFSS Volume!!\n"));
	}

    FsdFreePool(Buffer);

    return Status;
}

#pragma code_seg() // end FSD_PAGED_CODE

NTSTATUS
FsdIsDeviceSameRomfs (
    IN PDEVICE_OBJECT   DeviceObject,
    IN ULONG            CheckSum
    )
{
/*    struct romfs_super_block*   romfs_super_block;
    LARGE_INTEGER               Offset;
    NTSTATUS                    Status;

    ASSERT(DeviceObject != NULL);

    romfs_super_block = FsdAllocatePool(
        NonPagedPoolCacheAligned,
        SECTOR_SIZE,
        '2ceR'
        );

    if (!romfs_super_block)
    {
        return STATUS_INSUFFICIENT_RESOURCES;
    }

    Offset.QuadPart = 0; //ROMFS_MAGIC_OFFSET;

    Status = FsdReadWriteBlockDevice(
        IRP_MJ_READ,
        DeviceObject,
        &Offset,
        SECTOR_SIZE,
        TRUE,
        romfs_super_block
        );

    if (!NT_SUCCESS(Status) ||
        strncmp(romfs_super_block, FFSSDISK_ID, sizeof(FFSSDISK_ID)) ||
        be32_to_cpu(romfs_super_block->checksum) != CheckSum
        )
    {
        Status = STATUS_WRONG_VOLUME;
    }

    FsdFreePool(romfs_super_block);

    return Status;*/
  return STATUS_SUCCESS;
}
