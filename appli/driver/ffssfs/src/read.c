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
#include "ffss_tdi.h"

NTSTATUS
FsdRead (
    IN PFSD_IRP_CONTEXT IrpContext
    )
{
    NTSTATUS Status;

    ASSERT(IrpContext);

    ASSERT((IrpContext->Identifier.Type == ICX) &&
           (IrpContext->Identifier.Size == sizeof(FSD_IRP_CONTEXT)));

    if (FlagOn(IrpContext->MinorFunction, IRP_MN_COMPLETE))
    {
        Status = FsdReadComplete(IrpContext);
    }
    else
    {
        Status = FsdReadNormal(IrpContext);
    }

    return Status;
}

NTSTATUS
FsdReadNormal (
    IN PFSD_IRP_CONTEXT IrpContext
    )
{
    PDEVICE_OBJECT      DeviceObject;
    NTSTATUS            Status = STATUS_UNSUCCESSFUL;
    PFSD_VCB            Vcb;
    PFILE_OBJECT        FileObject;
    PFSD_FCB            Fcb;
    PFSD_CCB            Ccb;
    PIRP                Irp;
    PIO_STACK_LOCATION  IrpSp;
    ULONG               Length;
    ULONG               ReturnedLength;
    LARGE_INTEGER       ByteOffset;
    BOOLEAN             PagingIo;
    BOOLEAN             Nocache;
    BOOLEAN             SynchronousIo;
    BOOLEAN             VcbResourceAcquired = FALSE;
    BOOLEAN             FcbMainResourceAcquired = FALSE;
    BOOLEAN             FcbPagingIoResourceAcquired = FALSE;
    PUCHAR              UserBuffer;
    PDEVICE_OBJECT      DeviceToVerify;

    __try
    {
        ASSERT(IrpContext);

        ASSERT((IrpContext->Identifier.Type == ICX) &&
               (IrpContext->Identifier.Size == sizeof(FSD_IRP_CONTEXT)));

        DeviceObject = IrpContext->DeviceObject;

        if (DeviceObject == FsdGlobalData.DeviceObject)
        {
            Status = STATUS_INVALID_DEVICE_REQUEST;
            __leave;
        }

        Vcb = (PFSD_VCB) DeviceObject->DeviceExtension;

        ASSERT(Vcb != NULL);

        ASSERT((Vcb->Identifier.Type == VCB) &&
               (Vcb->Identifier.Size == sizeof(FSD_VCB)));

        FileObject = IrpContext->FileObject;

        Fcb = (PFSD_FCB) FileObject->FsContext;

        ASSERT(Fcb);

        if (Fcb->Identifier.Type == VCB)
        {
            Irp = IrpContext->Irp;

            IrpSp = IoGetCurrentIrpStackLocation(Irp);

            Length = IrpSp->Parameters.Read.Length;
            ByteOffset = IrpSp->Parameters.Read.ByteOffset;

            PagingIo = (BOOLEAN) FlagOn(Irp->Flags, IRP_PAGING_IO);
            Nocache = (BOOLEAN) FlagOn(Irp->Flags, IRP_NOCACHE);
            SynchronousIo = (BOOLEAN) FlagOn(FileObject->Flags, FO_SYNCHRONOUS_IO);

            if (Length == 0)
            {
                Irp->IoStatus.Information = 0;
                Status = STATUS_SUCCESS;
                __leave;
            }

            if (!Nocache || ByteOffset.LowPart & (SECTOR_SIZE - 1) || Length & (SECTOR_SIZE - 1))
            {
                Status = STATUS_INVALID_PARAMETER;
                __leave;
            }

            if (!ExAcquireResourceSharedLite(
                     &Vcb->MainResource,
                     IrpContext->IsSynchronous
                     ))
            {
                Status = STATUS_PENDING;
                __leave;
            }

            VcbResourceAcquired = TRUE;

            KdPrint(("AIE AIE AIE : Requesting I/O READ on VCB !!  -> TO DO\n"));
            /* TO DO : ByteOffset = Offset de depart de lecture dans le fichier */
            if (ByteOffset.QuadPart >=

#ifndef FSD_RO
                Vcb->PartitionInformation.PartitionLength.QuadPart
#else
                0x555555//be32_to_cpu(Vcb->romfs_super_block->size)
#endif
                )
            {
                Irp->IoStatus.Information = 0;
                Status = STATUS_END_OF_FILE;
                __leave;
            }

            /* TO DO */
            if ((ByteOffset.QuadPart + Length) >

#ifndef FSD_RO
                 Vcb->PartitionInformation.PartitionLength.QuadPart
#else
                 0x555555//be32_to_cpu(Vcb->romfs_super_block->size)
#endif
                 )
            {
                Length = (ULONG) (
#ifndef FSD_RO
                Vcb->PartitionInformation.PartitionLength.QuadPart -
#else
                0x555555 - //be32_to_cpu(Vcb->romfs_super_block->size) -
#endif
                ByteOffset.QuadPart);

                Length &= ~(SECTOR_SIZE - 1);
            }

            UserBuffer = FsdGetUserBuffer(Irp);

            if (UserBuffer == NULL)
            {
                Status = STATUS_INVALID_USER_BUFFER;
                __leave;
            }

            Status = FsdReadWriteBlockDevice(
                IRP_MJ_READ,
                Vcb->TargetDeviceObject,
                &ByteOffset,
                Length,
                FALSE,
                UserBuffer
                );

            if (Status == STATUS_VERIFY_REQUIRED)
            {
                Status = IoVerifyVolume(Vcb->TargetDeviceObject, FALSE);

                if (NT_SUCCESS(Status))
                {
                    Status = FsdReadWriteBlockDevice(
                        IRP_MJ_READ,
                        Vcb->TargetDeviceObject,
                        &ByteOffset,
                        Length,
                        FALSE,
                        UserBuffer
                        );
                }
            }

            if (NT_SUCCESS(Status))
            {
                Irp->IoStatus.Information = Length;
            }

            __leave;
        }

        ASSERT((Fcb->Identifier.Type == FCB) &&
               (Fcb->Identifier.Size == sizeof(FSD_FCB)));

        if (FlagOn(Fcb->FileAttributes, FILE_ATTRIBUTE_DIRECTORY))
        {
            Status = STATUS_INVALID_PARAMETER;
            __leave;
        }

        Ccb = (PFSD_CCB) FileObject->FsContext2;

        ASSERT(Ccb);

        ASSERT((Ccb->Identifier.Type == CCB) &&
               (Ccb->Identifier.Size == sizeof(FSD_CCB)));

        Irp = IrpContext->Irp;

        IrpSp = IoGetCurrentIrpStackLocation(Irp);

        Length = IrpSp->Parameters.Read.Length;
        ByteOffset = IrpSp->Parameters.Read.ByteOffset;

        PagingIo = (BOOLEAN) FlagOn(Irp->Flags, IRP_PAGING_IO);
        Nocache = (BOOLEAN) FlagOn(Irp->Flags, IRP_NOCACHE);
        SynchronousIo = (BOOLEAN) FlagOn(FileObject->Flags, FO_SYNCHRONOUS_IO);

        if (Irp->RequestorMode != KernelMode &&
            !Irp->MdlAddress &&
            Irp->UserBuffer)
        {
            ProbeForWrite(Irp->UserBuffer, Length, 1);
        }

        if (Length == 0)
        {
            Irp->IoStatus.Information = 0;
            Status = STATUS_SUCCESS;
            __leave;
        }

        if (Nocache &&
           (ByteOffset.LowPart & (SECTOR_SIZE - 1) ||
            Length & (SECTOR_SIZE - 1)))
        {
            Status = STATUS_INVALID_PARAMETER;
            __leave;
        }

        if (FlagOn(IrpContext->MinorFunction, IRP_MN_DPC))
        {
            ClearFlag(IrpContext->MinorFunction, IRP_MN_DPC);
            Status = STATUS_PENDING;
            __leave;
        }

        if (!PagingIo)
        {
            if (!ExAcquireResourceSharedLite(
                     &Fcb->MainResource,
                     IrpContext->IsSynchronous
                     ))
            {
                Status = STATUS_PENDING;
                __leave;
            }

            FcbMainResourceAcquired = TRUE;
        }
        else
        {
            if (!ExAcquireResourceSharedLite(
                     &Fcb->PagingIoResource,
                     IrpContext->IsSynchronous
                     ))
            {
                Status = STATUS_PENDING;
                __leave;
            }

            FcbPagingIoResourceAcquired = TRUE;
        }

        if (ByteOffset.QuadPart >= Fcb->ffss_inode->Size)
        {
            Irp->IoStatus.Information = 0;
            Status = STATUS_END_OF_FILE;
            __leave;
        }

        if (!PagingIo)
        {
            if (!FsRtlCheckLockForReadAccess(
                    &Fcb->FileLock,
                    Irp
                    ))
            {
                Status = STATUS_FILE_LOCK_CONFLICT;
                __leave;
            }
        }

        if (!Nocache)
        {
            if ((ByteOffset.QuadPart + Length) > Fcb->ffss_inode->Size)
            {
                Length = Fcb->ffss_inode->Size - ByteOffset.LowPart;
            }

            if (FileObject->PrivateCacheMap == NULL)
            {
                CcInitializeCacheMap(
                    FileObject,
                    (PCC_FILE_SIZES)(&Fcb->CommonFCBHeader.AllocationSize),
                    FALSE,
                    &FsdGlobalData.CacheManagerCallbacks,
                    Fcb
                    );
            }

            if (FlagOn(IrpContext->MinorFunction, IRP_MN_MDL))
            {
                CcMdlRead(
                    FileObject,
                    &ByteOffset,
                    Length,
                    &Irp->MdlAddress,
                    &Irp->IoStatus
                    );

                Status = Irp->IoStatus.Status;
            }
            else
            {
                UserBuffer = FsdGetUserBuffer(Irp);

                if (UserBuffer == NULL)
                {
                    Status = STATUS_INVALID_USER_BUFFER;
                    __leave;
                }

                if (!CcCopyRead(
                    FileObject,
                    &ByteOffset,
                    Length,
                    IrpContext->IsSynchronous,
                    UserBuffer,
                    &Irp->IoStatus
                    ))
                {
                    Status = STATUS_PENDING;
                    __leave;
                }

                Status = Irp->IoStatus.Status;
            }
        }
        else
        {
            ReturnedLength = Length;

            if ((ByteOffset.QuadPart + Length) > Fcb->ffss_inode->Size)
            {
                ReturnedLength = Fcb->ffss_inode->Size - ByteOffset.LowPart;

                Length = (ReturnedLength & ~(SECTOR_SIZE - 1)) + SECTOR_SIZE;
            }

            UserBuffer = FsdGetUserBuffer(Irp);

            if (UserBuffer == NULL)
            {
                Status = STATUS_INVALID_USER_BUFFER;
                __leave;
            }

            /* Check if file is already opened */
            if(Ccb->State != FFSS_HANDLE_STATE_OPEN)
            {
              if(!FsdRequestStrmOpen(Fcb->ffss_inode,Ccb,FFSS_STRM_OPEN_READ | FFSS_STRM_OPEN_BINARY))
              {
                Status = STATUS_NOT_FOUND;
                __leave;
              }
            }

            Status = FsdReadFileData(
                Vcb->TargetDeviceObject,
                Ccb,
                Fcb->ffss_inode,
                &ByteOffset.QuadPart,
                Length,
                UserBuffer
                );

            /*if (Status == STATUS_VERIFY_REQUIRED)
            {
                DeviceToVerify = IoGetDeviceToVerify(PsGetCurrentThread());

                IoSetDeviceToVerify(PsGetCurrentThread(), NULL);

                Status = IoVerifyVolume(DeviceToVerify, FALSE);

                if (NT_SUCCESS(Status))
                {
                    Status = FsdReadFileData(
                        Vcb->TargetDeviceObject,
                        Fcb,
                        &ByteOffset,
                        Length,
                        UserBuffer
                        );
                }
            }*/

            if (NT_SUCCESS(Status))
            {
                Irp->IoStatus.Information = ReturnedLength;
            }
        }
    }
    __finally
    {
        if (FcbPagingIoResourceAcquired)
        {
            ExReleaseResourceForThreadLite(
                &Fcb->PagingIoResource,
                ExGetCurrentResourceThread()
                );
        }

        if (FcbMainResourceAcquired)
        {
            ExReleaseResourceForThreadLite(
                &Fcb->MainResource,
                ExGetCurrentResourceThread()
                );
        }

        if (VcbResourceAcquired)
        {
            ExReleaseResourceForThreadLite(
                &Vcb->MainResource,
                ExGetCurrentResourceThread()
                );
        }

        if (!IrpContext->ExceptionInProgress)
        {
            if (Status == STATUS_PENDING)
            {
                Status = FsdLockUserBuffer(
                    IrpContext->Irp,
                    Length,
                    IoWriteAccess
                    );

                if (NT_SUCCESS(Status))
                {
                    Status = FsdQueueRequest(IrpContext);
                }
                else
                {
                    IrpContext->Irp->IoStatus.Status = Status;
                    FsdCompleteRequest(IrpContext->Irp, IO_NO_INCREMENT);
                    FsdFreeIrpContext(IrpContext);
                }
            }
            else
            {
                IrpContext->Irp->IoStatus.Status = Status;

                if (SynchronousIo && !PagingIo && NT_SUCCESS(Status))
                {
                    FileObject->CurrentByteOffset.QuadPart =
                        ByteOffset.QuadPart + Irp->IoStatus.Information;
                }

                if (!PagingIo && NT_SUCCESS(Status))
                {
                    FileObject->Flags &= ~FO_FILE_FAST_IO_READ;
                }

                FsdCompleteRequest(
                    IrpContext->Irp,
                    (CCHAR)
                    (NT_SUCCESS(Status) ? IO_DISK_INCREMENT : IO_NO_INCREMENT)
                    );

                FsdFreeIrpContext(IrpContext);
            }
        }
    }

    return Status;
}

NTSTATUS
FsdReadComplete (
    IN PFSD_IRP_CONTEXT IrpContext
    )
{
    NTSTATUS        Status = STATUS_UNSUCCESSFUL;
    PFILE_OBJECT    FileObject;
    PIRP            Irp;

    __try
    {
        ASSERT(IrpContext);

        ASSERT((IrpContext->Identifier.Type == ICX) &&
               (IrpContext->Identifier.Size == sizeof(FSD_IRP_CONTEXT)));

        FileObject = IrpContext->FileObject;

        Irp = IrpContext->Irp;

        CcMdlReadComplete(FileObject, Irp->MdlAddress);

        Irp->MdlAddress = NULL;

        Status = STATUS_SUCCESS;
    }
    __finally
    {
        if (!IrpContext->ExceptionInProgress)
        {
            IrpContext->Irp->IoStatus.Status = Status;

            FsdCompleteRequest(
                IrpContext->Irp,
                (CCHAR)
                (NT_SUCCESS(Status) ? IO_DISK_INCREMENT : IO_NO_INCREMENT)
                );

            FsdFreeIrpContext(IrpContext);
        }
    }

    return Status;
}

/*NTSTATUS
FsdReadFileData (
    IN PDEVICE_OBJECT       DeviceObject,
    IN struct ffss_inode*   Inode,
    IN PLARGE_INTEGER       Offset,
    IN ULONG                Length,
    IN OUT PVOID            Buffer
    )
{
    LARGE_INTEGER   PhysicalOffset;
    ULONG           PhysicalLength;
    PUCHAR          PhysicalBuffer;
    NTSTATUS        Status;

    ASSERT(DeviceObject != NULL);
    ASSERT(Inode != NULL);
    ASSERT(Offset != NULL);
    ASSERT(Buffer != NULL);

    KdPrint((
        DRIVER_NAME
        ": FsdReadFileData: Index: %#x Offset: %I64u Length: %u\n",
        Index,
        Offset->QuadPart,
        Length
        ));

    if (!((Index + SIZEOF_FFSS_INODE(Inode) + Offset->QuadPart) &
        (SECTOR_SIZE - 1))
        )
    {
        PhysicalOffset.QuadPart =
            Index + SIZEOF_FFSS_INODE(Inode) + Offset->QuadPart;

        Status = FsdReadWriteBlockDevice(
            IRP_MJ_READ,
            DeviceObject,
            &PhysicalOffset,
            Length,
            FALSE,
            Buffer
            );
    }
    else
    {
        PhysicalOffset.QuadPart = (Index + SIZEOF_FFSS_INODE(Inode) + Offset->QuadPart) & ~(SECTOR_SIZE - 1);

        PhysicalLength = Length + SECTOR_SIZE;

        PhysicalBuffer = (PUCHAR) FsdAllocatePool(
            NonPagedPoolCacheAligned,
            PhysicalLength,
            '3mTR'
            );

        if (PhysicalBuffer == NULL)
        {
            return STATUS_INSUFFICIENT_RESOURCES;
        }

        Status = FsdReadWriteBlockDevice(
            IRP_MJ_READ,
            DeviceObject,
            &PhysicalOffset,
            PhysicalLength,
            FALSE,
            PhysicalBuffer
            );

        RtlCopyMemory(
            Buffer,
            PhysicalBuffer +
            ((Index + SIZEOF_FFSS_INODE(Inode) + Offset->QuadPart) &
            (SECTOR_SIZE - 1)),
            Length
            );

        FsdFreePool(PhysicalBuffer);
    }

    return Status;
}*/


void OnStrmOpenAnswer(SU_PClientSocket Client,const char Path[],int Code,FFSS_Field Handle,FFSS_LongField FileSize,FFSS_LongField User)
{
  PFSD_CCB Ccb;

  Ccb = (PFSD_CCB) User;
  if(Ccb == NULL)
  {
    KdPrint(("OnStrmOpenAnswer : User Pointer is NULL... disconnecting\n"));
    return;
  }

  if(Code == FFSS_ERROR_NO_ERROR)
  {
    KdPrint(("OnStrmOpenAnswer : File successfully opened by server with handle %d\n",Handle));
    Ccb->State = FFSS_HANDLE_STATE_OPEN;
    Ccb->Handle = Handle;
  }
}

void OnStrmReadAnswer(SU_PClientSocket Client,FFSS_Field Handle,const char Bloc[],long int BlocSize,FFSS_LongField User)
{
  PFSD_CCB Ccb;

  Ccb = (PFSD_CCB) User;
  if(Ccb == NULL)
  {
    KdPrint(("OnStrmReadAnswer : User Pointer is NULL... disconnecting\n"));
    return;
  }

  if(BlocSize == 0)
  {
    KdPrint(("OnStrmReadAnswer : EOF for file %d\n",Handle));
    Ccb->eof = true;
  }
  else
  {
    if(BlocSize > STREAMING_BUFFER_SIZE)
      BlocSize = STREAMING_BUFFER_SIZE;
    KdPrint(("OnStrmReadAnswer : %ld bytes read from %d\n",BlocSize,Handle));
    RtlCopyMemory(Ccb->Buffer,Bloc,BlocSize);
    Ccb->BufferPos = BlocSize;
  }
}

