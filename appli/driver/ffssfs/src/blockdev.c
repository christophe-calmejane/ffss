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

NTSTATUS 
FsdBlockDeviceIoControl (
    IN PDEVICE_OBJECT   DeviceObject,
    IN ULONG            IoctlCode,
    IN PVOID            InputBuffer,
    IN ULONG            InputBufferSize,
    IN OUT PVOID        OutputBuffer,
    IN OUT PULONG       OutputBufferSize
    )
{
    ULONG           OutputBufferSize2 = 0;
    KEVENT          Event;
    PIRP            Irp;
    IO_STATUS_BLOCK IoStatus;
    NTSTATUS        Status;

    ASSERT(DeviceObject != NULL);

    if (OutputBufferSize)
    {
        OutputBufferSize2 = *OutputBufferSize;
    }

    KeInitializeEvent(&Event, NotificationEvent, FALSE);

    Irp = IoBuildDeviceIoControlRequest(
        IoctlCode,
        DeviceObject,
        InputBuffer,
        InputBufferSize,
        OutputBuffer,
        OutputBufferSize2,
        FALSE,
        &Event,
        &IoStatus
        );

    if (!Irp)
    {
        return STATUS_INSUFFICIENT_RESOURCES;
    }

    Status = IoCallDriver(DeviceObject, Irp);

    if (Status == STATUS_PENDING)
    {
        KeWaitForSingleObject(
            &Event,
            Executive,
            KernelMode,
            FALSE,
            NULL
            );
        Status = IoStatus.Status;
    }

    if (OutputBufferSize)
    {
        *OutputBufferSize = IoStatus.Information;
    }

    return Status;
}

NTSTATUS
FsdReadWriteBlockDevice (
    IN ULONG            Operation,
    IN PDEVICE_OBJECT   DeviceObject,
    IN PLARGE_INTEGER   Offset,
    IN ULONG            Length,
    IN BOOLEAN          OverrideVerify,
    IN OUT PVOID        Buffer
    )
{
    PIRP                Irp;
    IO_STATUS_BLOCK     IoStatus;
    KEVENT              Event;
    PIO_STACK_LOCATION  IrpSp;

#ifndef FSD_RO
    ASSERT(Operation == IRP_MJ_READ || Operation == IRP_MJ_WRITE);
#else
    ASSERT(Operation == IRP_MJ_READ);
#endif
    ASSERT(DeviceObject != NULL);
    ASSERT(Offset != NULL);
    ASSERT(Buffer != NULL);

    Irp = IoBuildAsynchronousFsdRequest(
        Operation,
        DeviceObject,
        Buffer,
        Length,
        Offset,
        &IoStatus
        );

    if (!Irp)
    {
        return STATUS_INSUFFICIENT_RESOURCES;
    }

    KeInitializeEvent(&Event, NotificationEvent, FALSE);

    Irp->UserEvent = &Event;

    IoSetCompletionRoutine(
        Irp,
        FsdReadWriteBlockDeviceCompletion,
        NULL,
        TRUE,
        TRUE,
        TRUE
        );

    if (OverrideVerify)
    {
        IrpSp = IoGetNextIrpStackLocation(Irp);

        SetFlag(IrpSp->Flags, SL_OVERRIDE_VERIFY_VOLUME);
    }

    IoCallDriver(DeviceObject, Irp);

    KeWaitForSingleObject(
        &Event,
        Executive,
        KernelMode,
        FALSE,
        NULL
        );

    return IoStatus.Status;
}

NTSTATUS
FsdReadWriteBlockDeviceCompletion (
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp,
    IN PVOID            Context
    )
{
    PMDL Mdl;

    ASSERT(Irp != NULL);

    *Irp->UserIosb = Irp->IoStatus;

    KeSetEvent(Irp->UserEvent, 0, FALSE);

    while ((Mdl = Irp->MdlAddress))
    {
        Irp->MdlAddress = Mdl->Next;

        IoFreeMdl(Mdl);
    }

    IoFreeIrp(Irp);

    return STATUS_MORE_PROCESSING_REQUIRED;
}
