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

#if DBG

PVOID
FsdAllocatePool (
    IN POOL_TYPE    PoolType,
    IN ULONG        NumberOfBytes,
    IN ULONG        Tag
    )
{
    PVOID               p;
    PFSD_ALLOC_HEADER   AllocHeader;

    NumberOfBytes += sizeof(FSD_ALLOC_HEADER);

    p = ExAllocatePoolWithTag(PoolType, NumberOfBytes, Tag);

    if (p)
    {
        RtlFillMemory(p, NumberOfBytes, '0');

        AllocHeader = (PFSD_ALLOC_HEADER) p;

        AllocHeader->Identifier.Type = FSD;
        AllocHeader->Identifier.Size = NumberOfBytes;

        return (PVOID)((PUCHAR)p + sizeof(FSD_ALLOC_HEADER));
    }
    else
    {
        return NULL;
    }
}

VOID
FsdFreePool (
    IN PVOID p
    )
{
    PFSD_ALLOC_HEADER AllocHeader;

    ASSERT(p != NULL);

    p = (PVOID)((PUCHAR)p - sizeof(FSD_ALLOC_HEADER));

    AllocHeader = (PFSD_ALLOC_HEADER) p;

    ASSERT(AllocHeader->Identifier.Type == FSD);

    RtlFillMemory(p, AllocHeader->Identifier.Size, 'X');

    ExFreePool(p);
}

#endif // DBG

PFSD_IRP_CONTEXT
FsdAllocateIrpContext (
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp
    )
{
    PIO_STACK_LOCATION  IrpSp;
    PFSD_IRP_CONTEXT    IrpContext;

    ASSERT(DeviceObject != NULL);
    ASSERT(Irp != NULL);

    IrpSp = IoGetCurrentIrpStackLocation(Irp);

    IrpContext = FsdAllocatePool(NonPagedPool, sizeof(FSD_IRP_CONTEXT), 'xcIR');

    if (!IrpContext)
    {
        return NULL;
    }

    IrpContext->Identifier.Type = ICX;
    IrpContext->Identifier.Size = sizeof(FSD_IRP_CONTEXT);

    IrpContext->Irp = Irp;

    IrpContext->MajorFunction = IrpSp->MajorFunction;
    IrpContext->MinorFunction = IrpSp->MinorFunction;

    IrpContext->DeviceObject = DeviceObject;

    IrpContext->FileObject = IrpSp->FileObject;

    if (IrpContext->MajorFunction == IRP_MJ_FILE_SYSTEM_CONTROL ||
        IrpContext->MajorFunction == IRP_MJ_DEVICE_CONTROL ||
        IrpContext->MajorFunction == IRP_MJ_SHUTDOWN)
    {
        IrpContext->IsSynchronous = TRUE;
    }
    else if (IrpContext->MajorFunction == IRP_MJ_CLEANUP ||
             IrpContext->MajorFunction == IRP_MJ_CLOSE)
    {
        IrpContext->IsSynchronous = FALSE;
    }
    else
    {
        IrpContext->IsSynchronous = IoIsOperationSynchronous(Irp);
    }

    //
    // Temporary workaround for a bug in close that makes it reference a
    // fileobject when it is no longer valid.
    //
    if (IrpContext->MajorFunction == IRP_MJ_CLOSE)
    {
        IrpContext->IsSynchronous = TRUE;
    }

    IrpContext->IsTopLevel = (IoGetTopLevelIrp() == Irp);

    IrpContext->ExceptionInProgress = FALSE;

    return IrpContext;
}

VOID
FsdFreeIrpContext (
    IN PFSD_IRP_CONTEXT IrpContext
    )
{
    ASSERT(IrpContext != NULL);

    ASSERT((IrpContext->Identifier.Type == ICX) &&
           (IrpContext->Identifier.Size == sizeof(FSD_IRP_CONTEXT)));

    FsdFreePool(IrpContext);
}

#pragma code_seg(FSD_PAGED_CODE)

PFSD_FCB
FsdAllocateFcb (
    IN PFSD_VCB             Vcb,
    IN PUNICODE_STRING      FileName,
    IN struct ffss_inode*   ffss_inode
    )
{
    PFSD_FCB Fcb;

    Fcb = FsdAllocatePool(NonPagedPool, sizeof(FSD_FCB), 'bcFR');

    if (!Fcb)
    {
        return NULL;
    }

    Fcb->Identifier.Type = FCB;
    Fcb->Identifier.Size = sizeof(FSD_FCB);

#ifndef FSD_RO
    RtlZeroMemory(&Fcb->ShareAccess, sizeof(SHARE_ACCESS));
#endif

    FsRtlInitializeFileLock(
        &Fcb->FileLock,
        NULL,
        NULL
        );

    Fcb->OpenHandleCount = 0;
    Fcb->ReferenceCount = 0;

    Fcb->FileName.Length = 0;

    Fcb->FileName.MaximumLength = FileName->Length;

    Fcb->FileName.Buffer = (PWSTR) FsdAllocatePool(
        NonPagedPool,
        Fcb->FileName.MaximumLength,
        '1cFR'
        );

    if (!Fcb->FileName.Buffer)
    {
        FsdFreePool(Fcb);
        return NULL;
    }

    RtlCopyUnicodeString(
        &Fcb->FileName,
        FileName
        );

    Fcb->AnsiFileName.Length = Fcb->FileName.Length / sizeof(WCHAR);

    Fcb->AnsiFileName.MaximumLength = Fcb->FileName.Length / sizeof(WCHAR) + 1;

    Fcb->AnsiFileName.Buffer = (PUCHAR) FsdAllocatePool(
        NonPagedPool,
        Fcb->FileName.Length / sizeof(WCHAR) + 1,
        '2cFR'
        );

    if (!Fcb->AnsiFileName.Buffer)
    {
        FsdFreePool(Fcb->FileName.Buffer);
        FsdFreePool(Fcb);
        return NULL;
    }

    FsdWcharToChar(
        Fcb->AnsiFileName.Buffer,
        Fcb->FileName.Buffer,
        Fcb->FileName.Length / sizeof(WCHAR)
        );

    Fcb->AnsiFileName.Buffer[Fcb->FileName.Length / sizeof(WCHAR)] = 0;

    Fcb->FileAttributes = FILE_ATTRIBUTE_NORMAL;

    if (FlagOn(ffss_inode->Flags, FFSS_FILE_DIRECTORY))
    {
        SetFlag(Fcb->FileAttributes, FILE_ATTRIBUTE_DIRECTORY);
    }

#ifndef FSD_RO
    if (FlagOn(Vcb->Flags, VCB_READ_ONLY))
#endif
    {
        //SetFlag(Fcb->FileAttributes, FILE_ATTRIBUTE_READONLY);
    }

    //Fcb->IndexNumber.QuadPart = be32_to_cpu(IndexNumber) & ROMFH_MASK;

    Fcb->Flags = 0;

    Fcb->ffss_inode = FsdAssignInode(ffss_inode,true);

    RtlZeroMemory(&Fcb->CommonFCBHeader, sizeof(FSRTL_COMMON_FCB_HEADER));

    Fcb->CommonFCBHeader.NodeTypeCode = (USHORT) FCB;
    Fcb->CommonFCBHeader.NodeByteSize = sizeof(FSD_FCB);
    Fcb->CommonFCBHeader.IsFastIoPossible = FastIoIsNotPossible;
    Fcb->CommonFCBHeader.Resource = &(Fcb->MainResource);
    Fcb->CommonFCBHeader.PagingIoResource = &(Fcb->PagingIoResource);
    Fcb->CommonFCBHeader.AllocationSize.QuadPart = ffss_inode->Size;
    Fcb->CommonFCBHeader.FileSize.QuadPart = ffss_inode->Size;
    Fcb->CommonFCBHeader.ValidDataLength.QuadPart = ffss_inode->Size;

    Fcb->SectionObject.DataSectionObject = NULL;
    Fcb->SectionObject.SharedCacheMap = NULL;
    Fcb->SectionObject.ImageSectionObject = NULL;

    ExInitializeResourceLite(&(Fcb->MainResource));
    ExInitializeResourceLite(&(Fcb->PagingIoResource));

    InsertTailList(&Vcb->FcbList, &Fcb->Next);

    return Fcb;
}

struct ffss_inode *FsdAllocInode(IN const char Name[],IN unsigned long int Type)
{
  struct ffss_inode *ffss_inode;

  ffss_inode = (struct ffss_inode *) FsdAllocatePool(NonPagedPoolCacheAligned, sizeof(struct ffss_inode), 'puSR');

  ffss_inode->Type = Type;
  ffss_inode->Parent = NULL;
  ffss_inode->IP = NULL;
  ffss_inode->Inodes = NULL;
  ffss_inode->NbInodes = 0;
  ffss_inode->RefCount = 0;
  ffss_inode->NameLength = strlen(Name)+1;
  ffss_inode->Name = FsdAllocatePool(NonPagedPool,ffss_inode->NameLength,"fiNP");
  RtlCopyMemory(ffss_inode->Name,Name,ffss_inode->NameLength);
  KdPrint(("FsdAllocInode : Allocating inode (Type %d) with name : %s\n",Type,ffss_inode->Name));

  return ffss_inode;
}

/* SuperBlock must be locked (or Lock must be TRUE) */
struct ffss_inode *FsdAssignInode(IN struct ffss_inode*  ffss_inode,IN SU_BOOL Lock)
{
#if DBG
  if(Lock != true && Lock != false)
    KdPrint(("AIE AIE AIE !!! bool != true or false in FsdAssignInode (%d %d %d) !!!\n",Lock,true,false));
#endif
  if(Lock)
  {
    LOCK_SUPERBLOCK_RESOURCE;
    ffss_inode->RefCount++;
    UNLOCK_SUPERBLOCK_RESOURCE;
  }
  else
    ffss_inode->RefCount++;
  return ffss_inode;
}

/* SuperBlock must be locked (or Lock must be TRUE) */
VOID FsdFreeInode(IN struct ffss_inode*  ffss_inode,IN SU_BOOL Lock)
{
  int i;

  if(ffss_inode == NULL)
    return;
#if DBG
  if(Lock != true && Lock != false)
    KdPrint(("AIE AIE AIE !!! bool != true or false in FsdFreeInode (%d %d %d) !!!\n",Lock,true,false));
#endif
  if(Lock)
    LOCK_SUPERBLOCK_RESOURCE;
  ffss_inode->RefCount--;
  if(ffss_inode->RefCount != 0)
  {
    if(Lock)
      UNLOCK_SUPERBLOCK_RESOURCE;
    return;
  }

  KdPrint(("FsdFreeInode : Freeing inode (%d) %s\n",ffss_inode->Type,ffss_inode->Name));
  if(ffss_inode->NbInodes != 0)
  {
    for(i=0;i<ffss_inode->NbInodes;i++)
    {
      FsdFreeInode(ffss_inode->Inodes[i],false);
    }
    FsdFreePool(ffss_inode->Inodes);
  }
  if(ffss_inode->Name != NULL)
    FsdFreePool(ffss_inode->Name);
  if(ffss_inode->Parent != NULL)
    FsdFreeInode(ffss_inode->Parent,false);
  if(ffss_inode->IP != NULL)
    free(ffss_inode->IP); /* Allocated by ffss library using 'malloc'... free it with 'free' */

  FsdFreePool(ffss_inode);
  if(Lock)
    UNLOCK_SUPERBLOCK_RESOURCE;
}

/* SuperBlock must be locked (or Lock must be TRUE) */
VOID FsdFreeSubInodes(IN struct ffss_inode*  ffss_inode,IN SU_BOOL Lock)
{
  int i;

  if(ffss_inode == NULL)
    return;
#if DBG
  if(Lock != true && Lock != false)
    KdPrint(("AIE AIE AIE !!! bool != true or false in FsdFreeSubInodes (%d %d %d) !!!\n",Lock,true,false));
#endif
  if(Lock)
    LOCK_SUPERBLOCK_RESOURCE;

  if(ffss_inode->NbInodes != 0)
  {
    for(i=0;i<ffss_inode->NbInodes;i++)
    {
      FsdFreeInode(ffss_inode->Inodes[i],false);
    }
    FsdFreePool(ffss_inode->Inodes);
    ffss_inode->Inodes = NULL;
    ffss_inode->NbInodes = 0;
  }

  if(Lock)
    UNLOCK_SUPERBLOCK_RESOURCE;
}

struct ffss_super_block *FsdAllocSuperBlock(void)
{
  struct ffss_super_block *super_block;

  super_block = (struct ffss_super_block *) FsdAllocatePool(NonPagedPoolCacheAligned, sizeof(struct ffss_super_block), 'puSR');

  super_block->Root = FsdAssignInode(FsdAllocInode("",FFSS_INODE_ROOT),false);
  super_block->Root->Flags = FFSS_FILE_DIRECTORY;
  ExInitializeResourceLite(&(super_block->Resource));

  return super_block;
}

VOID FsdFreeSuperBlock(IN struct ffss_super_block *ffss_super_block)
{
  int i;

  if(ffss_super_block == NULL)
    return;

  FsdFreeInode(ffss_super_block->Root,true);
  ffss_super_block->Root = NULL;
  ExDeleteResourceLite(&ffss_super_block->Resource);
  FsdFreePool(ffss_super_block);
}

VOID
FsdFreeFcb (
    IN PFSD_FCB Fcb
    )
{
  KdPrint(("FsdFreeFcb : %s\n",Fcb->FileName.Buffer));
    ASSERT(Fcb != NULL);

    ASSERT((Fcb->Identifier.Type == FCB) &&
           (Fcb->Identifier.Size == sizeof(FSD_FCB)));

    FsRtlUninitializeFileLock(&Fcb->FileLock);

    ExDeleteResourceLite(&Fcb->MainResource);

    ExDeleteResourceLite(&Fcb->PagingIoResource);

    RemoveEntryList(&Fcb->Next);

    FsdFreePool(Fcb->FileName.Buffer);

    FsdFreePool(Fcb->AnsiFileName.Buffer);

	FsdFreeInode(Fcb->ffss_inode,true);

    FsdFreePool(Fcb);
}

PFSD_CCB
FsdAllocateCcb (
    VOID
    )
{
    PFSD_CCB Ccb;

    Ccb = FsdAllocatePool(NonPagedPool, sizeof(FSD_CCB), 'bcCR');

    if (!Ccb)
    {
        return NULL;
    }

    Ccb->Identifier.Type = CCB;
    Ccb->Identifier.Size = sizeof(FSD_CCB);

    Ccb->CurrentByteOffset = 0;

    Ccb->DirectorySearchPattern.Length = 0;
    Ccb->DirectorySearchPattern.MaximumLength = 0;
    Ccb->DirectorySearchPattern.Buffer = 0;

    return Ccb;
}

VOID
FsdFreeCcb (
    IN PFSD_CCB Ccb
    )
{
    ASSERT(Ccb != NULL);

    ASSERT((Ccb->Identifier.Type == CCB) &&
           (Ccb->Identifier.Size == sizeof(FSD_CCB)));

    if (Ccb->DirectorySearchPattern.Buffer != NULL)
    {
        FsdFreePool(Ccb->DirectorySearchPattern.Buffer);
    }

    FsdFreePool(Ccb);
}

VOID
FsdFreeVcb (
    IN PFSD_VCB Vcb
    )
{
    ASSERT(Vcb != NULL);

    ASSERT((Vcb->Identifier.Type == VCB) &&
           (Vcb->Identifier.Size == sizeof(FSD_VCB)));

    FsdClearVpbFlag(Vcb->Vpb, VPB_MOUNTED);

    FsRtlNotifyUninitializeSync(&Vcb->NotifySync);

    ExAcquireResourceExclusiveLite(
        &FsdGlobalData.Resource,
        TRUE
        );

    RemoveEntryList(&Vcb->Next);

    ExReleaseResourceForThreadLite(
        &FsdGlobalData.Resource,
        ExGetCurrentResourceThread()
        );

    ExDeleteResourceLite(&Vcb->MainResource);

    ExDeleteResourceLite(&Vcb->PagingIoResource);

    IoDeleteDevice(Vcb->DeviceObject);

    KdPrint((DRIVER_NAME ": Vcb deallocated\n"));
}

#pragma code_seg() // end FSD_PAGED_CODE
