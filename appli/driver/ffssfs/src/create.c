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
#include <ffss.h>

#pragma code_seg(FSD_PAGED_CODE)

NTSTATUS
FsdCreate (
    IN PFSD_IRP_CONTEXT IrpContext
    )
{
    PDEVICE_OBJECT      DeviceObject;
    PIRP                Irp;
    PIO_STACK_LOCATION  IrpSp;

    DeviceObject = IrpContext->DeviceObject;

    Irp = IrpContext->Irp;

    IrpSp = IoGetCurrentIrpStackLocation(Irp);

    if (DeviceObject == FsdGlobalData.DeviceObject)
    {
        return FsdCreateFs(IrpContext);
    }
    else if (IrpSp->FileObject->FileName.Length == 0)
    {
        return FsdCreateVolume(IrpContext);
    }
    else
    {
        return FsdCreateFile(IrpContext);
    }
}

NTSTATUS
FsdCreateFs (
    IN PFSD_IRP_CONTEXT IrpContext
    )
{
    IrpContext->Irp->IoStatus.Status = STATUS_SUCCESS;

    IrpContext->Irp->IoStatus.Information = FILE_OPENED;
    
    FsdCompleteRequest(IrpContext->Irp, IO_NO_INCREMENT);
    
    FsdFreeIrpContext(IrpContext);
    
    return STATUS_SUCCESS;
}

NTSTATUS
FsdCreateVolume (
    IN PFSD_IRP_CONTEXT IrpContext
    )
{
    PDEVICE_OBJECT      DeviceObject;
    PFILE_OBJECT        FileObject;
    PFSD_VCB            Vcb;

    DeviceObject = IrpContext->DeviceObject;

    Vcb = (PFSD_VCB) DeviceObject->DeviceExtension;

    ASSERT(Vcb != NULL);

    ASSERT((Vcb->Identifier.Type == VCB) &&
           (Vcb->Identifier.Size == sizeof(FSD_VCB)));

    FileObject = IrpContext->FileObject;

    FileObject->FsContext = Vcb;

    ExAcquireResourceExclusiveLite(
        &Vcb->MainResource,
        TRUE
        );

    Vcb->ReferenceCount++;

    ExReleaseResourceForThreadLite(
        &Vcb->MainResource,
        ExGetCurrentResourceThread()
        );

    IrpContext->Irp->IoStatus.Status = STATUS_SUCCESS;

    IrpContext->Irp->IoStatus.Information = FILE_OPENED;
    
    FsdCompleteRequest(IrpContext->Irp, IO_NO_INCREMENT);
    
    FsdFreeIrpContext(IrpContext);
    
    return STATUS_SUCCESS;
}

NTSTATUS
FsdCreateFile (
    IN PFSD_IRP_CONTEXT IrpContext
    )
{
    PDEVICE_OBJECT      DeviceObject;
    PIRP                Irp;
    PIO_STACK_LOCATION  IrpSp;
    NTSTATUS            Status = STATUS_UNSUCCESSFUL;
    PFSD_VCB            Vcb = NULL;
    PFSD_FCB            Fcb;
    PFSD_CCB            Ccb;
    struct ffss_inode*  Inode = NULL;
    BOOLEAN             VcbResourceAcquired = FALSE;
    
    DeviceObject = IrpContext->DeviceObject;
    
    Vcb = (PFSD_VCB) DeviceObject->DeviceExtension;
    
    Irp = IrpContext->Irp;
    
    IrpSp = IoGetCurrentIrpStackLocation(Irp);
    
    __try
    {
        ExAcquireResourceExclusiveLite(
            &Vcb->MainResource,
            TRUE
            );
        
        VcbResourceAcquired = TRUE;
        
        Fcb = FsdLookupFcbByFileName(Vcb,&IrpSp->FileObject->FileName);
        
        if (!Fcb)
        {
            Inode = FsdGetInodeFromPath(&IrpSp->FileObject->FileName,&Status);
            
            if (!NT_SUCCESS(Status))
            {
                KdPrint((DRIVER_NAME ": STATUS_OBJECT_NAME_NOT_FOUND: \"%S\"\n",IrpSp->FileObject->FileName.Buffer));

                FsdFreeFFSSInode(Inode,true);
                Inode = NULL;
                //Status = STATUS_OBJECT_NAME_NOT_FOUND; /* Already set by FsdGetInodeFromPath */
                __leave;
            }

            Fcb = FsdAllocateFcb (
                ((PFSD_VCB)DeviceObject->DeviceExtension),
                &IrpSp->FileObject->FileName,
                Inode
                );
            
            FsdFreeFFSSInode(Inode,true); /* Release Inode returned by FsdGetInodeFromPath */
            Inode = NULL;
            if (Fcb == NULL)
            {
                Status = STATUS_INSUFFICIENT_RESOURCES;
                __leave;
            }
            
            KdPrint((
                DRIVER_NAME ": Allocated a new FCB for \"%s\" - ",
                Fcb->AnsiFileName.Buffer
                ));
        }
        
        Ccb = FsdAllocateCcb();

        if (Ccb == NULL)
        {
            Status = STATUS_INSUFFICIENT_RESOURCES;
            __leave;
        }

        Fcb->OpenHandleCount++;
        Vcb->OpenFileHandleCount++;
        Fcb->ReferenceCount++;
        Vcb->ReferenceCount++;
            
        /*if (!FlagOn(be32_to_cpu(Fcb->ffssfs_inode->next), ROMFH_DIR))
        {
            Fcb->CommonFCBHeader.IsFastIoPossible = FastIoIsPossible;
        }*/ /* TO DO */
            
        IrpSp->FileObject->FsContext = (void*) Fcb;
        IrpSp->FileObject->FsContext2 = (void*) Ccb;
        IrpSp->FileObject->PrivateCacheMap = NULL;
        IrpSp->FileObject->SectionObjectPointer = &(Fcb->SectionObject);
        IrpSp->FileObject->Vpb = Vcb->Vpb;
            
        Irp->IoStatus.Information = FILE_OPENED;
        Status = STATUS_SUCCESS;
            
        KdPrint((
            "OpenHandleCount: %u ReferenceCount: %u\n",
            Fcb->OpenHandleCount,
            Fcb->ReferenceCount
            ));
    }
    __finally
    {
        if (Inode)
        {
            FsdFreeFFSSInode(Inode,true);
            Inode = NULL;
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
            IrpContext->Irp->IoStatus.Status = Status;
            
            FsdCompleteRequest(
                IrpContext->Irp,
                (CCHAR)
                (NT_SUCCESS(Status) ? IO_DISK_INCREMENT : IO_NO_INCREMENT)
                );
            
            FsdFreeIrpContext(IrpContext);
            
            if (Vcb &&
                FlagOn(Vcb->Flags, VCB_DISMOUNT_PENDING) &&
                !Vcb->ReferenceCount
                )
            {
                FsdFreeVcb(Vcb);
            }
        }
    }
    
    return Status;
}

PFSD_FCB
FsdLookupFcbByFileName (
    IN PFSD_VCB         Vcb,
    IN PUNICODE_STRING  FullFileName
    )
{
    PLIST_ENTRY ListEntry;
    PFSD_FCB    Fcb;
    NTSTATUS    Status = STATUS_UNSUCCESSFUL;
    struct ffss_inode*  Inode = NULL,*Inode2;

    ListEntry = Vcb->FcbList.Flink;

    while (ListEntry != &Vcb->FcbList)
    {
        Fcb = CONTAINING_RECORD(ListEntry, FSD_FCB, Next);

        if (!RtlCompareUnicodeString(
            &Fcb->FileName,
            FullFileName,
            FALSE
            ))
        {
            KdPrint((DRIVER_NAME ": Found an allocated FCB for \"%s\"\n",Fcb->AnsiFileName.Buffer));
            
            Inode = FsdGetInodeFromPath(FullFileName,&Status);
            if(NT_SUCCESS(Status))
            {
              if(Inode != Fcb->ffss_inode)
              {
                Inode2 = Fcb->ffss_inode;
                Fcb->ffss_inode = Inode;
                FsdFreeFFSSInode(Inode2,true);
                Inode = Inode2;
                KdPrint(("FsdLookupFcbByFileName : Inode changed... updating Fcb->ffss_inode !\n"));
              }
            }
            FsdFreeFFSSInode(Inode,true); /* Release Inode returned by FsdGetInodeFromPath (or old Fcb Inode) */
            return Fcb;
        }

        ListEntry = ListEntry->Flink;
    }

    return NULL;
}

void FsdRescanInode(IN struct ffss_inode* Inode)
{
  if(Inode == NULL)
    return;
  switch(Inode->Type)
  {
    case FFSS_INODE_ROOT :
      KdPrint(("FsdRescanInode : Rescaning ROOT inode\n"));
      FC_SendMessage_DomainListing(FFSS_MASTER_IP);
      break;
    case FFSS_INODE_DOMAIN :
      KdPrint(("FsdRescanInode : Rescaning DOMAIN %s inode\n",Inode->Name));
      FC_SendMessage_ServerList(FFSS_MASTER_IP,NULL,Inode->Name);
      break;
    case FFSS_INODE_SERVER :
      KdPrint(("FsdRescanInode : Rescaning SERVER %s inode\n",Inode->Name));
      break;
    case FFSS_INODE_DIRECTORY :
      KdPrint(("FsdRescanInode : Rescaning DIRECTORY %s inode\n",Inode->Name));
      break;
    case FFSS_INODE_FILE :
      KdPrint(("FsdRescanInode : Rescan in meaningless for FILE inodes !!\n"));
      break;
  }
}

/* Returned inode must be freed */
struct ffss_inode *FsdGetInodeByIndex(IN PFSD_FCB Fcb,IN ULONG FileIndex,OUT NTSTATUS *Status)
{
  *Status = STATUS_INVALID_PARAMETER;
#if DBG
  if(Fcb == NULL)
  {
    KdPrint(("FsdGetInodeByIndex : Fcb = NULL\n"));
    return NULL;
  }
  if(Fcb->ffss_inode == NULL)
  {
    KdPrint(("FsdGetInodeByIndex : ffss_inode = NULL\n"));
    return NULL;
  }
#endif
  if((Fcb == NULL) || (Fcb->ffss_inode == NULL))
    return NULL;
  if((Fcb->ffss_inode->NbInodes == 0) || (FileIndex >= Fcb->ffss_inode->NbInodes))
  {
    FsdRescanInode(Fcb->ffss_inode);
    if((Fcb->ffss_inode->NbInodes == 0) || (FileIndex >= Fcb->ffss_inode->NbInodes)) /* Still not ok */
      return NULL;
  }
  *Status = STATUS_SUCCESS;
  KdPrint(("FsdGetInodeByIndex : Found inode for FileIndex %d\n",FileIndex));
  return FsdAssignFFSSInode(Fcb->ffss_inode->Inodes[FileIndex],true);
}

/* Returned inode must be freed */
struct ffss_inode *FsdGetInodeFromDomain(IN char *domain)
{
  int i;
  struct ffss_inode *Inode;

  LOCK_SUPERBLOCK_RESOURCE;
  if(FFSS_SuperBlock->Root->Inodes == NULL)
  {
    UNLOCK_SUPERBLOCK_RESOURCE;
    KdPrint(("FsdGetInodeFromDomain : No domains... requesting domains list\n"));
    FC_SendMessage_DomainListing(FFSS_MASTER_IP);
    LOCK_SUPERBLOCK_RESOURCE;
  }
  KdPrint(("FsdGetInodeFromDomain : Looking for %s in domains list\n",domain));

  for(i=0;i<FFSS_SuperBlock->Root->NbInodes;i++)
  {
    if(FFSS_strcasecmp(domain,FFSS_SuperBlock->Root->Inodes[i]->Name))
    {
      KdPrint(("FsdGetInodeFromDomain : Inode found\n"));
      Inode = FsdAssignFFSSInode(FFSS_SuperBlock->Root->Inodes[i],false);
      UNLOCK_SUPERBLOCK_RESOURCE;
      return Inode;
    }
  }

  UNLOCK_SUPERBLOCK_RESOURCE;
  KdPrint(("FsdGetInodeFromDomain : Inode not found for domain %s\n",domain));
  return NULL;
}

/* Returned inode must be freed */
struct ffss_inode *FsdGetInodeFromServer(IN char *server,IN struct ffss_inode *Domain)
{
  int i;
  struct ffss_inode *Inode;

  LOCK_SUPERBLOCK_RESOURCE;
  if(Domain->Inodes == NULL)
  {
    UNLOCK_SUPERBLOCK_RESOURCE;
    KdPrint(("FsdGetInodeFromServer : No servers... requesting servers list for %s\n",Domain->Name));
    FC_SendMessage_ServerList(FFSS_MASTER_IP,NULL,Domain->Name);
    LOCK_SUPERBLOCK_RESOURCE;
  }
  KdPrint(("FsdGetInodeFromServer : Looking for %s in servers list for domain %s\n",server,Domain->Name));

  for(i=0;i<Domain->NbInodes;i++)
  {
    if(FFSS_strcasecmp(server,Domain->Inodes[i]->Name))
    {
      KdPrint(("FsdGetInodeFromServer : Inode found\n"));
      Inode = FsdAssignFFSSInode(Domain->Inodes[i],false);
      UNLOCK_SUPERBLOCK_RESOURCE;
      return Inode;
    }
  }

  UNLOCK_SUPERBLOCK_RESOURCE;
  KdPrint(("FsdGetInodeFromServer : Inode not found for server %s\n",server));
  return NULL;
}

/* Returned inode must be freed */
struct ffss_inode *FsdGetInodeFromShare(IN char *share,IN struct ffss_inode *Domain,IN struct ffss_inode *Server)
{
  KdPrint(("FsdGetInodeFromShare : Searching share %s in server %s in domain %s\n",share,Server->Name,Domain->Name));
  return NULL;
}

/* Returned inode must be freed */
struct ffss_inode *FsdGetInodeFromPath(IN PUNICODE_STRING FullFileName,OUT NTSTATUS *Status)
{
  char Buf[1024],*parse,*p;
  struct ffss_inode *Domain,*Server,*Share;

  *Status = STATUS_OBJECT_NAME_NOT_FOUND;

  if(FullFileName->Length == 0)
    return NULL;

  FsdWcharToChar(Buf,FullFileName->Buffer,FullFileName->Length);
  KdPrint(("FsdGetInodeFromPath : Parsing %s\n",Buf));

  if(Buf[0] != '\\') /* Invalid path */
    return NULL;

  if(Buf[1] == 0) /* Root of FileSystem */
  {
    *Status = STATUS_SUCCESS;
    return FsdAssignFFSSInode(FFSS_SuperBlock->Root,true);
  }

  /* Start of path parsing */
  parse = Buf+1;

  /* Parse domain */
  p = strchr(parse,'\\');
  if(p != NULL)
    p[0] = 0;
  /* Check Domain name */
  Domain = FsdGetInodeFromDomain(parse);
  if(Domain == NULL) /* Domain does not exists */
  {
    return NULL;
  }
  if((p == NULL) || (p[1] == 0)) /* End of parse */
  {
    KdPrint(("end of parse (domain)\n"));
    *Status = STATUS_SUCCESS;
    return Domain; /* Already assigned */
  }
  parse = p + 1; /* Update parse position */

  /* Parse Server */
  p = strchr(parse,'\\');
  if(p != NULL)
    p[0] = 0;
  /* Check Server name */
  Server = FsdGetInodeFromServer(parse,Domain);
  if(Server == NULL) /* Server does not exists */
  {
    FsdFreeFFSSInode(Domain,true);
    return NULL;
  }
  if((p == NULL) || (p[1] == 0)) /* End of parse */
  {
    KdPrint(("end of parse (server)\n"));
    *Status = STATUS_SUCCESS;
    FsdFreeFFSSInode(Domain,true);
    return Server; /* Already assigned */
  }
  parse = p + 1; /* Update parse position */

  /* Parse Share */
  p = strchr(parse,'\\');
  if(p != NULL)
    p[0] = 0;
  /* Check Share name */
  Share = FsdGetInodeFromShare(parse,Domain,Server);
  if(Share == NULL) /* Share does not exists */
  {
    FsdFreeFFSSInode(Domain,true);
    FsdFreeFFSSInode(Server,true);
    return NULL;
  }
  if((p == NULL) || (p[1] == 0)) /* End of parse */
  {
    KdPrint(("end of parse (share)\n"));
    *Status = STATUS_SUCCESS;
    FsdFreeFFSSInode(Domain,true);
    FsdFreeFFSSInode(Server,true);
    return Share; /* Already assigned */
  }
  parse = p + 1; /* Update parse position */

  // FfssTCP * FsdGetConnection();
  /* Parse directories */
  p = strchr(parse,'\\');
  //while(p != NULL)
  {
  }
  /* Check file name */

  FsdFreeFFSSInode(Domain,true);
  FsdFreeFFSSInode(Server,true);
  FsdFreeFFSSInode(Share,true);

  return NULL;
}


#pragma code_seg() // end FSD_PAGED_CODE