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
/*#include "ntifs.h"
#include "fsd.h"
#include "ffss_fs.h"*/
#include <ffss_tdi.h>

/* ***************************** TO DO ***************************** *
   - Tester un FileOpen sur un path complet (existant) sans avoir scanner avec le domain/server/share -> Devrait pas trouver le fichier (le domain ... server ... et/ou share)
 * ***************************************************************** */

/* ********************************************************************** */
/* **************************** GLOBALS ********************************* */
/* ********************************************************************** */
FfssUDP *pUDP;


/* ********************************************************************** */
/* ************************** UDP CALLBACKS ***************************** */
/* ********************************************************************** */
void OnDomainListingAnswer(const char **Domains,int NbDomains)
{
  int i;
  struct ffss_inode *Inode;

  LOCK_SUPERBLOCK_RESOURCE;
  if(FFSS_SuperBlock->Root->NbInodes != 0)
  {
    for(i=0;i<FFSS_SuperBlock->Root->NbInodes;i++)
    {
      FsdFreeFFSSInode(FFSS_SuperBlock->Root->Inodes[i],false);
    }
    FsdFreePool(FFSS_SuperBlock->Root->Inodes);
    FFSS_SuperBlock->Root->Inodes = NULL;
    FFSS_SuperBlock->Root->NbInodes = 0;
  }

  if(NbDomains != 0)
  {
    FFSS_SuperBlock->Root->Inodes = (struct ffss_inode **) FsdAllocatePool(NonPagedPoolCacheAligned, sizeof(struct ffss_inode *)*NbDomains, 'nuSD');
    for(i=0;i<NbDomains;i++)
    {
      Inode = FsdAllocInode(Domains[i],FFSS_INODE_DOMAIN);
      Inode->Flags = FFSS_FILE_DIRECTORY;
      Inode->Parent = FsdAssignFFSSInode(FFSS_SuperBlock->Root,false);
      FFSS_SuperBlock->Root->Inodes[i] = FsdAssignFFSSInode(Inode,false);
    }
    FFSS_SuperBlock->Root->NbInodes = NbDomains;
  }
  UNLOCK_SUPERBLOCK_RESOURCE;
}

  /* WARNING !! (char *) of the FM_PHost structure are pointers to STATIC buffer, and must be dupped ! */
  /* Except for the FM_PHost->IP that is dupped internaly, and if you don't use it, you MUST free it !! */
void OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList) /* SU_PList of FM_PHost */
{
  FM_PHost Host;
  SU_PList Ptr;
  struct ffss_inode *Inode,*domain;
  int i,count = 0;

  LOCK_SUPERBLOCK_RESOURCE;
  domain = FsdGetInodeFromDomain((char *)Domain);
  if(domain == NULL)
  {
    /* Free IPs */
    KdPrint(("OnServerListingAnswer : WARNING : Domain %s does not exists !!!!!\n",Domain));
    /* TO DO -> Creer le domain, et l'ajouter a la liste des domaines du root */
    UNLOCK_SUPERBLOCK_RESOURCE;
    return;
  }
  FsdFreeSubInodes(domain,false);
  if(NbHost != 0)
  {
    domain->Inodes = (struct ffss_inode **) FsdAllocatePool(NonPagedPoolCacheAligned, sizeof(struct ffss_inode *)*NbHost, 'nuSH');
    Ptr = HostList;
    for(i=0;i<NbHost;i++)
    {
      Host = (FM_PHost) Ptr->Data;
      KdPrint(("OnServerListingAnswer : Got server %s (State = %d)\n",Host->Name,Host->State));
      if(Host->State != FFSS_STATE_OFF)
      {
        Inode = FsdAllocInode(Host->Name,FFSS_INODE_SERVER);
        Inode->Flags = FFSS_FILE_DIRECTORY;
        //--- Host->IP
        Inode->Parent = FsdAssignFFSSInode(domain,false);
        domain->Inodes[i] = FsdAssignFFSSInode(Inode,false);
        count++;
      }
      Ptr = Ptr->Next;
    }
    domain->NbInodes = count;
  }
  UNLOCK_SUPERBLOCK_RESOURCE;
}

/* ********************************************************************** */
/* ************************** TCP CALLBACKS ***************************** */
/* ********************************************************************** */


/* ********************************************************************** */
/* ************************** FFSS TIMER ******************************** */
/* ********************************************************************** */
VOID TimerHandler(IN struct _KDPC *Dpc,IN PVOID DeferredContext,IN PVOID SystemArgument1,IN PVOID SystemArgument2)
{ 
  ((FfssTimer *)DeferredContext)->pSem->Signal();
}

void FfssTimer::SetTimeout(LONGLONG msec)
{
  usec=msec*(-10000);
}

bool FfssTimer::Set(void)
{
  return KTimedCallback::Set((LONGLONG)usec,TimerHandler,this);
}


/* ********************************************************************** */
/* *************************** FFSS SEM ********************************* */
/* ********************************************************************** */
void FfssSem::SignalTimer(void)
{
  //KdPrint(("SEM : SIGNAL TIMER\n"));
  pTimer->Cancel();
  if(Locked)
    KSemaphore::Signal();
  Locked = false;
}

void FfssSem::WaitTimer(void)
{
  //KdPrint(("SEM : WAIT TIMER\n"));
  pTimer->Set();
  KSemaphore::Wait();
  Locked = true;
}

void FfssSem::Signal(void)
{
  //KdPrint(("SEM : SIGNAL...\n"));
  if(Locked)
    KSemaphore::Signal();
  Locked = false;
  //KdPrint(("SEM : SIGNALED\n"));
}

void FfssSem::Wait(void)
{
  //KdPrint(("SEM : WAIT...\n"));
  KSemaphore::Wait();
  Locked = true;
  //KdPrint(("SEM : WAITED\n"));
}

void FfssSem::SetTimeout(LONGLONG msec)
{
  pTimer->SetTimeout(msec);
}


/* ********************************************************************** */
/* *************************** FFSS UDP ********************************* */
/* ********************************************************************** */
FfssUDP::FfssUDP() : KDatagramSocket()
{
  BufSize = FFSS_UDP_CLIENT_BUFFER_SIZE;
  context;
  Buf = (char *) malloc(BufSize);
  len = 0;
  Sem = new FfssSem;
  if(Sem == NULL)
    ASSERT(0);
  Timer = new (NonPagedPool)FfssTimer(Sem,FFSS_TIMEOUT_UDP_MESSAGE*1000);
  if(Timer == NULL)
    ASSERT(0);
  Sem->SetTimer(Timer);
  /* UDP callbacks */
  FFSS_CB.CCB.OnDomainListingAnswer = OnDomainListingAnswer;
  FFSS_CB.CCB.OnServerListingAnswer = OnServerListingAnswer;
  /*FFSS_CB.CCB.OnSharesListing = OnSharesListing;*/
  /* TCP callbacks */
  /*FFSS_CB.CCB.OnError = OnError;
  FFSS_CB.CCB.OnDirectoryListingAnswer = OnDirectoryListingAnswer;*/
  /* Streaming callbacks */
  /*FFSS_CB.CCB.OnStrmOpenAnswer = OnStrmOpenAnswer;
  FFSS_CB.CCB.OnStrmReadAnswer = OnStrmReadAnswer;
  FFSS_CB.CCB.OnStrmWriteAnswer = OnStrmWriteAnswer;*/
};

FfssUDP::~FfssUDP()
{
  free(Buf);
}

TDI_STATUS FfssUDP::sendto(PTDI_CONNECTION_INFORMATION pConn,void* pBuf,uint Size,PVOID pCxt)
{
  TDI_STATUS ret;
  Sem->WaitTimer();
  ret = KDatagramSocket::sendto(pConn,pBuf,Size,pCxt);
  Sem->Wait();
  Sem->Signal();
  return ret;
}

void FfssUDP::On_sendtoComplete(PVOID pCxt,TDI_STATUS Status,uint ByteCount)
{
  if(pCxt != NULL)
  {
    free(pCxt);
  }
}


uint FfssUDP::OnReceive(uint AddressLength,PTRANSPORT_ADDRESS pTA,uint OptionsLength,PVOID Options,uint Indicated,uchar* Data,uint Available,uchar** RcvBuffer,uint* RcvBufferLen)
{ 
  GetDatagram(Data,Indicated,pTA);
  KdPrint(("end of getdatagram\n"));

  if (Indicated < Available)
  {
    KdPrint(("UDP OnReceive : Available > Indicated (%d > %d)\n",Available,Indicated));
    *RcvBuffer = new uchar [*RcvBufferLen = Available];
  }
  return Indicated;
}

void FfssUDP::OnReceiveComplete(TDI_STATUS status,uint AddressLength,PTRANSPORT_ADDRESS pTA,uint OptionsLength,PVOID Options,uint Indicated,uchar* Data)
{
  if(status == TDI_SUCCESS)
  {
    KdPrint(("UDP OnReceiveComplete : Remaining %d bytes\n",Indicated));
    GetDatagram(Data,Indicated,pTA);
  }
  else
  {
    KdPrint(("UDP OnReceiveComplete : Failed completing receive, discarding packet : err %X\n", status));
    len = 0; /* Discard packet */
  }
  if(status != TDI_PENDING)
    delete Data;
}

void FfssUDP::GetDatagram(uchar *Data,uint Indicated,PTRANSPORT_ADDRESS pTA)
{
  FFSS_Field Size;
  bool analyse;
  char szIPaddr[150];

  if(len >= BufSize)
  {
    KdPrint(("WARNING : UDP buffer too short for this message !! Ignoring message...\n"));
    len = 0;
  }
  if(Indicated >= (BufSize - len))
  {
    KdPrint(("WARNING : UDP buffer too short for this message !! Ignoring message...\n"));
    len = 0;
    return;
  }
  memcpy(Buf+len,Data,Indicated);
  inet_ntoa(PTDI_ADDRESS_IP(pTA->Address[0].Address)->in_addr, szIPaddr, sizeof(szIPaddr));
  KdPrint(("Data found on UDP port from %s (%d bytes)... analysing\n",szIPaddr,Indicated));
  /* If size of message won't fit in the buffer */
  if((len == 0) && ((*(FFSS_Field *)Buf) > BufSize))
  {
    KdPrint(("Length of the message won't fit in the UDP buffer !!\n"));
	return;
  }
  len += Indicated;
  analyse = true;
  while(analyse)
  {
    if(len < 5)
    {
      KdPrint(("Length of the message is less than 5 (%d)... DoS attack ?\n",len));
      len = 0;
      break;
    }
    Size = *(FFSS_Field *)Buf;
    if(Size < 5)
    {
      KdPrint(("Size of the message is less than 5 (%d)... DoS attack ?\n",Size));
      len = 0;
      break;
    }
    if(Size > len)
    {
      KdPrint(("Warning, Size of the message is greater than received data (%d - %d)... Message splitted ?\n",Size,len));
      break;
      /* Keeps waiting for data */
    }
    else
    {
      FC_AnalyseUDP(pTA,Buf,Size);
      Sem->SignalTimer();
      if(len > Size)
      {
        KdPrint(("Warning, Size of the message is less than received data (%d - %d)... multiple messages ?\n",Size,len));
        memmove(Buf,Buf+Size,len-Size);
        len -= Size;
        /* Keeps analysing the buffer */
      }
      else
      {
        analyse = false;
        len = 0;
      }
    }
  }
}

/* ********************************************************************** */
/* *************************** FFSS TCP ********************************* */
/* ********************************************************************** */
FfssTCP::FfssTCP() : KStreamSocket()
{
  BufSize = FFSS_TCP_CLIENT_BUFFER_SIZE;
  Buf = (char *) malloc(BufSize);
  len = 0;
  /*Entries = NULL;
  Mark = false;
  Used = 0;*/
  Sem = new FfssSem;
  if(Sem == NULL)
    ASSERT(0);
  Timer = new (NonPagedPool)FfssTimer(Sem,FFSS_TIMEOUT_TCP_MESSAGE*1000);
  if(Timer == NULL)
    ASSERT(0);
  Sem->SetTimer(Timer);
}

FfssTCP::~FfssTCP()
{
  Sem->SignalTimer();
  delete Sem;
  delete Timer;
  free(Buf);
}


TDI_STATUS FfssTCP::connect(PTDI_CONNECTION_INFORMATION RequestAddr)
{
  TDI_STATUS ret;
  Sem->WaitTimer();
  ret = KStreamSocket::connect(RequestAddr,FFSS_TIMEOUT_TCP_MESSAGE*1000);
  Sem->Wait();
  Sem->Signal();
  return ret;

}

void FfssTCP::On_connectComplete(PVOID pCxt,TDI_STATUS Status,uint ByteCount)
{
  if(Status != TDI_SUCCESS)
    KdPrint(("OnConnectComplete : Cannot connect : %d\n",Status));
  Sem->SignalTimer();
}

TDI_STATUS FfssTCP::send(void* pBuf,uint Size,bool FreeMsg,bool sem)
{
  TDI_STATUS ret;

  if(!IsConnected())
  {
    KdPrint(("FfssTCP::send : Not connected !\n"));
    free(pBuf);
    return TDI_NO_RESOURCES;
  }
  if(sem)
    Sem->WaitTimer();
  ret = KStreamSocket::send(pBuf,Size,FreeMsg?pBuf:NULL,0);
  if((ret != TDI_PENDING) && (ret != TDI_SUCCESS))
  {
    /* Connection has been lost */
    KdPrint(("FfssTCP::send : Cannot send... connection lost ?\n"));
    free(pBuf);
  }
  if(sem)
  {
    Sem->Wait();
    Sem->Signal();
  }
  return ret;
}


void FfssTCP::On_sendComplete(PVOID pCxt,TDI_STATUS Status,uint ByteCount)
{
  if(pCxt != NULL)
  {
    free(pCxt);
  }
}

uint FfssTCP::OnReceive(uint Indicated,uchar* Data,uint Available,uchar** RcvBuffer,uint* RcvBufferLen)
{ 
  /*if(!GetPacket(this,Data,Indicated))
    disconnect();*/

  if (Indicated < Available)
  {
    KdPrint(("TCP OnReceive : Available > Indicated (%d > %d)\n",Available,Indicated));
    *RcvBuffer = new uchar [*RcvBufferLen = (Available*10)];
  }
  return Indicated;
}

void FfssTCP::OnReceiveComplete(TDI_STATUS status,uint Indicated,uchar* Data)
{
  KdPrint(("TCP OnReceiveComplete : Remaining %d bytes\n",Indicated));
  if(status == TDI_SUCCESS)
  {
    /*if(!GetPacket(this,Data,Indicated))
      disconnect();*/
  }
  else
  {
    KdPrint(("TCP OnReceiveComplete : Failed completing receive, discarding packet : err %X\n", status));
    len = 0; /* Discard packet */
  }
  if(status != TDI_PENDING)
    delete Data;
}

void FfssTCP::On_disconnectComplete(PVOID pCxt,TDI_STATUS Status,uint ByteCount)
{
  KdPrint(("FfssTCP::OnDisconnectComplete : Completed\n"));
  if(pCxt != NULL)
  {
    free(pCxt);
  }
}

void FfssTCP::OnDisconnect(uint OptionsLength,PVOID Options,BOOLEAN bAbort)
{
  KdPrint(("FfssTCP::OnDisconnect : Remote peer disconnected...\n"));
}



/* ********************************************************************** */
/* ***************************** UTILS ********************************** */
/* ********************************************************************** */



/* ********************************************************************** */
/* *************************** C/C++ API ******************************** */
/* ********************************************************************** */

extern "C" {
NTSTATUS TDI_Init()
{
  // Initialize the TDIClient framework first

  if (!KTDInterface::Initialize())
  {
    return STATUS_NOT_FOUND; // something wrong with TDI
  }

  /* Create an UDP socket */
  pUDP = new FfssUDP;
  if(pUDP && pUDP->IsCreated())
  {
    pUDP->SetEvents(true);
    KdPrint(("FFSS : UDP Socket created at port %d\n",pUDP->QueryPort()));
  }
  else
  {
    KdPrint(("FFSS : Socket creation failed\n"));
    return STATUS_NOT_FOUND;
  }
  return STATUS_SUCCESS;
}
};
