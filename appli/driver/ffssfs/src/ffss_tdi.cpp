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

#include <ffss_tdi.h>

/* ***************************** TO DO ***************************** *
   - Tester un FileOpen sur un path complet (existant) sans avoir scanner avec le domain/server/share -> Devrait pas trouver le fichier (le domain ... server ... et/ou share)
   - Tester un GROS malloc dans une routine en DIRQL
 * ***************************************************************** */

/* ********************************************************************** */
/* **************************** GLOBALS ********************************* */
/* ********************************************************************** */
FfssUDP *pUDP;

/* ********************************************************************** */
/* ************************** UDP CALLBACKS ***************************** */
/* ********************************************************************** */
void OnDomainListingAnswer(const char **Domains,int NbDomains,FFSS_LongField User)
{
  int i;
  struct ffss_inode *Inode,*Root;

  Root = (struct ffss_inode *) User;
  if(Root == NULL)
    return;

  FsdFreeSubInodes(Root,false);

  if(NbDomains != 0)
  {
    Root->Inodes = (struct ffss_inode **) FsdAllocatePool(NonPagedPool, sizeof(struct ffss_inode *)*NbDomains, 'nuSD');
    for(i=0;i<NbDomains;i++)
    {
      Inode = FsdAllocInode(Domains[i],FFSS_INODE_DOMAIN);
      Inode->Flags = FFSS_FILE_DIRECTORY;
      Inode->Parent = FsdAssignInode(Root,false);
      Root->Inodes[i] = FsdAssignInode(Inode,false);
    }
    Root->NbInodes = NbDomains;
  }
  FsdFreeInode(Root,false);
}

  /* WARNING !! (char *) of the FM_PHost structure are pointers to STATIC buffer, and must be dupped ! */
  /* Except for the FM_PHost->IP that is dupped internaly, and if you don't use it, you MUST free it !! */
void OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList,FFSS_LongField User) /* SU_PList of FM_PHost */
{
  FM_PHost Host;
  SU_PList Ptr;
  struct ffss_inode *Inode,*domain;
  int i,count = 0;

  domain = (struct ffss_inode *) User;
  if(domain == NULL)
  {
    KdPrint(("OnServerListingAnswer : User Pointer is NULL... retrieving domain inode from SuperBlock\n"));
    domain = FsdGetInodeFromDomain((char *)Domain);
  }
  if(domain == NULL)
  {
    /* Free IPs */
    KdPrint(("OnServerListingAnswer : WARNING : Domain %s does not exist !!!!!\n",Domain));
    /* TO DO -> Creer le domain, et l'ajouter a la liste des domaines du root */
    return;
  }
  FsdFreeSubInodes(domain,false);
  if(NbHost != 0)
  {
    domain->Inodes = (struct ffss_inode **) FsdAllocatePool(NonPagedPool, sizeof(struct ffss_inode *)*NbHost, 'nuSH');
    Ptr = HostList;
    for(i=0;i<NbHost;i++)
    {
      Host = (FM_PHost) Ptr->Data;
      KdPrint(("OnServerListingAnswer : Got server %s (State = %d)\n",Host->Name,Host->State));
#if !DBG
      if(Host->State != FFSS_STATE_OFF)
#endif
      {
        Inode = FsdAllocInode(Host->Name,FFSS_INODE_SERVER);
        Inode->Flags = FFSS_FILE_DIRECTORY;
        Inode->IP = Host->IP;
        Inode->Parent = FsdAssignInode(domain,false);
        domain->Inodes[i] = FsdAssignInode(Inode,false);
        count++;
      }
#if !DBG
      else
        free(Host->IP);
#endif
      Ptr = Ptr->Next;
    }
    domain->NbInodes = count;
  }
  FsdFreeInode(domain,false);
}

void OnSharesListing(const char IP[],const char **Names,const char **Comments,int NbShares,FFSS_LongField User)
{
  struct ffss_inode *Inode,*server;
  int i,len;

  server = (struct ffss_inode *) User;
  if(server == NULL)
  {
    KdPrint(("OnSharesListing : User Pointer is NULL... retrieving server inode from SuperBlock\n"));
    server = FsdGetInodeFromServerIP((char *)IP);
  }
  if(server == NULL)
  {
    KdPrint(("OnSharesListing : WARNING : Server %s does not exist !!!!!\n",IP));
    return;
  }
  FsdFreeSubInodes(server,false);
  if(NbShares != 0)
  {
    server->Inodes = (struct ffss_inode **) FsdAllocatePool(NonPagedPool, sizeof(struct ffss_inode *)*NbShares, 'nuSS');
    for(i=0;i<NbShares;i++)
    {
      KdPrint(("OnSharesListing : Got share %s\n",Names[i]));
      Inode = FsdAllocInode(Names[i],FFSS_INODE_SHARE);
      Inode->Flags = FFSS_FILE_DIRECTORY;
      len = strlen(IP) + 1;
      Inode->IP = (char *) malloc(len);
      RtlCopyMemory(Inode->IP,IP,len);
      Inode->Parent = FsdAssignInode(server,false);
      server->Inodes[i] = FsdAssignInode(Inode,false);
    }
    server->NbInodes = NbShares;
  }
  FsdFreeInode(server,false);
}

/* ********************************************************************** */
/* ************************** TCP CALLBACKS ***************************** */
/* ********************************************************************** */
SU_BOOL OnError(FfssTCP *Server,int Code,const char Descr[],FFSS_LongField Value,FFSS_LongField User)
{
  struct ffss_inode *Root = (struct ffss_inode *) User; /* Don't free this inode here ! */

  switch(Code)
  {
  case FFSS_ERROR_RESOURCE_NOT_AVAIL :
  case FFSS_ERROR_NEED_LOGIN_PASS :
  case FFSS_ERROR_TOO_MANY_CONNECTIONS :
  case FFSS_ERROR_IDLE_TIMEOUT :
  case FFSS_ERROR_SHARE_DISABLED :
  case FFSS_ERROR_SHARE_EJECTED :
  case FFSS_ERROR_INTERNAL_ERROR :
    FsdFreeInode(Root,false);
    KdPrint(("OnError : Found fatal error code (%s). Disconnecting\n",Descr));
    return false;
  case FFSS_ERROR_FILE_NOT_FOUND :
  case FFSS_ERROR_ACCESS_DENIED :
  case FFSS_ERROR_NOT_ENOUGH_SPACE :
  case FFSS_ERROR_CANNOT_CONNECT :
  case FFSS_ERROR_TOO_MANY_TRANSFERS :
  case FFSS_ERROR_DIRECTORY_NOT_EMPTY :
  case FFSS_ERROR_FILE_ALREADY_EXISTS :
  case FFSS_ERROR_SERVER_IS_QUIET :
  case FFSS_ERROR_BUFFER_OVERFLOW :
  case FFSS_ERROR_XFER_MODE_NOT_SUPPORTED :
  case FFSS_ERROR_RESEND_LAST_UDP :
  case FFSS_ERROR_BAD_SEARCH_REQUEST :
  case FFSS_ERROR_NOT_IMPLEMENTED :
  case FFSS_ERROR_NO_ERROR :
    FsdFreeInode(Root,false);
    KdPrint(("OnError : Found non fatal error code (%s). Continuing\n",Descr));
    break;
  }
  return true;
}

SU_BOOL OnDirectoryListingAnswer(FfssTCP *Server,const char Path[],int NbEntries,SU_PList Entries,FFSS_LongField User) /* FC_PEntry */
{
  struct ffss_inode *Inode,*parent;
  int i,len;
  SU_PList Ptr;
  FC_PEntry Ent;

  parent = (struct ffss_inode *) User;
  if(parent == NULL)
  {
    KdPrint(("OnDirectoryListingAnswer : User Pointer is NULL... disconnecting\n"));
    return false;
  }
  FsdFreeSubInodes(parent,false);

  if(NbEntries != 0)
  {
    parent->Inodes = (struct ffss_inode **) FsdAllocatePool(NonPagedPool, sizeof(struct ffss_inode *)*NbEntries, 'nuSS');
    Ptr = Entries;
    for(i=0;i<NbEntries;i++)
    {
      Ent = (FC_PEntry) Ptr->Data;
      KdPrint(("OnDirectoryListingAnswer : Got entry '%s' for '%s'\n",Ent->Name,Path));
      Inode = FsdAllocInode(Ent->Name,(Ent->Flags & FFSS_FILE_DIRECTORY)?FFSS_INODE_DIRECTORY:FFSS_INODE_FILE);
      Inode->Flags = Ent->Flags;
      Inode->Parent = FsdAssignInode(parent,false);
      Inode->Conn = Server;
      len = strlen(Path) + 1;
      Inode->Path = (char *) malloc(len);
      RtlCopyMemory(Inode->Path,Path,len);
      parent->Inodes[i] = FsdAssignInode(Inode,false);
      Ptr = Ptr->Next;
    }
    parent->NbInodes = NbEntries;
  }
  parent->Listed = 1;
  FsdFreeInode(parent,false);
  
  return true;
}


/* ********************************************************************** */
/* ************************** FFSS TIMER ******************************** */
/* ********************************************************************** */
VOID TimerHandler(IN struct _KDPC *Dpc,IN PVOID DeferredContext,IN PVOID SystemArgument1,IN PVOID SystemArgument2)
{ 
  ((FfssTimer *)DeferredContext)->pSem->Signal();
  ((FfssTimer *)DeferredContext)->pSem->TimedOut = true;
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
  TimedOut = false;
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
  FFSS_CB.CCB.OnSharesListing = OnSharesListing;
  /* TCP callbacks */
  FFSS_CB.CCB.OnError = OnError;
  FFSS_CB.CCB.OnDirectoryListingAnswer = OnDirectoryListingAnswer;
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
  Sem->Wait();   /* Force to bloc until answer arrives */
  Sem->Signal();
  return ret;
}

void FfssUDP::On_sendtoComplete(PVOID pCxt,TDI_STATUS Status,uint ByteCount)
{
  if((Status != TDI_PENDING) && (Status != TDI_SUCCESS))
  {
    KdPrint(("FfssUDP::On_sendtoComplete : Cannot send UDP packet... signaling sem\n"));
    Sem->SignalTimer();
  }
  if(pCxt != NULL)
  {
    free(pCxt);
  }
}

uint FfssUDP::OnReceive(uint AddressLength,PTRANSPORT_ADDRESS pTA,uint OptionsLength,PVOID Options,uint Indicated,uchar* Data,uint Available,uchar** RcvBuffer,uint* RcvBufferLen)
{ 
  if(!Sem->Locked) /* A time out might have occured... better ignore message */
    KdPrint(("AIE AIE AIE... FfssUDP::OnReceive : Sem is not locked !! Ignoring message (time out might have occured\n"));
  GetDatagram(Data,Indicated,pTA,Sem->Locked);
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
    if(!Sem->Locked) /* A time out might have occured... better ignore message */
      KdPrint(("AIE AIE AIE... FfssUDP::OnReceiveComplete : Sem is not locked !! Ignoring message (time out might have occured\n"));
    GetDatagram(Data,Indicated,pTA,Sem->Locked);
  }
  else
  {
    KdPrint(("UDP OnReceiveComplete : Failed completing receive, discarding packet : err %X\n", status));
    len = 0; /* Discard packet */
  }
  if(status != TDI_PENDING)
    delete Data;
}

void FfssUDP::GetDatagram(uchar *Data,uint Indicated,PTRANSPORT_ADDRESS pTA,bool ProcessMessage)
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
      if(ProcessMessage)
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
FfssTCP::FfssTCP(const char Server[],const char ShareName[]) : KStreamSocket()
{
  int len;

  len = strlen(ShareName) + 1;
  Share = (char *) malloc(len);
  RtlCopyMemory(Share,ShareName,len);

  len = strlen(Server) + 1;
  IP = (char *) malloc(len);
  RtlCopyMemory(IP,Server,len);

  BufSize = FFSS_TCP_CLIENT_BUFFER_SIZE;
  Buf = (char *) malloc(BufSize);
  len = 0;
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
  free(Share);
  free(IP);
  FsdFreeInode(Root,true);
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
    if(sem)
      Sem->SignalTimer();
    disconnect();
  }
  if(sem)
  {
    Sem->Wait();  /* Force to bloc until answer arrives */
    if(Sem->TimedOut)
    {
      KdPrint(("FfssTCP::send : Semd/Receive timed out !!! Disconnecting\n"));
      disconnect();
    }
    Sem->Signal();
  }
  return ret;
}


void FfssTCP::On_sendComplete(PVOID pCxt,TDI_STATUS Status,uint ByteCount)
{
  if((Status != TDI_PENDING) && (Status != TDI_SUCCESS))
    KdPrint(("FfssTCP::On_sendComplete : Cannot send... AIE AIE AIE.. please check if FfssTCP::send displayed same message !!\n"));
  if(pCxt != NULL)
  {
    free(pCxt);
  }
}

uint FfssTCP::OnReceive(uint Indicated,uchar* Data,uint Available,uchar** RcvBuffer,uint* RcvBufferLen)
{
  if(!Sem->Locked) /* A time out might have occured... we should disconnect now */
  {
    KdPrint(("AIE AIE AIE... FfssTCP::OnReceive (%d-%d) : Sem is not locked !! Disconnecting (time out might have occured)\n",Indicated,Available));
    disconnect();
    return Indicated;
  }
  if(!GetPacket(Data,Indicated))
    disconnect();

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
    if(!Sem->Locked) /* A time out might have occured... we should disconnect now */
    {
      KdPrint(("AIE AIE AIE... FfssTCP::OnReceiveComplete : Sem is not locked !! Disconnecting (time out might have occured\n"));
      disconnect();
    }
    if(!GetPacket(Data,Indicated))
      disconnect();
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


bool FfssTCP::GetPacket(uchar *Data,uint Indicated)
{
  int res;
  FFSS_Field Size;
  bool analyse;
  bool retval = true;

  if(len >= BufSize)
  {
    FFSS_PrintSyslog(LOG_INFO,"WARNING : Driver's buffer too short for this message (%d-%d) ... DoS attack ?\n",BufSize,len);
    if(FFSS_CB.CCB.OnError != NULL)
      FFSS_CB.CCB.OnError(this,FFSS_ERROR_ATTACK,FFSS_ErrorTable[FFSS_ERROR_ATTACK],0,0);
    return false;
  }
  if(Indicated >= (BufSize - len))
  {
    FFSS_PrintSyslog(LOG_INFO,"WARNING : Remaining space in driver's buffer too short for this packet (%d-%d) ... DoS attack ?\n",BufSize-len,Indicated);
    if(FFSS_CB.CCB.OnError != NULL)
      FFSS_CB.CCB.OnError(this,FFSS_ERROR_ATTACK,FFSS_ErrorTable[FFSS_ERROR_ATTACK],0,0);
    return false;
  }
  memcpy(Buf+len,Data,Indicated);
  FFSS_PrintDebug(6,"Data found on TCP port (%d bytes - %d) ... analysing\n",Indicated,len);
  len += Indicated;
  analyse = true;
  while (analyse)
  {
    if(len < 5)
    {
      FFSS_PrintSyslog(LOG_WARNING,"Length of the message is less than 5 (%d) ... DoS attack ?\n",len);
      if(FFSS_CB.CCB.OnError != NULL)
        FFSS_CB.CCB.OnError(this,FFSS_ERROR_ATTACK,FFSS_ErrorTable[FFSS_ERROR_ATTACK],0,0);
      return false;
    }
    Size = *(FFSS_Field *)Buf;
    if(Size > len)
    {
      FFSS_PrintDebug(5,"Warning, Size of the message is greater than received data (%d - %d)... Message splitted ?\n",Size,len);
      break;
      /* Keeps waiting for data */
    }
    else
    {
      retval = FC_AnalyseTCP(this,Buf,Size);
      Sem->SignalTimer();
      if(len > Size)
      {
        FFSS_PrintDebug(5,"Warning, Size of the message is less than received data (%d - %d)... multiple messages ?\n",Size,len);
        memmove(Buf,Buf+Size,len-Size);
        len -= Size;
        /* Keeps analysing the buffer */
      }
      else
      {
        analyse = false;
        len = 0;
      }
      if(!retval)
      {
        return false;
      }
    }
  }
  return true;
}


/* ********************************************************************** */
/* ***************************** UTILS ********************************** */
/* ********************************************************************** */


/* ********************************************************************** */
/* *************************** C/C++ API ******************************** */
/* ********************************************************************** */

extern "C" {
/* Returned inode must be freed */
struct ffss_inode *FsdGetConnection(IN struct ffss_inode *Share)
{
  SU_PList Ptr;
  struct ffss_inode *Inode;
  FfssTCP *Conn;

  LOCK_SUPERBLOCK_RESOURCE;
  Ptr = Share->Conns;
  while(Ptr != NULL)
  {
    Inode = (struct ffss_inode *) Ptr->Data;
    if(FFSS_strcasecmp(Share->Name,((FfssTCP *)Inode->Conn)->Share))
    {
      KdPrint(("FsdGetConnection : Active connection found\n"));
      Inode = FsdAssignInode(Inode,false);
#if DBG
      if(Share->Type != FFSS_INODE_SHARE)
        KdPrint(("AIE AIE AIE : FsdGetConnection : Inode %s is not of type SHARE !!!\n",Share->Name));
      if(Inode->Type != FFSS_INODE_DIRECTORY)
        KdPrint(("AIE AIE AIE : FsdGetConnection : Inode %s is not of type DIRECTORY !!!\n",Inode->Name));
#endif
      UNLOCK_SUPERBLOCK_RESOURCE;
      return Inode;
    }
    Ptr = Ptr->Next;
  }
  KdPrint(("FsdGetConnection : No active connection for %s... creating new one\n",Share->Name));

  Inode = FsdAllocInode("",FFSS_INODE_DIRECTORY);
  Inode->Flags = FFSS_FILE_DIRECTORY;
  Inode->Parent = FsdAssignInode(Share,false);
  Inode->Path = (char *) malloc(1);
  RtlCopyMemory(Inode->Path,"",1);
  Conn = FC_SendMessage_ShareConnect(Share->IP,Share->Name,NULL,NULL,0);
  if(Conn == NULL)
  {
    KdPrint(("FsdGetConnection : Error creating connection to %s/%s\n",Share->IP,Share->Name));
    FsdFreeInode(Inode,false);
    UNLOCK_SUPERBLOCK_RESOURCE;
    return NULL;
  }
  Inode = FsdAssignInode(Inode,false);
  Inode->Conn = Conn;
  Conn->Root = FsdAssignInode(Inode,false);
  KdPrint(("FsdGetConnection : Successfully connected to %s/%s\n",Share->IP,Share->Name));
  if(!FsdRequestInodeListing(Inode))
  {
    KdPrint(("FsdGetConnection : Error getting listing of root directory for %s/%s\n",Share->IP,Share->Name));
    FsdFreeInode(Inode,false);
    UNLOCK_SUPERBLOCK_RESOURCE;
    return NULL;
  }
  Share->Conns = SU_AddElementHead(Share->Conns,FsdAssignInode(Inode,false));
  UNLOCK_SUPERBLOCK_RESOURCE;
  return Inode;
}

void FsdFreeConnection(IN struct ffss_inode *Conn)
{
  struct ffss_inode *Inode,*parent;

  if(Conn == NULL)
    return;
  if(Conn->Parent == NULL)
  {
    KdPrint(("AIE AIE AIE : FsdFreeConnection : Conn->Parent is NULL !!!!\n"));
    return;
  }
  LOCK_SUPERBLOCK_RESOURCE;
  KdPrint(("FsdFreeConnection : Removing Connection from parent's list\n"));
  Conn->Parent->Conns = SU_DelElementElem(Conn->Parent->Conns,Conn);
  KdPrint(("FsdFreeConnection : Freeing Connection assigned to parent's list\n"));
  FsdFreeInode(Conn,false); /* Freeing parent's list assigned */
  KdPrint(("FsdFreeConnection : Freeing Connection itself\n"));
  FsdFreeInode(Conn,false); /* Freeing connection itself */
  UNLOCK_SUPERBLOCK_RESOURCE;
}

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
