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

#ifndef _FFSS_TDI_H_
#define _FFSS_TDI_H_

//#define FFSS_MASTER_IP "172.17.64.3"
//#define FFSS_MASTER_IP "192.168.223.1"
#define FFSS_MASTER_IP "192.168.1.36"

#ifdef __cplusplus

#include <vdw.h>
#include <tdiclient.h>   // KTDI
#include <tdiSclient.h>   // KTDI

class FfssTCP;
#define SU_PClientSocket FfssTCP *
#include <ffss.h>
void FC_AnalyseUDP(struct sockaddr_in Client,char Buf[],long int Len);
SU_BOOL FC_AnalyseTCP(SU_PClientSocket Server,char Buf[],long int Len);

/* ********************************************************************** */
/* **************************** CLASSES ********************************* */
/* ********************************************************************** */
class FfssTCPXfer : public KStreamServerSession
{
public:
  // handlers - overrides from KStreamServerSession
  BOOLEAN  OnConnect(uint AddressLength, PTRANSPORT_ADDRESS pTA, uint OptionsLength, PVOID Options);
  void OnDisconnect(uint OptionsLength, PVOID Options, BOOLEAN bAbort);
	uint OnReceive(uint Indicated, uchar *Data, uint Available, uchar **RcvBuffer, uint* RcvBufferLen);
	void OnReceiveComplete(TDI_STATUS Status, uint Indicated, uchar *Data);
  void OnSendComplete(PVOID buf, TDI_STATUS status, uint bytecnt);
private:
};

class FfssTimer;

class FfssSem : public KSemaphore
{
public:
  FfssSem() : KSemaphore(1,1) {Locked=false;}
  void SetTimer(FfssTimer *Timer) {pTimer = Timer;}
  void SignalTimer();
  void WaitTimer();
  void Signal();
  void Wait();
  void SetTimeout(LONGLONG msec);
//  void wait() { Wait(); }
//  void signal() { Signal(); }
private:
  FfssTimer *pTimer;
  bool Locked;
};

class FfssTimer : public KTimedCallback
{
public:
  FfssTimer(FfssSem *Sem,LONGLONG msec) : KTimedCallback() {pSem = Sem;usec = msec*(-10000);}
  bool Set(void);
  void SetTimeout(LONGLONG);
  FfssSem *pSem;
private:
  LONGLONG usec;
};


class FfssUDP : public KDatagramSocket
{
public:
  FfssSem *Sem;
  FfssUDP();
  ~FfssUDP();
  TDI_STATUS sendto(PTDI_CONNECTION_INFORMATION pConn,void* pBuf,uint Size,PVOID pCxt = NULL);
protected:
  void On_sendtoComplete(PVOID,TDI_STATUS,uint);
  uint OnReceive(uint,PTRANSPORT_ADDRESS,uint,PVOID,uint,uchar*,uint,uchar**,uint*);
  void OnReceiveComplete(TDI_STATUS,uint,PTRANSPORT_ADDRESS,uint,PVOID,uint,uchar*);
private:
  FfssTimer *Timer;
  char *Buf;
  long int BufSize;
  int len;
  void GetDatagram(uchar *Data,uint Indicated,PTRANSPORT_ADDRESS pTA);
};

class FfssTCP : public KStreamSocket
{
public:
  FfssSem *Sem;
  char *Share;
  char *IP;
  struct ffss_inode *Root; /* Assigned upon connection, released when destroying this class */
  FfssTCP(const char Server[],const char ShareName[]);
  ~FfssTCP();
  TDI_STATUS connect(PTDI_CONNECTION_INFORMATION);
  TDI_STATUS send(void*,uint,bool,bool sem=true);
protected:
  void On_connectComplete(PVOID,TDI_STATUS,uint);
  void On_sendComplete(PVOID,TDI_STATUS,uint);
  uint OnReceive(uint,uchar*,uint,uchar**,uint*);
  void OnReceiveComplete(TDI_STATUS,uint,uchar*);
  void On_disconnectComplete(PVOID,TDI_STATUS,uint);
  void OnDisconnect(uint OptionsLength,PVOID Options,BOOLEAN bAbort);
private:
  FfssTimer *Timer;
  char *Buf;
  long int BufSize;
  unsigned int len;
  bool GetPacket(uchar *Data,uint Indicated);
};


/* ********************************************************************** */
/* **************************** GLOBALS ********************************* */
/* ********************************************************************** */
extern FfssUDP *pUDP;

extern "C" {
#if DBG
PVOID FsdAllocatePool (IN POOL_TYPE PoolType,IN ULONG NumberOfBytes,IN ULONG Tag);
VOID FsdFreePool (IN PVOID p);
#else // !DBG
#define FsdAllocatePool(PoolType, NumberOfBytes, Tag) ExAllocatePool(PoolType, NumberOfBytes)
#define FsdFreePool(p) ExFreePool(p)
#endif // !DBG
#endif /* __cplusplus */

#undef malloc
#undef free
#define malloc(NumberOfBytes) ExAllocatePool(NonPagedPool,NumberOfBytes)
#define free(p) ExFreePool(p)

struct ffss_super_block {
  ERESOURCE Resource; /* Synchronisation resource */

  struct ffss_inode *Root; /* Root inode = Domains */
};

#define FFSS_INODE_ROOT      1
#define FFSS_INODE_DOMAIN    2
#define FFSS_INODE_SERVER    3
#define FFSS_INODE_SHARE     4
#define FFSS_INODE_DIRECTORY 5
#define FFSS_INODE_FILE      6

struct ffss_inode {
  unsigned long int  Type;
  char               *Name;
  unsigned long int  NameLength;
  unsigned long int  Flags;
  unsigned __int64   Size;
  unsigned long int  Stamp;
  signed long int    RefCount;
  struct ffss_inode  **Inodes; /* Tab of sub inodes */
  unsigned long int  NbInodes; /* Number of sub inodes */

  char               *IP;      /* For Host Inodes */

  void               *Conn;    /* In tcp connected mode, FfssTCP class we are attached to */
  unsigned short int Listed;   /* In tcp connected mode, if node has been listed yet */
  char               *Path;    /* In tcp connected mode, path from root of share */ 

  struct ffss_inode *Parent;
};

extern struct ffss_super_block *FFSS_SuperBlock;

#define LOCK_SUPERBLOCK_RESOURCE ExAcquireResourceExclusiveLite(&FFSS_SuperBlock->Resource,TRUE)
#define UNLOCK_SUPERBLOCK_RESOURCE ExReleaseResourceForThreadLite(&FFSS_SuperBlock->Resource,ExGetCurrentResourceThread())

struct ffss_inode *FsdAllocInode(IN const char Name[],IN unsigned long int Type);
struct ffss_inode *FsdAssignInode(IN struct ffss_inode*  ffss_inode,IN SU_BOOL Lock);
VOID FsdFreeInode(IN struct ffss_inode*  ffss_inode,IN SU_BOOL Lock);
VOID FsdFreeSubInodes(IN struct ffss_inode*  ffss_inode,IN SU_BOOL Lock);

void FsdRescanInode(IN struct ffss_inode* Inode);
/* Returned inode must be freed */
struct ffss_inode *FsdGetInodeFromDomain(IN char *domain);
/* Returned inode must be freed */
struct ffss_inode *FsdGetInodeFromServer(IN char *server,IN struct ffss_inode *Domain);
/* Returned inode must be freed */
struct ffss_inode *FsdGetInodeFromServerIP(IN char *IP);
/* Returned inode must be freed */
struct ffss_inode *FsdGetInodeFromShare(IN char *share,IN struct ffss_inode *Server);

int FFSS_strcasecmp(const char *s,const char *p); /* != 0 if strings are equal */

NTSTATUS TDI_Init();
struct ffss_inode *FsdGetConnection(IN struct ffss_inode *Share);

#ifdef __cplusplus
};
#endif /* __cplusplus */


#endif /* !_FFSS_TDI_H_ */
