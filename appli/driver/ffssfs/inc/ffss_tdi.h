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
//#define FFSS_MASTER_IP "192.168.1.36"
#define FFSS_MASTER_IP "192.168.1.33"
//#define DBG_MEM


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
  bool TimedOut; /* Last signaled by time out ? */
  bool Locked;   /* Avoid multiple lock */
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
  void GetDatagram(uchar *Data,uint Indicated,PTRANSPORT_ADDRESS pTA,bool ProcessMessage);
};

class FfssTCP : public KStreamSocket
{
public:
  FfssSem *Sem;
  char *Share;
  char *IP;
  struct ffss_inode *Root; /* Assigned upon connection, released when destroying this class */
  FfssTCP(const char Server[],const char ShareName[]);
  FfssTCP *Assign(void);
  void Delete(void);
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
  FfssSem *IncLock;
  FfssTimer *Timer;
  char *Buf;
  long int BufSize;
  unsigned int len;
  signed int RefCount;
  bool GetPacket(uchar *Data,uint Indicated);
  ~FfssTCP();
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

// Constants for FFSS File State
#define FFSS_HANDLE_STATE_OPEN    1
#define FFSS_HANDLE_STATE_CLOSE   2
#define STREAMING_BUFFER_SIZE 65536

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

  SU_PList           Conns;    /* struct ffss_inode * */ /* List of connections */
  void               *Conn;    /* In tcp connected mode, FfssTCP class we are attached to */
  unsigned short int Listed;   /* In tcp connected mode, if node has been listed yet */
  char               *Path;    /* In tcp connected mode, path from root of share */ 

  struct ffss_inode *Parent;
};

//
// FSD_IDENTIFIER_TYPE
//
// Identifiers used to mark the structures
//
typedef enum _FSD_IDENTIFIER_TYPE2 {
    FGD2 = ':DGF',
    VCB2 = ':BCV',
    FCB2 = ':BCF',
    CCB2 = ':BCC',
    ICX2 = ':XCI',
    FSD2 = ':DSF'
} FSD_IDENTIFIER_TYPE2;

//
// FSD_IDENTIFIER
//
// Header used to mark the structures
//
typedef struct _FSD_IDENTIFIER2 {
    FSD_IDENTIFIER_TYPE2     Type;
    ULONG                   Size;
} FSD_IDENTIFIER2, *PFSD_IDENTIFIER2;

typedef struct _FSD_CCB {

    // Identifier for this structure
    FSD_IDENTIFIER2  Identifier;

    // State that may need to be maintained
    ULONG           CurrentByteOffset;
    UNICODE_STRING  DirectorySearchPattern;

    /* FFSS file handle */
    unsigned long int              Handle;
    /* State of file (see FFSS_HANDLE_STATE_xxx) */
    int                            State;
    /* Is EndOfFile ? */
    unsigned char                  eof;
    /* Error during last I/O ? */
    unsigned char                  error;
    /* Buffer for streaming */
    char *                         Buffer[STREAMING_BUFFER_SIZE];
    /* Current pos of buffer */
    unsigned long int              BufferPos;
    /* Current file position */
    __int64                        FilePos;

} FSD_CCB, *PFSD_CCB;

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
/* Superblock must be locked */
bool FsdRequestInodeListing(struct ffss_inode *Inode);

PFSD_CCB FsdAllocateCcb(VOID);
VOID FsdFreeCcb(IN PFSD_CCB Ccb,IN struct ffss_inode *Inode);
/* Superblock must be locked */
int FsdRequestStrmOpen(struct ffss_inode *Inode,PFSD_CCB Ccb,long int OpenFlags); /* a boolean value is returned */

int FFSS_strcasecmp(const char *s,const char *p); /* != 0 if strings are equal */

NTSTATUS TDI_Init();
struct ffss_inode *FsdGetConnection(IN struct ffss_inode *Share);
void FsdFreeConnection(IN struct ffss_inode *Conn);
NTSTATUS FsdReadFileData(IN PDEVICE_OBJECT DeviceObject,IN OUT PFSD_CCB Ccb,struct ffss_inode *IN FcbInode,IN __int64 Offset,IN ULONG Length,IN OUT PVOID Buffer);
void OnStrmOpenAnswer(SU_PClientSocket Client,const char Path[],FFSS_Field ErrorCode,FFSS_Field Handle,FFSS_LongField FileSize,FFSS_LongField User);
void OnStrmReadAnswer(SU_PClientSocket Client,FFSS_Field Handle,const char Bloc[],long int BlocSize,FFSS_Field ErrorCode,FFSS_LongField User);

#ifdef __cplusplus
};
#endif /* __cplusplus */


#endif /* !_FFSS_TDI_H_ */
