#include "confapi.h"

bool FSCA_RequestAndReceive(SU_PClientSocket Client,char Buf[],FFSS_Field *Len)
{
  FFSS_Field Size;
  int Got;
  fd_set rfds;
  struct timeval tv;
  int retval;

  Size = *Len;
  /* Send request */
  SU_ClientSendBuf(Client,(char *)&Size,sizeof(Size));
  SU_ClientSendBuf(Client,Buf,Size);

  /* Prepare to get answer */
  FD_ZERO(&rfds);
  FD_SET(Client->sock,&rfds);
  tv.tv_sec = 5;
  tv.tv_usec = 0;
  retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
    return false;
  Got = recv(Client->sock,&Size,sizeof(Size),SU_MSG_NOSIGNAL);
  if(Got != sizeof(Size))
    return false;
  FD_ZERO(&rfds);
  FD_SET(Client->sock,&rfds);
  tv.tv_sec = 5;
  tv.tv_usec = 0;
  retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
    return false;
  Got = recv(Client->sock,Buf,Size,SU_MSG_NOSIGNAL);
  if(Got < 1)
    return false;
  if(Buf[0] != FS_OPCODE_ACK)
  {
    *Len = 1;
    return true;
  }
  while(Got < Size)
  {
    FD_ZERO(&rfds);
    FD_SET(Client->sock,&rfds);
    tv.tv_sec = 5;
    tv.tv_usec = 0;
    retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
    if(!retval)
      return false;
    Got += recv(Client->sock,Buf+Got,Size-Got,SU_MSG_NOSIGNAL);
  }
  *Len = Size;
  return true;
}

bool FSCA_Request(SU_PClientSocket Client,char Buf[],FFSS_Field *Len)
{
  FFSS_Field Size;

  Size = *Len;
  /* Send request */
  SU_ClientSendBuf(Client,(char *)&Size,sizeof(Size));
  SU_ClientSendBuf(Client,Buf,Size);
  return true;
}

FSCA_PGlobal FSCA_RequestGlobalInfo(SU_PClientSocket Client)
{
  FSCA_PGlobal Gbl = NULL;
  char Buf[10000];
  FFSS_Field Size,Pos;

  Buf[0] = FS_OPCODE_GETGLOBAL;
  Size = 1;
  if(!FSCA_RequestAndReceive(Client,Buf,&Size))
    return NULL;

  Pos = 1;
  Gbl = (FSCA_PGlobal) malloc(sizeof(FSCA_TGlobal));
  memset(Gbl,0,sizeof(FSCA_TGlobal));

  Gbl->Name = strdup(Buf+Pos);
  Pos += strlen(Buf+Pos)+1;
  Gbl->Comment = strdup(Buf+Pos);
  Pos += strlen(Buf+Pos)+1;
  Gbl->Master = strdup(Buf+Pos);
  Pos += strlen(Buf+Pos)+1;
  Gbl->Idle = atoi(Buf+Pos);
  Pos += strlen(Buf+Pos)+1;
  Gbl->MaxConn = atoi(Buf+Pos);
  Pos += strlen(Buf+Pos)+1;
  Gbl->MaxXFerPerConn = atoi(Buf+Pos);
  Pos += strlen(Buf+Pos)+1;
  Gbl->FTP = atoi(Buf+Pos);
  Pos += strlen(Buf+Pos)+1;
  Gbl->FTP_MaxConn = atoi(Buf+Pos);
  return Gbl;
}

FSCA_PShare FSCA_RequestShareInfo(SU_PClientSocket Client,const char ShareName[],const char SharePath[])
{
  char Buf[10000];
  FFSS_Field Size;
  int Pos;
  FSCA_PShare Share;

  /* Create request */
  Buf[0] = FS_OPCODE_GETSHARE;
  Size = 1;
  SU_strcpy(Buf+Size,ShareName,sizeof(Buf)-Size);
  Size += strlen(ShareName) + 1;
  SU_strcpy(Buf+Size,SharePath,sizeof(Buf)-Size);
  Size += strlen(SharePath) + 1;
  if(!FSCA_RequestAndReceive(Client,Buf,&Size))
    return NULL;

  Share = (FSCA_PShare) malloc(sizeof(FSCA_TShare));
  memset(Share,0,sizeof(FSCA_TShare));
  Pos = 1;
  Share->Name = strdup(Buf+Pos);
  Pos += strlen(Buf+Pos) +1;
  Share->Comment = strdup(Buf+Pos);
  Pos += strlen(Buf+Pos) +1;
  Share->Writeable = atoi(Buf+Pos);
  Pos += strlen(Buf+Pos) +1;
  Share->Private = atoi(Buf+Pos);
  Pos += strlen(Buf+Pos) +1;
  Share->MaxConn = atoi(Buf+Pos);
  Pos += strlen(Buf+Pos) +1;
  return Share;
}

int FSCA_RequestStateInfo(SU_PClientSocket Client) /* -1 on error */
{
  char Buf[10000];
  FFSS_Field Size;

  Buf[0] = FS_OPCODE_GETSTATE;
  Size = 1;
  if(!FSCA_RequestAndReceive(Client,Buf,&Size))
    return -1;
  return (int) Buf[0];
}

SU_PList FSCA_RequestConns(SU_PClientSocket Client,const char Path[])
{
  char Buf[10000];
  FFSS_Field Size;
  int Pos,Nb,I,J;
  char *IP,*X,*Y,*File,*Pct;
  SU_PList Conns = NULL;
  FSCA_PConn Conn;
  FSCA_PFileInfo Info;

  /* Create request */
  Buf[0] = FS_OPCODE_GETSHRCONNS;
  Size = 1;
  SU_strcpy(Buf+Size,Path,sizeof(Buf)-Size);
  Size += strlen(Path) + 1;
  if(!FSCA_RequestAndReceive(Client,Buf,&Size))
    return NULL;

  Pos = 1;
  Nb = atoi(Buf+Pos);
  Pos += strlen(Buf+Pos) + 1;
  if(Nb == 0)
    return NULL;
  for(I=0;I<Nb;I++)
  {
    IP = Buf + Pos;
    Pos += strlen(IP) + 1;
    X = Buf + Pos;
    Pos += strlen(X) + 1;
    Y = Buf + Pos;
    Pos += strlen(Y) + 1;
    Conn = (FSCA_PConn) malloc(sizeof(FSCA_TConn));
    memset(Conn,0,sizeof(FSCA_TConn));
    Conn->IP = strdup(IP);
    Conn->NbXfers = atoi(X);
    Conn->NbStrms = atoi(Y);
    if(Conn->NbXfers != 0)
    {
      for(J=0;J<Conn->NbXfers;J++)
      {
        File = Buf + Pos;
        Pos += strlen(File) + 1;
        Pct = Buf + Pos;
        Pos += strlen(Pct) + 1;
        Info = (FSCA_PFileInfo) malloc(sizeof(FSCA_TFileInfo));
        memset(Info,0,sizeof(FSCA_TFileInfo));
        Info->Name = strdup(File);
        Info->Pct = strdup(Pct);
        Conn->Xfers = SU_AddElementHead(Conn->Xfers,Info);
      }
    }
    if(Conn->NbStrms != 0)
    {
      for(J=0;J<Conn->NbStrms;J++)
      {
        File = Buf + Pos;
        Pos += strlen(File) + 1;
        Pct = Buf + Pos;
        Pos += strlen(Pct) + 1;
        Info = (FSCA_PFileInfo) malloc(sizeof(FSCA_TFileInfo));
        memset(Info,0,sizeof(FSCA_TFileInfo));
        Info->Name = strdup(File);
        Info->Pct = strdup(Pct);
        Conn->Strms = SU_AddElementHead(Conn->Strms,Info);
      }
    }
    Conns = SU_AddElementHead(Conns,Conn);
  }
  return Conns;
}

SU_PList FSCA_RequestSharesList(SU_PClientSocket Client)
{
  char Buf[10000];
  FFSS_Field Size;
  int Pos,Nb,I,Conns,XFers,Active;
  char *Shr,*Path;
  SU_PList Shares = NULL;
  FSCA_PShareLst Share;

  /* Create request */
  Buf[0] = FS_OPCODE_GETSHRLIST;
  Size = 1;
  if(!FSCA_RequestAndReceive(Client,Buf,&Size))
    return NULL;

  Pos = 1;
  Nb = atoi(Buf+Pos);
  Pos += strlen(Buf+Pos) + 1;
  if(Nb == 0)
    return NULL;
  for(I=0;I<Nb;I++)
  {
    Shr = Buf + Pos;
    Pos += strlen(Buf+Pos) +1;
    Path = Buf + Pos;
    Pos += strlen(Buf+Pos) +1;
    Active = atoi(Buf + Pos);
    Pos += strlen(Buf+Pos) +1;
    Conns = atoi(Buf + Pos);
    Pos += strlen(Buf+Pos) +1;
    XFers = atoi(Buf + Pos);
    Pos += strlen(Buf+Pos) +1;

    Share = (FSCA_PShareLst) malloc(sizeof(FSCA_TShareLst));
    memset(Share,0,sizeof(FSCA_TShareLst));
    Share->Name = strdup(Shr);
    Share->Path = strdup(Path);
    Share->Active = Active;
    Share->NbConns = Conns;
    Share->NbXfers = XFers;
    Shares = SU_AddElementHead(Shares,Share);
  }
  return Shares;
}

bool FSCA_RequestEject(SU_PClientSocket Client,const char ShareName[])
{
  char Buf[10000];
  FFSS_Field Size;

  /* Create request */
  Buf[0] = FS_OPCODE_EJECT;
  Size = 1;
  SU_strcpy(Buf+Size,ShareName,sizeof(Buf)-Size);
  Size += strlen(ShareName) + 1;
  if(!FSCA_Request(Client,Buf,&Size))
    return false;
  return true;
}
