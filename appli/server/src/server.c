#include "server.h"
#include "plugin.h"

/* Order of Semaphore locking */
SU_SEM_HANDLE FS_SemGbl;   /* Semaphore to protect the use of MyGlobal */
SU_SEM_HANDLE FS_SemShr;   /* Semaphore to protect the use of FS_Index and all sub structs */
SU_SEM_HANDLE FS_SemPlugin;/* Semaphore to protect the use of FS_Plugins */
SU_THREAD_KEY_HANDLE FS_tskey;
SU_THREAD_ONCE_HANDLE FS_once = SU_THREAD_ONCE_INIT;

FS_TGlobal FS_MyGlobal;
char *FS_MyDomain = "None";
#ifdef __BSD__
char *FS_MyIntName = "xl0";
#else
char *FS_MyIntName = "eth0";
#endif
int FS_MyState;
char *FS_TimeTable[]={"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"};
long int FS_CurrentStrmTag = 1;
SU_PList FS_Plugins; /* FS_PPlugin */

#ifdef _WIN32
HINSTANCE FS_hInstance = NULL;
HWND FS_hwnd;
#endif /* _WIN32 */

/* Assumes FS_SemShr is locked */
void FS_FreeStreaming(FS_PStreaming FS)
{
  if(FS->fp != NULL)
    fclose(FS->fp);
  if(FS->FileName != NULL)
    free(FS->FileName);
  free(FS);
}

/* Assumes FS_SemShr is locked */
FS_PStreaming FS_GetStreamingByHandle(SU_PList Strms,FFSS_Field Handle)
{
  SU_PList Ptr;

  Ptr = Strms;
  while(Ptr != NULL)
  {
    if(((FS_PStreaming)Ptr->Data)->Handle == Handle)
      return (FS_PStreaming)Ptr->Data;
    Ptr = Ptr->Next;
  }
  return NULL;
}

/* Assumes FS_SemShr is locked */
void FS_EjectFromShare(FS_PShare Share,bool EjectXFers)
{
  SU_PList Ptr,Ptr2;

  Ptr = Share->Conns;
  while(Ptr != NULL)
  {
    if(((FS_PConn)Ptr->Data)->ts != NULL)
    {
      ((FS_PConn)Ptr->Data)->ts->Remove = true;
      FS_SendMessage_Error(((FS_PConn)Ptr->Data)->ts->Client->sock,FFSS_ERROR_SHARE_EJECTED,FFSS_ErrorTable[FFSS_ERROR_SHARE_EJECTED],9,((FS_PConn)Ptr->Data)->ts->User);
      SU_CLOSE_SOCKET(((FS_PConn)Ptr->Data)->ts->Client->sock);
      ((FS_PConn)Ptr->Data)->ts->Client->User = (void *)1; /* Sets a time out for ejecting the client */
    }
    if(EjectXFers)
    {
      Ptr2 = ((FS_PConn)Ptr->Data)->XFers;
      while(Ptr2 != NULL)
      {
        ((FFSS_PTransfer)Ptr2->Data)->Cancel = true;
        Ptr2 = Ptr2->Next;
      }
    }
    Ptr = Ptr->Next;
  }
}

/* Locks FS_SemShr */
void FS_EjectAll(bool EjectXFers)
{
  SU_PList Ptr;

  SU_SEM_WAIT(FS_SemShr);
  Ptr = FS_Index;
  while(Ptr != NULL)
  {
    FS_EjectFromShare((FS_PShare)Ptr->Data,EjectXFers);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemShr);
}

/* Assumes FS_SemShr is locked */
void FS_EjectFromShareByIP(FS_PShare Share,const char IP[],bool EjectXFers)
{
  SU_PList Ptr,Ptr2;

  Ptr = Share->Conns;
  while(Ptr != NULL)
  {
    if(strcmp(((FS_PConn)Ptr->Data)->Remote,IP) == 0)
    {
      if(((FS_PConn)Ptr->Data)->ts != NULL)
      {
        ((FS_PConn)Ptr->Data)->ts->Remove = true;
        FS_SendMessage_Error(((FS_PConn)Ptr->Data)->ts->Client->sock,FFSS_ERROR_SHARE_EJECTED,FFSS_ErrorTable[FFSS_ERROR_SHARE_EJECTED],0,((FS_PConn)Ptr->Data)->ts->User);
        SU_CLOSE_SOCKET(((FS_PConn)Ptr->Data)->ts->Client->sock);
        ((FS_PConn)Ptr->Data)->ts->Client->User = (void *)1; /* Sets a time out for ejecting the client */
      }
      if(EjectXFers)
      {
        Ptr2 = ((FS_PConn)Ptr->Data)->XFers;
        while(Ptr2 != NULL)
        {
          ((FFSS_PTransfer)Ptr2->Data)->Cancel = true;
          Ptr2 = Ptr2->Next;
        }
      }
    }
    Ptr = Ptr->Next;
  }
}

/* Assumes FS_SemShr is locked */
FS_PShare FS_GetShareFromName(const char Name[])
{
  SU_PList Ptr;

  if(Name == NULL)
    return NULL;
  Ptr = FS_Index;
  while(Ptr != NULL)
  {
    if(SU_strcasecmp(Name,((FS_PShare)Ptr->Data)->ShareName))
      return (FS_PShare) Ptr->Data;
    Ptr = Ptr->Next;
  }
  return NULL;
}

/* Assumes FS_SemShr is locked */
FS_PShare FS_GetShareFromPath(const char Path[])
{
  SU_PList Ptr;

  if(Path == NULL)
    return NULL;
  Ptr = FS_Index;
  while(Ptr != NULL)
  {
    if(SU_strcasecmp(Path,((FS_PShare)Ptr->Data)->Path))
      return (FS_PShare) Ptr->Data;
    Ptr = Ptr->Next;
  }
  return NULL;
}

/* Assumes FS_SemShr is locked */
/* Locks FS_SemGbl */
void FS_AddConnectionToShare(FS_PShare Share,const char RemoteIP[],FS_PUser Usr,FS_PThreadSpecific ts)
{
  FS_PConn Conn;

#ifdef DEBUG
  printf("ADD CONN TO SHARE\n");
#endif /* DEBUG */
  Conn = (FS_PConn) malloc(sizeof(FS_TConn));
  memset(Conn,0,sizeof(FS_TConn));
  Conn->Remote = strdup(RemoteIP);
  Conn->User = Usr;
  Conn->ts = ts;
  Conn->Share = Share;
  Share->Conns = SU_AddElementHead(Share->Conns,Conn);

  SU_SEM_WAIT(FS_SemGbl);
  FS_MyGlobal.Conn++;
  SU_SEM_POST(FS_SemGbl);
}

/* Assumes FS_SemShr is locked */
unsigned int FS_XFersCount(FS_PConn Conn)
{
  return SU_ListCount(Conn->XFers);
}

/* Assumes FS_SemShr is locked */
void FS_FreeConn(FS_PConn Conn,bool RemoveXFers)
{
  SU_PList Ptr;

#ifdef DEBUG
  if(RemoveXFers)
    printf("Removing connection, and xfers are also requested to be removed\n");
  else
    printf("Removing connection, setting Conn object of XFers to NULL\n");
#endif /* DEBUG */
  Ptr = Conn->XFers;
  while(Ptr != NULL)
  {
    if(RemoveXFers)
      ((FFSS_PTransfer)Ptr->Data)->Cancel = true;
    ((FFSS_PTransfer)Ptr->Data)->User = NULL;
    Ptr = Ptr->Next;
  }
  SU_FreeList(Conn->XFers);
  Conn->XFers = NULL;
  Ptr = Conn->Strms;
  while(Ptr != NULL)
  {
    FS_FreeStreaming((FS_PStreaming)Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Conn->Strms);
  Conn->Strms = NULL;
  if(Conn->Remote != NULL)
    free(Conn->Remote);
  if(Conn->TransferBuffer != NULL)
    free(Conn->TransferBuffer);
  free(Conn);
}

/* Assumes FS_SemShr is locked */
FS_PConn FS_GetConnFromTS(FS_PThreadSpecific ts,SU_PList Conns)
{
  SU_PList Ptr;
  FS_PConn Conn;

  Ptr = Conns;
  while(Ptr != NULL)
  {
    Conn = (FS_PConn)Ptr->Data;
    if(Conn->ts == ts)
      return Conn;
    Ptr = Ptr->Next;
  }
  return NULL;
}

/* Assumes FS_SemShr is locked */
/* Locks FS_SemGbl */
void FS_DoRemoveConnectFromShare(FS_PConn Conn,FS_PShare Share,bool RemoveXFers)
{
#ifdef DEBUG
  printf("REMOVE CONN FORM SHARE\n");
#endif /* DEBUG */
  context;
  FS_FreeConn(Conn,RemoveXFers);
  Share->Conns = SU_DelElementElem(Share->Conns,Conn);
  SU_SEM_WAIT(FS_SemGbl);
  FS_MyGlobal.Conn--;
  SU_SEM_POST(FS_SemGbl);
}

/* Assumes FS_SemShr is locked */
void FS_RemoveConnectionFromShare(FS_PShare Share,FS_PThreadSpecific ts,bool RemoveXFers)
{
  FS_PConn Conn;

  context;
  if(Share == NULL)
    return;
  Conn = FS_GetConnFromTS(ts,Share->Conns);
  if(Conn != NULL)
  {
    /* If xfers are active, we don't really free connection. But if streamings are actives, we DO free it */
    if((Conn->XFers != NULL) && !RemoveXFers)
    {
#ifdef DEBUG
      printf("NOT REMOVING CONN... SINCE XFERS ARE STILL ACTIVES\n");
#endif /* DEBUG */
      Conn->ToRemove = true;
      Conn->ts = NULL;
    }
    else
      FS_DoRemoveConnectFromShare(Conn,Share,RemoveXFers);
  }
  else
    printf("HUMMMMMMMMMMMM : Conn matching ts %p not found in FS_RemoveConnectionFromShare !!!!!!\n",ts);
}

/* Assumes FS_SemShr is locked */
void FS_CheckConnectionForRemoval(FS_PConn Conn,FS_PShare Share)
{
  /* If xfers are active, we don't really free connection. But if streamings are actives, we DO free it */
  if(Conn->ToRemove && (Conn->XFers == NULL))
  {
    FS_DoRemoveConnectFromShare(Conn,Share,true);
  }
}

void destroyts(void *ptr)
{
  FS_PThreadSpecific ts = (FS_PThreadSpecific) ptr;

  if(ts == NULL) return;
  if(ts->ShareName != NULL)
    free(ts->ShareName);
  free(ts);
  SU_THREAD_SET_SPECIFIC(FS_tskey,NULL);
}

void tsinitkey(void)
{
  SU_CreateThreadKey(&FS_tskey,&FS_once,destroyts);
}

/* Assumes FS_SemShr is locked */
FS_PThreadSpecific FS_GetThreadSpecific(bool DontCreate)
{
  FS_PThreadSpecific ts;

  SU_THREAD_ONCE(FS_once,tsinitkey);
  ts = (FS_PThreadSpecific) SU_THREAD_GET_SPECIFIC(FS_tskey);
  if(ts == NULL)
  {
    if(DontCreate)
      return NULL;
    ts = (FS_PThreadSpecific) malloc(sizeof(FS_TThreadSpecific));
    memset(ts,0,sizeof(FS_TThreadSpecific));
    SU_THREAD_SET_SPECIFIC(FS_tskey,ts);
  }
  if(ts->Share != NULL) /* (NOT FTP) AND (NOT FIRST CALL) */
  {
    if(ts->Share->Remove) /* If share is waiting to be removed */
    {
#ifdef DEBUG
      printf("Share %s is to be removed, %d remaining connections\n",ts->Share->ShareName,SU_ListCount(ts->Share->Conns));
#endif /* DEBUG */
      FS_SendMessage_Error(ts->Client->sock,FFSS_ERROR_SHARE_EJECTED,FFSS_ErrorTable[FFSS_ERROR_SHARE_EJECTED],0,ts->User);
      FS_RemoveConnectionFromShare(ts->Share,ts,true);
      if(ts->Share->Conns == NULL) /* No more connection, removing share */
        FS_FreeShare(ts->Share);
      /* Clean and kill this thread */
      SU_CLOSE_SOCKET(ts->Client->sock);
      SU_THREAD_DESTROY_SPECIFIC(destroyts,ts);
      return NULL;
    }
    if(ts->Remove) /* If connection is waiting to be removed */
    {
#ifdef DEBUG
      printf("This connection has been requested to exit (Share %s)\n",ts->Share->ShareName);
#endif /* DEBUG */
      FS_SendMessage_Error(ts->Client->sock,FFSS_ERROR_SHARE_EJECTED,FFSS_ErrorTable[FFSS_ERROR_SHARE_EJECTED],0,ts->User);
      FS_RemoveConnectionFromShare(ts->Share,ts,false);
      /* Clean and kill this thread */
      SU_CLOSE_SOCKET(ts->Client->sock);
      SU_THREAD_DESTROY_SPECIFIC(destroyts,ts);
      return NULL;
    }
  }
  return ts;
}

void OnEndTCPThread(void)
{
  FS_PThreadSpecific ts;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(true);

  if(ts != NULL)
  {
    if(ts->ShareName != NULL) /* Not ftp */
    {
      FS_RemoveConnectionFromShare(ts->Share,ts,false);
    }
    else
    {
#ifdef DEBUG
      printf("REMOVE FTP CONN\n");
#endif /* DEBUG */
      SU_SEM_WAIT(FS_SemGbl);
      FS_MyGlobal.FTPConn--;
      SU_SEM_POST(FS_SemGbl);
    }
  }
  SU_SEM_POST(FS_SemShr);
  SU_THREAD_DESTROY_SPECIFIC(destroyts,ts);
}

/* Assumes FS_SemShr is locked */
bool FS_SendDirectoryListing(SU_PClientSocket Client,FS_PShare Share,const char Path[],long int Comps,FFSS_LongField User)
{
  char *buf;
  long int len;
  int comp;
  bool ans;

  buf = FS_BuildDirectoryBuffer(Share,Path,&len);
  if(buf == NULL)
  {
    return FS_SendMessage_Error(Client->sock,FFSS_ERROR_FILE_NOT_FOUND,FFSS_ErrorTable[FFSS_ERROR_FILE_NOT_FOUND],0,User);
  }
#ifdef HAVE_BZLIB
  if((len+strlen(Path)) >= FS_COMPRESSION_TRIGGER_BZLIB)
  {
    if(Comps & FFSS_COMPRESSION_BZLIB)
      comp = FFSS_COMPRESSION_BZLIB;
    else if(Comps & FFSS_COMPRESSION_ZLIB)
      comp = FFSS_COMPRESSION_ZLIB;
    else
      comp = FFSS_COMPRESSION_NONE;
  }
  else
#endif /* HAVE_BZLIB */
#ifndef DISABLE_ZLIB
  if((len+strlen(Path)) >= FS_COMPRESSION_TRIGGER_ZLIB)
  {
    if(Comps & FFSS_COMPRESSION_ZLIB)
      comp = FFSS_COMPRESSION_ZLIB;
    else
      comp = FFSS_COMPRESSION_NONE;
  }
  else
#endif /* !DISABLE_ZLIB */
    comp = FFSS_COMPRESSION_NONE;
  ans = FS_SendMessage_DirectoryListingAnswer(Client->sock,Path,buf,len,comp,User);
  free(buf);
  return ans;
}

/* Assumes FS_SemShr is locked */
bool FS_SendRecursiveDirectoryListing(SU_PClientSocket Client,FS_PShare Share,const char Path[],long int Comps,FFSS_LongField User)
{
  char *buf;
  long int len;
  int comp;
  bool ans;

  buf = FS_BuildRecursiveDirectoryBuffer(Share,Path,&len);
  if(buf == NULL)
  {
    return FS_SendMessage_Error(Client->sock,FFSS_ERROR_FILE_NOT_FOUND,FFSS_ErrorTable[FFSS_ERROR_FILE_NOT_FOUND],1,User);
  }
#ifdef HAVE_BZLIB
  if((len+strlen(Path)) >= FS_COMPRESSION_TRIGGER_BZLIB)
  {
    if(Comps & FFSS_COMPRESSION_BZLIB)
      comp = FFSS_COMPRESSION_BZLIB;
    else if(Comps & FFSS_COMPRESSION_ZLIB)
      comp = FFSS_COMPRESSION_ZLIB;
    else
      comp = FFSS_COMPRESSION_NONE;
  }
  else
#endif /* HAVE_BZLIB */
#ifndef DISABLE_ZLIB
  if((len+strlen(Path)) >= FS_COMPRESSION_TRIGGER_ZLIB)
  {
    if(Comps & FFSS_COMPRESSION_ZLIB)
      comp = FFSS_COMPRESSION_ZLIB;
    else
      comp = FFSS_COMPRESSION_NONE;
  }
  else
#endif /* !DISABLE_ZLIB */
    comp = FFSS_COMPRESSION_NONE;
  ans = FS_SendMessage_RecursiveDirectoryListingAnswer(Client->sock,Path,buf,len,comp,User);
  free(buf);
  return ans;
}

/* UDP callbacks */
void OnPing(struct sockaddr_in Master)
{
  SU_PList Ptr;
  static int Count = 0;
  bool Changed = false;

  if(FS_MyState == FFSS_STATE_OFF)
    return;
  FS_SendMessage_Pong(Master,FS_MyState);

  /* Checking for change in shares */
  Count++;
  if(Count >= FS_CHECK_EVERY_X_PING)
  {
    Count = 0;
    SU_SEM_WAIT(FS_SemShr);
    Ptr = FS_Index;
    while(Ptr != NULL)
    {
      if(FS_CheckDirectoryChanged((FS_PShare)Ptr->Data))
      {
        FFSS_PrintDebug(5,"Detected a change... Rescaning share\n");
        FS_EjectFromShare((FS_PShare)Ptr->Data,false);
        FS_RescanShare((FS_PShare)Ptr->Data);
        Changed = true;
      }
      Ptr = Ptr->Next;
    }
    SU_SEM_POST(FS_SemShr);
  }

  if(Changed)
  {
    SU_SEM_WAIT(FS_SemGbl);
    /* Sending index message to my master */
    FS_SendIndex(FS_MyGlobal.Master,FFSS_MASTER_PORT_S);
    SU_SEM_POST(FS_SemGbl);
  }

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnPing != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnPing(Master);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}

void OnStateAnswer(const char Domain[])
{
  SU_PList Ptr;

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnStateAnswer != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnStateAnswer(Domain);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  FS_MyDomain = strdup(Domain);
}

void OnServerSearch(struct sockaddr_in Client)
{
  SU_PList Ptr;

  if(FS_MyState == FFSS_STATE_OFF)
    return;
  FFSS_PrintDebug(1,"Server received a server search from %s (%s)\n",inet_ntoa(Client.sin_addr),SU_NameOfPort(inet_ntoa(Client.sin_addr)));
  SU_SEM_WAIT(FS_SemGbl);
  if(!FS_SendMessage_ServerSearchAnswer(Client,FS_MyDomain,FS_MyGlobal.Name,FFSS_GetOS(),FS_MyGlobal.Comment,FS_MyState,FS_MyGlobal.MyIP,FS_MyGlobal.MasterIP))
    FFSS_PrintDebug(1,"Error replying to client : %d\n",errno);
  SU_SEM_POST(FS_SemGbl);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnServerSearch != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnServerSearch(Client);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}

void OnSharesListing(struct sockaddr_in Client,FFSS_LongField User)
{
  int nb,i,count;
  char **Names,**Comments;
  SU_PList Ptr,Plugs;
  bool checked,do_it;

  if(FS_MyState == FFSS_STATE_OFF)
    return;
  SU_SEM_WAIT(FS_SemShr);
  nb = SU_ListCount(FS_Index);
  if(nb == 0)
  {
    SU_SEM_WAIT(FS_SemGbl);
    if(!FS_SendMessage_ServerSharesAnswer(Client,FS_MyGlobal.MyIP,NULL,NULL,nb,User))
      FFSS_PrintDebug(1,"Error replying to client : %d\n",errno);
    SU_SEM_POST(FS_SemGbl);
  }
  else
  {
    Names = (char **) malloc(nb*sizeof(char *));
    Comments = (char **) malloc(nb*sizeof(char *));
    count = 0;
    Ptr = FS_Index;
    for(i=0;i<nb;i++)
    {
      do_it = true;
      checked = false;
      SU_SEM_WAIT(FS_SemPlugin);
      Plugs = FS_Plugins;
      while(Plugs != NULL)
      {
        if(((FS_PPlugin)Plugs->Data)->OnCheckShowShare != NULL)
        {
          do_it &= ((FS_PPlugin)Plugs->Data)->OnCheckShowShare((FS_PShare)Ptr->Data);
          checked = true;
        }
        Plugs = Plugs->Next;
      }
      SU_SEM_POST(FS_SemPlugin);
      if(!checked || (checked && do_it))
      {
        Names[count] = ((FS_PShare)Ptr->Data)->ShareName;
        Comments[count] = ((FS_PShare)Ptr->Data)->Comment;
        count++;
      }
      Ptr = Ptr->Next;
    }
    SU_SEM_WAIT(FS_SemGbl);
    if(!FS_SendMessage_ServerSharesAnswer(Client,FS_MyGlobal.MyIP,(const char **)Names,(const char **)Comments,count,User))
      FFSS_PrintDebug(1,"Error replying to client : %d\n",errno);
    SU_SEM_POST(FS_SemGbl);
    free(Names);
    free(Comments);
  }
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnSharesListing != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnSharesListing(Client,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}

void OnIndexRequest(struct sockaddr_in Master,FFSS_Field Port)
{
  SU_PList Ptr;

  if(FS_MyState == FFSS_STATE_OFF)
    return;
  FFSS_PrintDebug(1,"Server received an index request from a %s at address %s (%s)\n",(Port == FFSS_MASTER_PORT)?"Master":"Client",inet_ntoa(Master.sin_addr),SU_NameOfPort(inet_ntoa(Master.sin_addr)));

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnIndexRequest != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnIndexRequest(Master,Port);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}

void OnError(FFSS_Field ErrorCode,const char Description[])
{
  SU_PList Ptr;

  FFSS_PrintSyslog(LOG_ERR,"Server received an error message : (%ld) %s\n",ErrorCode,Description);
  if(ErrorCode == FFSS_ERROR_PROTOCOL_VERSION_ERROR)
  {
#ifdef _WIN32
    MessageBox(NULL,"Protocol version mismatch with your master, please upgrade your server","FFSS Server Info",MB_OK);
#endif
  }
  if(ErrorCode == FFSS_ERROR_RESEND_LAST_UDP) /* Ignore right now */
    return;
  if(ErrorCode == FFSS_ERROR_BUFFER_OVERFLOW) /* Ignore */
    return;

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnError != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnError(ErrorCode,Description);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);

  /* As we are in the UDP thread, a FS_Uninit will kill it first... */
  FS_UnLoadAllPlugin();
  SU_TermThread(FFSS_MainThread);
  //FS_ShutDown();
  exit(-1);
}

void OnMasterSearchAnswer(struct sockaddr_in Master,FFSS_Field ProtocolVersion,const char Domain[],FFSS_LongField User)
{
  SU_PList Ptr;

  FFSS_PrintDebug(1,"Server received a master search answer from %s (%s) using version %x for domain %s\n",inet_ntoa(Master.sin_addr),SU_NameOfPort(inet_ntoa(Master.sin_addr)),ProtocolVersion,Domain);

  FS_SendMessage_Pong(Master,FS_MyState);
  SU_SEM_WAIT(FS_SemGbl);
  if(FS_MyGlobal.Master == NULL)
  {
    if((ProtocolVersion <= FFSS_PROTOCOL_VERSION) && (ProtocolVersion >= FFSS_PROTOCOL_VERSION_LEAST_COMPATIBLE))
    {
      FS_MyGlobal.MasterIP = inet_ntoa(Master.sin_addr);
      FS_MyGlobal.Master = SU_NameOfPort(FS_MyGlobal.MasterIP);
    }
    else
    {
      if(ProtocolVersion > FFSS_PROTOCOL_VERSION)
        FFSS_PrintSyslog(LOG_WARNING,"Master %s uses a superior FFSS protocol version (%x), maybe you should upgrade your server\n",inet_ntoa(Master.sin_addr),ProtocolVersion);
      if(ProtocolVersion < FFSS_PROTOCOL_VERSION_LEAST_COMPATIBLE)
        FFSS_PrintSyslog(LOG_WARNING,"Master %s uses an inferior FFSS protocol version (%x), maybe you should contact ffss-master administrator for upgrade\n",inet_ntoa(Master.sin_addr),ProtocolVersion);
    }
  }
  SU_SEM_POST(FS_SemGbl);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnMasterSearchAnswer != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnMasterSearchAnswer(Master,ProtocolVersion,Domain,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}


/* TCP callbacks */
void OnBeginTCPThread(SU_PClientSocket Client,void *Info)
{ /* This is the first callback in the new client-server connection thread */
  FS_PThreadSpecific ts = (FS_PThreadSpecific) Info;

  SU_SEM_WAIT(FS_SemShr);
  FS_GetThreadSpecific(true); /* Only called to create the ts key */
  SU_THREAD_SET_SPECIFIC(FS_tskey,ts); /* Now, really set the struct in the key */
  FFSS_PrintDebug(1,"Begining client's thread for %s\n",inet_ntoa(Client->SAddr.sin_addr));
  SU_SEM_POST(FS_SemShr);
}

void *OnShareConnection(SU_PClientSocket Client,const char ShareName[],const char Login[],const char Password[],long int Compressions,FFSS_LongField User)
{
  SU_PList Ptr;
  FS_PShare Share;
  FS_PThreadSpecific ts;
  FS_PUser Usr;
  int Idle;
#ifdef USE_CRYPT
  char Key[3];
#endif

  FFSS_PrintDebug(1,"Received a SHARE CONNECTION message (%s)\n",ShareName);

  /* Creates a new ts */
  ts = (FS_PThreadSpecific) malloc(sizeof(FS_TThreadSpecific));
  memset(ts,0,sizeof(FS_TThreadSpecific));
  if(ts == NULL)
    return NULL;
  SU_SEM_WAIT(FS_SemShr);
  ts->ShareName = strdup(ShareName);
  ts->Client = Client;
  ts->Comps = Compressions;
  ts->User = User;

  /* If Server if OFF */
  if(FS_MyState == FFSS_STATE_OFF)
  { /* Server is off */
    SU_SEM_POST(FS_SemShr);
    FS_SendMessage_Error(Client->sock,FFSS_ERROR_RESOURCE_NOT_AVAIL,FFSS_ErrorTable[FFSS_ERROR_RESOURCE_NOT_AVAIL],0,User);
    return NULL;
  }

  /* Retrieve Share structure */
  Share = FS_GetShareFromName(ShareName);
  if(Share == NULL)
  { /* Share not found - Sending error message */
    SU_SEM_POST(FS_SemShr);
    FS_SendMessage_Error(Client->sock,FFSS_ERROR_RESOURCE_NOT_AVAIL,FFSS_ErrorTable[FFSS_ERROR_RESOURCE_NOT_AVAIL],0,User);
    return NULL;
  }

  /* Check if share is enabled */
  if(Share->Disabled)
  { /* Share disabled - Sending error message */
    SU_SEM_POST(FS_SemShr);
    FS_SendMessage_Error(Client->sock,FFSS_ERROR_SHARE_DISABLED,FFSS_ErrorTable[FFSS_ERROR_SHARE_DISABLED],0,User);
    return NULL;
  }

  /* Check Global's MaxConnection value */
  SU_SEM_WAIT(FS_SemGbl);
  if(FS_MyGlobal.MaxConn != 0)
  {
    if(FS_MyGlobal.Conn >= FS_MyGlobal.MaxConn)
    { /* Too many connections */
      SU_SEM_POST(FS_SemShr);
      FS_SendMessage_Error(Client->sock,FFSS_ERROR_TOO_MANY_CONNECTIONS,FFSS_ErrorTable[FFSS_ERROR_TOO_MANY_CONNECTIONS],FS_MyGlobal.MaxConn,User);
      SU_SEM_POST(FS_SemGbl);
      return NULL;
    }
  }
  Idle = FS_MyGlobal.Idle;
  SU_SEM_POST(FS_SemGbl);

  /* Check Share's MaxConnection value */
  if(Share->MaxConnections != 0)
  {
    if(SU_ListCount(Share->Conns) >= Share->MaxConnections)
    { /* Too many connections */
      SU_SEM_POST(FS_SemShr);
      FS_SendMessage_Error(Client->sock,FFSS_ERROR_TOO_MANY_CONNECTIONS,FFSS_ErrorTable[FFSS_ERROR_TOO_MANY_CONNECTIONS],Share->MaxConnections,User);
      return NULL;
    }
  }
  ts->Writeable = Share->Writeable;

  Usr = NULL;
  if((Login[0] != 0) || (Password[0] != 0))
  {
    FFSS_PrintDebug(3,"Login/Password specified... looking for valid user\n");
    if((Login[0] == 0) || (Password[0] == 0)) /* Need both login and password - Send error message */
    {
      SU_SEM_POST(FS_SemShr);
      FS_SendMessage_Error(Client->sock,FFSS_ERROR_NEED_LOGIN_PASS,FFSS_ErrorTable[FFSS_ERROR_NEED_LOGIN_PASS],0,User);
      return NULL;
    }
    Ptr = Share->Users;
    while(Ptr != NULL)
    {
      if(strcmp(((FS_PUser)Ptr->Data)->Login,Login) == 0)
        break;
      Ptr = Ptr->Next;
    }
    if(Ptr == NULL) /* Login not found in my list - Send error message */
    {
      SU_SEM_POST(FS_SemShr);
      FS_SendMessage_Error(Client->sock,FFSS_ERROR_NEED_LOGIN_PASS,FFSS_ErrorTable[FFSS_ERROR_NEED_LOGIN_PASS],0,User);
      return NULL;
    }
    /* Now check password */
    FFSS_PrintDebug(3,"Found user %s... checking password\n",Login);
    Usr = (FS_PUser)Ptr->Data;
#ifdef USE_CRYPT
    if(strlen(Password) < 3)
    {
      Key[0] = 'F'; Key[1] = 'S'; Key[2] = 0;
    }
    else
    {
      Key[0] = Password[0]; Key[1] = Password[1]; Key[2] = 0;
    }
    if(strncmp(crypt(Password,Key),Usr->Password,FFSS_MAX_PASSWORD_LENGTH-1) != 0)
#else
    if(strncmp(Password,Usr->Password,FFSS_MAX_PASSWORD_LENGTH-1) != 0)
#endif
    { /* Wrong password */
      SU_SEM_POST(FS_SemShr);
      FFSS_PrintDebug(3,"Tadam... wrong password !!\n");
      FS_SendMessage_Error(Client->sock,FFSS_ERROR_NEED_LOGIN_PASS,FFSS_ErrorTable[FFSS_ERROR_NEED_LOGIN_PASS],0,User);
      return NULL;
    }
    FFSS_PrintDebug(3,"Ok, user accepted\n");
    ts->Writeable = Usr->Writeable;
  }
  if(Share->Private && (Usr == NULL)) /* Share is private, and login/pass info does not match */
  {
    SU_SEM_POST(FS_SemShr);
    FS_SendMessage_Error(Client->sock,FFSS_ERROR_NEED_LOGIN_PASS,FFSS_ErrorTable[FFSS_ERROR_NEED_LOGIN_PASS],0,User);
    return NULL;
  }

  /* Set Idle time out value */
  Client->User = (void *)Idle;
  ts->Share = Share;
  FS_AddConnectionToShare(Share,inet_ntoa(Client->SAddr.sin_addr),Usr,ts);
  SU_SEM_POST(FS_SemShr);

  /* Send a OK message */
  if(!FS_SendMessage_Error(Client->sock,FFSS_ERROR_NO_ERROR,NULL,0,User))
  {
    FFSS_PrintDebug(1,"Error replying to client : %d\n",errno);
    return NULL;
  }

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnShareConnection != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnShareConnection(Client,ShareName,Login,Password,Compressions,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return ts;
}

bool OnDirectoryListing(SU_PClientSocket Client,const char Path[],FFSS_LongField User) /* Path IN the share (without share name) */
{
  FS_PThreadSpecific ts;
  SU_PList Ptr;

  FFSS_PrintDebug(1,"Received a DIRECTORY LISTING message\n");

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return false;
  }
  if(!FS_SendDirectoryListing(Client,ts->Share,Path,ts->Comps,User))
  {
    SU_SEM_POST(FS_SemShr);
    FFSS_PrintDebug(1,"Error replying to client : %d\n",errno);
    return false;
  }
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnDirectoryListing != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnDirectoryListing(Client,Path,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return true;
}

bool OnRecursiveDirectoryListing(SU_PClientSocket Client,const char Path[],FFSS_LongField User) /* Path IN the share (without share name) */
{
  FS_PThreadSpecific ts;
  SU_PList Ptr;

  FFSS_PrintDebug(1,"Received a RECURSIVE DIRECTORY LISTING message\n");

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return false;
  }
  if(!FS_SendRecursiveDirectoryListing(Client,ts->Share,Path,ts->Comps,User))
  {
    SU_SEM_POST(FS_SemShr);
    FFSS_PrintDebug(1,"Error replying to client : %d\n",errno);
    return false;
  }
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnRecursiveDirectoryListing != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnRecursiveDirectoryListing(Client,Path,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return true;
}

bool OnDownload(SU_PClientSocket Client,const char Path[],FFSS_LongField StartPos,int Port,FFSS_LongField User) /* Path IN the share (without share name) */
{
  FS_PThreadSpecific ts;
  char buf[FFSS_MAX_FILEPATH_LENGTH];
  FFSS_PTransfer FT;
  FS_PConn Conn;
  SU_PList Ptr;
  bool ret;

  FFSS_PrintDebug(1,"Received a DOWNLOAD message for file %s (starting at pos %ld). Send it to port %d\n",Path,(long int)StartPos,Port);

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return false;
  }
  Conn = FS_GetConnFromTS(ts,ts->Share->Conns);
  if(Conn == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    FFSS_PrintDebug(1,"Conn not found in Share->Conns\n");
    return false;
  }
  SU_SEM_WAIT(FS_SemGbl);
  if(FS_MyGlobal.XFerInConn || Conn->XFerInConn)
  {
    if(Port != -1) /* XFer mode not supported */
    {
      SU_SEM_POST(FS_SemShr);
      ret = FS_SendMessage_Error(Client->sock,FFSS_ERROR_XFER_MODE_NOT_SUPPORTED,FFSS_ErrorTable[FFSS_ERROR_XFER_MODE_NOT_SUPPORTED],0,User);
      SU_SEM_POST(FS_SemGbl);
      return ret;
    }
  }
  if(FS_MyState != FFSS_STATE_ON)
  { /* Quiet mode - Sending error message */
    SU_SEM_POST(FS_SemShr);
    ret = FS_SendMessage_Error(Client->sock,FFSS_ERROR_SERVER_IS_QUIET,FFSS_ErrorTable[FFSS_ERROR_SERVER_IS_QUIET],0,User);
    SU_SEM_POST(FS_SemGbl);
    return ret;
  }

  if(FS_MyGlobal.MaxXFerPerConn != 0)
  {
    if(FS_XFersCount(Conn) >= FS_MyGlobal.MaxXFerPerConn)
    { /* Too many xfers - Retry */
      int retries = 0;
      bool accepted = false;
      while(retries < FS_ON_DOWNLOAD_MAX_RETRIES)
      {
        if(FS_XFersCount(Conn) < FS_MyGlobal.MaxXFerPerConn)
        {
          accepted = true;
          break;
        }
        SU_USLEEP(FS_ON_DOWNLOAD_SLEEP_RETRY);
        retries++;
      }
      if(!accepted) /* Still too many connections */
      {
        SU_SEM_POST(FS_SemShr);
        FFSS_PrintDebug(6,"Too many active xfers : %d\n",FS_XFersCount(Conn));
        ret = FS_SendMessage_Error(Client->sock,FFSS_ERROR_TOO_MANY_TRANSFERS,FFSS_ErrorTable[FFSS_ERROR_TOO_MANY_TRANSFERS],Port,User);
        SU_SEM_POST(FS_SemGbl);
        return ret;
      }
    }
  }
  SU_SEM_POST(FS_SemGbl);
  if(Port == -1) /* Xfer in sock */
  {
    if((!Conn->XFerInConn) && (Conn->XFers != NULL)) /* Can't start xfer in socket while there are active 'normal' xfers */
    {
      SU_SEM_POST(FS_SemShr);
      return FS_SendMessage_Error(Client->sock,FFSS_ERROR_XFER_MODE_NOT_SUPPORTED,FFSS_ErrorTable[FFSS_ERROR_XFER_MODE_NOT_SUPPORTED],0,User);
    }
    Conn->XFerInConn = true;
    if(Conn->TransferBuffer == NULL)
    {
      Conn->TransferBuffer = (char *) malloc(FFSS_TRANSFER_READ_BUFFER_SIZE);
      if(Conn->TransferBuffer == NULL)
      {
        SU_SEM_POST(FS_SemShr);
        FFSS_PrintDebug(1,"Cannot allocate buffer for xfers\n");
        return FS_SendMessage_Error(Client->sock,FFSS_ERROR_INTERNAL_ERROR,FFSS_ErrorTable[FFSS_ERROR_INTERNAL_ERROR],FFSS_TRANSFER_READ_BUFFER_SIZE,User);
      }
    }
  }
  if(!FS_CaseFilePath(ts->Share,(char *)Path))
  {
    SU_SEM_POST(FS_SemShr);
    FFSS_PrintDebug(1,"Couldn't open file for upload : %s (%d)\n",Path,errno);
    return FS_SendMessage_Error(Client->sock,FFSS_ERROR_FILE_NOT_FOUND,FFSS_ErrorTable[FFSS_ERROR_FILE_NOT_FOUND],2,User);
  }
#ifdef __unix__
  snprintf(buf,sizeof(buf),"%s%s",ts->Share->Path,Path);
#else /* !__unix__ */
  snprintf(buf,sizeof(buf),"%s\\%s",ts->Share->Path,(Path[0] == 0)?Path:(Path+1));
#endif /* __unix__ */
  if(!FFSS_UploadFile(Client,buf,StartPos,Port,Conn,(Port == -1),User,&FT))
  {
    SU_SEM_POST(FS_SemShr);
    FFSS_PrintDebug(1,"Error replying to client : %d\n",errno);
    return false;
  }
  if(FT == NULL) /* If FFSS_UploadFile failed to launch XFer */
  {
    /* If FFSS_UploadFile failed, it can be because index is not up-to-date */
    bool rescan = false;
#ifdef __unix__
    struct stat st;
    if(stat(buf,&st) != 0)
    {
      /* Can't stat file */
      if(errno != EACCES) /* Not acces denied.. file must not exists */
        rescan = true;
    } /* Else...file exists, so must be acces denied */
#else /* !__unix__ */
    FILE *fp;
    fp = fopen(buf,"rb");
    if(fp == NULL)
    {
      rescan = true;
    }
    else
      fclose(fp);
#endif /* __unix__ */
    if(rescan)
    {
      /* Index is not up-to-date, force rescan */
      FFSS_PrintDebug(1,"Index for %s is not up-to-date : Force a rescan (eject everybody)\n",ts->ShareName);
      FS_EjectFromShare(ts->Share,true);
      FS_RescanShare(ts->Share);
    }
    SU_SEM_POST(FS_SemShr);
    return true;
  }
  if(Port == -1)
  {
    if(!FS_InitXFerUpload(Client,FT,Path,false))
    {
      SU_SEM_POST(FS_SemShr);
      return false;
    }
  }
#ifdef DEBUG
  printf("ADD XFER %p TO CONN (%ld)\n",FT,SU_THREAD_SELF);
#endif /* DEBUG */
  Conn->XFers = SU_AddElementHead(Conn->XFers,FT);
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnDownload != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnDownload(Client,Path,StartPos,Port,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return true;
}

bool OnUpload(SU_PClientSocket Client,const char Path[],FFSS_LongField Size,int Port,FFSS_LongField User) /* Path IN the share (without share name) */
{
  SU_PList Ptr;
  FS_PThreadSpecific ts;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false); /* Get ts to check if conn to be removed */
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return false;
  }
  FFSS_PrintDebug(1,"Received an UPLOAD message\n");
  FS_SendMessage_Error(Client->sock,FFSS_ERROR_NOT_IMPLEMENTED,"Command not yet implemented",0,User);
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnUpload != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnUpload(Client,Path,Size,Port,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return true;
}

bool OnRename(SU_PClientSocket Client,const char Path[],const char NewPath[],FFSS_LongField User) /* Path IN the share (without share name) */
{
  SU_PList Ptr;
  FS_PThreadSpecific ts;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false); /* Get ts to check if conn to be removed */
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return false;
  }
  FFSS_PrintDebug(1,"Received a RENAME message\n");
  FS_SendMessage_Error(Client->sock,FFSS_ERROR_NOT_IMPLEMENTED,"Command not yet implemented",0,User);
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnRename != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnRename(Client,Path,NewPath,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return true;
}

bool OnCopy(SU_PClientSocket Client,const char Path[],const char NewPath[],FFSS_LongField User) /* Path IN the share (without share name) */
{
  SU_PList Ptr;
  FS_PThreadSpecific ts;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false); /* Get ts to check if conn to be removed */
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return false;
  }
  FFSS_PrintDebug(1,"Received a COPY message\n");
  FS_SendMessage_Error(Client->sock,FFSS_ERROR_NOT_IMPLEMENTED,"Command not yet implemented",0,User);
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnCopy != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnCopy(Client,Path,NewPath,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return true;
}

bool OnDelete(SU_PClientSocket Client,const char Path[],FFSS_LongField User) /* Path IN the share (without share name) */
{
  SU_PList Ptr;
  FS_PThreadSpecific ts;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false); /* Get ts to check if conn to be removed */
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return false;
  }
  FFSS_PrintDebug(1,"Received a DELETE message\n");
  FS_SendMessage_Error(Client->sock,FFSS_ERROR_NOT_IMPLEMENTED,"Command not yet implemented",0,User);
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnDelete != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnDelete(Client,Path,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return true;
}

bool OnMkDir(SU_PClientSocket Client,const char Path[],FFSS_LongField User) /* Path IN the share (without share name) */
{
  SU_PList Ptr;
  FS_PThreadSpecific ts;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false); /* Get ts to check if conn to be removed */
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return false;
  }
  FFSS_PrintDebug(1,"Received a MKDIR message\n");
  FS_SendMessage_Error(Client->sock,FFSS_ERROR_NOT_IMPLEMENTED,"Command not yet implemented",0,User);
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnMkDir != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnMkDir(Client,Path,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return true;
}

void OnIdleTimeout(SU_PClientSocket Client)
{
  SU_PList Ptr;
  FS_PThreadSpecific ts;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false); /* Get ts to check if conn to be removed */
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  FS_SendMessage_Error(Client->sock,FFSS_ERROR_IDLE_TIMEOUT,FFSS_ErrorTable[FFSS_ERROR_IDLE_TIMEOUT],(FFSS_LongField)(int)Client->User,ts->User); /* User = Idle time out value */
  /* OnEndThread will be called just after returning this function */
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnIdleTimeout != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnIdleTimeout(Client);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}

int OnSelect(void) /* 0=Do timed-out select ; 1=don't do timed-out select, but sleep ; 2=don't do timed-out select, and continue */
{
  FS_PThreadSpecific ts;
  FFSS_PTransfer FT;
  FS_PConn Conn;
  bool res;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(true);
  /* Getting PConn structure */
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return 0;
  }
  if(ts->Share == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return 0;
  }
  Conn = FS_GetConnFromTS(ts,ts->Share->Conns);
  if(Conn == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    printf("Couldn't find PConn in OnSelect !!!\n");
    return 0;
  }
  if(!Conn->XFerInConn) /* If use separate socket for xfers */
  {
    res = (Conn->XFers != NULL); /* Don't timeout if a xfer is active... but DO if it is a streaming */ /* 0 or 1 */
    SU_SEM_POST(FS_SemShr);
    return res;
  }
  if(Conn->XFers == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return 0;
  }
  Conn->CurrentXFer++;
  FT = (FFSS_PTransfer) SU_GetElementPos(Conn->XFers,Conn->CurrentXFer);
  if(FT == NULL)
  {
    Conn->CurrentXFer = 0;
    FT = (FFSS_PTransfer) SU_GetElementHead(Conn->XFers);
    if(FT == NULL)
    {
      SU_SEM_POST(FS_SemShr);
      printf("Error getting PTransfer structure in OnSelect !!!\n");
      return 0;
    }
  }
  SU_SEM_POST(FS_SemShr);
  res = FS_TransferBloc(FT,Conn);
  return 2;
}

void OnCancelXFer(SU_PClientSocket Server,FFSS_Field XFerTag)
{
  FS_PThreadSpecific ts;
  SU_PList Ptr;
  FFSS_PTransfer FT = NULL;
  FS_PConn Conn;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  /* Getting PConn structure */
  Conn = FS_GetConnFromTS(ts,ts->Share->Conns);
  if(Conn == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    printf("Couldn't find PConn in OnCancelXFer !!!\n");
    return;
  }
  if(!Conn->XFerInConn)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  if(Conn->XFers == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  Ptr = Conn->XFers;
  while(Ptr == NULL)
  {
    FT = (FFSS_PTransfer)  Ptr->Data;
    if(FT->XI.XFerTag == XFerTag)
      break;
  }
  if(Ptr == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    printf("Couldn't find PTransfer in OnCancelXFer !!!\n");
    return;
  }
  FT->Cancel = true;
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnCancelXFer != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnCancelXFer(Server,XFerTag);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}

/* Streaming callbacks */
void OnStrmOpen(SU_PClientSocket Client,long int Flags,const char Path[],FFSS_LongField User) /* Path IN the share (without share name) */
{
  FS_PConn Conn;
  FS_PThreadSpecific ts;
  FS_PStreaming FS;
  char flg[3];
  FILE *fp;
  char FilePath[2048];
  SU_PList Ptr;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  if(FS_MyState != FFSS_STATE_ON)
  { /* Quiet mode - Sending error message */
    SU_SEM_POST(FS_SemShr);
    FS_SendMessage_StrmOpenAnswer(Client->sock,Path,FFSS_ERROR_SERVER_IS_QUIET,0,0,User);
    return;
  }

  if(Flags & FFSS_STRM_OPEN_WRITE)
  {
    flg[0] = 'w';
    if(!ts->Writeable)
    {
      SU_SEM_POST(FS_SemShr);
      FS_SendMessage_StrmOpenAnswer(Client->sock,Path,FFSS_ERROR_ACCESS_DENIED,0,0,User);
      return;
    }
  }
  else
    flg[0] = 'r';
  if(Flags & FFSS_STRM_OPEN_TEXT)
    flg[1] = 't';
  else
    flg[1] = 'b';
  flg[2] = 0;

  if(!FS_CaseFilePath(ts->Share,(char *)Path))
  {
    SU_SEM_POST(FS_SemShr);
    FFSS_PrintDebug(1,"Couldn't open file for streaming (CaseFilePath) : %s (%d)\n",Path,errno);
    FS_SendMessage_StrmOpenAnswer(Client->sock,Path,FFSS_ERROR_FILE_NOT_FOUND,0,0,User);
    return;
  }
  snprintf(FilePath,sizeof(FilePath),"%s%s",ts->Share->Path,Path);
  fp = fopen(FilePath,flg);
  if(fp == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    FFSS_PrintDebug(1,"Couldn't open file for streaming : %s (%d)\n",FilePath,errno);
    FS_SendMessage_StrmOpenAnswer(Client->sock,Path,FFSS_ERROR_FILE_NOT_FOUND,0,0,User);
    return;
  }
  FS = (FS_PStreaming) malloc(sizeof(FS_TStreaming));
  memset(FS,0,sizeof(FS_TStreaming));
  FS->fp = fp;
  FS->Handle = FS_CurrentStrmTag++;
  FS->FileName = strdup(Path);
  fseek(FS->fp,0,SEEK_END);
  FS->fsize = ftell(FS->fp);
  rewind(FS->fp);
  if(FS_CurrentStrmTag == 0)
    FS_CurrentStrmTag++;
  FS_SendMessage_StrmOpenAnswer(Client->sock,Path,FFSS_ERROR_NO_ERROR,FS->Handle,FS->fsize,User);
  Conn = FS_GetConnFromTS(ts,ts->Share->Conns);
  if(Conn != NULL)
  {
    Conn->Strms = SU_AddElementHead(Conn->Strms,FS);
#ifdef DEBUG
    printf("OnStrmOpen : Adding Streaming to conn (Handle=%ld Size=%ld)\n",FS->Handle,(long int)FS->fsize);
#endif /* DEBUG */
  }
  else
    printf("OnStrmOpen : Conn not found !!\n");
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnStrmOpen != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnStrmOpen(Client,Flags,Path,User);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}

void OnStrmClose(SU_PClientSocket Client,FFSS_Field Handle)
{
  FS_PStreaming FS;
  FS_PConn Conn;
  FS_PThreadSpecific ts;
  SU_PList Ptr;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  Conn = FS_GetConnFromTS(ts,ts->Share->Conns);
  if(Conn != NULL)
  {
    FS = FS_GetStreamingByHandle(Conn->Strms,Handle);
    if(FS == NULL)
    {
      SU_SEM_POST(FS_SemShr);
      return;
    }
    FS_FreeStreaming(FS);
    Conn->Strms = SU_DelElementElem(Conn->Strms,FS);
#ifdef DEBUG
    printf("OnStrmClose : Removing Streaming from conn (Handle=%ld)\n",Handle);
#endif /* DEBUG */
    SU_SEM_POST(FS_SemShr);

    SU_SEM_WAIT(FS_SemPlugin);
    Ptr = FS_Plugins;
    while(Ptr != NULL)
    {
      if(((FS_PPlugin)Ptr->Data)->CB.OnStrmClose != NULL)
        ((FS_PPlugin)Ptr->Data)->CB.OnStrmClose(Client,Handle);
      Ptr = Ptr->Next;
    }
    SU_SEM_POST(FS_SemPlugin);
  }
  else
  {
    SU_SEM_POST(FS_SemShr);
    printf("OnStrmClose : Conn not found !!\n");
  }
}

void OnStrmRead(SU_PClientSocket Client,FFSS_Field Handle,FFSS_LongField StartPos,long int Length,FFSS_LongField User)
{
  FS_PStreaming FS;
  FS_PConn Conn;
  FS_PThreadSpecific ts;
  char Buffer[FFSS_STREAMING_BUFFER_SIZE];
  long int res,len;
  SU_PList Ptr;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  Conn = FS_GetConnFromTS(ts,ts->Share->Conns);
  if(Conn != NULL)
  {
    FS = FS_GetStreamingByHandle(Conn->Strms,Handle);
    if(FS == NULL)
    {
      SU_SEM_POST(FS_SemShr);
      printf("OnStrmRead : Bad handle or already closed : %ld\n",Handle);
      /* Error message */
      FS_SendMessage_StrmReadAnswer(Client->sock,FS->Handle,Buffer,0,FFSS_ERROR_BAD_HANDLE,User);
      return;
    }
    if(FS->Position != StartPos)
    {
      fseek(FS->fp,StartPos,SEEK_SET);
      FS->Position = StartPos;
    }
    len = Length;
    if(len > sizeof(Buffer))
      len = sizeof(Buffer);
    if(len < (sizeof(Buffer)/2))
      len = sizeof(Buffer)/2;
#ifdef DEBUG
    printf("Reading %ld bytes from file starting at %ld\n",len,StartPos);
#endif /* DEBUG */
    res = fread(Buffer,1,len,FS->fp);
    if(res == 0)
    {
      if(feof(FS->fp)) /* EOF */
      {
#ifdef DEBUG
        printf("OnStrmRead  : EOF\n");
#endif /* DEBUG */
        FS_SendMessage_StrmReadAnswer(Client->sock,FS->Handle,Buffer,0,FFSS_ERROR_END_OF_FILE,User);
      }
      else
      {
        printf("OnStrmRead : Error reading %ld bytes in file of handle %ld (%d:%s)\n",len,Handle,errno,strerror(errno));
        /* Error message */
        FS_SendMessage_StrmReadAnswer(Client->sock,FS->Handle,Buffer,0,FFSS_ERROR_IO_ERROR,User);
      }
      SU_SEM_POST(FS_SemShr);
      return;
    }
    FS->Position += res;
    FS_SendMessage_StrmReadAnswer(Client->sock,FS->Handle,Buffer,res,(feof(FS->fp)==0)?FFSS_ERROR_NO_ERROR:FFSS_ERROR_END_OF_FILE,User);
    SU_SEM_POST(FS_SemShr);

    SU_SEM_WAIT(FS_SemPlugin);
    Ptr = FS_Plugins;
    while(Ptr != NULL)
    {
      if(((FS_PPlugin)Ptr->Data)->CB.OnStrmRead != NULL)
        ((FS_PPlugin)Ptr->Data)->CB.OnStrmRead(Client,Handle,StartPos,Length,User);
      Ptr = Ptr->Next;
    }
    SU_SEM_POST(FS_SemPlugin);
  }
  else
  {
    SU_SEM_POST(FS_SemShr);
    printf("OnStrmRead : Conn not found !!\n");
  }
}

void OnStrmWrite(SU_PClientSocket Client,FFSS_Field Handle,FFSS_LongField StartPos,const char Bloc[],long int BlocSize,FFSS_LongField User)
{
  FS_PStreaming FS;
  FS_PConn Conn;
  FS_PThreadSpecific ts;
  SU_PList Ptr;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  Conn = FS_GetConnFromTS(ts,ts->Share->Conns);
  if(Conn != NULL)
  {
    FS = FS_GetStreamingByHandle(Conn->Strms,Handle);
    if(FS == NULL)
    {
      SU_SEM_POST(FS_SemShr);
      printf("OnStrmWrite : Bad handle or already closed : %ld\n",Handle);
      /* Error message */
      FS_SendMessage_StrmWriteAnswer(Client->sock,FS->Handle,FFSS_ERROR_BAD_HANDLE,User);
      return;
    }
    SU_SEM_POST(FS_SemShr);

    FS_SendMessage_StrmWriteAnswer(Client->sock,FS->Handle,FFSS_ERROR_NOT_IMPLEMENTED,User);

    SU_SEM_WAIT(FS_SemPlugin);
    Ptr = FS_Plugins;
    while(Ptr != NULL)
    {
      if(((FS_PPlugin)Ptr->Data)->CB.OnStrmWrite != NULL)
        ((FS_PPlugin)Ptr->Data)->CB.OnStrmWrite(Client,Handle,StartPos,Bloc,BlocSize,User);
      Ptr = Ptr->Next;
    }
    SU_SEM_POST(FS_SemPlugin);
  }
  else
  {
    SU_SEM_POST(FS_SemShr);
    printf("OnStrmWrite : Conn not found !!\n");
  }
}

void OnStrmSeek(SU_PClientSocket Client,FFSS_Field Handle,long int Flags,FFSS_LongField Pos)
{
  FS_PStreaming FS;
  FS_PConn Conn;
  FS_PThreadSpecific ts;
  SU_PList Ptr;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  Conn = FS_GetConnFromTS(ts,ts->Share->Conns);
  if(Conn != NULL)
  {
    FS = FS_GetStreamingByHandle(Conn->Strms,Handle);
    if(FS == NULL)
    {
      SU_SEM_POST(FS_SemShr);
      return;
    }
    switch(Flags)
    {
      case FFSS_SEEK_SET :
        fseek(FS->fp,Pos,SEEK_SET);
        break;
      case FFSS_SEEK_CUR :
        fseek(FS->fp,Pos,SEEK_CUR);
        break;
      case FFSS_SEEK_END :
        fseek(FS->fp,Pos,SEEK_END);
        break;
      default :
        SU_SEM_POST(FS_SemShr);
        return;
    }
    FS->Position = ftell(FS->fp);
    SU_SEM_POST(FS_SemShr);

    SU_SEM_WAIT(FS_SemPlugin);
    Ptr = FS_Plugins;
    while(Ptr != NULL)
    {
      if(((FS_PPlugin)Ptr->Data)->CB.OnStrmSeek != NULL)
        ((FS_PPlugin)Ptr->Data)->CB.OnStrmSeek(Client,Handle,Flags,Pos);
      Ptr = Ptr->Next;
    }
    SU_SEM_POST(FS_SemPlugin);
  }
  else
  {
    SU_SEM_POST(FS_SemShr);
    printf("OnStrmSeek : Conn not found !!\n");
  }
}

/* FTP callbacks */
bool OnConnectionFTP(SU_PClientSocket Client)
{
  FS_PThreadSpecific ts;
  char msg[10000];
  SU_PList Ptr;

  FFSS_PrintDebug(1,"Received a FTP CONNECTION message\n");

  /* Creates a new ts */
  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return false;
  }

#ifdef DEBUG
  printf("ADD FTP CONN\n");
#endif /* DEBUG */
  SU_SEM_WAIT(FS_SemGbl);
  FS_MyGlobal.FTPConn++;
  if(FS_MyGlobal.FTPMaxConn != 0)
  {
    if(FS_MyGlobal.FTPConn > FS_MyGlobal.FTPMaxConn)
    {
      SU_SEM_POST(FS_SemShr);
      snprintf(msg,sizeof(msg),"421 Service not available, remote server has closed connection" CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      SU_SEM_POST(FS_SemGbl);
      return false;
    }
  }
  SU_SEM_POST(FS_SemGbl);

  ts->Type = 'B';
  SU_strcpy(ts->Path,"/",sizeof(ts->Path));
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnConnectionFTP != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnConnectionFTP(Client);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return true;
}

void OnPWDFTP(SU_PClientSocket Client)
{
  FS_PThreadSpecific ts;
  char msg[10000];
  char tspath[FFSS_MAX_PATH_LENGTH];
  SU_PList Ptr;

  FFSS_PrintDebug(1,"Received a FTP PWD message\n");

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  SU_strcpy(tspath,ts->Path,sizeof(tspath));
  if(tspath[1] != 0)
  {
    if(tspath[strlen(tspath)-1] == '/')
      tspath[strlen(tspath)-1] = 0;
  }
  snprintf(msg,sizeof(msg),"257 \"%s\" is current directory." CRLF,tspath);
  send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnPWDFTP != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnPWDFTP(Client);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}

void OnTypeFTP(SU_PClientSocket Client,const char Type)
{
  FS_PThreadSpecific ts;
  char msg[10000];
  SU_PList Ptr;

  FFSS_PrintDebug(1,"Received a FTP TYPE message\n");

  switch(Type)
  {
    case 'A' :
      snprintf(msg,sizeof(msg),"200 Type set to A." CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      break;
    case 'I'  :
      snprintf(msg,sizeof(msg),"200 Type set to I." CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      break;
    default :
      snprintf(msg,sizeof(msg),"504 TYPE %c not supported" CRLF,Type);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
  }
  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  ts->Type = Type;
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnTypeFTP != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnTypeFTP(Client,Type);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}

void OnModeFTP(SU_PClientSocket Client,const char Mode)
{
//  FS_PThreadSpecific ts;
  char msg[10000];
  SU_PList Ptr;

  FFSS_PrintDebug(1,"Received a FTP MODE message\n");

  switch(Mode)
  {
    case 'S' :
      snprintf(msg,sizeof(msg),"200 Mode set to S." CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      break;
    default :
      snprintf(msg,sizeof(msg),"504 MODE %c not supported" CRLF,Mode);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
  }
/*  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  ts->Type = Type;*/

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnModeFTP != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnModeFTP(Client,Mode);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}

bool OnDirectoryListingFTP(SU_PClientSocket Client,SU_PClientSocket DataPort,const char Path[])
{
  FS_PThreadSpecific ts;
  char msg[10000];
  char tspath[FFSS_MAX_PATH_LENGTH];
  char *p;
#ifndef _WIN32
  char *tmp;
#endif /* !_WIN32 */
  char Tim[100];
  SU_PList Ptr;
  FS_PShare Share;
  FS_PNode Node;
  struct tm *Tm;

  FFSS_PrintDebug(1,"Received a LIST message for %s\n",(Path == NULL)?"cwd":Path);

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return false;
  }
  snprintf(msg,sizeof(msg),"drwxr-xr-x    1 nobody   nobody       1024 Jan  1 00:00 ." CRLF);
  send(DataPort->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  snprintf(msg,sizeof(msg),"drwxr-xr-x    1 nobody   nobody       1024 Jan  1 00:00 .." CRLF);
  send(DataPort->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  if(strcmp(ts->Path,"/") == 0)
  {
    /* FTP root directory requested - Listing all shares */
    Ptr = FS_Index;
    while(Ptr != NULL)
    {
      snprintf(msg,sizeof(msg),"drwxr-xr-x    1 nobody   nobody       1024 Jan  1 00:00 %s" CRLF,((FS_PShare)Ptr->Data)->ShareName);
      send(DataPort->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      Ptr = Ptr->Next;
    }
    SU_SEM_POST(FS_SemShr);

    SU_SEM_WAIT(FS_SemPlugin);
    Ptr = FS_Plugins;
    while(Ptr != NULL)
    {
      if(((FS_PPlugin)Ptr->Data)->CB.OnDirectoryListingFTP != NULL)
        ((FS_PPlugin)Ptr->Data)->CB.OnDirectoryListingFTP(Client,DataPort,Path);
      Ptr = Ptr->Next;
    }
    SU_SEM_POST(FS_SemPlugin);
    return true;
  }
  SU_strcpy(tspath,ts->Path,sizeof(tspath));
  p = strtok_r(tspath,"/",&tmp);
  Share = FS_GetShareFromName(p);
  if(Share == NULL)
  { /* Share not found - Aborting */
    SU_SEM_POST(FS_SemShr);
    return false;
  }
  p = strtok_r(NULL,"/",&tmp);
  Node = &Share->Root;
  while(p != NULL)
  {
    Ptr = Node->Dirs;
    while(Ptr != NULL)
    {
      if(SU_strcasecmp(p,((FS_PDir)Ptr->Data)->DirName))
        break;
      Ptr = Ptr->Next;
    }
    if(Ptr == NULL)
    { /* Path not found in share - Aborting */
      SU_SEM_POST(FS_SemShr);
      return false;
    }
    Node = &((FS_PDir)Ptr->Data)->Files;
    p = strtok_r(NULL,"/",&tmp);
  }

  /* Ok, got the directory structure... listing */
  Ptr = Node->Dirs;
  while(Ptr != NULL)
  {
    Tm = localtime(&((FS_PDir)Ptr->Data)->Time);
    snprintf(Tim,sizeof(Tim),"%s %2d %2d:%2d",FS_TimeTable[Tm->tm_mon],Tm->tm_mday,Tm->tm_hour,Tm->tm_min);
    snprintf(msg,sizeof(msg),"drwxr-xr-x    1 nobody   nobody       1024 %s %s" CRLF,Tim,((FS_PDir)Ptr->Data)->DirName);
    send(DataPort->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    Ptr = Ptr->Next;
  }
  Ptr = Node->Files;
  while(Ptr != NULL)
  {
    Tm = localtime(&((FS_PFile)Ptr->Data)->Time);
    snprintf(Tim,sizeof(Tim),"%s %2d %2d:%2d",FS_TimeTable[Tm->tm_mon],Tm->tm_mday,Tm->tm_hour,Tm->tm_min);
    snprintf(msg,sizeof(msg),"-rw%cr-%cr-%c    1 nobody   nobody   %8ld %s %s" CRLF,(((FS_PFile)Ptr->Data)->Flags & FFSS_FILE_EXECUTABLE)?'x':'-',(((FS_PFile)Ptr->Data)->Flags & FFSS_FILE_EXECUTABLE)?'x':'-',(((FS_PFile)Ptr->Data)->Flags & FFSS_FILE_EXECUTABLE)?'x':'-',(long int)((FS_PFile)Ptr->Data)->Size,Tim,((FS_PFile)Ptr->Data)->FileName);
    send(DataPort->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemShr);

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnDirectoryListingFTP != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnDirectoryListingFTP(Client,DataPort,Path);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return true;
}

/* Assumes FS_SemShr is locked */
FS_PNode FS_GetNodeFromPath(FS_PShare Share,const char Path[]) /* Path in the share */
{
  char tspath[FFSS_MAX_PATH_LENGTH];
  char *p;
#ifndef _WIN32
  char *tmp;
#endif /* !_WIN32 */
  FS_PNode Node;
  SU_PList Ptr;

  if(Path == NULL)
    return &Share->Root;
  SU_strcpy(tspath,Path,sizeof(tspath));
  p = strtok_r(tspath,"/",&tmp);
  Node = &Share->Root;
  while(p != NULL)
  {
    Ptr = Node->Dirs;
    while(Ptr != NULL)
    {
      if(SU_strcasecmp(p,((FS_PDir)Ptr->Data)->DirName))
        break;
      Ptr = Ptr->Next;
    }
    if(Ptr == NULL)
      return NULL;
    Node = &((FS_PDir)Ptr->Data)->Files;
    p = strtok_r(NULL,"/",&tmp);
  }
  return Node;
}

/* Assumes FS_SemShr is locked */
FS_PFile FS_GetFileFromPath(FS_PShare Share,const char Path[]) /* Path in the share */
{
  char tspath[FFSS_MAX_PATH_LENGTH];
  char *p,*q;
#ifndef _WIN32
  char *tmp;
#endif /* !_WIN32 */
  FS_PNode Node;
  SU_PList Ptr;

  if(Path == NULL)
    return NULL;
  SU_strcpy(tspath,Path,sizeof(tspath));
  p = strtok_r(tspath,"/",&tmp);
  Node = &Share->Root;
  while(p != NULL)
  {
    q = strtok_r(NULL,"/",&tmp);
    if(q == NULL)
      break;
    Ptr = Node->Dirs;
    while(Ptr != NULL)
    {
      if(SU_strcasecmp(p,((FS_PDir)Ptr->Data)->DirName))
        break;
      Ptr = Ptr->Next;
    }
    if(Ptr == NULL)
      return NULL;
    Node = &((FS_PDir)Ptr->Data)->Files;
    p = q;
  }
  if(p == NULL)
    return NULL;
  Ptr = Node->Files;
  while(Ptr != NULL)
  {
    if(SU_strcasecmp(p,((FS_PFile)Ptr->Data)->FileName))
      return (FS_PFile)Ptr->Data;
    Ptr = Ptr->Next;
  }
  return NULL;
}

void OnCWDFTP(SU_PClientSocket Client,const char Path[])
{
  FS_PThreadSpecific ts;
  char msg[10000];
  char tspath[FFSS_MAX_PATH_LENGTH];
  char New[FFSS_MAX_PATH_LENGTH];
  char *p,*q;
#ifndef _WIN32
  char *tmp;
#endif /* !_WIN32 */
  SU_PList Ptr;
  FS_PShare Share;
  FS_PNode Node;

  FFSS_PrintDebug(1,"Received a CWD message for %s\n",Path);

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }
  if(Path[0] == '/') /* Absolute Path */
  {
    SU_strcpy(New,Path,sizeof(New));
    p = strtok_r(New,"/",&tmp);
    if(p == NULL) /* Root directory */
    {
      SU_strcpy(ts->Path,"/",sizeof(ts->Path));
      SU_SEM_POST(FS_SemShr);
      snprintf(msg,sizeof(msg),"200 CWD command successful." CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
    }
    Share = FS_GetShareFromName(p);
    if(Share == NULL)
    { /* Share not found - Aborting */
      SU_SEM_POST(FS_SemShr);
      snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
    }
    p = strtok_r(NULL,"\0",&tmp);
    Node = FS_GetNodeFromPath(Share,p);
    if(Node == NULL)
    { /* Path not found in share - Aborting */
      SU_SEM_POST(FS_SemShr);
      snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
    }
    /* CWD successful - Updating Path and sending reply */
    SU_strcpy(ts->Path,Path,sizeof(ts->Path));
    ts->Share = Share;
    snprintf(msg,sizeof(msg),"200 CWD command successful." CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  }
  else /* Relative path */
  {
    SU_strcpy(New,Path,sizeof(New));
    SU_strcpy(tspath,ts->Path,sizeof(tspath));
    q = strrchr(tspath,'/');
    q[0] = 0;
    p = strtok_r(New,"/",&tmp);
    while(p != NULL)
    {
      if(strcmp(p,"..") == 0) /* Check backward */
      {
        q = strrchr(tspath,'/');
        if(q == NULL)
        {
          SU_SEM_POST(FS_SemShr);
          snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
          send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
          return;
        }
        q[0] = 0;
        p = strtok_r(NULL,"/",&tmp);
      }
      else
        break;
    }
    SU_strcat(tspath,"/",sizeof(tspath));
    if(strcmp(tspath,"/") == 0) /* Root directory */
    {
      if(p == NULL) /* Root requested */
      {
        /* CWD successful - Updating Path and sending reply */
        ts->Share = NULL;
        SU_strcpy(ts->Path,"/",sizeof(ts->Path));
        SU_SEM_POST(FS_SemShr);
        snprintf(msg,sizeof(msg),"200 CWD command successful." CRLF);
        send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
        return;
      }
      Share = FS_GetShareFromName(p);
      if(Share == NULL)
      { /* Share not found - Aborting */
        SU_SEM_POST(FS_SemShr);
        snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
        send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
        return;
      }
      SU_strcat(tspath,p,sizeof(tspath));
      SU_strcat(tspath,"/",sizeof(tspath));
      p = strtok_r(NULL,"/",&tmp);
    }
    else
      Share = ts->Share;
    q = strchr(tspath+1,'/');
    if(q == NULL)
      Node = NULL;
    else
      Node = FS_GetNodeFromPath(Share,q+1);
    if((Node == NULL) && (p != NULL))
    {
      SU_SEM_POST(FS_SemShr);
      snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
    }
    while(p != NULL)
    {
      Ptr = Node->Dirs;
      while(Ptr != NULL)
      {
        if(SU_strcasecmp(p,((FS_PDir)Ptr->Data)->DirName))
          break;
        Ptr = Ptr->Next;
      }
      if(Ptr == NULL)
      { /* Path not found in share - Aborting */
        SU_SEM_POST(FS_SemShr);
        snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
        send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
        return;
      }
      Node = &((FS_PDir)Ptr->Data)->Files;
      SU_strcat(tspath,p,sizeof(tspath));
      SU_strcat(tspath,"/",sizeof(tspath));
      p = strtok_r(NULL,"/",&tmp);
    }
    /* CWD successful - Updating Path and sending reply */
    ts->Share = Share;
    SU_strcpy(ts->Path,tspath,sizeof(ts->Path));
    snprintf(msg,sizeof(msg),"200 CWD command successful." CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  }
  SU_SEM_POST(FS_SemShr);
}

void FS_FreeTransferFTP(FS_PTransferFTP TFTP)
{
  if(TFTP->fp != NULL)
    fclose(TFTP->fp);
  if(TFTP->LocalPath != NULL)
    free(TFTP->LocalPath);
  SU_FreeCS(TFTP->Client);
  free(TFTP);
}

SU_THREAD_ROUTINE(FS_DownloadFileFunc,Info)
{
  char msg[10000];
  FS_PTransferFTP FT = (FS_PTransferFTP) Info;
  fd_set rfds;
  struct timeval tv;
  int retval,res,len;
  char *RBuf;
  unsigned long int total=0,rpos=0,rlen;
  FFSS_Field fsize;

  SU_ThreadBlockSigs();
  fseek(FT->fp,0,SEEK_END);
  fsize = ftell(FT->fp);
  rewind(FT->fp);

  RBuf = (char *) malloc(FFSS_TRANSFER_READ_BUFFER_SIZE);
  if(RBuf == NULL)
  {
    FFSS_PrintDebug(1,"Cannot allocate buffer in Upload function\n");
    snprintf(msg,sizeof(msg),"426 Memory allocation error.\n");
    send(FT->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    FS_FreeTransferFTP(FT);
    SU_END_THREAD(NULL);
  }

  while(total < fsize)
  {
    if((total+FFSS_TRANSFER_READ_BUFFER_SIZE) <= fsize)
      rlen = FFSS_TRANSFER_READ_BUFFER_SIZE;
    else
      rlen = fsize - total;
    if(fread(RBuf,1,rlen,FT->fp) != rlen)
    {
      FFSS_PrintDebug(1,"Error reading file while uploading : %d\n",errno);
      snprintf(msg,sizeof(msg),"426 Error reading file.\n");
      send(FT->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      FS_FreeTransferFTP(FT);
      free(RBuf);
      SU_END_THREAD(NULL);
    }
    rpos = 0;
    while(rpos < rlen)
    {
      if((rpos+FFSS_TRANSFER_BUFFER_SIZE) <= rlen)
        len = FFSS_TRANSFER_BUFFER_SIZE;
      else
        len = rlen - rpos;
      FD_ZERO(&rfds);
      FD_SET(FT->Client->sock,&rfds);
      tv.tv_sec = FFSS_TIMEOUT_TRANSFER;
      tv.tv_usec = 0;
      retval = select(FT->Client->sock+1,NULL,&rfds,NULL,&tv);
      if(!retval)
      {
        FFSS_PrintDebug(1,"Transfer timed out while uploading file\n");
        snprintf(msg,sizeof(msg),"426 Transfer timed out.\n");
        send(FT->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
        FS_FreeTransferFTP(FT);
        free(RBuf);
        SU_END_THREAD(NULL);
      }
      res = send(FT->Client->sock,RBuf+rpos,len,SU_MSG_NOSIGNAL);
      if(res != len)
      {
        FFSS_PrintDebug(1,"Error while uploading file : %d\n",errno);
        snprintf(msg,sizeof(msg),"426 Error sending file.\n");
        send(FT->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
        FS_FreeTransferFTP(FT);
        free(RBuf);
        SU_END_THREAD(NULL);
      }
      /* Faire ici ce qu'on veut... on vient d'envoyer un paquet */
      rpos += len;
    }
    total += rlen;
  }

  snprintf(msg,sizeof(msg),"226 Transfer complete.\n");
  send(FT->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  FS_FreeTransferFTP(FT);
  SU_END_THREAD(NULL);
}

void OnDownloadFTP(SU_PClientSocket Client,const char Path[],FFSS_LongField StartPos,const char Host[],const char Port[])
{
  SU_PClientSocket DataPort;
  char msg[10000];
  FS_PTransferFTP TFTP;
  FILE *fp;
  SU_THREAD_HANDLE Thread;
  FS_PThreadSpecific ts;
  char *p,*q;
#ifndef _WIN32
  char *tmp;
#endif /* !_WIN32 */
  FS_PShare Share;
  FS_PFile File;
  char tspath[FFSS_MAX_PATH_LENGTH];
  char New[FFSS_MAX_PATH_LENGTH];
  char New2[FFSS_MAX_PATH_LENGTH];
  char FPath[FFSS_MAX_PATH_LENGTH];
  SU_PList Ptr;

  SU_SEM_WAIT(FS_SemShr);
  ts = FS_GetThreadSpecific(false);
  if(ts == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    return;
  }

  if(Path[0] == '/') /* Absolute Path */
  {
    SU_strcpy(New,Path,sizeof(New));
    p = strtok_r(New,"/",&tmp);
    if(p == NULL) /* Root directory */
    {
      SU_SEM_POST(FS_SemShr);
      snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
    }
    Share = FS_GetShareFromName(p);
    if(Share == NULL)
    { /* Share not found - Aborting */
      SU_SEM_POST(FS_SemShr);
      snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
    }
    p = strtok_r(NULL,"\0",&tmp);
    File = FS_GetFileFromPath(Share,p);
    if(File == NULL)
    { /* Path not found in share - Aborting */
      SU_SEM_POST(FS_SemShr);
      snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
    }
    snprintf(FPath,sizeof(FPath),"%s/%s",Share->Path,p);
  }
  else /* Relative path */
  {
    SU_strcpy(New,Path,sizeof(New));
    SU_strcpy(tspath,ts->Path,sizeof(tspath));
    q = strrchr(tspath,'/');
    q[0] = 0;
    p = strtok_r(New,"/",&tmp);
    while(p != NULL)
    {
      if(strcmp(p,"..") == 0) /* Check backward */
      {
        q = strrchr(tspath,'/');
        if(q == NULL)
        {
          SU_SEM_POST(FS_SemShr);
          snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
          send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
          return;
        }
        q[0] = 0;
        p = strtok_r(NULL,"/",&tmp);
      }
      else
        break;
    }
    SU_strcat(tspath,"/",sizeof(tspath));
    if(strcmp(tspath,"/") == 0) /* Root directory */
    {
      if(p == NULL) /* Root requested */
      {
        SU_SEM_POST(FS_SemShr);
        snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
        send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
        return;
      }
      Share = FS_GetShareFromName(p);
      if(Share == NULL)
      { /* Share not found - Aborting */
        SU_SEM_POST(FS_SemShr);
        snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
        send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
        return;
      }
      SU_strcat(tspath,p,sizeof(tspath));
      SU_strcat(tspath,"/",sizeof(tspath));
      p = strtok_r(NULL,"/",&tmp);
    }
    else
      Share = ts->Share;
    q = strchr(tspath+1,'/');
    if(q == NULL)
    { /* Path not found in share - Aborting */
      SU_SEM_POST(FS_SemShr);
      snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
    }
    snprintf(New2,sizeof(New2),"%s%s",q,p);
    p = strtok_r(NULL,"\0",&tmp);
    if(p != NULL)
    {
      SU_strcat(New2,"/",sizeof(New2));
      SU_strcat(New2,p,sizeof(New2));
    }
    File = FS_GetFileFromPath(Share,New2);
    if(File == NULL)
    { /* Path not found in share - Aborting */
      SU_SEM_POST(FS_SemShr);
      snprintf(msg,sizeof(msg),"550 %s: no such file or directory." CRLF,Path);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
    }
    snprintf(FPath,sizeof(FPath),"%s%s",Share->Path,New2);
  }

  if(ts->Type == 'I')
    fp = fopen(FPath,"rb");
  else
    fp = fopen(FPath,"rt");
  if(fp == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    snprintf(msg,sizeof(msg),"550 File %s not found." CRLF,Path);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    return;
  }
  snprintf(msg,sizeof(msg),"150 Opening %s mode data connection for /bin/ls." CRLF,(ts->Type == 'I')?"BINARY":"ASCII");
  send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  DataPort = SU_ClientConnect((char *)Host,(char *)Port,SOCK_STREAM);
  if(DataPort == NULL)
  {
    SU_SEM_POST(FS_SemShr);
    fclose(fp);
    snprintf(msg,sizeof(msg),"425 Can't open data connection to %s:%s : %d." CRLF,Host,Port,errno);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    return;
  }
  else
  {
    TFTP = (FS_PTransferFTP) malloc(sizeof(FS_TTransferFTP));
    memset(TFTP,0,sizeof(FS_TTransferFTP));
    TFTP->sock = Client->sock;
    TFTP->fp = fp;
    TFTP->LocalPath = strdup(Path);
    TFTP->StartingPos = StartPos;
    TFTP->Client = DataPort;
    if(!SU_CreateThread(&Thread,FS_DownloadFileFunc,(void *)TFTP,true))
    {
      SU_SEM_POST(FS_SemShr);
      FS_FreeTransferFTP(TFTP);
      snprintf(msg,sizeof(msg),"426 Error creating upload thread." CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      return;
    }
    SU_SEM_POST(FS_SemShr);

    SU_SEM_WAIT(FS_SemPlugin);
    Ptr = FS_Plugins;
    while(Ptr != NULL)
    {
      if(((FS_PPlugin)Ptr->Data)->CB.OnDownloadFTP != NULL)
        ((FS_PPlugin)Ptr->Data)->CB.OnDownloadFTP(Client,Path,StartPos,Host,Port);
      Ptr = Ptr->Next;
    }
    SU_SEM_POST(FS_SemPlugin);
  }
}

void OnIdleTimeoutFTP(SU_PClientSocket Client)
{
  char msg[10000];
  SU_PList Ptr;

  snprintf(msg,sizeof(msg),"421 Idle time out." CRLF);
  send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  /* OnEndThread will be called just after returning this function */

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->CB.OnIdleTimeoutFTP != NULL)
      ((FS_PPlugin)Ptr->Data)->CB.OnIdleTimeoutFTP(Client);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
}

bool FS_PowerUp(const char IntName[])
{
  if(!FS_Init(FFSS_SERVER_PORT,FS_MyGlobal.FTP))
    return false;
  if(FS_MyGlobal.MyIP == NULL)
  {
    if(!FFSS_GetMyIP(FS_SI_TCP,IntName))
    {
      FFSS_PrintSyslog(LOG_ERR,"Couldn't find interface %s. Exiting\n",IntName);
      FS_UnInit();
      return false;
    }
    FS_MyGlobal.MyIP = FFSS_MyIP;
  }
  else
  {
#ifdef DEBUG
    printf("Forcing IP %s\n",FS_MyGlobal.MyIP);
#endif /* DEBUG */
  }
#ifdef __linux__
  if(FS_MyGlobal.LimitedBind)
  {
    if(getuid() == 0)
    {
      FFSS_PrintSyslog(LOG_WARNING,"Entering limited bind mode to %s\n",IntName);
      if(setsockopt(FS_SI_TCP->sock,SOL_SOCKET,SO_BINDTODEVICE,IntName,strlen(IntName)+1) == SOCKET_ERROR)
        FFSS_PrintSyslog(LOG_WARNING,"Warnig : Cannot bind only for %s for TCP socket : %s\n",IntName,strerror(errno));
      if(setsockopt(FS_SI_UDP->sock,SOL_SOCKET,SO_BINDTODEVICE,IntName,strlen(IntName)+1) == SOCKET_ERROR)
        FFSS_PrintSyslog(LOG_WARNING,"Warnig : Cannot bind only for %s for UDP socket : %s\n",IntName,strerror(errno));
      if(FS_MyGlobal.FTP)
      {
        if(setsockopt(FS_SI_TCP_FTP->sock,SOL_SOCKET,SO_BINDTODEVICE,IntName,strlen(IntName)+1) == SOCKET_ERROR)
          FFSS_PrintSyslog(LOG_WARNING,"Warnig : Cannot bind only for %s for TCP_FTP socket : %s\n",IntName,strerror(errno));
      }
    }
    else
      FFSS_PrintSyslog(LOG_WARNING,"Warning : Server launched from a non-root user. Cannot bind to %s only\n",IntName);
  }
#endif /* __linux__ */
  FFSS_PrintDebug(1,"Server running...\n");
  FS_RealBuildIndex();
  if(FS_MyGlobal.Master != NULL)
  {
    /* Sending login message to my master */
    FS_SendMessage_State(FS_MyGlobal.Master,FS_MyGlobal.Name,FFSS_GetOS(),FS_MyGlobal.Comment,FFSS_STATE_ON);
    /* Sending index message to my master */
    FS_SendIndex(FS_MyGlobal.Master,FFSS_MASTER_PORT_S);
    /* Sending stats message to my master */
  }
  FS_MyState = FFSS_STATE_ON;
  return true;
}

bool FS_ShutDown()
{
  FS_MyState = FFSS_STATE_OFF;
  /* Remove all plugins */
  FS_UnLoadAllPlugin();
  if(FS_MyGlobal.Master != NULL)
  {
    /* Sending logout message to my master */
    FS_SendMessage_State(FS_MyGlobal.Master,FS_MyGlobal.Name,FFSS_GetOS(),FS_MyGlobal.Comment,FS_MyState);
  }
  /* Calling arch dep shut down routine */
  FS_ShuttingDown();

  /* Shutting down server */
  FS_UnInit();
  return true;
}

void OnTransferFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download)
{
#ifdef DEBUG
  SU_PList Ptr;
#endif /* DEBUG */

  /* Remove XFer from Conn (Conn in FT->User) */
  if(FT->User != NULL)
  {
    SU_SEM_WAIT(FS_SemShr);
#ifdef DEBUG
    Ptr = ((FS_PConn)FT->User)->XFers;
    while(Ptr != NULL)
    {
      if(Ptr->Data == FT)
        break;
      Ptr = Ptr->Next;
    }
    if(Ptr != NULL)
    {
      printf("REMOVE XFER %p FORM CONN %d (%ld)\n",FT,SU_ListCount(((FS_PConn)FT->User)->XFers),SU_THREAD_SELF);
      ((FS_PConn)FT->User)->XFers = SU_DelElementElem(((FS_PConn)FT->User)->XFers,FT);
    }
    else
    {
      printf("HUMMMMMMMMMMMM : XFer %p not found in OnTransferFailed !!!!!!\n",FT);
      abort();
    }
#else /* !DEBUG */
    ((FS_PConn)FT->User)->XFers = SU_DelElementElem(((FS_PConn)FT->User)->XFers,FT);
#endif /* DEBUG */
    FS_CheckConnectionForRemoval((FS_PConn)FT->User,((FS_PConn)FT->User)->Share);
    SU_SEM_POST(FS_SemShr);
  }
#ifdef DEBUG
  else
    printf("REMOVE XFER %p\n",FT);
#endif /* DEBUG */
}

void OnTransferSuccess(FFSS_PTransfer FT,bool Download)
{
#ifdef DEBUG
  SU_PList Ptr;
#endif /* DEBUG */

  /* Remove XFer from Conn (in FT->User) */
  if(FT->User != NULL)
  {
    SU_SEM_WAIT(FS_SemShr);
#ifdef DEBUG
    Ptr = ((FS_PConn)FT->User)->XFers;
    while(Ptr != NULL)
    {
      if(Ptr->Data == FT)
        break;
      Ptr = Ptr->Next;
    }
    if(Ptr != NULL)
    {
      //printf("REMOVE XFER %p FORM CONN (%ld)\n",FT,SU_THREAD_SELF);
      printf("REMOVE XFER %p FORM CONN %d SUCCESS (%ld)\n",FT,SU_ListCount(((FS_PConn)FT->User)->XFers),SU_THREAD_SELF);
      ((FS_PConn)FT->User)->XFers = SU_DelElementElem(((FS_PConn)FT->User)->XFers,FT);
    }
    else
    {
      printf("HUMMMMMMMMMMMM : XFer %p not found in OnTransferSuccess !!!!!!\n",FT);
      abort();
    }
#else /* !DEBUG */
    ((FS_PConn)FT->User)->XFers = SU_DelElementElem(((FS_PConn)FT->User)->XFers,FT);
#endif /* DEBUG */
    FS_CheckConnectionForRemoval((FS_PConn)FT->User,((FS_PConn)FT->User)->Share);
    SU_SEM_POST(FS_SemShr);
  }
#ifdef DEBUG
  else
    printf("REMOVE XFER %p\n",FT);
#endif /* DEBUG */
}

void OnTransferActive(FFSS_PTransfer FT,long int Amount,bool Download)
{
}


SU_THREAD_ROUTINE(FS_ConfFunc,Info);

char *FS_CheckGlobal(void)
{
  char *ip;
  SU_THREAD_HANDLE Thread;
  SU_PServerInfo SI;

  FS_MyGlobal.Conn = 0;
  FS_MyGlobal.FTPConn = 0;
  if(FS_MyGlobal.Master != NULL)
  {
    ip = SU_AdrsOfPort(FS_MyGlobal.Master);
    if(ip == NULL)
    {
      FFSS_PrintSyslog(LOG_ERR,"Cannot find master's ip : %s\n",FS_MyGlobal.Master);
      FS_UnLoadAllPlugin();
      FS_ShuttingDown();
      exit(-2);
    }
    FS_MyGlobal.MasterIP = strdup(SU_AdrsOfPort(FS_MyGlobal.Master));
  }
  if(FS_MyGlobal.Name == NULL)
    return "No Name for the server has been defined";
  if(FS_MyGlobal.Comment == NULL)
    return "No Comment for the server has been defined";
  if((FS_MyGlobal.ConfSock) && (FS_MyState == FFSS_STATE_OFF))
  {
    SI = SU_CreateServer(FFSS_SERVER_CONF_PORT,SOCK_STREAM,false);
    if(SI != NULL)
    {
      if(SU_ServerListen(SI) == SOCKET_ERROR)
      {
        FFSS_PrintSyslog(LOG_WARNING,"Cannot create listening socket for runtime configuration (listen)\n");
        SU_FreeSI(SI);
      }
      else
      {
        if(!SU_CreateThread(&Thread,FS_ConfFunc,(void *)SI,true))
        {
          FFSS_PrintSyslog(LOG_WARNING,"Cannot create listening socket for runtime configuration (thread)\n");
          SU_FreeSI(SI);
        }
      }
    }
    else
      FFSS_PrintSyslog(LOG_WARNING,"Cannot create listening socket for runtime configuration (socket)\n");
  }
  return NULL;
}

void PrintHelp(void)
{
  printf("Usage : ffss-server [options]\n");
  printf("Options : -h or --help : This\n");
  printf("          -v or --version : Prints server version and exits\n");
  printf("          -d or --daemon   : Daemonize the server\n");
  printf("          -c <config file> : Loads this configuration file, instead of the default one \"%s\"\n",CONFIG_FILE_NAME);
  exit(0);
}

void handint(int sig)
{
  static bool done = false;

  if(!done)
  {
    done = true;
    FFSS_PrintSyslog(LOG_ERR,"Received a %d signal in %d\n",sig,getpid());
    memset(&FFSS_CB.SCB,0,sizeof(FFSS_CB.SCB));
    /* Shutting down server */
    FS_ShutDown();
    FFSS_PrintDebug(1,"exiting\n");
    _exit(0);
  }
  else
    FFSS_PrintSyslog(LOG_WARNING,"Signal handler in %d (%d) : Server is being shut down... please wait\n",getpid(),sig);
}

#ifdef _WIN32
LRESULT CALLBACK FS_wndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  switch(message)
  {
    case WM_DESTROY:
      handint(15);
      return 0;
    case WM_CLOSE:
      handint(15);
      return 0;
  }
  return DefWindowProc(hwnd, message, wParam, lParam);
}

void ThreadFunc(void *info)
{
  MSG msg;
  WNDCLASS wc;

  wc.style = 0;
  wc.lpfnWndProc = FS_wndProc;
  wc.cbClsExtra = wc.cbWndExtra = 0;
  wc.hInstance = FS_hInstance;
  wc.hIcon = LoadIcon(FS_hInstance, MAKEINTRESOURCE(101));
  wc.hCursor = LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
  wc.lpszMenuName = NULL;
  wc.lpszClassName = "FFSSServer";
  if(RegisterClass(&wc) == 0)
    return;
  FS_hwnd = CreateWindow("FFSSServer","FFSS Server", WS_POPUP, CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, NULL, NULL, FS_hInstance, NULL);
  if(FS_hwnd == NULL)
    return;

  while(GetMessage(&msg,FS_hwnd,0,0))
  {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
  /* Application received a WM_CLOSE message */
  handint(15);
}

bool FS_InitWindow(void)
{
  DWORD tmp;

  /* Create a thread to manage messages to our virtual window */
  if(CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThreadFunc,NULL,0,&tmp) == NULL)
    return false;
  return true;
}

#ifdef DEBUG
int main(int argc,char *argv[])
#else /* !DEBUG */
int APIENTRY WinMain(HINSTANCE hInstance,HINSTANCE hPrevInstance,LPSTR lpCmdLine,int nCmdShow)
#endif /* DEBUG */
#else /* !_WIN32 */
int main(int argc,char *argv[])
#endif /* _WIN32 */
{
  char *Error;
#ifdef __unix__
  int i;
#endif /* __unix__ */
  char ConfigFile[1024];
  bool daemonize = false;

  printf("FFSS Server v%s (c) Ze KiLleR / SkyTech 2001'02\n",FFSS_SERVER_VERSION);
  printf("%s\n",FFSS_COPYRIGHT);

  if(FS_IsAlreadyRunning())
  {
#ifdef __unix__
    printf("FFSS Server is already running... exiting\n");
#else /* !__unix__ */
    MessageBox(NULL,"FFSS Server is already running","FFSS Server",MB_OK | MB_ICONEXCLAMATION);
#endif /* __unix__ */
    return -1;
  }

#ifdef __unix__
  SU_strcpy(ConfigFile,CONFIG_FILE_NAME,sizeof(ConfigFile));
  if(argc != 1)
  {
    i = 1;
    while(i<argc)
    {
      if(strcmp(argv[i],"--help") == 0)
        PrintHelp();
      else if(strcmp(argv[i],"-h") == 0)
        PrintHelp();
      else if(strcmp(argv[i],"--version") == 0)
        return 0;
      else if(strcmp(argv[i],"-v") == 0)
        return 0;
      else if(strcmp(argv[i],"-d") == 0)
        daemonize = true;
      else if(strcmp(argv[i],"--daemon") == 0)
        daemonize = true;
      else if(strcmp(argv[i],"-c") == 0)
      {
        if(argc <= (i+1))
          PrintHelp();
        SU_strcpy(ConfigFile,argv[i+1],sizeof(ConfigFile));
        i++;
      }
      else
      {
        printf("Unkown option : %s\n",argv[i]);
        PrintHelp();
      }
      i++;
    }
  }

  if(daemonize)
  {
    openlog("Ffss Server",0,LOG_DAEMON);
    FFSS_PrintSyslog(LOG_INFO,"Server started\n");
    if(SU_Daemonize() == false)
    {
      FFSS_PrintSyslog(LOG_ERR,"Cannot daemonize process");
      return -1;
    }
  }
  else
  {
    openlog("Ffss Server",LOG_PERROR,LOG_USER);
    FFSS_PrintSyslog(LOG_INFO,"Server started with pid %d\n",getpid());
  }
#else /* !__unix__ */
#ifndef DEBUG
  FS_hInstance = hInstance;
#endif /* !DEBUG */
  FS_InitWindow();
  FFSS_LogFile = SU_OpenLogFile("FFSS_Server.log");
  FFSS_PrintSyslog(LOG_INFO,"Server started\n");
  if(!SU_WSInit(2,2))
  {
    FFSS_PrintSyslog(LOG_ERR,"Cannot start WinSock\n");
  }
#endif /* __unix__ */
  if(!SU_CreateSem(&FS_SemGbl,1,1,"FFSSServerSemGbl"))
  {
    FFSS_PrintSyslog(LOG_ERR,"FFSS Server Error : Couldn't allocate semaphore\n");
    return -3;
  }
  if(!SU_CreateSem(&FS_SemShr,1,1,"FFSSServerSemShr"))
  {
    FFSS_PrintSyslog(LOG_ERR,"FFSS Server Error : Couldn't allocate semaphore\n");
    return -3;
  }
  if(!SU_CreateSem(&FS_SemPlugin,1,1,"FFSSServerSemPlugin"))
  {
    FFSS_PrintSyslog(LOG_ERR,"FFSS Server Error : Couldn't allocate semaphore\n");
    return -3;
  }

  FFSS_Filter_Init(FFSS_THREAD_SERVER); /* Init FFSS Filter Engine here... as FS_Init has not been called yet */
  FS_MyState = FFSS_STATE_OFF;
  FS_Plugins = NULL;
  memset(&FS_MyGlobal,0,sizeof(FS_MyGlobal));
  if(!FS_LoadConfig(ConfigFile))
  {
    FFSS_PrintSyslog(LOG_ERR,"FFSS Server Error : Config file loader error. Exiting\n");
    return -1;
  }
  Error = FS_CheckGlobal();
  if(Error != NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"FFSS Server Error : Global field empty : %s\n",Error);
    return -2;
  }
#ifndef _WIN32
#ifndef DEBUG
  if(fork() == 0)
#endif /* !DEBUG */
#endif /* !_WIN32 */
  {
    memset(&FFSS_CB,0,sizeof(FFSS_CB));
    /* UDP callbacks */
    FFSS_CB.SCB.OnPing = OnPing;
    FFSS_CB.SCB.OnStateAnswer = OnStateAnswer;
    FFSS_CB.SCB.OnServerSearch = OnServerSearch;
    FFSS_CB.SCB.OnSharesListing = OnSharesListing;
    FFSS_CB.SCB.OnIndexRequest = OnIndexRequest;
    FFSS_CB.SCB.OnError = OnError;
    FFSS_CB.SCB.OnMasterSearchAnswer = OnMasterSearchAnswer;
    /* TCP callbacks */
    FFSS_CB.SCB.OnBeginTCPThread = OnBeginTCPThread;
    FFSS_CB.SCB.OnShareConnection = OnShareConnection;
    FFSS_CB.SCB.OnDirectoryListing = OnDirectoryListing;
    FFSS_CB.SCB.OnRecursiveDirectoryListing = OnRecursiveDirectoryListing;
    FFSS_CB.SCB.OnDownload = OnDownload;
    FFSS_CB.SCB.OnUpload = OnUpload;
    FFSS_CB.SCB.OnRename = OnRename;
    FFSS_CB.SCB.OnCopy = OnCopy;
    FFSS_CB.SCB.OnDelete = OnDelete;
    FFSS_CB.SCB.OnMkDir = OnMkDir;
    FFSS_CB.SCB.OnEndTCPThread = OnEndTCPThread;
    FFSS_CB.SCB.OnSelect = OnSelect;
    FFSS_CB.SCB.OnCancelXFer = OnCancelXFer;
    FFSS_CB.SCB.OnIdleTimeout = OnIdleTimeout;
    FFSS_CB.SCB.OnTransferFailed = OnTransferFailed;
    FFSS_CB.SCB.OnTransferSuccess = OnTransferSuccess;
    FFSS_CB.SCB.OnTransferActive = OnTransferActive;
    /* Streaming callbacks */
    FFSS_CB.SCB.OnStrmOpen = OnStrmOpen;
    FFSS_CB.SCB.OnStrmClose = OnStrmClose;
    FFSS_CB.SCB.OnStrmRead = OnStrmRead;
    FFSS_CB.SCB.OnStrmWrite = OnStrmWrite;
    FFSS_CB.SCB.OnStrmSeek = OnStrmSeek;
    /* FTP callbacks */
    FFSS_CB.SCB.OnConnectionFTP = OnConnectionFTP;
    FFSS_CB.SCB.OnEndTCPThreadFTP = OnEndTCPThread;
    FFSS_CB.SCB.OnPWDFTP = OnPWDFTP;
    FFSS_CB.SCB.OnTypeFTP = OnTypeFTP;
    FFSS_CB.SCB.OnModeFTP = OnModeFTP;
    FFSS_CB.SCB.OnDirectoryListingFTP = OnDirectoryListingFTP;
    FFSS_CB.SCB.OnCWDFTP = OnCWDFTP;
    FFSS_CB.SCB.OnDownloadFTP = OnDownloadFTP;
    FFSS_CB.SCB.OnIdleTimeoutFTP = OnIdleTimeoutFTP;

    if(!FS_PowerUp(FS_MyIntName))
      return -4;

#ifdef __unix__
    signal(SIGTSTP,handint);
#endif /* __unix__ */
    signal(SIGINT,handint);
    signal(SIGTERM,handint);

    FS_MainThread();

    FS_ShutDown();
    /* Freeing index */
    FS_FreeIndex();
  }
  return 0;
}

