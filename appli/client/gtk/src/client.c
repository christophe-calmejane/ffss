#include "client.h"
#include "interface.h"
#include "main.h"
#include "draw.h"

/* ************************* */

void FC_DQ_Init()
{
  static bool init = false;
  if(!init)
  {
    init = true;
    SU_CreateSem(&FCQ_Sem,1,1,"FCQ_Sem");
    g_idle_add(FCQ_IdleFunc,NULL);
  }
}

void FC_DQ_AddServerToList(long int State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[])
{
  FCQ_PHost H2;

  H2 = (FCQ_PHost) malloc(sizeof(FCQ_THost));
  memset(H2,0,sizeof(FCQ_THost));
  H2->Name = strdup(Name);
  H2->OS = strdup(OS);
  H2->Comment = strdup(Comment);
  H2->IP = (char *)IP;
  H2->State = State;
  H2->Domain = strdup(Domain);

  SU_SEM_WAIT(FCQ_Sem);
  FCQ_Hosts = SU_AddElementTail(FCQ_Hosts,H2);
  SU_SEM_POST(FCQ_Sem);
}

void FC_DQ_AddHostToList(FM_PHost H,const char Domain[])
{
  FCQ_PHost H2;

  H2 = (FCQ_PHost) malloc(sizeof(FCQ_THost));
  memset(H2,0,sizeof(FCQ_THost));
  H2->Name = strdup(H->Name);
  H2->OS = strdup(H->OS);
  H2->Comment = strdup(H->Comment);
  H2->IP = H->IP;
  H2->State = H->State;
  H2->Domain = strdup(Domain);

  SU_SEM_WAIT(FCQ_Sem);
  FCQ_Hosts = SU_AddElementTail(FCQ_Hosts,H2);
  SU_SEM_POST(FCQ_Sem);
}

void FC_DQ_AddDomainToList(const char Domain[])
{
  SU_SEM_WAIT(FCQ_Sem);
  FCQ_Domains = SU_AddElementTail(FCQ_Domains,strdup(Domain));
  SU_SEM_POST(FCQ_Sem);
}

void FC_DQ_AddShareToList(const char Name[],const char Comment[])
{
  FCQ_PShare Shr;

  Shr = (FCQ_PShare) malloc(sizeof(FCQ_TShare));
  memset(Shr,0,sizeof(FCQ_TShare));
  Shr->Name = strdup(Name);
  Shr->Comment = strdup(Comment);
  SU_SEM_WAIT(FCQ_Sem);
  FCQ_Shares = SU_AddElementTail(FCQ_Shares,Shr);
  SU_SEM_POST(FCQ_Sem);
}

void FC_DQ_AddOpToList_Status(PConn Conn,const char Text[])
{
  FCQ_POperation Op;

  Op = (FCQ_POperation) malloc(sizeof(FCQ_TOperation));
  memset(Op,0,sizeof(FCQ_TOperation));
  Op->Type = FCQ_OP_STATUS;
  Op->Conn = Conn;
  Op->Strings[0] = strdup(Text);
  SU_SEM_WAIT(FCQ_Sem);
  FCQ_Ops = SU_AddElementTail(FCQ_Ops,Op);
  SU_SEM_POST(FCQ_Sem);
}

FC_PEntry FC_DupEntry(FC_PEntry Ent)
{
  FC_PEntry Ent2;

  Ent2 = (FC_PEntry) malloc(sizeof(FC_TEntry));
  memset(Ent2,0,sizeof(FC_TEntry));
  Ent2->Name = strdup(Ent->Name);
  Ent2->Flags = Ent->Flags;
  Ent2->Size = Ent->Size;
  Ent2->Stamp = Ent->Stamp;

  return Ent2;
}

void FC_DQ_AddOpToList_DirListing(PConn Conn,SU_PList Entries)
{
  FCQ_POperation Op;
  SU_PList Ptr;

  Op = (FCQ_POperation) malloc(sizeof(FCQ_TOperation));
  memset(Op,0,sizeof(FCQ_TOperation));
  Op->Type = FCQ_OP_DIRLIST;
  Op->Conn = Conn;
  Ptr = Entries;
  while(Ptr != NULL)
  {
    Op->Entries = SU_AddElementTail(Op->Entries,FC_DupEntry((FC_PEntry)Ptr->Data));
    Ptr = Ptr->Next;
  }
  SU_SEM_WAIT(FCQ_Sem);
  FCQ_Ops = SU_AddElementTail(FCQ_Ops,Op);
  SU_SEM_POST(FCQ_Sem);
}

void FC_DQ_AddOpToList_AddFileToDownload(PConn Conn,const char Remote[],const char Local[],const char Size[])
{
  FCQ_POperation Op;

  add_conn_count(Conn);

  Op = (FCQ_POperation) malloc(sizeof(FCQ_TOperation));
  memset(Op,0,sizeof(FCQ_TOperation));
  Op->Type = FCQ_OP_ADDFILEDWL;
  Op->Conn = Conn;
  Op->Strings[0] = strdup(Conn->host_share);
  Op->Strings[1] = strdup(Remote);
  Op->Strings[2] = strdup(Local);
  Op->Strings[3] = strdup(Size);
  Op->Strings[4] = strdup("0");
  SU_SEM_WAIT(FCQ_Sem);
  FCQ_Ops = SU_AddElementTail(FCQ_Ops,Op);
  SU_SEM_POST(FCQ_Sem);
}

void FC_DQ_AddOpToList_EndThread(PConn Conn)
{
  FCQ_POperation Op;

  Op = (FCQ_POperation) malloc(sizeof(FCQ_TOperation));
  memset(Op,0,sizeof(FCQ_TOperation));
  Op->Type = FCQ_OP_ENDTHREAD;
  Op->Conn = Conn;
  SU_SEM_WAIT(FCQ_Sem);
  FCQ_Ops = SU_AddElementTail(FCQ_Ops,Op);
  SU_SEM_POST(FCQ_Sem);
}

void FC_DQ_AddOpToList_XFerFailed(PConn Conn,const char Error[])
{
  FCQ_POperation Op;

  Op = (FCQ_POperation) malloc(sizeof(FCQ_TOperation));
  memset(Op,0,sizeof(FCQ_TOperation));
  Op->Type = FCQ_OP_XFERFAILED;
  Op->Conn = Conn;
  Op->Strings[0] = strdup(Error);
  SU_SEM_WAIT(FCQ_Sem);
  FCQ_Ops = SU_AddElementTail(FCQ_Ops,Op);
  SU_SEM_POST(FCQ_Sem);
}

void FC_DQ_AddOpToList_XFerSuccess(PConn Conn)
{
  FCQ_POperation Op;

  Op = (FCQ_POperation) malloc(sizeof(FCQ_TOperation));
  memset(Op,0,sizeof(FCQ_TOperation));
  Op->Type = FCQ_OP_XFERSUCCESS;
  Op->Conn = Conn;
  SU_SEM_WAIT(FCQ_Sem);
  FCQ_Ops = SU_AddElementTail(FCQ_Ops,Op);
  SU_SEM_POST(FCQ_Sem);
}

void FC_DQ_AddOpToList_XFerActive(PConn Conn,const long int Amount)
{
  FCQ_POperation Op;

  Op = (FCQ_POperation) malloc(sizeof(FCQ_TOperation));
  memset(Op,0,sizeof(FCQ_TOperation));
  Op->Type = FCQ_OP_XFERACTIVE;
  Op->Conn = Conn;
  Op->Amount = Amount;
  SU_SEM_WAIT(FCQ_Sem);
  FCQ_Ops = SU_AddElementTail(FCQ_Ops,Op);
  SU_SEM_POST(FCQ_Sem);
}


/* ************************* */

/* UDP callbacks */
void OnNewState(long int State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[])
{
  FC_DQ_Init();
  printf("Received a new state (%ld) from %s (%s-%s-%s-%s) using master %s\n",State,IP,Domain,Name,OS,Comment,MasterIP);
  FC_DQ_AddServerToList(State,IP,Domain,Name,OS,Comment);
}

void OnSharesListing(const char IP[],const char **Names,const char **Comments,int NbShares)
{
  int i;

  for(i=0;i<NbShares;i++)
  {
    FC_DQ_AddShareToList(Names[i],Comments[i]);
  }
}

/* WARNING !! (char *) of the FM_PHost structure are pointers to STATIC buffer, and must be dupped ! */
/* Except for the FM_PHost->IP that is dupped internaly, and if you don't use it, you MUST free it !! */
void OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList)
{
  SU_PList Ptr,Lst = NULL;
  FM_PHost H;

  FC_DQ_Init();
  Ptr = HostList;
  while(Ptr != NULL)
  {
    H = (FM_PHost) Ptr->Data;
    FC_DQ_AddHostToList(H,Domain);
    Ptr = Ptr->Next;
  }
}

void OnEndServerListingAnswer(void)
{
}

void OnDomainListingAnswer(const char **Domains,int NbDomains)
{
  int i;

  FC_DQ_Init();
  for(i=0;i<NbDomains;i++)
  {
    FC_DQ_AddDomainToList(Domains[i]);
  }
}

void OnMasterSearchAnswer(struct sockaddr_in Master,FFSS_Field MasterVersion,const char Domain[])
{
  printf("Received a MASTER at ip %s using version %ld for domain %s\n",inet_ntoa(Master.sin_addr),MasterVersion,Domain);
  MyMaster = strdup(inet_ntoa(Master.sin_addr));
  if(FC_SendMessage_DomainListing(inet_ntoa(Master.sin_addr)))
    FC_SendMessage_ServerList(inet_ntoa(Master.sin_addr),NULL,NULL);
  else
    FC_SendMessage_ServerSearch();
}

/* ************************* */

void add_conn_count(PConn Conn)
{
  assert(Conn);
  G_LOCK(gbl_conns_lock);
  Conn->ref_count++;
  G_UNLOCK(gbl_conns_lock);
}

bool remove_conn(PConn Conn,bool clear_wnd)
{
  bool res = false;
  assert(Conn);

  G_LOCK(gbl_conns_lock);
  Conn->ref_count--;
  assert(Conn->ref_count >= 0);
  if(Conn->ref_count == 0)
  {
    /* Remove Conn from gbl_conns, to protect future invalid uses */
    gbl_conns = g_list_remove(gbl_conns,(gpointer)Conn);
    if(Conn->Active)
    {
      FC_SendMessage_Disconnect(Conn->Server);
    }
    if(Conn->host_share != NULL)
      free(Conn->host_share);
    if(Conn->path != NULL)
      free(Conn->path);
    free(Conn);
    res = true;
  }
  else if(clear_wnd)
    Conn->wnd = NULL;
  G_UNLOCK(gbl_conns_lock);
  return res;
}

PConn lookup_conn(SU_PClientSocket Server,bool even_null_wnd)
{
  GList *ptr;
  PConn Conn = NULL;

  G_LOCK(gbl_conns_lock);
  ptr = gbl_conns;
  while(ptr)
  {
    Conn = (PConn) ptr->data;
    assert(Conn != NULL);
    if(Conn->Active && (Conn->wnd || even_null_wnd))
    {
      if(Conn->Server == (gpointer)Server)
        break;
    }
    Conn = NULL;
    ptr = ptr->next;
  }
  G_UNLOCK(gbl_conns_lock);
  return Conn;
}

/* TCP callbacks */
bool OnError(SU_PClientSocket Server,int Code,const char Descr[])
{
  PConn Conn = lookup_conn(Server,false);

  if(Conn == NULL)
    return false;

  if(Code == FFSS_ERROR_NO_ERROR)
  {
    FC_DQ_AddOpToList_Status(Conn,"Successfully connected to server");
    return FC_SendMessage_DirectoryListing(Server,Conn->path);
  }
  FC_DQ_AddOpToList_Status(Conn,Descr);
  return true;
}

bool OnDirectoryListingAnswer(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries)
{
  PConn Conn = lookup_conn(Server,false);
  FC_PEntry Ent;
  char buf[1024];
  char buf2[1024];
  SU_PList Ptr;

  if(Conn == NULL)
    return false;

  if(Conn->recursif) /* Within recursive listing */
  {
    Ptr = Entries;
    while(Ptr != NULL)
    {
      Ent = (FC_PEntry) Ptr->Data;
      snprintf(buf,sizeof(buf),"%s/%s",Path,Ent->Name);
      snprintf(buf2,sizeof(buf2),"%s%s",Conn->rec_local_path,buf+strlen(Conn->rec_path));
      if(Ent->Flags & FFSS_FILE_DIRECTORY)
      {
        MKDIR(buf2);
        G_LOCK(gbl_conns_lock); /* To protect Conn->rec_dir_count */
        Conn->rec_dir_count++;
        G_UNLOCK(gbl_conns_lock);
        FC_SendMessage_DirectoryListing(Conn->Server,buf);
      }
      else
      {
        char tmp[1024];
        snprintf(tmp,sizeof(tmp),"%ld",Ent->Size);
        FC_DQ_AddOpToList_AddFileToDownload(Conn,buf,buf2,tmp);
      }
      Ptr = Ptr->Next;
    }
    G_LOCK(gbl_conns_lock); /* To protect Conn->rec_dir_count */
    Conn->rec_dir_count--;
    G_UNLOCK(gbl_conns_lock);
    assert(Conn->rec_dir_count >= 0);
    if(Conn->rec_dir_count == 0) /* End of recursive directory listing */
    {
      Conn->recursif = false;
      if(Conn->rec_path != NULL)
        free(Conn->rec_path);
      if(Conn->rec_local_path != NULL)
        free(Conn->rec_local_path);
      FC_DQ_AddOpToList_Status(Conn,"Recursive download : Listing complete");
    }
    else
    {
      char tmp[1024];
      snprintf(tmp,sizeof(tmp),"Recursive download : Listing directory %s",Path);
      FC_DQ_AddOpToList_Status(Conn,tmp);
    }
  }
  else
  {
    free(Conn->path);
    Conn->path = strdup(Path);
    FC_DQ_AddOpToList_DirListing(Conn,Entries);
  }
  return true;
}

void OnEndTCPThread(SU_PClientSocket Server)
{
  PConn Conn = lookup_conn(Server,true);

  printf("On End TCP Thread\n");
  if(Conn == NULL)
    return;
  Conn->Active = false;
  FC_DQ_AddOpToList_EndThread(Conn);
}

void OnIdleTimeout(SU_PClientSocket Server)
{
  PConn Conn = lookup_conn(Server,true);

  if(Conn == NULL)
    return;
  Conn->Active = false;
}

void OnTransfertFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download)
{
  PConn Conn;

  Conn = (PConn)FT->User;
  assert(Conn);
  if(Conn != NULL)
  {
    FT->User = NULL;
    remove_conn(Conn,false);
    FC_DQ_AddOpToList_XFerFailed(Conn,Error);
  }
}

void OnTransfertSuccess(FFSS_PTransfer FT,bool Download)
{
  PConn Conn;

  Conn = (PConn)FT->User;
  assert(Conn);
  if(Conn != NULL)
  {
    FT->User = NULL;
    remove_conn(Conn,false);
    FC_DQ_AddOpToList_XFerSuccess(Conn);
  }
}

void OnTransfertActive(FFSS_PTransfer FT,long int Amount,bool Download)
{
  PConn Conn;

  Conn = (PConn)FT->User;
  assert(Conn);
  if(Conn != NULL)
  {
    FC_DQ_AddOpToList_XFerActive(Conn,Amount);
  }
}

/*
FFSS_PTransfer OnInitXFer(SU_PClientSocket Server,const char RequestedFileName[])
{
  TConn *Conn;

  Conn = Form1->GetConn(Server);
  if(Conn == NULL)
    return NULL;
  return Conn->OnInitXFer(Server,RequestedFileName);
}

FFSS_PTransfer OnData(SU_PClientSocket Server,FFSS_Field XFerTag)
{
  TConn *Conn;

  Conn = Form1->GetConn(Server);
  if(Conn == NULL)
    return NULL;
  return Conn->OnData(Server,XFerTag);
}
*/
