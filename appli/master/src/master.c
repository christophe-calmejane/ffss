/* INFOS :
 - Hosts in a domain are kept during 30 days of OFF state
*/
//#define DISABLE_INDEX
#define ENABLE_SAMBA

#include "master.h"
#include "index.h"

FM_TDomain FM_MyDomain;
SU_PList FM_Domains; /* FM_PDomain */
SU_PList FM_MyQueue; /* FM_PQueue */
SU_PList FM_OtherQueue; /* FM_PQueue */
SU_PList FM_SearchQueue; /* FM_PSearch */

SU_SEM_HANDLE FM_MySem;  /* Semaphore to protect the use of FM_MyQueue */
SU_SEM_HANDLE FM_MySem2; /* Semaphore to protect the use of the Hosts of FM_MyDomain */
SU_SEM_HANDLE FM_MySem3; /* Semaphore to protect the use of FM_OtherQueue */
SU_SEM_HANDLE FM_MySem4; /* Semaphore to protect the use of FM_SearchQueue */
SU_SEM_HANDLE FM_MySem5; /* Semaphore to protect the use of the index */
SU_SEM_HANDLE FM_TmpSem; /* Temporary semaphore */

SU_THREAD_HANDLE FM_THR_PING,FM_THR_SEARCH;

volatile FFSS_Field FM_CurrentIndexSize = 0;
FFSS_Field FM_CurrentFileTreeSize = 0;
FFSS_Field FM_CurrentNodesSize = 0;
int FM_CurrentCompression = FFSS_COMPRESSION_NONE;
bool FM_CurrentSamba;
struct sockaddr_in FM_CurrentClient;
char *FM_User=NULL,*FM_Group=NULL,*FM_Iface=NULL;
FILE *FM_SearchLogFile=NULL;
volatile bool FM_ShuttingDown = false;

bool FM_IsMyDomain(FM_PDomain Dom)
{
  return (Dom == (&FM_MyDomain));
}

FM_PHost FM_SearchHostByIP(FM_PDomain Domain,const char IP[])
{
  SU_PList Ptr;

  context;
  SU_SEM_WAIT(FM_MySem2);
  Ptr = Domain->Hosts;
  while(Ptr != NULL)
  {
    if(strcmp(((FM_PHost)Ptr->Data)->IP,IP) == 0)
    {
      SU_SEM_POST(FM_MySem2);
      return (FM_PHost)Ptr->Data;
    }
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FM_MySem2);
  return NULL;
}

void FM_FreeHost(FM_PHost Hst)
{
  context;
  if(Hst->Name != NULL)
    free(Hst->Name);
  if(Hst->OS != NULL)
    free(Hst->OS);
  if(Hst->Comment != NULL)
    free(Hst->Comment);
  if(Hst->IP != NULL)
    free(Hst->IP);
  free(Hst);
}

void FM_SetHostStateInIndex(const char Host[],FFSS_Field State)
{
  int i;

  context;
  for(i=0;i<FM_Controler.NbHosts;i++)
  {
    if(SU_strcasecmp(Host,FM_Controler.Hosts[i]->Name))
    {
      FM_Controler.Hosts[i]->State = State;
      return;
    }
  }
}

FM_PHost FM_AddHostToDomain(FM_PDomain Domain,const char Name[],const char OS[],const char Comment[],const char IP[],FFSS_Field State)
{
  FM_PHost Hst;

  context;
#ifdef DEBUG
  //printf("MASTER : Adding host %s (%s) to domain %s\n",Name,IP,Domain->Name);
#endif /* DEBUG */
  Hst = (FM_PHost) malloc(sizeof(FM_THost));
  memset(Hst,0,sizeof(FM_THost));
  Hst->Name = strdup(Name);
  Hst->OS = strdup(OS);
  Hst->Comment = strdup(Comment);
  Hst->IP = strdup(IP);
  Hst->State = State;
  Hst->LastPong = time(NULL);
  FM_SetHostStateInIndex(Name,State);
  if(FM_IsMyDomain(Domain))
  {
    SU_SEM_WAIT(FM_MySem2);
    Domain->Hosts = SU_AddElementHead(Domain->Hosts,Hst);
    SU_SEM_POST(FM_MySem2);
  }
  else
    Domain->Hosts = SU_AddElementHead(Domain->Hosts,Hst);

  return Hst;
}

void FM_UpdateHost(FM_PHost Hst,const char Name[],const char OS[],const char Comment[],FFSS_Field State)
{
  context;
  SU_SEM_WAIT(FM_MySem2);
#ifdef DEBUG
  //printf("Updating host info : %s - %s - %s - %ld\n",Name,OS,Comment,State);
  //printf("Old infos : %s - %s - %s - %ld\n",Hst->Name,Hst->OS,Hst->Comment,Hst->State);
#endif /* DEBUG */
  if(strcmp(Hst->Name,Name) != 0)
  {
    free(Hst->Name);
    Hst->Name = strdup(Name);
  }
  if(strcmp(Hst->OS,OS) != 0)
  {
    free(Hst->OS);
    Hst->OS = strdup(OS);
  }
  if(strcmp(Hst->Comment,Comment) != 0)
  {
    free(Hst->Comment);
    Hst->Comment = strdup(Comment);
  }
  Hst->State = State;
  FM_SetHostStateInIndex(Name,State);
  SU_SEM_POST(FM_MySem2);
}

bool FM_IsHostInMyQueue(FM_PHost Hst)
{
  SU_PList Ptr;

  context;
  Ptr = FM_MyQueue;
  while(Ptr != NULL)
  {
    if(((FM_PQueue)Ptr->Data)->Host == Hst)
      return true;
    Ptr = Ptr->Next;
  }
  return false;
}

bool FM_IsHostInOtherQueue(FM_PHost Hst)
{
  SU_PList Ptr;

  context;
  Ptr = FM_OtherQueue;
  while(Ptr != NULL)
  {
    if(((FM_PQueue)Ptr->Data)->Host == Hst)
      return true;
    Ptr = Ptr->Next;
  }
  return false;
}

void FM_AddStateToMyQueue(FM_PDomain Domain,FM_PHost Hst)
{
  FM_PQueue Que;

  context;
#ifdef DEBUG
  //printf("MASTER : Adding state to my queue : %s (%s) %ld\n",Hst->Name,Hst->IP,Hst->State);
#endif /* DEBUG */
  if(Hst->State == FFSS_STATE_OFF)
    Hst->OffSince = time(NULL);
  else
    Hst->OffSince = 0;
  if(FM_IsHostInMyQueue(Hst))
    return;

  Que = (FM_PQueue) malloc(sizeof(FM_TQueue));
  memset(Que,0,sizeof(FM_TQueue));
  Que->Domain = Domain;
  Que->Host = Hst;
  SU_SEM_WAIT(FM_MySem);
  FM_SetHostStateInIndex(Hst->Name,Hst->State);
  FM_MyQueue = SU_AddElementHead(FM_MyQueue,Que);
  SU_SEM_POST(FM_MySem);
}

void FM_AddStateToOtherQueue(FM_PDomain Domain,FM_PHost Hst)
{
  FM_PQueue Que;

  context;
#ifdef DEBUG
  //printf("MASTER : Adding state to other queue : %s (%s) %ld\n",Hst->Name,Hst->IP,Hst->State);
#endif /* DEBUG */
  if(Hst->State == FFSS_STATE_OFF)
    Hst->OffSince = time(NULL);
  else
    Hst->OffSince = 0;
  if(FM_IsHostInOtherQueue(Hst))
    return;

  Que = (FM_PQueue) malloc(sizeof(FM_TQueue));
  Que->Domain = Domain;
  Que->Host = Hst;
  SU_SEM_WAIT(FM_MySem3);
  FM_OtherQueue = SU_AddElementHead(FM_OtherQueue,Que);
  SU_SEM_POST(FM_MySem3);
}

FM_PDomain FM_SearchDomainByIP(const char IP[])
{
  SU_PList Ptr;

  context;
  Ptr = FM_Domains;
  while(Ptr != NULL)
  {
    if(strcmp(((FM_PDomain)Ptr->Data)->Master,IP) == 0)
      return (FM_PDomain)Ptr->Data;
    Ptr = Ptr->Next;
  }
  return NULL;
}

/* UDP callbacks */
/* OnState is raised only by local hosts */
void OnState(struct sockaddr_in Server,FFSS_Field State,const char Name[],const char OS[],const char Comment[])
{
  FM_PHost Hst;

  context;
#ifdef DEBUG
  //printf("MASTER : State : %s-%s-%s\n",Name,Comment,OS);
#endif /* DEBUG */
  Hst = FM_SearchHostByIP(&FM_MyDomain,inet_ntoa(Server.sin_addr));
  if(Hst == NULL)
    Hst = FM_AddHostToDomain(&FM_MyDomain,Name,OS,Comment,inet_ntoa(Server.sin_addr),State);
  else
    FM_UpdateHost(Hst,Name,OS,Comment,State);
  FM_AddStateToMyQueue(&FM_MyDomain,Hst);
}

/* OnServerListing is raised by local clients */
void OnServerListing(struct sockaddr_in Client,const char OS[],const char Domain[],long int Compressions)
{
  char *buf,*s_dom,*s_os;
  long int len;
  long int comp;

  context;
  buf = inet_ntoa(Client.sin_addr);
  if(Domain[0] == 0)
    s_dom = "*";
  else
    s_dom = (char *)Domain;
  if(OS[0] == 0)
    s_os = "*";
  else
    s_os = (char *)OS;
  buf = FM_BuildServerListing(s_dom,s_os,&len);
  if(len >= FM_COMPRESSION_TRIGGER_BZLIB)
  {
    if(Compressions & FFSS_COMPRESSION_BZLIB)
      comp = FFSS_COMPRESSION_BZLIB;
    else if(Compressions & FFSS_COMPRESSION_ZLIB)
      comp = FFSS_COMPRESSION_ZLIB;
    else
      comp = FFSS_COMPRESSION_NONE;
  }
  else if(len >= FM_COMPRESSION_TRIGGER_ZLIB)
  {
    if(Compressions & FFSS_COMPRESSION_ZLIB)
      comp = FFSS_COMPRESSION_ZLIB;
    else
      comp = FFSS_COMPRESSION_NONE;
  }
  else
    comp = FFSS_COMPRESSION_NONE;
  FM_SendMessage_ServerListing(Client,buf,len,comp);
  free(buf);
}

/* OnClientServerFailed is raised by local clients that failed to connect to a *said* non-OFF server */
void OnClientServerFailed(const char IP[])
{
  SU_PList Ptr;
  FM_PHost Hst;

  context;
  Ptr = FM_Domains;
  while(Ptr != NULL)
  {
    Hst = FM_SearchHostByIP((FM_PDomain)Ptr->Data,IP);
    if(Hst != NULL)
    {
#ifdef DEBUG
      printf("MASTER : Client said connection to server %s timed out, changing state\n",IP);
#endif /* DEBUG */
      Hst->State = FFSS_STATE_OFF;
      Hst->OffSince = time(NULL);
      if(FM_IsMyDomain(Ptr->Data))
        FM_AddStateToMyQueue((FM_PDomain)Ptr->Data,Hst);
      /*else
        FM_AddStateToOtherQueue((FM_PDomain)Ptr->Data,Hst);*/
      break;
    }
    Ptr = Ptr->Next;
  }
}

/* OnPong is raised by local servers */
void OnPong(struct sockaddr_in Server,FFSS_Field State)
{
  FM_PHost Hst;

  context;
  Hst = FM_SearchHostByIP(&FM_MyDomain,inet_ntoa(Server.sin_addr));
  if(Hst != NULL)
  {
    if(State != Hst->State)
      FM_AddStateToMyQueue(&FM_MyDomain,Hst);
    Hst->State = State;
    Hst->LastPong = time(NULL);
    Hst->OffSince = 0;
  }
}

/* OnDomainListing is raised by local clients */
void OnDomainListing(struct sockaddr_in Client)
{
  char *buf[1024];
  SU_PList Ptr;
  int nb;

  context;
  nb = 0;
  Ptr = FM_Domains;
  while(Ptr != NULL)
  {
    buf[nb] = ((FM_PDomain)Ptr->Data)->Name;
    nb++;
    Ptr = Ptr->Next;
  }
  FM_SendMessage_DomainListingAnswer(Client,nb,buf);
}

/* OnSearch is raised by local clients */
void OnSearch(struct sockaddr_in Client,int Port,const char Domain[],const char KeyWords[],long int Compressions)
{
  char *s_dom;
  FM_PSearch Sch;

  context;
#ifdef DEBUG
  printf("Received a SEARCH message for domain %s : %s\n",(Domain[0] == 0)?"All":Domain,KeyWords);
#endif /* DEBUG */

  if(strlen(KeyWords) < FFSS_MIN_SEARCH_REQUEST_LENGTH)
  {
    FM_SendMessage_ErrorClient(Client,FFSS_ERROR_BAD_SEARCH_REQUEST,FFSS_ErrorTable[FFSS_ERROR_BAD_SEARCH_REQUEST]);
    return;
  }
  if(Domain[0] == 0)
    s_dom = "*";
  else
    s_dom = (char *)Domain;

  Sch = (FM_PSearch) malloc(sizeof(FM_TSearch));
  memcpy(&Sch->Client,&Client,sizeof(struct sockaddr_in));
  Sch->Port = Port;
  Sch->Domain = strdup(s_dom);
  Sch->KeyWords = strdup(KeyWords);
  Sch->Compressions = Compressions;
  Sch->Master = false;

  SU_SEM_WAIT(FM_MySem4);
  FM_SearchQueue = SU_AddElementTail(FM_SearchQueue,Sch);
  SU_SEM_POST(FM_MySem4);
}

/* OnMasterSearch is raised by local clients */
void OnMasterSearch(struct sockaddr_in Client,bool Server)
{
  context;
  FM_SendMessage_MasterSearchAnswer(Client,Server,FM_MyDomain.Name);
}

SU_THREAD_ROUTINE(ThreadIndexing,Info)
{
  int sock = (int) Info;
  fd_set rfds;
  struct timeval tv;
  int retval;
  struct sockaddr_in SAddr;
  FFSS_Field IndexSize = FM_CurrentIndexSize,total = 0;
  FFSS_Field FileTreeSize = FM_CurrentFileTreeSize,FT_pos = 0;
  FFSS_Field NodesSize = FM_CurrentNodesSize,Node_pos = 0;
  int Compression = FM_CurrentCompression,res,i;
  bool Samba = FM_CurrentSamba;
  FFSS_Field NbShares,Size,actual,length;
  char *buf,*data,*tmp_ip;
  bool error,free_it;
  FM_PFTControler Host;
  int NumHost;
  FM_PHost Hst = NULL;

  context;
  SU_ThreadBlockSigs();
#ifdef DEBUG
  printf("Starting index thread\n");
#endif /* DEBUG */
  memcpy(&SAddr,&FM_CurrentClient,sizeof(FM_CurrentClient));
  tmp_ip = inet_ntoa(SAddr.sin_addr);
  FM_CurrentIndexSize = 0;
  if(!Samba)
  {
    Hst = FM_SearchHostByIP(&FM_MyDomain,tmp_ip);
    if(Hst == NULL)
    {
#ifdef DEBUG
      printf("%s is not on my domain... rejecting index\n",tmp_ip);
#endif /* DEBUG */
      SU_CLOSE_SOCKET(sock);
      SU_END_THREAD(NULL);
    }
  }

  context;
  FD_ZERO(&rfds);
  FD_SET(sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_INDEX_XFER;
  tv.tv_usec = 0;
  retval = select(sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
  {
#ifdef DEBUG
    printf("WARNING : Timed out waiting index from server\n");
#endif /* DEBUG */
    SU_CLOSE_SOCKET(sock);
    SU_END_THREAD(NULL);
  }
  res = recv(sock,(char *)&NbShares,sizeof(NbShares),SU_MSG_NOSIGNAL);
  if(res <= 0)
  {
    SU_CLOSE_SOCKET(sock);
    SU_END_THREAD(NULL);
  }
  Host = (FM_PFTControler) malloc(sizeof(FM_TFTControler));
  memset(Host,0,sizeof(FM_TFTControler));
  Host->IP = strdup(tmp_ip);
  if(!Samba)
  {
    Host->Name = strdup(Hst->Name);
    Host->State = Hst->State;
  }
  Host->FileTree = (char *) malloc(FileTreeSize);
  Host->FileTreeLength = FileTreeSize;
  Host->NbNodes = NodesSize/sizeof(FM_TFTNode);
  Host->FTNodes = (FM_TFTNode *) malloc(NodesSize);
  context;
  for(i=0;i<NbShares*2;i++)
  {
    FD_ZERO(&rfds);
    FD_SET(sock,&rfds);
    tv.tv_sec = FFSS_TIMEOUT_INDEX_XFER;
    tv.tv_usec = 0;
    retval = select(sock+1,&rfds,NULL,NULL,&tv);
    if(!retval)
    {
#ifdef DEBUG
      printf("WARNING : Timed out waiting index from server\n");
#endif /* DEBUG */
      FMI_FreeFTControler(Host);
      SU_CLOSE_SOCKET(sock);
      SU_END_THREAD(NULL);
    }
    res = recv(sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
    if(res <= 0)
    {
#ifdef DEBUG
      printf("WARNING : Error receiving index from server\n");
#endif /* DEBUG */
      FMI_FreeFTControler(Host);
      SU_CLOSE_SOCKET(sock);
      SU_END_THREAD(NULL);
    }
    total += Size;
    if(total > IndexSize)
    {
      printf("WARNING : Index sent is greater than specified size : %ld-%ld\n",total,IndexSize);
      FMI_FreeFTControler(Host);
      SU_CLOSE_SOCKET(sock);
      SU_END_THREAD(NULL);
    }

    buf = (char *) malloc(Size);
    actual = 0;
    while(actual < Size)
    {
      FD_ZERO(&rfds);
      FD_SET(sock,&rfds);
      tv.tv_sec = FFSS_TIMEOUT_INDEX_XFER;
      tv.tv_usec = 0;
      retval = select(sock+1,&rfds,NULL,NULL,&tv);
      if(!retval)
      {
#ifdef DEBUG
        printf("WARNING : Timed out waiting index from server\n");
#endif /* DEBUG */
        FMI_FreeFTControler(Host);
        SU_CLOSE_SOCKET(sock);
        SU_END_THREAD(NULL);
      }
      res = recv(sock,buf+actual,Size-actual,SU_MSG_NOSIGNAL);
      if(res <= 0)
      {
#ifdef DEBUG
        printf("WARNING : Error receiving index from server\n");
#endif /* DEBUG */
        FMI_FreeFTControler(Host);
        SU_CLOSE_SOCKET(sock);
        free(buf);
        SU_END_THREAD(NULL);
      }
      actual += res;
    }

    error = false;
    free_it = false;
    data = buf;
    switch(Compression)
    {
      case FFSS_COMPRESSION_NONE :
        length = Size;
        break;
      case FFSS_COMPRESSION_ZLIB :
        data = FFSS_UncompresseZlib(buf,Size,&length);
        if(data == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n",tmp_ip);
          error = true;
          break;
        }
        free_it = true;
        break;
      case FFSS_COMPRESSION_BZLIB :
        data = FFSS_UncompresseBZlib(buf,Size,&length);
        if(data == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"Corrupted BZ compressed buffer (%s) ... DoS attack ?\n",tmp_ip);
          error = true;
          break;
        }
        free_it = true;
        break;
      default :
        FFSS_PrintSyslog(LOG_WARNING,"Unknown compression type (%s) : %d\n",Compression,tmp_ip);
        error = true;
    }
    if(error)
    {
      FMI_FreeFTControler(Host);
      SU_CLOSE_SOCKET(sock);
      free(buf);
      SU_END_THREAD(NULL);
    }
    if(i%2) /* Nodes */
    {
      memcpy((char *)(Host->FTNodes)+Node_pos,data,length);
      Node_pos += length;
    }
    else /* FileTree */
    {
      memcpy(Host->FileTree+FT_pos,data,length);
      FT_pos += length;
    }
    free(buf);
    if(free_it)
      free(data);
  }
  SU_CLOSE_SOCKET(sock);
  if(Samba)
  {
    Host->Name = strdup(Host->FileTree);
    Host->Samba = true;
  }
#ifdef DEBUG
  printf("Index thread : buffer received... now indexing\n");
#endif /* DEBUG */
  SU_SEM_WAIT(FM_MySem5);
  context;
  NumHost = -1;
  context;
  for(i=0;i<FM_Controler.NbHosts;i++)
  {
    if(SU_strcasecmp(FM_Controler.Hosts[i]->Name,Host->Name))
    {
      NumHost = i;
      break;
    }
  }
  if(NumHost != -1)
  {
#ifdef DEBUG
    printf("Index thread : host '%s' already exists with NumHost %d... replacing\n",Host->Name,NumHost);
#endif /* DEBUG */
    FMI_RemoveHostFromSuffixTree(NumHost);
    FMI_FreeFTControler(FM_Controler.Hosts[NumHost]);
    FM_Controler.Hosts[NumHost] = Host;
  }
  else
  {
    NumHost = FM_Controler.NbHosts;
    if(FM_Controler.NbHosts == 0)
      FM_Controler.Hosts = (FM_PFTControler *) malloc(sizeof(FM_PFTControler));
    else
      FM_Controler.Hosts = (FM_PFTControler *) realloc(FM_Controler.Hosts,(FM_Controler.NbHosts+1)*sizeof(FM_PFTControler));
    FM_Controler.Hosts[NumHost] = Host;
    FM_Controler.NbHosts++;
  }
  context;
  if(FMI_CheckFileTree(Host))
  {
    FMI_InsertHostInIndex(Host,NumHost); /* calling insert engine */
#ifdef DEBUG
    if(FMI_CheckOrderIntegrity())
      FMI_PrintWholeSuffixTree("st.txt");
#endif /* DEBUG */
  }
#ifdef DEBUG
  printf("Ending index thread... indexing completed (%d hosts)\n",FM_Controler.NbHosts);
#endif /* DEBUG */
  SU_SEM_POST(FM_MySem5);
  SU_END_THREAD(NULL);
}

void GlobalIndexAnswer(struct sockaddr_in Client,FFSS_Field CompressionType,FFSS_Field IndexSize,FFSS_Field FileTreeSize,FFSS_Field NodesSize,int Port,bool Samba)
{
  FM_PHost Hst;
  time_t tim;
  int sock=0;
  struct sockaddr_in SAddr;
  SU_THREAD_HANDLE ClientThr;

  context;
  if((FileTreeSize == 0) || (NodesSize == 0))
  {
#ifdef DEBUG
    printf("WARNING : Index from %s is empty... ignoring\n",inet_ntoa(Client.sin_addr));
#endif /* DEBUG */
    return;
  }
  if(!Samba)
  {
    Hst = FM_SearchHostByIP(&FM_MyDomain,inet_ntoa(Client.sin_addr));
    if(Hst == NULL)
    {
#ifdef DEBUG
      printf("%s is not on my domain... rejecting index\n",inet_ntoa(Client.sin_addr));
#endif /* DEBUG */
      return;
    }

    tim = time(NULL);

    if(Hst->LastIndex != 0)
    {
      if(tim < (Hst->LastIndex+FFSS_INDEX_INTERVAL)) /* Index received too soon */
      {
#ifdef DEBUG
        printf("WARNING : Index from %s received too soon, rejecting... DoS Attack ?\n",Hst->Name);
#else /* DEBUG */
        return;
#endif /* DEBUG */
      }
    }
    Hst->LastIndex = tim;
  }

  context;
  sock = socket(AF_INET,SOCK_STREAM,getprotobyname("tcp")->p_proto);
  if(sock == SOCKET_ERROR)
    return;
  SAddr.sin_family = AF_INET;
  SAddr.sin_port = htons(Port);
  SAddr.sin_addr.s_addr = Client.sin_addr.s_addr;
  if(connect(sock,(struct sockaddr *)(&SAddr),sizeof(SAddr)) == SOCKET_ERROR)
  {
    SU_CLOSE_SOCKET(sock);
    return;
  }
  while(FM_CurrentIndexSize != 0)
    SU_SLEEP(1);
  FM_CurrentIndexSize = IndexSize;
  FM_CurrentFileTreeSize = FileTreeSize;
  FM_CurrentNodesSize = NodesSize;
  FM_CurrentCompression = CompressionType;
  FM_CurrentSamba = Samba;
  memcpy(&FM_CurrentClient,&Client,sizeof(FM_CurrentClient));
  if(!SU_CreateThread(&ClientThr,ThreadIndexing,(void *)sock,true))
  {
    SU_CLOSE_SOCKET(sock);
    return;
  }
}

void OnIndexAnswer(struct sockaddr_in Client,FFSS_Field CompressionType,FFSS_Field IndexSize,FFSS_Field FileTreeSize,FFSS_Field NodesSize,int Port)
{
  context;
  GlobalIndexAnswer(Client,CompressionType,IndexSize,FileTreeSize,NodesSize,Port,false);
}
void OnIndexAnswerSamba(struct sockaddr_in Client,FFSS_Field CompressionType,FFSS_Field IndexSize,FFSS_Field FileTreeSize,FFSS_Field NodesSize,int Port)
{
  context;
  GlobalIndexAnswer(Client,CompressionType,IndexSize,FileTreeSize,NodesSize,Port,true);
}

/* TCP callbacks */
bool OnCheckConnection(SU_PClientSocket Master)
{
  FM_PDomain Dom;
  char *buf;

  context;
  buf = inet_ntoa(Master->SAddr.sin_addr);
  Dom = FM_SearchDomainByIP(buf);
  if((Dom != NULL) && (!FM_IsMyDomain(Dom))) /* Foreign master, and not me ! */
    return true;
  return false;
}

void OnMasterConnected(SU_PClientSocket Master)
{
  FM_PDomain Dom;

  context;
  Dom = FM_SearchDomainByIP(inet_ntoa(Master->SAddr.sin_addr));
  if((Dom != NULL) && (Dom->CS == NULL))
  {
#ifdef DEBUG
    printf("MASTER : New master connected for domain %s, requesting hosts list\n",Dom->Name);
#endif /* DEBUG */
    Dom->CS = Master;
    FM_SendMessage_MasterConnection(Master->sock);
    FM_SendMessage_ServerList(Master->sock);
  }
}

void OnMasterDisconnected(SU_PClientSocket Master)
{
  FM_PDomain Dom;
  SU_PList Ptr;

  context;
  SU_SEM_WAIT(FM_TmpSem); /* Lock to stall the callback if we are still in the FM_LoadConfigFile function */
  Dom = FM_SearchDomainByIP(inet_ntoa(Master->SAddr.sin_addr));
  if(Dom != NULL)
  {
#ifdef DEBUG
    printf("MASTER : Connection lost with master of domain %s\n",Dom->Name);
#endif /* DEBUG */
    Dom->CS = NULL;
    /* Remove all hosts from this domain, as we'll get a fresh clean new list on next connection */
    SU_SEM_WAIT(FM_MySem2);
    Ptr = Dom->Hosts;
    while(Ptr != NULL)
    {
      FM_FreeHost((FM_PHost)Ptr->Data);
      Ptr = Ptr->Next;
    }
    Dom->Hosts = NULL;
    SU_SEM_POST(FM_MySem2);
  }
  SU_SEM_POST(FM_TmpSem);
}

void OnNewState(FFSS_Field State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[])
{
  FM_PDomain Dom;
  FM_PHost Hst;

  context;
#ifdef DEBUG
  //printf("MASTER : NewState : %s-%s-%s-%s-%s-%s\n",Name,Comment,OS,Domain,IP,MasterIP);
#endif /* DEBUG */
  Dom = FM_SearchDomainByIP(MasterIP);
  if(Dom == NULL)
  {
#ifdef DEBUG
    printf("MASTER : WARNING : Domain not found searching with this ip : %s\n",MasterIP);
#endif /* DEBUG */
    return;
  }

  Hst = FM_SearchHostByIP(Dom,IP);
  if(Hst == NULL)
    Hst = FM_AddHostToDomain(Dom,Name,OS,Comment,IP,State);
  else
    FM_UpdateHost(Hst,Name,OS,Comment,State);
  if(Hst->State == FFSS_STATE_OFF)
    Hst->OffSince = time(NULL);
  else
    Hst->OffSince = 0;
  if(FM_IsMyDomain(Dom))
    FM_AddStateToMyQueue(Dom,Hst);
  /*else
    FM_AddStateToOtherQueue(Dom,Hst);*/
}

void OnServerListingMaster(SU_PClientSocket Master,const char OS[],const char Domain[],long int Compressions)
{
  char *buf;
  long int len;
  SU_PList Queue,Ptr;
  FM_PQueue Que;
  FM_PDomain Dom;

  context;
  buf = inet_ntoa(Master->SAddr.sin_addr);
  /* Known master ? */
  Dom = FM_SearchDomainByIP(buf);
  if((Dom != NULL) && (!FM_IsMyDomain(Dom)))
  {
#ifdef DEBUG
    //printf("MASTER : Received a ServerListing message from foreign master of %s\n",Dom->Name);
#endif /* DEBUG */
    Queue = NULL;
    Ptr = FM_MyDomain.Hosts;
    while(Ptr != NULL)
    {
      Que = (FM_PQueue) malloc(sizeof(FM_TQueue));
      Que->Domain = &FM_MyDomain;
      Que->Host = (FM_PHost)Ptr->Data;
      Queue = SU_AddElementHead(Queue,Que);
      Ptr = Ptr->Next;
    }
    buf = FM_BuildStatesBuffer(Queue,&len);
    if(buf != NULL)
    {
      if(!FM_SendMessage_NewStatesMaster(Master->sock,buf,len,FFSS_COMPRESSION_BZLIB))
        FFSS_PrintSyslog(LOG_WARNING,"Error sending New States message %s (%d:%s)\n",Dom->Master,errno,strerror(errno));
      free(buf);
    }
  }
}

void OnSearchForward(SU_PClientSocket Master,const char ClientIP[],int Port,const char KeyWords[],long int Compressions)
{
  FM_PSearch Sch;
  FM_PDomain Dom;

  context;
#ifdef DEBUG
  printf("Received a SEARCH FORWARD message : %s\n",KeyWords);
#endif /* DEBUG */

  Dom = FM_SearchDomainByIP(inet_ntoa(Master->SAddr.sin_addr));
  if(Dom == NULL)
  {
#ifdef DEBUG
    printf("MASTER : WARNING : Domain not found searching with this ip : %s\n",inet_ntoa(Master->SAddr.sin_addr));
#endif /* DEBUG */
    return;
  }

  if(strlen(KeyWords) < FFSS_MIN_SEARCH_REQUEST_LENGTH)
  {
    return;
  }

  Sch = (FM_PSearch) malloc(sizeof(FM_TSearch));
  Sch->Client.sin_port = htons(Port);
  Sch->Client.sin_addr.s_addr = inet_addr(ClientIP);
  Sch->Client.sin_family = AF_INET;
  Sch->Domain = strdup(FM_MyDomain.Name);
  Sch->KeyWords = strdup(KeyWords);
  Sch->Compressions = Compressions;
  Sch->Master = true;

  SU_SEM_WAIT(FM_MySem4);
  FM_SearchQueue = SU_AddElementTail(FM_SearchQueue,Sch);
  SU_SEM_POST(FM_MySem4);
}


/* *********************** */
void PrintHelp(void)
{
  printf("Usage : ffss-master [options]\n");
  printf("Options : -h or --help : This\n");
  printf("          -v or --version  : Prints server version and exits\n");
  printf("          -d or --daemon   : Daemonize the master\n");
  printf("          -l or --log      : Log all search request\n");
  printf("          -c <config file> : Loads this configuration file, instead of the default one \"%s\"\n",CONFIG_FILE_NAME);
  exit(0);
}

void handint(int sig)
{
  SU_PList Ptr;

  if(!FM_ShuttingDown)
  {
    FM_ShuttingDown = true;
    FFSS_PrintSyslog(LOG_ERR,"Received a %d signal in %d\n",sig,getpid());
    memset(&FFSS_CB.MCB,0,sizeof(FFSS_CB.MCB));
    /* Close all foreign master */
    Ptr = FM_Domains;
    while(Ptr != NULL)
    {
      if(((FM_PDomain)Ptr->Data)->CS != NULL)
        SU_FreeCS(((FM_PDomain)Ptr->Data)->CS);
      Ptr = Ptr->Next;
    }
    /* Close log file, if opened */
    if(FM_SearchLogFile != NULL)
      SU_CloseLogFile(FM_SearchLogFile);
    /* Writing hosts */
    FM_SaveHosts(FM_MyDomain.Hosts,FM_MYHOSTS_FILE);
    /* Writing index */
    FMI_SaveIndex(FM_MYINDEX_FILE);
    /* Term other threads */
    SU_TermThread(FM_THR_PING);
    SU_TermThread(FM_THR_SEARCH);
    /* Shutting down master */
    FM_UnInit();
    remove(FM_PID_FILE);
    exit(0);
  }
  else
    FFSS_PrintSyslog(LOG_WARNING,"Signal handler in %d (%d) : Master is being shut down... please wait\n",getpid(),sig);
}

#ifdef _WIN32
#ifdef DEBUG
int main(int argc,char *argv[])
#else /* !DEBUG */
int APIENTRY WinMain(HINSTANCE hInstance,HINSTANCE hPrevInstance,LPSTR lpCmdLine,int nCmdShow)
#endif /* DEBUG */
#else /* !_WIN32 */
int main(int argc,char *argv[])
#endif /* _WIN32 */
{
  int i;
  char ConfigFile[1024];
  bool daemonize = false;
  bool log = false;
  FILE *fp;
  pid_t MainPid;

  printf("FFSS Master v%s (c) Ze KiLleR / SkyTech 2001'02\n",FFSS_MASTER_VERSION);
  printf("%s\n",FFSS_COPYRIGHT);

  fp = fopen(FM_PID_FILE,"rt");
  if(fp != NULL)
  {
    fscanf(fp,"%d",&MainPid);
    printf("ffss-master is already running with pid %d\nIf not, remove %s file\n",MainPid,FM_PID_FILE);
    fclose(fp);
    return 1;
  }

  SU_strcpy(ConfigFile,CONFIG_FILE_NAME,sizeof(ConfigFile));
#ifdef __unix__
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
      else if(strcmp(argv[i],"-l") == 0)
        log = true;
      else if(strcmp(argv[i],"--log") == 0)
        log = true;
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
    openlog("Ffss Master",0,LOG_DAEMON);
    FFSS_PrintSyslog(LOG_INFO,"Master started\n");
    if(SU_Daemonize() == false)
    {
      FFSS_PrintSyslog(LOG_ERR,"Cannot daemonize process");
      return -1;
    }
  }
  else
  {
    openlog("Ffss Master",LOG_PERROR,LOG_USER);
    FFSS_PrintSyslog(LOG_INFO,"Master started\n");
  }
#else /* !__unix__ */
  FFSS_LogFile = SU_OpenLogFile("FFSS_Master.log");
  FFSS_PrintSyslog(LOG_INFO,"Master started\n");
  if(!SU_WSInit(2,2))
  {
    FFSS_PrintSyslog(LOG_ERR,"Cannot start WinSock\n");
  }
#endif /* __unix__ */
  FM_Domains = NULL;
  FM_MyQueue = NULL;
  FM_SearchQueue = NULL;
  memset(&FM_MyDomain,0,sizeof(FM_MyDomain));

  context;
  if(!FM_LoadConfigFile(ConfigFile,true)) /* Only get user and group */
  {
    FFSS_PrintSyslog(LOG_ERR,"Cannot open config file : %s\n",ConfigFile);
    return -1;
  }

  context;
#ifdef __unix__
#ifndef DEBUG
  if(fork() == 0)
#endif /* !DEBUG */
#endif /* __unix__ */
  {
    if(!SU_CreateSem(&FM_MySem,1,1,"FFSSMasterSem"))
    {
      FFSS_PrintSyslog(LOG_ERR,"FFSS Master Error : Couldn't allocate semaphore\n");
      return -3;
    }
    if(!SU_CreateSem(&FM_MySem2,1,1,"FFSSMasterSem2"))
    {
      FFSS_PrintSyslog(LOG_ERR,"FFSS Master Error : Couldn't allocate semaphore\n");
      return -3;
    }
    if(!SU_CreateSem(&FM_MySem3,1,1,"FFSSMasterSem3"))
    {
      FFSS_PrintSyslog(LOG_ERR,"FFSS Master Error : Couldn't allocate semaphore\n");
      return -3;
    }
    if(!SU_CreateSem(&FM_MySem4,1,1,"FFSSMasterSem4"))
    {
      FFSS_PrintSyslog(LOG_ERR,"FFSS Master Error : Couldn't allocate semaphore\n");
      return -3;
    }
    if(!SU_CreateSem(&FM_MySem5,1,1,"FFSSMasterSem5"))
    {
      FFSS_PrintSyslog(LOG_ERR,"FFSS Master Error : Couldn't allocate semaphore\n");
      return -3;
    }
    if(!SU_CreateSem(&FM_TmpSem,1,1,"FFSSMasterTmpSem"))
    {
      FFSS_PrintSyslog(LOG_ERR,"FFSS Master Error : Couldn't allocate semaphore\n");
      return -3;
    }
    context;
    FMI_IndexInit(); /* Init indexing engine */
    context;
#ifdef DEBUG
    FMI_LoadIndex("./Dump.dat");
#else /* !DEBUG */
    FMI_LoadIndex(FM_MYINDEX_FILE);
#endif /* DEBUG */
    if(log)
      FM_SearchLogFile = SU_OpenLogFile(FM_SEARCHLOG_FILE);

    context;
    if(!FM_Init(FFSS_MASTER_PORT,FM_User,FM_Group,FM_Iface))
      return -2;

    memset(&FFSS_CB,0,sizeof(FFSS_CB));
    /* UDP callbacks */
    FFSS_CB.MCB.OnState = OnState;
    FFSS_CB.MCB.OnServerListing = OnServerListing;
    FFSS_CB.MCB.OnClientServerFailed = OnClientServerFailed;
    FFSS_CB.MCB.OnPong = OnPong;
    FFSS_CB.MCB.OnDomainListing = OnDomainListing;
    FFSS_CB.MCB.OnSearch = OnSearch;
    FFSS_CB.MCB.OnMasterSearch = OnMasterSearch;
#ifndef DISABLE_INDEX
    FFSS_CB.MCB.OnIndexAnswer = OnIndexAnswer;
#ifdef ENABLE_SAMBA
    FFSS_CB.MCB.OnIndexAnswerSamba = OnIndexAnswerSamba;
#endif /* ENABLE_SAMBA */
#endif /* !DISABLE_INDEX */
    /* TCP callbacks */
    FFSS_CB.MCB.OnCheckConnection = OnCheckConnection;
    FFSS_CB.MCB.OnMasterConnected = OnMasterConnected;
    FFSS_CB.MCB.OnMasterDisconnected = OnMasterDisconnected;
    FFSS_CB.MCB.OnNewState = OnNewState;
    FFSS_CB.MCB.OnServerListingMaster = OnServerListingMaster;
    FFSS_CB.MCB.OnSearchForward = OnSearchForward;

    context;
    if(!FM_LoadConfigFile(ConfigFile,false))
    {
      context;
      FFSS_PrintSyslog(LOG_ERR,"Cannot open config file : %s\n",ConfigFile);
      FM_UnInit();
      return -1;
    }
    FFSS_MyIP = FM_MyDomain.Master;

    context;
    if(!SU_CreateThread(&FM_THR_PING,FM_ThreadPing,NULL,true))
    {
      FFSS_PrintSyslog(LOG_ERR,"Error creating PING thread\n");
      FM_UnInit();
      return -3;
    }

    if(!SU_CreateThread(&FM_THR_SEARCH,FM_ThreadSearch,NULL,true))
    {
      FFSS_PrintSyslog(LOG_ERR,"Error creating SEARCH thread\n");
      FM_UnInit();
      return -5;
    }

//#ifndef DEBUG
#ifdef __unix__
    signal(SIGTSTP,handint);
#endif /* __unix__ */
    signal(SIGINT,handint);
    signal(SIGTERM,handint);
//#endif /* !DEBUG */

    context;
    MainPid = getpid();
    fp = fopen(FM_PID_FILE,"wt");
    if(fp == NULL)
    {
      FFSS_PrintSyslog(LOG_ERR,"Cannot create pid file (%s). Check write access... exiting\n",FM_PID_FILE);
      FM_UnInit();
      return -6;
    }
    fprintf(fp,"%d",MainPid);
    fclose(fp);
    FFSS_PrintSyslog(LOG_INFO,"Master running with pid : %d\n",MainPid);

    /* Main loop */
    while(1) SU_SLEEP(10);

    /* Shutting down master */
    FM_UnInit();
  }
  return 0;
}
