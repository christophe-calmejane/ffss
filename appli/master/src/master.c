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
#ifdef __unix__
sem_t FM_MySem;  /* Semaphore to protect the use of FM_MyQueue */
sem_t FM_MySem2; /* Semaphore to protect the use of the Hosts of FM_MyDomain */
sem_t FM_MySem3; /* Semaphore to protect the use of FM_OtherQueue */
sem_t FM_MySem4; /* Semaphore to protect the use of FM_SearchQueue */
sem_t FM_MySem5; /* Semaphore to protect the use of the index */
#else /* __unix__ */
HANDLE FM_MySem;  /* Semaphore to protect the use of FM_MyQueue */
HANDLE FM_MySem2; /* Semaphore to protect the use of the Hosts of FM_MyDomain */
HANDLE FM_MySem3; /* Semaphore to protect the use of FM_OtherQueue */
HANDLE FM_MySem4; /* Semaphore to protect the use of FM_SearchQueue */
HANDLE FM_MySem5; /* Semaphore to protect the use of the index */
#endif /* __unix__ */

#ifdef _WIN32
unsigned long FM_THR_PING,FM_THR_QUEUE;
#else /* _WIN32 */
pthread_t FM_THR_PING,FM_THR_QUEUE;
#endif /* _WIN32 */

volatile FFSS_Field FM_CurrentIndexSize = 0;
FFSS_Field FM_CurrentFileTreeSize = 0;
FFSS_Field FM_CurrentNodesSize = 0;
int FM_CurrentCompression = FFSS_COMPRESSION_NONE;
bool FM_CurrentSamba;
struct sockaddr_in FM_CurrentClient;
char *FM_User=NULL,*FM_Group=NULL;
FILE *FM_SearchLogFile=NULL;

bool FM_IsMyDomain(FM_PDomain Dom)
{
  return (Dom == (&FM_MyDomain));
}

FM_PHost FM_SearchHostByIP(FM_PDomain Domain,const char IP[])
{
  SU_PList Ptr;

  context;
  Ptr = Domain->Hosts;
  while(Ptr != NULL)
  {
    if(strcmp(((FM_PHost)Ptr->Data)->IP,IP) == 0)
      return (FM_PHost)Ptr->Data;
    Ptr = Ptr->Next;
  }
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
  printf("MASTER : Adding host %s (%s) to domain %s\n",Name,IP,Domain->Name);
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
  if(Domain == (&FM_MyDomain))
  {
#ifdef __unix__
    sem_wait(&FM_MySem2);
#else /* __unix__ */
    WaitForSingleObject(FM_MySem2,INFINITE);
#endif /* __unix__ */
    Domain->Hosts = SU_AddElementHead(Domain->Hosts,Hst);
#ifdef __unix__
    sem_post(&FM_MySem2);
#else /* __unix__ */
    ReleaseSemaphore(FM_MySem2,1,NULL);
#endif /* __unix__ */
  }
  else
    Domain->Hosts = SU_AddElementHead(Domain->Hosts,Hst);

  return Hst;
}

void FM_UpdateHost(FM_PHost Hst,const char Name[],const char OS[],const char Comment[],FFSS_Field State)
{
  context;
#ifdef __unix__
    sem_wait(&FM_MySem2);
#else /* __unix__ */
    WaitForSingleObject(FM_MySem2,INFINITE);
#endif /* __unix__ */
#ifdef DEBUG
  printf("Updating host info : %s - %s - %s - %ld\n",Name,OS,Comment,State);
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
#ifdef __unix__
    sem_post(&FM_MySem2);
#else /* __unix__ */
    ReleaseSemaphore(FM_MySem2,1,NULL);
#endif /* __unix__ */
}

bool FM_IsHostInMyQueue(FM_PHost Hst)
{
  SU_PList Ptr;

  context;
  Ptr = FM_MyQueue;
  while(Ptr != NULL)
  {
    if(((FM_PQueue)Ptr->Data)->Host == Hst)
//    if(strcmp(((FM_PQueue)Ptr->Data)->Host->IP,Hst->IP) == 0)
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
//    if(strcmp(((FM_PQueue)Ptr->Data)->Host->IP,Hst->IP) == 0)
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
  printf("MASTER : Adding state to my queue : %s (%s) %ld\n",Hst->Name,Hst->IP,Hst->State);
#endif /* DEBUG */
  if(FM_IsHostInMyQueue(Hst))
  {
#ifdef DEBUG
    printf("MASTER : WARNING : Host %s already in my queue\n",Hst->IP);
#endif /* DEBUG */
    return;
  }
  if(Hst->State == FFSS_STATE_OFF)
    Hst->OffSince = time(NULL);
  else
    Hst->OffSince = 0;

  Que = (FM_PQueue) malloc(sizeof(FM_TQueue));
  Que->Domain = Domain;
  Que->Host = Hst;
#ifdef __unix__
  sem_wait(&FM_MySem);
#else /* __unix__ */
  WaitForSingleObject(FM_MySem,INFINITE);
#endif /* __unix__ */
  FM_MyQueue = SU_AddElementHead(FM_MyQueue,Que);
#ifdef __unix__
  sem_post(&FM_MySem);
#else /* __unix__ */
  ReleaseSemaphore(FM_MySem,1,NULL);
#endif /* __unix__ */
}

void FM_AddStateToOtherQueue(FM_PDomain Domain,FM_PHost Hst)
{
  FM_PQueue Que;

  context;
#ifdef DEBUG
  printf("MASTER : Adding state to other queue : %s (%s) %ld\n",Hst->Name,Hst->IP,Hst->State);
#endif /* DEBUG */
  if(FM_IsHostInOtherQueue(Hst))
  {
#ifdef DEBUG
    printf("MASTER : WARNING : Host %s already in other queue\n",Hst->IP);
#endif /* DEBUG */
    return;
  }
  if(Hst->State == FFSS_STATE_OFF)
    Hst->OffSince = time(NULL);
  else
    Hst->OffSince = 0;

  Que = (FM_PQueue) malloc(sizeof(FM_TQueue));
  Que->Domain = Domain;
  Que->Host = Hst;
#ifdef __unix__
  sem_wait(&FM_MySem3);
#else /* __unix__ */
  WaitForSingleObject(FM_MySem3,INFINITE);
#endif /* __unix__ */
  FM_OtherQueue = SU_AddElementHead(FM_OtherQueue,Que);
#ifdef __unix__
  sem_post(&FM_MySem3);
#else /* __unix__ */
  ReleaseSemaphore(FM_MySem3,1,NULL);
#endif /* __unix__ */
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
void OnState(struct sockaddr_in Server,FFSS_Field State,FFSS_Field ServerVersion,const char Name[],const char OS[],const char Comment[])
{
  FM_PHost Hst;

  context;
  if(ServerVersion < FFSS_PROTOCOLE_VERSION)
  {
    if(!FM_SendMessage_Error(inet_ntoa(Server.sin_addr),FFSS_ERROR_SERVER_TOO_OLD,FFSS_ErrorTable[FFSS_ERROR_SERVER_TOO_OLD]))
      printf("MASTER : Error sending error to server : %s\n",strerror(errno));
  }
  else
  {
    Hst = FM_SearchHostByIP(&FM_MyDomain,inet_ntoa(Server.sin_addr));
    if(Hst == NULL)
      Hst = FM_AddHostToDomain(&FM_MyDomain,Name,OS,Comment,inet_ntoa(Server.sin_addr),State);
    else
      FM_UpdateHost(Hst,Name,OS,Comment,State);
    FM_AddStateToMyQueue(&FM_MyDomain,Hst);
  }
}

/* OnNewState is raised by foreign masters */
void OnNewState(FFSS_Field State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[])
{
  FM_PDomain Dom;
  FM_PHost Hst;

  context;
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
  if(FM_IsMyDomain(Dom))
    FM_AddStateToMyQueue(Dom,Hst);
  else
    FM_AddStateToOtherQueue(Dom,Hst);
}

/* OnServerListing is raised by local clients or foreign masters */
void OnServerListing(struct sockaddr_in Client,const char OS[],const char Domain[],long int Compressions)
{
  char *buf,*s_dom,*s_os;
  long int len;
  SU_PList Queue,Ptr;
  FM_PQueue Que;
  FM_PDomain Dom;
  long int comp;

  context;
  buf = inet_ntoa(Client.sin_addr);
  /* Request from a master ? */
  Dom = FM_SearchDomainByIP(buf);
  if((Dom != NULL) && (strcmp(FM_MyDomain.Master,buf) != 0))
  {
    if(Dom->Listed == false) /* If I don't have asked for hosts of this domain */
    {
#ifdef DEBUG
      printf("MASTER : I don't have any hosts for domain %s... requesting hosts list\n",Dom->Name);
#endif /* DEBUG */
      if(FM_SendMessage_ServerList(Dom->Master))
        Dom->Listed = true;
    }
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
      FM_SendMessage_NewStatesMaster(inet_ntoa(Client.sin_addr),buf,len,FFSS_COMPRESSION_BZLIB);
      free(buf);
    }
  }
  else
  {
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
      if(Ptr->Data == (&FM_MyDomain))
        FM_AddStateToMyQueue((FM_PDomain)Ptr->Data,Hst);
      else
        FM_AddStateToOtherQueue((FM_PDomain)Ptr->Data,Hst);
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
    Hst->State = State;
    Hst->LastPong = time(NULL);
    Hst->OffSince = 0;
  }
}

/* OnDomainListing is raised by local clients */
void OnDomainListing(struct sockaddr_in Client)
{
  FM_SendMessage_DomainListingAnswer(Client,FM_Domains);
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

#ifdef __unix__
  sem_wait(&FM_MySem4);
#else /* __unix__ */
  WaitForSingleObject(FM_MySem4,INFINITE);
#endif /* __unix__ */
  FM_SearchQueue = SU_AddElementTail(FM_SearchQueue,Sch);
#ifdef __unix__
  sem_post(&FM_MySem4);
#else /* __unix__ */
  ReleaseSemaphore(FM_MySem4,1,NULL);
#endif /* __unix__ */
}

/* OnSearchForward is raised by foreign masters */
void OnSearchForward(struct sockaddr_in Master,const char ClientIP[],int Port,const char KeyWords[],long int Compressions)
{
  FM_PSearch Sch;
  FM_PDomain Dom;

  context;
#ifdef DEBUG
  printf("Received a SEARCH FORWARD message : %s\n",KeyWords);
#endif /* DEBUG */

  Dom = FM_SearchDomainByIP(inet_ntoa(Master.sin_addr));
  if(Dom == NULL)
  {
#ifdef DEBUG
    printf("MASTER : WARNING : Domain not found searching with this ip : %s\n",inet_ntoa(Master.sin_addr));
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

#ifdef __unix__
  sem_wait(&FM_MySem4);
#else /* __unix__ */
  WaitForSingleObject(FM_MySem4,INFINITE);
#endif /* __unix__ */
  FM_SearchQueue = SU_AddElementTail(FM_SearchQueue,Sch);
#ifdef __unix__
  sem_post(&FM_MySem4);
#else /* __unix__ */
  ReleaseSemaphore(FM_MySem4,1,NULL);
#endif /* __unix__ */
}

/* OnMasterSearch is raised by local clients */
void OnMasterSearch(struct sockaddr_in Client,bool Server)
{
  FM_SendMessage_MasterSearchAnswer(Client,Server,FM_MyDomain.Name);
}

#ifdef _WIN32
void ThreadIndexing(void *Info)
#else /* _WIN32 */
void *ThreadIndexing(void *Info)
#endif /* _WIN32 */
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
  char *buf,*data;
  bool error,free_it;
  FM_PFTControler Host;
  int NumHost;
  FM_PHost Hst = NULL;

  context;
#ifdef DEBUG
  printf("Starting index thread\n");
#endif /* DEBUG */
  memcpy(&SAddr,&FM_CurrentClient,sizeof(FM_CurrentClient));
  FM_CurrentIndexSize = 0;
  if(!Samba)
  {
    Hst = FM_SearchHostByIP(&FM_MyDomain,inet_ntoa(SAddr.sin_addr));
    if(Hst == NULL)
    {
#ifdef DEBUG
      printf("%s is not on my domain... rejecting index\n",inet_ntoa(SAddr.sin_addr));
#endif /* DEBUG */
#ifdef __unix__
      close(sock);
#else /* __unix__ */
      closesocket(sock);
#endif /* __unix__ */
#ifdef __unix__
      return 0;
#endif /* __unix__ */
    }
  }

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
#ifdef __unix__
    close(sock);
#else /* __unix__ */
    closesocket(sock);
#endif /* __unix__ */
#ifdef _WIN32
    _endthread();
#else /* _WIN32 */
    return (void *)1;
#endif /* _WIN32 */
  }
  res = recv(sock,(char *)&NbShares,sizeof(NbShares),SU_MSG_NOSIGNAL);
  if(res <= 0)
  {
#ifdef __unix__
    close(sock);
#else /* __unix__ */
    closesocket(sock);
#endif /* __unix__ */
#ifdef _WIN32
    _endthread();
#else /* _WIN32 */
    return (void *)1;
#endif /* _WIN32 */
  }
  Host = (FM_PFTControler) malloc(sizeof(FM_TFTControler));
  memset(Host,0,sizeof(FM_TFTControler));
  if(!Samba)
    Host->Name = strdup(Hst->Name);
  Host->FileTree = (char *) malloc(FileTreeSize);
  Host->FileTreeLength = FileTreeSize;
  Host->NbNodes = NodesSize/sizeof(FM_TFTNode);
  Host->FTNodes = (FM_TFTNode *) malloc(NodesSize);
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
#ifdef __unix__
      close(sock);
#else /* __unix__ */
      closesocket(sock);
#endif /* __unix__ */
#ifdef _WIN32
      _endthread();
#else /* _WIN32 */
      return (void *)1;
#endif /* _WIN32 */
    }
    res = recv(sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
    if(res <= 0)
    {
#ifdef DEBUG
      printf("WARNING : Error receiving index from server\n");
#endif /* DEBUG */
      FMI_FreeFTControler(Host);
#ifdef __unix__
      close(sock);
#else /* __unix__ */
      closesocket(sock);
#endif /* __unix__ */
#ifdef _WIN32
      _endthread();
#else /* _WIN32 */
      return (void *)1;
#endif /* _WIN32 */
    }
    total += Size;
    if(total > IndexSize)
    {
      printf("WARNING : Index sent is greater than specified size : %ld-%ld\n",total,IndexSize);
      FMI_FreeFTControler(Host);
#ifdef __unix__
      close(sock);
#else /* __unix__ */
      closesocket(sock);
#endif /* __unix__ */
#ifdef _WIN32
      _endthread();
#else /* _WIN32 */
      return (void *)1;
#endif /* _WIN32 */
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
#ifdef __unix__
        close(sock);
#else /* __unix__ */
        closesocket(sock);
#endif /* __unix__ */
#ifdef _WIN32
        _endthread();
#else /* _WIN32 */
        return (void *)1;
#endif /* _WIN32 */
      }
      res = recv(sock,buf+actual,Size-actual,SU_MSG_NOSIGNAL);
      if(res <= 0)
      {
#ifdef DEBUG
        printf("WARNING : Error receiving index from server\n");
#endif /* DEBUG */
        FMI_FreeFTControler(Host);
#ifdef __unix__
        close(sock);
#else /* __unix__ */
        closesocket(sock);
#endif /* __unix__ */
        free(buf);
#ifdef _WIN32
        _endthread();
#else /* _WIN32 */
        return (void *)1;
#endif /* _WIN32 */
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
          FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(SAddr.sin_addr));
          error = true;
          break;
        }
        free_it = true;
        break;
      case FFSS_COMPRESSION_BZLIB :
        data = FFSS_UncompresseBZlib(buf,Size,&length);
        if(data == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"Corrupted BZ compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(SAddr.sin_addr));
          error = true;
          break;
        }
        free_it = true;
        break;
      default :
        FFSS_PrintSyslog(LOG_WARNING,"Unknown compression type (%s) : %d\n",Compression,inet_ntoa(SAddr.sin_addr));
        error = true;
    }
    if(error)
    {
      FMI_FreeFTControler(Host);
#ifdef __unix__
      close(sock);
#else /* __unix__ */
      closesocket(sock);
#endif /* __unix__ */
      free(buf);
#ifdef _WIN32
      _endthread();
#else /* _WIN32 */
      return (void *)1;
#endif /* _WIN32 */
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
#ifdef __unix__
  close(sock);
#else /* __unix__ */
  closesocket(sock);
#endif /* __unix__ */
  if(Samba)
    Host->Name = strdup(Host->FileTree);
#ifdef DEBUG
  printf("Index thread : buffer received... now indexing\n");
#endif /* DEBUG */
#ifdef __unix__
  sem_wait(&FM_MySem5);
#else /* __unix__ */
  WaitForSingleObject(FM_MySem5,INFINITE);
#endif /* __unix__ */
  context;
  NumHost = -1;
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
#ifdef __unix__
  sem_post(&FM_MySem5);
#else /* __unix__ */
  ReleaseSemaphore(FM_MySem5,1,NULL);
#endif /* __unix__ */
#ifdef __unix__
  return 0;
#endif /* __unix__ */
}

void GlobalIndexAnswer(struct sockaddr_in Client,FFSS_Field CompressionType,FFSS_Field IndexSize,FFSS_Field FileTreeSize,FFSS_Field NodesSize,int Port,bool Samba)
{
  FM_PHost Hst;
  time_t tim;
  int sock=0;
  struct sockaddr_in SAddr;
#ifdef _WIN32
  unsigned long ClientThr;
#else /* _WIN32 */
  pthread_t ClientThr;
#endif /* _WIN32 */

  context;
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

  sock = socket(AF_INET,SOCK_STREAM,getprotobyname("tcp")->p_proto);
  if(sock == SOCKET_ERROR)
    return;
  SAddr.sin_family = AF_INET;
  SAddr.sin_port = htons(Port);
  SAddr.sin_addr.s_addr = Client.sin_addr.s_addr;
  if(connect(sock,(struct sockaddr *)(&SAddr),sizeof(SAddr)) == SOCKET_ERROR)
  {
#ifdef __unix__
    close(sock);
#else /* __unix__ */
    closesocket(sock);
#endif /* __unix__ */
    return;
  }
  while(FM_CurrentIndexSize != 0)
#ifdef __unix__
    sleep(1);
#else /* __unix__ */
    Sleep(1000);
#endif /* __unix__ */
  FM_CurrentIndexSize = IndexSize;
  FM_CurrentFileTreeSize = FileTreeSize;
  FM_CurrentNodesSize = NodesSize;
  FM_CurrentCompression = CompressionType;
  FM_CurrentSamba = Samba;
  memcpy(&FM_CurrentClient,&Client,sizeof(FM_CurrentClient));
#ifdef _WIN32
  if((ClientThr = _beginthread(ThreadIndexing,0,(void *)sock)) == -1)
#else /* _WIN32 */
  if(pthread_create(&ClientThr,NULL,&ThreadIndexing,(void *)sock) != 0)
#endif /* _WIN32 */
  {
#ifdef __unix__
    close(sock);
#else /* __unix__ */
    closesocket(sock);
#endif /* __unix__ */
    return;
  }
#ifdef __unix__
  pthread_detach(ClientThr);
#endif /* __unix__ */
}

void OnIndexAnswer(struct sockaddr_in Client,FFSS_Field CompressionType,FFSS_Field IndexSize,FFSS_Field FileTreeSize,FFSS_Field NodesSize,int Port)
{
  GlobalIndexAnswer(Client,CompressionType,IndexSize,FileTreeSize,NodesSize,Port,false);
}
void OnIndexAnswerSamba(struct sockaddr_in Client,FFSS_Field CompressionType,FFSS_Field IndexSize,FFSS_Field FileTreeSize,FFSS_Field NodesSize,int Port)
{
  GlobalIndexAnswer(Client,CompressionType,IndexSize,FileTreeSize,NodesSize,Port,true);
}

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

#ifdef __unix__
void handint(int sig)
{
  static bool done = false;

  if(!done)
  {
    done = true;
    /* Close log file, if opened */
    if(FM_SearchLogFile != NULL)
      SU_CloseLogFile(FM_SearchLogFile);
    /* Writing hosts */
    FM_SaveHosts(FM_MyDomain.Hosts,FM_MYHOSTS_FILE);
    /* Writing index */
    FMI_SaveIndex(FM_MYINDEX_FILE);
    /* Shutting down master */
    FM_UnInit();
    exit(0);
  }
  else
    sleep(5000);
}
#endif /* __unix__ */

#ifdef _WIN32
#ifdef DEBUG
int main(int argc,char *argv[])
#else /* DEBUG */
int APIENTRY WinMain(HINSTANCE hInstance,HINSTANCE hPrevInstance,LPSTR lpCmdLine,int nCmdShow)
#endif /* DEBUG */
#else /* _WIN32 */
int main(int argc,char *argv[])
#endif /* _WIN32 */
{
  int i;
  char ConfigFile[1024];
  bool daemonize = false;
  bool log = false;
#ifdef __unix__
  pthread_t Thread;
  struct sigaction action;
#else /* __unix__ */
  unsigned long Thread;
#endif /* __unix__ */

  printf("FFSS Master v%s (c) Ze KiLleR / SkyTech 2001\n",FFSS_MASTER_VERSION);
  printf("%s\n",FFSS_COPYRIGHT);

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
#else /* __unix__ */
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
    FM_UnInit();
    return -1;
  }
#ifdef __unix__
  if(getuid() == 0)
  {
    if(!SU_SetUserGroup(NULL,FM_Group))
      FFSS_PrintSyslog(LOG_WARNING,"Warning : cannot setgid to group %s\n",FM_Group);
    if(!SU_SetUserGroup(FM_User,NULL))
      FFSS_PrintSyslog(LOG_WARNING,"Warning : cannot setuid to user %s\n",FM_User);
  }
  else
    FFSS_PrintSyslog(LOG_WARNING,"Warning : Master launched from a non-root user. Cannot use setuid/setgid\n");
#endif /* __unix__ */

#ifdef __unix__
#ifndef DEBUG
  if(fork() == 0)
#endif /* DEBUG */
#endif /* __unix__ */
  {
#ifdef __unix__
    sem_init(&FM_MySem,0,1);
    sem_init(&FM_MySem2,0,1);
    sem_init(&FM_MySem3,0,1);
    sem_init(&FM_MySem4,0,1);
    sem_init(&FM_MySem5,0,1);
#else /* __unix__ */
    FM_MySem = CreateSemaphore(NULL,1,1,"FFSSMasterSem");
    if(FM_MySem == NULL)
    {
      printf("FFSS Server Error : Couldn't allocate semaphore\n");
      return -3;
    }
    FM_MySem2 = CreateSemaphore(NULL,1,1,"FFSSMasterSem2");
    if(FM_MySem2 == NULL)
    {
      printf("FFSS Server Error : Couldn't allocate semaphore\n");
      return -3;
    }
    FM_MySem3 = CreateSemaphore(NULL,1,1,"FFSSMasterSem3");
    if(FM_MySem3 == NULL)
    {
      printf("FFSS Server Error : Couldn't allocate semaphore\n");
      return -3;
    }
    FM_MySem4 = CreateSemaphore(NULL,1,1,"FFSSMasterSem4");
    if(FM_MySem4 == NULL)
    {
      printf("FFSS Server Error : Couldn't allocate semaphore\n");
      return -3;
    }
    FM_MySem5 = CreateSemaphore(NULL,1,1,"FFSSMasterSem5");
    if(FM_MySem5 == NULL)
    {
      printf("FFSS Server Error : Couldn't allocate semaphore\n");
      return -3;
    }
#endif /* __unix__ */
    context;
    FMI_IndexInit(); /* Init indexing engine */
    context;
#ifdef DEBUG
    FMI_LoadIndex("./Dump.dat");
#else /* DEBUG */
    FMI_LoadIndex(FM_MYINDEX_FILE);
#endif /* DEBUG */
    if(log)
      FM_SearchLogFile = SU_OpenLogFile(FM_SEARCHLOG_FILE);

    context;
    if(!FM_Init(FFSS_MASTER_PORT))
      return -2;

    memset(&FFSS_CB,0,sizeof(FFSS_CB));
    /* UDP callbacks */
    FFSS_CB.MCB.OnState = OnState;
    FFSS_CB.MCB.OnNewState = OnNewState;
    FFSS_CB.MCB.OnServerListing = OnServerListing;
    FFSS_CB.MCB.OnClientServerFailed = OnClientServerFailed;
    FFSS_CB.MCB.OnPong = OnPong;
    FFSS_CB.MCB.OnDomainListing = OnDomainListing;
    FFSS_CB.MCB.OnSearch = OnSearch;
    FFSS_CB.MCB.OnSearchForward = OnSearchForward;
    FFSS_CB.MCB.OnMasterSearch = OnMasterSearch;
#ifndef DISABLE_INDEX
    FFSS_CB.MCB.OnIndexAnswer = OnIndexAnswer;
#ifdef ENABLE_SAMBA
    FFSS_CB.MCB.OnIndexAnswerSamba = OnIndexAnswerSamba;
#endif
#endif

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
#ifdef __unix__
    if(pthread_create(&Thread,NULL,&FM_ThreadPing,NULL) != 0)
#else /* __unix__ */
    if((Thread = _beginthread(FM_ThreadPing,0,NULL)) == -1)
#endif /* __unix__ */
    {
      FFSS_PrintSyslog(LOG_ERR,"Error creating PING thread\n");
      FM_UnInit();
      return -3;
    }
#ifdef __unix__
    pthread_detach(Thread);
#endif /* __unix__ */

#ifdef __unix__
    if(pthread_create(&Thread,NULL,&FM_ThreadQueue,NULL) != 0)
#else /* __unix__ */
    if((Thread = _beginthread(FM_ThreadQueue,0,NULL)) == -1)
#endif /* __unix__ */
    {
      FFSS_PrintSyslog(LOG_ERR,"Error creating QUEUE thread\n");
      FM_UnInit();
      return -4;
    }
#ifdef __unix__
    pthread_detach(Thread);
#endif /* __unix__ */

#ifdef __unix__
    if(pthread_create(&Thread,NULL,&FM_ThreadSearch,NULL) != 0)
#else /* __unix__ */
    if((Thread = _beginthread(FM_ThreadSearch,0,NULL)) == -1)
#endif /* __unix__ */
    {
      FFSS_PrintSyslog(LOG_ERR,"Error creating SEARCH thread\n");
      FM_UnInit();
      return -5;
    }
#ifdef __unix__
    sigemptyset(&action.sa_mask);
    action.sa_flags=0;
    action.sa_handler=handint;
#ifndef DEBUG
    sigaction(SIGINT,&action,NULL);
    sigaction(SIGTSTP,&action,NULL);
    sigaction(SIGTERM,&action,NULL);
#endif /* DEBUG */

    pthread_detach(Thread);
#endif /* __unix__ */

    FFSS_PrintSyslog(LOG_INFO,"Master running with pid : %d\n",getpid());

#ifdef __unix__
    pthread_join(FM_THR_UDP,NULL);
#else /* __unix__ */
    while(1) Sleep(10000);
#endif /* __unix__ */

    /* Shutting down master */
    FM_UnInit();
  }
  return 0;
}
