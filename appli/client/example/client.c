#include <ffss.h>

//#define FFSS_XFERS_IN_CONN
/* Defines to replace with variables taken from the config file */
//#define HAVE_MASTER
#ifdef HAVE_MASTER
#define CLT_MASTER "ffss"
#endif

FFSS_PTransfer FC_FT = NULL;

void OnTransfertFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download)
{
  printf("File %s from %s for %s failed (%ld:%s)\n",Download?"download":"upload",inet_ntoa(FT->Client->SAddr.sin_addr),FT->FileName,ErrorCode,Error);
  FC_FT = NULL;
}

void OnTransfertSuccess(FFSS_PTransfer FT,bool Download)
{
  printf("File %s from %s for %s completed.\n",Download?"download":"upload",inet_ntoa(FT->Client->SAddr.sin_addr),FT->FileName);
  FC_FT = NULL;
}


/* UDP callbacks */
void OnNewState(long int State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[])
{
  static int done = 0;

  //printf("Received a new state (%ld) from %s (%s-%s-%s-%s) using master %s\n",State,IP,Domain,Name,OS,Comment,MasterIP);
  if(!done)
  {
    /* Sending shares listing */
    if(!FC_SendMessage_SharesListing(IP))
    {
      printf("Error sending Shares listing to server\n");
      exit(-1);
    }
    done = 1;
  }
}

void OnSharesListing(const char IP[],const char **Names,const char **Comments,int NbShares)
{
  /*int i;
  SU_PClientSocket CS;*/

  printf("Received shares listing of %s (%d shares) :\n",IP,NbShares);
/*  for(i=0;i<NbShares;i++)
  {
    printf("\t%s : %s\n",Names[i],Comments[i]);
    CS = FC_SendMessage_ShareConnect(IP,Names[i],NULL,NULL);
    if(CS == NULL)
      printf("Error connecting to server's %s share\n",Names[i]);
  }*/
}

/* WARNING !! (char *) of the FM_PHost structure are pointers to STATIC buffer, and must be dupped ! */
/* Except for the FM_PHost->IP that is dupped internaly, and if you don't use it, you MUST free it !! */
void OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList)
{
  SU_PList Ptr;
  FM_PHost H;
  char *States[]={"","ON","OFF","QUIET"};

  printf("Received a listing of servers for domain %s (%d servers)\n",Domain,NbHost);
  Ptr = HostList;
  while(Ptr != NULL)
  {
    H = (FM_PHost) Ptr->Data;
    printf("\t%s (%s) running %s is %s : %s\n",H->Name,H->IP,H->OS,States[H->State],H->Comment);
    FC_SendMessage_SharesListing(H->IP);
    free(H->IP);
    Ptr = Ptr->Next;
  }
}

void OnEndServerListingAnswer(void)
{
  printf("End of listing\n");
}

void OnDomainListingAnswer(const char **Domains,int NbDomains)
{
  int i;
  printf("Received a listing of domains (%d domains)\n",NbDomains);
  for(i=0;i<NbDomains;i++)
    printf("\t%s\n",Domains[i]);
}

void OnMasterSearchAnswer(struct sockaddr_in Master,FFSS_Field MasterVersion,const char Domain[])
{
  printf("Received a MASTER at ip %s using version %ld for domain %s\n",inet_ntoa(Master.sin_addr),MasterVersion,Domain);
  if(!FC_SendMessage_Search(inet_ntoa(Master.sin_addr),"Fleming","metallica mp3 toto"))
    printf("Error sending search message to master\n");
}

void OnSearchAnswer(const char Query[],const char Domain[],const char **Answers,int NbAnswers)
{
  printf("Received a SEARCH answer from domain %s for query %s : %d answers\n",Domain,Query,NbAnswers);
}


/* TCP callbacks */
bool OnError(SU_PClientSocket Server,int Code,const char Descr[])
{
  if(Code == FFSS_ERROR_NO_ERROR)
  {
    FC_SendMessage_DirectoryListing(Server,"/");
  }
  else
    printf("Received an error code from %s (%d:%s)\n",inet_ntoa(Server->SAddr.sin_addr),Code,Descr);
  return true;
}

bool OnDirectoryListingAnswer(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries)
{
  SU_PList Ptr;
  FC_PEntry Ent;

  char *tmp=NULL,buf_tmp[1024];
  char *tmp2=NULL;

  printf("Received a directory listing from %s for %s (%d entries) :\n",inet_ntoa(Server->SAddr.sin_addr),Path,NbEntries);
  Ptr = Entries;
  while(Ptr != NULL)
  {
    Ent = (FC_PEntry) Ptr->Data;
    printf("\t%s : %lld (%s) %s",Ent->Name,Ent->Size,(Ent->Flags & FFSS_FILE_DIRECTORY)?"dir":"file",ctime((time_t *)&Ent->Stamp));

    if(Ent->Flags & FFSS_FILE_DIRECTORY)
      tmp = Ent->Name;
    else
      tmp2 = Ent->Name;

    Ptr = Ptr->Next;
  }

  if(tmp != NULL)
  {
    snprintf(buf_tmp,sizeof(buf_tmp),"%s/%s",Path,tmp);
    FC_SendMessage_DirectoryListing(Server,buf_tmp);
  }

  if(tmp2 != NULL)
  {
    snprintf(buf_tmp,sizeof(buf_tmp),"%s/%s",Path,tmp2);
#ifdef FFSS_XFERS_IN_CONN
    if(FC_FT == NULL)
      FFSS_DownloadFile(Server,buf_tmp,"test_download",0,NULL,true,&FC_FT);
#else
    FFSS_DownloadFile(Server,buf_tmp,"test_download",0,NULL,false,&FC_FT);
#endif
  }

  return true;
}

FFSS_PTransfer OnInitXFer(SU_PClientSocket Server,const char RequestedFileName[])
{
  return FC_FT;
}

FFSS_PTransfer OnData(SU_PClientSocket Server,FFSS_Field XFerTag)
{
  return FC_FT;
}

/* Fatal error, must shutdown */
void OnUDPError()
{
  /* Shutting down server */
  FC_UnInit();
  exit(-1);
}

int main()
{
//if(fork() == 0)
  {
    memset(&FFSS_CB,0,sizeof(FFSS_CB));
    FFSS_CB.CCB.OnNewState = OnNewState;
    FFSS_CB.CCB.OnSharesListing = OnSharesListing;
    FFSS_CB.CCB.OnServerListingAnswer = OnServerListingAnswer;
    FFSS_CB.CCB.OnEndServerListingAnswer = OnEndServerListingAnswer;
    FFSS_CB.CCB.OnDomainListingAnswer = OnDomainListingAnswer;
    FFSS_CB.CCB.OnMasterSearchAnswer = OnMasterSearchAnswer;
    FFSS_CB.CCB.OnSearchAnswer = OnSearchAnswer;
    FFSS_CB.CCB.OnError = OnError;
    FFSS_CB.CCB.OnDirectoryListingAnswer = OnDirectoryListingAnswer;
    FFSS_CB.CCB.OnTransferFailed = OnTransfertFailed;
    FFSS_CB.CCB.OnTransferSuccess = OnTransfertSuccess;
    FFSS_CB.CCB.OnUDPError = OnUDPError;
    FFSS_CB.CCB.OnInitXFer = OnInitXFer;
    FFSS_CB.CCB.OnData = OnData;

    if(!FC_Init())
      return -1;
    printf("Client running...\n");

    FC_SendMessage_ShareConnect("172.17.64.15","test","kiki","toto");
    //FC_SendMessage_ShareConnect("172.17.64.15","test",NULL,NULL);
    sleep(2);
    return 0;

    /* Sending server request message to broadcast */
#ifdef HAVE_MASTER
    FC_SendMessage_MasterSearch();
    FC_SendMessage_DomainListing(CLT_MASTER);
    FC_SendMessage_ServerList(CLT_MASTER,NULL,NULL);
#else
    if(!FC_SendMessage_MasterSearch())
    {
      printf("Error sending master search\n");
      /* Shutting down server */
      FC_UnInit();
      return 0;
    }
#endif
    pthread_join(FC_THR_UDP,NULL);

    /* Sending logout message to my master (if existing) */

    /* Shutting down server */
    FC_UnInit();
  }
  return 0;
}
