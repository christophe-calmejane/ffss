#include <ffss.h>

//#define FFSS_XFERS_IN_CONN 1
//#define FFSS_XFERS_BLOC 1

#define STARTPOS 0
#define ENDPOS 0

/* Defines to replace with variables taken from the config file */
//#define HAVE_MASTER
#ifdef HAVE_MASTER
#define CLT_MASTER "ffss"
#endif

FFSS_PTransfer FC_FT = NULL;
FILE *FC_fp = NULL;

void OnTransfertFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download)
{
  printf("File %s from %s for %s failed (%ld:%s)\n",Download?"download":"upload",inet_ntoa(FT->Client->SAddr.sin_addr),FT->FileName,ErrorCode,Error);
  FC_FT = NULL;
#ifdef FFSS_XFERS_BLOC
  fclose(FC_fp);
  FC_fp = NULL;
#endif
}

void OnTransfertSuccess(FFSS_PTransfer FT,bool Download)
{
  printf("File %s from %s for %s completed.\n",Download?"download":"upload",inet_ntoa(FT->Client->SAddr.sin_addr),FT->FileName);
  FC_FT = NULL;
#ifdef FFSS_XFERS_BLOC
  fclose(FC_fp);
  FC_fp = NULL;
#endif
}

#ifdef FFSS_XFERS_BLOC
bool OnTransferFileWrite(FFSS_PTransfer FT,const char Buf[],FFSS_Field Size,FFSS_LongField Offset) /* 'Offset' from FT->StartingPos */ /* True on success */
{
  printf("Seeking to %lld + %lld\n",FT->StartingPos,Offset);
  if(fseek(FC_fp,FT->StartingPos+Offset,SEEK_SET) != 0)
    return false;
  return fwrite(Buf,1,Size,FC_fp) == Size;
}
#endif

/* UDP callbacks */
void OnShortMessage(struct sockaddr_in Server,const char Message[])
{
  printf("Server answered to my short message : \n%s\n",Message);
}

void OnNewState(FFSS_Field State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[])
{
  static int done = 0;

  //printf("Received a new state (%ld) from %s (%s-%s-%s-%s) using master %s\n",State,IP,Domain,Name,OS,Comment,MasterIP);
  if(!done)
  {
    /* Sending shares listing */
    if(!FC_SendMessage_SharesListing(IP,0))
    {
      printf("Error sending Shares listing to server\n");
      exit(-1);
    }
    done = 1;
  }
}

void OnSharesListing(const char IP[],const char **Names,const char **Comments,int NbShares,FFSS_LongField User)
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
void OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList,FFSS_LongField User) /* SU_PList of FM_PHost */
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
    FC_SendMessage_SharesListing(H->IP,0);
    free(H->IP);
    Ptr = Ptr->Next;
  }
}

void OnEndServerListingAnswer(void)
{
  printf("End of listing\n");
}

void OnDomainListingAnswer(const char **Domains,int NbDomains,FFSS_LongField User) /* First domain is assumed to be domain from the answering master */
{
  int i;
  printf("Received a listing of domains (%d domains)\n",NbDomains);
  for(i=0;i<NbDomains;i++)
    printf("\t%s\n",Domains[i]);
}

void OnMasterSearchAnswer(struct sockaddr_in Master,FFSS_Field ProtocolVersion,const char Domain[],FFSS_LongField User)
{
  printf("Received a MASTER at ip %s using version %ld for domain %s\n",inet_ntoa(Master.sin_addr),ProtocolVersion,Domain);
  if(!FC_SendMessage_Search(inet_ntoa(Master.sin_addr),"Fleming","metallica mp3 toto",0))
    printf("Error sending search message to master\n");
}

/* Each IP from IPs table is dupped internaly, and if you don't use it, you MUST free it !! */
void OnSearchAnswer(const char Query[],const char Domain[],const char **Answers,char **IPs,FFSS_LongField *ChkSums,FFSS_LongField *Sizes,int NbAnswers,FFSS_LongField User)
{
  printf("Received a SEARCH answer from domain %s for query %s : %d answers\n",Domain,Query,NbAnswers);
}


/* TCP callbacks */
bool OnError(SU_PClientSocket Server,FFSS_Field ErrorCode,const char Descr[],FFSS_LongField Value,FFSS_LongField User)
{
  if(ErrorCode == FFSS_ERROR_NO_ERROR)
  {
    FC_SendMessage_DirectoryListing(Server,"/",0);
  }
  else
    printf("Received an error code from %s (%ld:%s)\n",inet_ntoa(Server->SAddr.sin_addr),ErrorCode,Descr);
  return true;
}

bool OnDirectoryListingAnswer(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries,FFSS_LongField User) /* FC_PEntry */
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
    FC_SendMessage_DirectoryListing(Server,buf_tmp,0);
  }

  if(tmp2 != NULL)
  {
    snprintf(buf_tmp,sizeof(buf_tmp),"%s/%s",Path,tmp2);
#ifdef FFSS_XFERS_IN_CONN
    if(FC_FT == NULL)
      FFSS_DownloadFile(Server,buf_tmp,"test_download",STARTPOS,ENDPOS,NULL,true,0,&FC_FT);
#elif FFSS_XFERS_BLOC
    FC_fp = fopen("test_bloc","rb+");
    if(FC_fp == NULL)
    {
      FC_fp = fopen("test_bloc","wb+");
      if(FC_fp == NULL)
      {
        abort();
        return false;
      }
    }
    FFSS_DownloadFile(Server,buf_tmp,NULL,STARTPOS,ENDPOS,NULL,false,0,&FC_FT);
#else
    FFSS_DownloadFile(Server,buf_tmp,"test_download",STARTPOS,ENDPOS,NULL,false,0,&FC_FT);
#endif
  }

  return true;
}

FFSS_PTransfer OnInitXFer(SU_PClientSocket Server,const char RequestedFileName[],FFSS_Field XFerTag)
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
    FFSS_CB.CCB.OnShortMessage = OnShortMessage;
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
#ifdef FFSS_XFERS_BLOC
    FFSS_CB.CCB.OnTransferFileWrite = OnTransferFileWrite;
#endif
    FFSS_CB.CCB.OnUDPError = OnUDPError;
    FFSS_CB.CCB.OnInitXFer = OnInitXFer;
    FFSS_CB.CCB.OnData = OnData;

  SU_DBG_SetOutput(SU_DBG_OUTPUT_PRINTF);
  SU_DBG_SetOptions(true,true);
#ifdef DEBUG
  SU_DBG_SetFlags(FFSS_DBGMSG_ALL);
#endif /* DEBUG */

    if(!FC_Init())
      return -1;
    printf("Client running...\n");

    FC_SendMessage_ShortMessage("10.0.0.2","hello");
    //FC_SendMessage_ShareConnect("10.0.0.2","mp3",NULL,NULL,0);
    //FC_SendMessage_ShareConnect("172.17.64.135","debug",NULL,NULL);
    sleep(20);
    return 0;

    /* Sending server request message to broadcast */
#ifdef HAVE_MASTER
    FC_SendMessage_MasterSearch();
    FC_SendMessage_DomainListing(CLT_MASTER);
    FC_SendMessage_ServerList(CLT_MASTER,NULL,NULL);
#else
    if(!FC_SendMessage_MasterSearch(0))
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
