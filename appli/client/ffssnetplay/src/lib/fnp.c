#include "fnp.h"

/*
  TODO : Attention, le send du StrmClose ne passe pas (bad file descriptor sur la socket)
         Envoyer un StrmClose qd on change de chanson !!!
*/

SU_PClientSocket FNP_CurrentServer = NULL;
SU_PClientSocket FNP_CurrentPlayer = NULL;
char *FNP_CurrentIP = NULL;
char *FNP_CurrentShare = NULL;
char *FNP_CurrentFile = NULL;
long int FNP_CurrentFileHandle = 0;
FFSS_LongField FNP_CurrentFilePos = 0;
FFSS_LongField FNP_CurrentFileSize = 0;

char *FNP_Master = NULL;
char FNP_Error[1024];
SU_SEM_HANDLE FNP_Sem;
SU_THREAD_HANDLE FNP_Thread;

FNP_TCB FNP_CB;


/* ******************* FFSS CALLBACKS ******************* */
/* FFSS UDP callbacks */
/* Each IP from IPs table is dupped internaly, and if you don't use it, you MUST free it !! */
void FFSS_OnSearchAnswer(const char Query[],const char Domain[],const char **Answers,char **IPs,int NbAnswers,FFSS_LongField User)
{
  int i;

  if(FNP_CB.OnSearchAnswerStart != NULL)
    FNP_CB.OnSearchAnswerStart();
  for(i=0;i<NbAnswers;i++)
  {
    if(((Answers[i][0] & FFSS_STATE_ALL) == FFSS_STATE_ON) /* Host is ON */
      && ((Answers[i][0] & FFSS_SEARCH_IS_FILE) == FFSS_SEARCH_IS_FILE) /* Answer is a file */
      && ((Answers[i][0] & FFSS_SEARCH_IS_SAMBA) == 0)) /* Answer is NOT on a samba share */
    {
      char *p;
      p = SU_nocasestrstr((char *)&Answers[i][1],".mp3");
      if((p != NULL) && (strlen(p) == 4))
      {
        if(FNP_CB.OnSearchAnswerItem != NULL)
          FNP_CB.OnSearchAnswerItem(IPs[i],&Answers[i][1]);
        free(IPs[i]);
      }
   }
  }
  if(FNP_CB.OnSearchAnswerEnd != NULL)
    FNP_CB.OnSearchAnswerEnd();
}

/* FFSS TCP callbacks */
bool FFSS_OnError(SU_PClientSocket Server,FFSS_Field Code,const char Descr[],FFSS_LongField Value,FFSS_LongField User)
{
  if(Code == FFSS_ERROR_NO_ERROR)
  {
    FC_SendMessage_StrmOpen(Server,FNP_CurrentFile,FFSS_STRM_OPEN_READ | FFSS_STRM_OPEN_BINARY,0);
    return true;
  }
  return true;
}

void FFSS_OnEndTCPThread(SU_PClientSocket Server)
{
  FNP_CurrentServer = NULL;
  if(FNP_CurrentIP != NULL)
    free(FNP_CurrentIP);
  FNP_CurrentIP = NULL;
  if(FNP_CurrentShare != NULL)
    free(FNP_CurrentShare);
  FNP_CurrentShare = NULL;
  if(FNP_CB.OnEndTCPThread != NULL)
    FNP_CB.OnEndTCPThread();
}

void FFSS_OnStrmOpenAnswer(SU_PClientSocket Client,const char Path[],FFSS_Field Code,FFSS_Field Handle,FFSS_LongField FileSize,FFSS_LongField User)
{
  FNP_CurrentFilePos = 0;
  FNP_CurrentFileSize = FileSize;
  if(Code != FFSS_ERROR_NO_ERROR)
  {
    if(FNP_CB.OnError != NULL)
      FNP_CB.OnError(Code);
    return;
  }
  FNP_CurrentFileHandle = Handle;
  /* Read first bloc */
  FC_SendMessage_StrmRead(Client,Handle,FNP_CurrentFilePos,FNP_BUFFER_SIZE,0);
}

void FFSS_OnStrmReadAnswer(SU_PClientSocket Client,FFSS_Field Handle,const char Bloc[],long int BlocSize,FFSS_Field ErrorCode,FFSS_LongField User)
{
  if(FNP_CurrentFileHandle != Handle) /* FNP_CurrentFileHandle Changed since last call */
  {
    printf("FNP : OnStrmReadAnswer : FNP_CurrentFileHandle changed\n");
    return;
  }
  if(BlocSize == 0) /* End of file */
  {
    if(FNP_CB.OnEndOfFile != NULL)
      FNP_CB.OnEndOfFile();
    return;
  }
  SU_SEM_WAIT(FNP_Sem);
  if(FNP_CurrentPlayer == NULL)
  { /* Player disconnected */
    SU_SEM_POST(FNP_Sem);
    return;
  }
  /* Read next bloc - Optimize network card by sending request now */
  FNP_CurrentFilePos += BlocSize;
  FC_SendMessage_StrmRead(Client,Handle,FNP_CurrentFilePos,FNP_BUFFER_SIZE,0);
  /* Send bloc to Player while next bloc is comming from Server */
  send(FNP_CurrentPlayer->sock,Bloc,BlocSize,SU_MSG_NOSIGNAL);
  SU_SEM_POST(FNP_Sem);
}

/* ******************* FNP CALLBACKS ******************* */
void FNP_ServerDisconnect(bool SendStrmClose)
{
  printf("FNP : Disconnecting from server %s\n",FNP_CurrentIP);
  if((FNP_CurrentFileHandle != 0) && SendStrmClose)
    FC_SendMessage_StrmClose(FNP_CurrentServer,FNP_CurrentFileHandle);
  FNP_CurrentFileHandle = 0;
  FC_SendMessage_Disconnect(FNP_CurrentServer);
}

void FNP_ConnectServer(const char IP[],const char Share[])
{
  if(FNP_CurrentFileHandle != 0)
    FC_SendMessage_StrmClose(FNP_CurrentServer,FNP_CurrentFileHandle);
  FNP_CurrentFileHandle = 0;
  if((FNP_CurrentServer != NULL) && (strcmp(IP,FNP_CurrentIP) == 0) && (strcmp(Share,FNP_CurrentShare) == 0))
  { /* Same as actual connection */
    printf("FNP : Same as actual connection... simulating ConnectionSuccess\n");
    FFSS_OnError(FNP_CurrentServer,FFSS_ERROR_NO_ERROR,"",0,0); /* Simulate successful connection */
    return;
  }
  if(FNP_CurrentServer != NULL) /* Disconnect from old server */
    FNP_ServerDisconnect(true);
  printf("FNP : Sending Connection message to %s/%s\n",IP,Share);
  FNP_CurrentServer = FC_SendMessage_ShareConnect(IP,Share,NULL,NULL,0);
  if(FNP_CurrentServer != NULL)
  {
    if(FNP_CurrentIP != NULL)
      free(FNP_CurrentIP);
    FNP_CurrentIP = strdup(IP);
    if(FNP_CurrentShare != NULL)
      free(FNP_CurrentShare);
    FNP_CurrentShare = strdup(Share);
  }
}

bool FNP_ParseBuffer(const char Buf[],int Len)
{
  printf("FNP : Parsing HTTP request\n");
  return true;
}

bool FNP_SendReply(SU_PClientSocket Client)
{
  char Buf[512];

  printf("FNP : Sending HTTP Reply to server\n");
  snprintf(Buf,sizeof(Buf),"HTTP/1.0 200 OK%c%cServer: %s/%s%c%cContent-Type: audio/mpeg%c%c%c%c",0x0D,0x0A,FNP_NAME,FNP_VERSION,0x0D,0x0A,0x0D,0x0A,0x0D,0x0A);
  /* Mettre un select ici... avec un petit timeout */
  send(Client->sock,Buf,sizeof(Buf),SU_MSG_NOSIGNAL);
  return true;
}

SU_THREAD_ROUTINE(FNP_StreamingRoutine,User)
{
  SU_PServerInfo SI = (SU_PServerInfo) User;
  char buf[1024];
  int read;

  while(1)
  {
    FNP_CurrentPlayer = SU_ServerAcceptConnection(SI);
    if(FNP_CurrentPlayer != NULL)
    {
      printf("FNP : Player connected from %s... reading HTTP request\n",inet_ntoa(FNP_CurrentPlayer->SAddr.sin_addr));
      if((read = recv(FNP_CurrentPlayer->sock,buf,sizeof(buf),SU_MSG_NOSIGNAL)) <= 0)
      {
        printf("FNP : Error reading HTTP request\n");
        SU_FreeCS(FNP_CurrentPlayer);
        break;
      }
      if(!FNP_ParseBuffer(buf,read))
      {
        printf("FNP : Error parsing HTTP request\n");
        SU_FreeCS(FNP_CurrentPlayer);
        break;
      }
      if(!FNP_SendReply(FNP_CurrentPlayer))
      {
        printf("FNP : Error replying to client\n");
        SU_FreeCS(FNP_CurrentPlayer);
        break;
      }
      while(1)
      {
        if(recv(FNP_CurrentPlayer->sock,buf,sizeof(buf),SU_MSG_NOSIGNAL) <= 0) /* Wait for connection lost with Player */
        {
          SU_SEM_WAIT(FNP_Sem);
          FNP_ServerDisconnect(false);
          SU_FreeCS(FNP_CurrentPlayer);
          FNP_CurrentPlayer = NULL;
          SU_SEM_POST(FNP_Sem);
          break;
        }
      }
    }
  }
}


/* ***************** INIT AND UNINIT ****************** */
bool FNP_Init(const char Master[])
{
  SU_PServerInfo SI;

  SI = SU_CreateServer(FNP_PORT,SOCK_STREAM,false);
  if(SI == NULL)
  {
    snprintf(FNP_Error,sizeof(FNP_Error),"Cannot create socket on port %d",FNP_PORT);
    return false;
  }
  if(SU_ServerListen(SI) == SOCKET_ERROR)
  {
    snprintf(FNP_Error,sizeof(FNP_Error),"Cannot create listening socket");
    return false;
  }
  if(!SU_CreateThread(&FNP_Thread,FNP_StreamingRoutine,(void *)SI,true))
  {
    snprintf(FNP_Error,sizeof(FNP_Error),"Cannot create listening thread");
    return false;
  }
  if(!SU_CreateSem(&FNP_Sem,1,1,"FNP_Sem"))
  {
    snprintf(FNP_Error,sizeof(FNP_Error),"Couldn't allocate semaphore");
    return false;
  }

  /* FFSS Initialization */
  memset(&FFSS_CB,0,sizeof(FFSS_CB));
  /* UDP CALLBACKS */
  FFSS_CB.CCB.OnSearchAnswer = FFSS_OnSearchAnswer;
  /* TCP CALLBACKS */
  FFSS_CB.CCB.OnError = FFSS_OnError;
  FFSS_CB.CCB.OnEndTCPThread = FFSS_OnEndTCPThread;
  FFSS_CB.CCB.OnStrmOpenAnswer = FFSS_OnStrmOpenAnswer;
  FFSS_CB.CCB.OnStrmReadAnswer = FFSS_OnStrmReadAnswer;
  if(!FC_Init())
  {
    snprintf(FNP_Error,sizeof(FNP_Error),"FFSS library init failed");
    return false;
  }
  FNP_Master = strdup(Master);

  return true;
}

bool FNP_Uninit(void)
{
  /* Shutting down server */
  FC_UnInit();
  return true;
}

/* ********************** QUERY INFOS ********************** */
FFSS_LongField FNP_GetCurrentFilePos(void)
{
  return FNP_CurrentFilePos;
}

FFSS_LongField FNP_GetCurrentFileSize(void)
{
  return FNP_CurrentFileSize;
}

char *FNP_GetLastError()
{
  return FNP_Error;
}

void FNP_SearchFiles(const char Key[])
{
  char buf[1024];

  snprintf(buf,sizeof(buf),"%s mp3",Key);
  FC_SendMessage_Search(FNP_Master,NULL,buf,0);
}

/* **************** PLAY FUNCTIONS *********************** */
void FNP_GetShareAndFile(const char Path[],char *Share,int len1,char *File,int len2)
{ /* Path is something like HOST/Share/Path/File */
  char *s,*p;
  int len;

  Share[0] = 0;
  File[0] = 0;
  s = strchr(Path,'/'); /* Get beginning of Share */
  if(s != NULL)
  {
    s++;
    p = strchr(s,'/'); /* Get beginning of Path */
    if(p != NULL)
    {
      len = p - s + 1;
      if(len > len1)
        len = len1;
      SU_strcpy(Share,s,len);
      len = strlen(p) + 1;
      if(len > len2)
        len = len2;
      SU_strcpy(File,p,len);
      printf("FNP_GetShareAndFile : Got %s and %s\n",Share,File);
    }
  }
}

void FNP_PlayFile_Path(const char IP[],const char Path[])
{
  char Share[100];
  char File[512];

  FNP_GetShareAndFile(Path,Share,sizeof(Share),File,sizeof(File));
  if(FNP_CurrentFile != NULL)
    free(FNP_CurrentFile);
  FNP_CurrentFile = strdup(File);
  /* Start connection with Server */
  FNP_ConnectServer(IP,Share);
}

void FNP_PlayFile(const char IP[],const char Share[],const char File[])
{
  if(FNP_CurrentFile != NULL)
    free(FNP_CurrentFile);
  FNP_CurrentFile = strdup(File);
  /* Start connection with Server */
  FNP_ConnectServer(IP,Share);
}
