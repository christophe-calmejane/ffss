#include "client.h"
#include "interface.h"

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
SU_SEM_HANDLE FNP_Sem;

/* ************************** */
/* START OF OS DEPENDANT CODE */
/* ************************** */

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

void FNP_PlayNextFile(bool Lock)
{
  GList *items;
  gchar *path;
  gchar *ip;
  char Share[100];
  char File[512];

  if(Lock)
    gdk_threads_enter();
  items = FNP_clist->selection;
  if(items != NULL)
  {
    gtk_clist_get_text(FNP_clist,(gint)items->data,0,&path);
    gtk_clist_get_text(FNP_clist,(gint)items->data,1,&ip);
    assert(path);
    assert(ip);
    FNP_GetShareAndFile(path,Share,sizeof(Share),File,sizeof(File));
    if(FNP_CurrentFile != NULL)
      free(FNP_CurrentFile);
    FNP_CurrentFile = strdup(File);
    //g_free(path);
    //g_free(ip);
  }
  if(Lock)
    gdk_threads_leave();

  /* Start connection with Server */
  FNP_ConnectServer(ip,Share);
}

void FNP_OnError(int Code)
{
  printf("Received an error code : %d\n",Code);
}

/* **************************** */
/* START OF OS INDEPENDANT CODE */
/* **************************** */
void FNP_ServerDisconnect()
{
  printf("FNP : Disconnecting from server %s\n",FNP_CurrentIP);
  if(FNP_CurrentFileHandle != 0)
    FC_SendMessage_StrmClose(FNP_CurrentServer,FNP_CurrentFileHandle);
  FNP_CurrentFileHandle = 0;
  SU_FreeCS(FNP_CurrentServer);
  FNP_CurrentServer = NULL;
  if(FNP_CurrentIP != NULL)
    free(FNP_CurrentIP);
  FNP_CurrentIP = NULL;
  if(FNP_CurrentShare != NULL)
    free(FNP_CurrentShare);
  FNP_CurrentShare = NULL;
}

void FNP_ConnectServer(const char IP[],const char Share[])
{
  if((FNP_CurrentServer != NULL) && (strcmp(IP,FNP_CurrentIP) == 0) && (strcmp(Share,FNP_CurrentShare) == 0))
  { /* Same as actual connection */
    printf("FNP : Same as actual connection... simulating ConnectionSuccess\n");
    OnError(FNP_CurrentServer,FFSS_ERROR_NO_ERROR,"",0); /* Simulate successful connection */
    return;
  }
  if(FNP_CurrentServer != NULL) /* Disconnect from old server */
    FNP_ServerDisconnect();
  printf("FNP : Sending Connection message to %s/%s\n",IP,Share);
  FNP_CurrentServer = FC_SendMessage_ShareConnect(IP,Share,NULL,NULL);
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

SU_THREAD_ROUTINE(StreamingRoutine,User)
{
  SU_PServerInfo SI;
  char buf[1024];

  SI = SU_CreateServer(FNP_PORT,SOCK_STREAM,false);
  if(SI == NULL)
    return;
  SU_ServerListen(SI);

  while(1)
  {
    FNP_CurrentPlayer = SU_ServerAcceptConnection(SI);
    if(FNP_CurrentPlayer != NULL)
    {
      printf("FNP : Player connected from %s... reading HTTP request\n",inet_ntoa(FNP_CurrentPlayer->SAddr.sin_addr));
      if(recv(FNP_CurrentPlayer->sock,buf,sizeof(buf),SU_MSG_NOSIGNAL) <= 0)
      {
        printf("FNP : Error reading HTTP request\n");
        SU_FreeCS(FNP_CurrentPlayer);
        break;
      }
      while(1)
      {
        if(recv(FNP_CurrentPlayer->sock,buf,sizeof(buf),SU_MSG_NOSIGNAL) <= 0) /* Wait for connection lost with Player */
        {
          SU_SEM_WAIT(FNP_Sem);
          FNP_ServerDisconnect();
          SU_FreeCS(FNP_CurrentPlayer);
          FNP_CurrentPlayer = NULL;
          SU_SEM_POST(FNP_Sem);
          break;
        }
      }
    }
  }
}

/* UDP callbacks */
/* Each IP from IPs table is dupped internaly, and if you don't use it, you MUST free it !! */
void OnSearchAnswer(const char Query[],const char Domain[],const char **Answers,char **IPs,int NbAnswers)
{
  int i;
  GtkCList *clist;
  gchar *strings[2];

  gdk_threads_enter();
  clist = (GtkCList *) lookup_widget(wnd_main,"clist1");
  if(clist == NULL)
  {
    gdk_threads_leave();
    return;
  }

  gtk_clist_clear(clist);
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
        strings[0] = (gchar *)&Answers[i][1];
        strings[1] = (gchar *)IPs[i];
        gtk_clist_append(clist,strings);
      }
   }
  }
  gdk_threads_leave();
}

/* TCP callbacks */
bool OnError(SU_PClientSocket Server,int Code,const char Descr[],FFSS_LongField Value)
{
  if(Code == FFSS_ERROR_NO_ERROR)
  {
    FC_SendMessage_StrmOpen(Server,FNP_CurrentFile,FFSS_STRM_OPEN_READ | FFSS_STRM_OPEN_BINARY);
    return true;
  }
  return true;
}

void OnEndTCPThread(SU_PClientSocket Server)
{
  printf("On End TCP Thread\n");
}

void OnStrmOpenAnswer(SU_PClientSocket Client,const char Path[],int Code,long int Handle,FFSS_LongField FileSize)
{
  FNP_CurrentFilePos = 0;
  FNP_CurrentFileSize = FileSize;
  if(Code != FFSS_ERROR_NO_ERROR)
  {
    FNP_OnError(Code);
    return;
  }
  FNP_CurrentFileHandle = Handle;
  /* Read first bloc */
  FC_SendMessage_StrmRead(Client,Handle,FNP_CurrentFilePos,FNP_BUFFER_SIZE);
}

void OnStrmReadAnswer(SU_PClientSocket Client,long int Handle,const char Bloc[],long int BlocSize)
{
  if(BlocSize == 0) /* End of file */
  {
    FNP_PlayNextFile(true);
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
  FC_SendMessage_StrmRead(Client,Handle,FNP_CurrentFilePos,FNP_BUFFER_SIZE);
  /* Send bloc to Player while next bloc is comming from Server */
  send(FNP_CurrentPlayer->sock,Bloc,BlocSize,SU_MSG_NOSIGNAL);
  SU_SEM_POST(FNP_Sem);
}
