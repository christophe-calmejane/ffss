#include <fnp.h>

char *ip = NULL;
char *path = NULL;

void EndOfFile(void)
{
  printf("End of file\n");
  exit(0);
}

void EndTCPThread(void)
{
  printf("Connection lost\n");
  exit(0);
}

void Error(int Code)
{
  printf("Error code : %d\n",Code);
  exit(0);
}

void SearchAnswerStart(void)
{
  printf("Got answers : \n");
}

void SearchAnswerItem(const char IP[],const char Path[])
{
  printf("\tIP=%10s - Path=%50s\n",IP,Path);
  if(ip == NULL)
  {
    ip = strdup(IP);
    path = strdup(Path);
  }
}

void SearchAnswerEnd(void)
{
  printf("End of List\n");
  FNP_PlayFile_Path(ip,path);
}

void handint(int sig)
{
  static bool done = false;

  if(!done)
  {
    FNP_Uninit();
    exit(0);
  }
}

/* Main */
int main(int argc, char *argv[])
{
  FNP_CB.OnEndOfFile = EndOfFile;
  FNP_CB.OnEndTCPThread = EndTCPThread;
  FNP_CB.OnError = Error;
  FNP_CB.OnSearchAnswerStart = SearchAnswerStart;
  FNP_CB.OnSearchAnswerItem = SearchAnswerItem;
  FNP_CB.OnSearchAnswerEnd = SearchAnswerEnd;

  if(argc != 2)
  {
    printf("Usage : %s \"<key words>\"\n",argv[0]);
    return -1;
  }
  if(!FNP_Init("ffss"))
  {
    printf("Error : %s\n",FNP_GetLastError());
    return -2;
  }
#ifdef __unix__
  signal(SIGTSTP,handint);
#endif /* __unix__ */
  signal(SIGINT,handint);
  signal(SIGTERM,handint);

  FNP_SearchFiles(argv[1]);
  while(1) SU_SLEEP(10);
  return 0;
}

