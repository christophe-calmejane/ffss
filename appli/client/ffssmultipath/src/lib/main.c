#include <fmp.h>

struct FMP_SFile *fil = NULL;
struct FMP_SSearch *sch = NULL;
volatile bool Complete = false;

void SearchAnswerStart(struct FMP_SSearch *Sch,FFSS_LongField UserTag)
{
  printf("Got answers for Tag %lld\n",UserTag);
}

void SearchAnswerItem(struct FMP_SSearch *Sch,FFSS_LongField UserTag,const char Name[],FFSS_LongField Size,FFSS_Field Count,struct FMP_SFile *File)
{
  printf("\tName=%50s - Size=%8lld - %d occurences\n",Name,Size,Count);
  if(fil == NULL)
  {
    fil = File;
  }
  else
    FMP_FreeFile(File);
}

void SearchAnswerEnd(struct FMP_SSearch *Sch,FFSS_LongField UserTag)
{
  printf("End of List for Tag %lld\n",UserTag);
  if(!FMP_StartDownload(fil,"./toto.out",1))
    printf("Start download failed : %s\n",FMP_GetLastError());
}


void OnError(struct FMP_SFile *File,FFSS_LongField UserTag,const FFSS_Field Idx,const char IP[],const char Path[],const char Name[],FFSS_Field ErrorCode) /* 'UserTag' and 'File' passed to FMP_StartDownload */
{
  printf("Error : %ld at Idx %ld from %s for %s\n",ErrorCode,Idx,IP,Name);
}

void OnPacketReceived(struct FMP_SFile *File,FFSS_LongField UserTag,const FFSS_Field Idx,const char IP[],const char Path[],const char Name[],FFSS_Field SizeOfPacket) /* 'UserTag' and 'File' passed to FMP_StartDownload, 'Idx' of FileBloc, 'IP' from where we got the packet,'Path' in its share, 'Name' of the file */
{
  printf("Packet received for Idx %ld from %s - Size = %ld\n",Idx,IP,SizeOfPacket);
}

void OnBlocComplete(struct FMP_SFile *File,FFSS_LongField UserTag,const FFSS_Field Idx,const char IP[],const char Path[],const char Name[]) /* 'UserTag' and 'File' passed to FMP_StartDownload, 'Idx' of FileBloc, 'IP' from where we got the packet,'Path' in its share, 'Name' of the file */
{
  printf("Bloc complete for Idx %ld from %s\n",Idx,IP);
}

void OnDownloadComplete(struct FMP_SFile *File,FFSS_LongField UserTag,const char Name[]) /* 'UserTag' and 'File' passed to FMP_StartDownload */
{
  printf("Download complete for file %s\n",Name);
  FMP_FreeFile(File);
  FMP_FreeSearch(sch);
  Complete = true;
}


void handint(int sig)
{
  static bool done = false;

  if(!done)
  {
    FMP_Uninit();
    exit(0);
  }
}

/* Main */
int main(int argc, char *argv[])
{
  FMP_CB.OnSearchAnswerStart = SearchAnswerStart;
  FMP_CB.OnSearchAnswerItem = SearchAnswerItem;
  FMP_CB.OnSearchAnswerEnd = SearchAnswerEnd;

  FMP_CB.OnError = OnError;
  FMP_CB.OnPacketReceived = OnPacketReceived;
  FMP_CB.OnBlocComplete = OnBlocComplete;
  FMP_CB.OnDownloadComplete = OnDownloadComplete;

  SU_DBG_SetOutput(SU_DBG_OUTPUT_PRINTF);
  SU_DBG_SetOptions(true,true);
#ifdef DEBUG
  SU_DBG_SetFlags(FFSS_DBGMSG_ALL);
#endif /* DEBUG */

  if(argc != 2)
  {
    printf("Usage : %s \"<key words>\"\n",argv[0]);
    return -1;
  }
  if(!FMP_Init("orion"))
  {
    printf("Error : %s\n",FMP_GetLastError());
    return -2;
  }
#ifdef __unix__
  signal(SIGTSTP,handint);
#endif /* __unix__ */
  signal(SIGINT,handint);
  signal(SIGTERM,handint);

  sch = FMP_SearchFiles(argv[1],0);
  while(!Complete) SU_SLEEP(5);
  return 0;
}

