#include "fmp.h"
#define FMP_NAME "FFSS Multi Path Library"
#define FMP_VERSION "1.0"
#define FMP_DEFAULT_BLOC_SIZE 4*1024*1024
#define FMP_BUFFER_SIZE 256*1024

#define FMP_DELAY_RETRY_SEM  30
#define FMP_DELAY_RETRY_RECO 2*60

/* ************* */
struct FMP_SPath;

typedef struct
{
  FFSS_Field State;
  FFSS_LongField Pos;

  struct FMP_SPath *Path;
} FMP_TBloc, *FMP_PBloc;

typedef struct FMP_SFile
{
  char *Name;
  FFSS_LongField Size;
  FFSS_Field Chksum;
  SU_PList Paths; /* FMP_PPath */
  FFSS_LongField UserTag;

  /* Run time Values */
  FILE *fp; /* Same FP for all Paths */
  FFSS_Field BlocSize;
  FFSS_Field NbBlocs;
  FMP_TBloc *Blocs;
  bool Complete;

  signed int RefCount;
} FMP_TFile, *FMP_PFile;

typedef struct FMP_SPath
{
  char *FullPath;
  char *IP;
  FMP_PFile File; /* DO NOT FREE - Only a pointer back to the FMP_PFile */
  char Path[512];
  char Share[100];

  /* Run time Values */
  FFSS_Field State;
  SU_THREAD_HANDLE HThr;
  SU_PClientSocket Server;
  SU_SEM_HANDLE Sem;
  FFSS_Field Idx;
  FFSS_Field Handle;
  bool Locked;
  bool Error;
} FMP_TPath, *FMP_PPath;

typedef struct FMP_SSearch
{
  FFSS_LongField UserTag;
  SU_PList Files; /* FMP_PFile */
} FMP_TSearch, *FMP_PSearch;

/* ************* */

char *FMP_Master = NULL;
char FMP_Error[1024];
FFSS_Field FMP_BlocSize = FMP_DEFAULT_BLOC_SIZE;

FMP_TCB FMP_CB;

SU_PList FMP_Searchs = NULL; /* FMP_PSearch */
SU_SEM_HANDLE FMP_Sem_Search,FMP_Sem_Blocs;

SU_THREAD_KEY_HANDLE FMP_tskey;
SU_THREAD_ONCE_HANDLE FMP_once = SU_THREAD_ONCE_INIT;

/* ******************* THREAD FUNCTIONS ******************* */
void destroyts(void *ptr)
{
  SU_THREAD_SET_SPECIFIC(FMP_tskey,NULL);
}

void tsinitkey(void)
{
  SU_CreateThreadKey(&FMP_tskey,&FMP_once,destroyts);
}

FMP_PPath FMP_GetThreadSpecific()
{
  SU_THREAD_ONCE(FMP_once,tsinitkey);
  return (FMP_PPath)SU_THREAD_GET_SPECIFIC(FMP_tskey);
}

/* ******************* UTILS FUNCTIONS ******************* */
void FMP_GetShareAndPath(const char FullPath[],char *Share,int len1,char *Path,int len2)
{ /* FullPath is something like HOST/Share/Path */
  char *s,*p;
  int len;
  char tt[]="";

  Share[0] = 0;
  Path[0] = 0;
  printf("FMP_GetShareAndPath : Scanning %s\n",FullPath);
  s = strchr(FullPath,'/'); /* Get beginning of Share */
  if(s != NULL)
  {
    s++;
    p = strchr(s,'/'); /* Get beginning of Path */
    if(p == NULL)
    {
      len = strlen(s) + 1;
      p = tt;
    }
    else
      len = p - s + 1;
    if(len > len1)
      len = len1;
    SU_strcpy(Share,s,len);
    len = strlen(p) + 1;
    if(len > len2)
      len = len2;
    SU_strcpy(Path,p,len);
    printf("FMP_GetShareAndPath : Got %s and %s\n",Share,Path);
  }
}

/* FMP_Sem_Search must be locked */
FMP_PSearch FMP_SearchSearch(FFSS_LongField UserTag)
{
  SU_PList Ptr;
  FMP_PSearch Sch;

  Ptr = FMP_Searchs;
  while(Ptr != NULL)
  {
    Sch = (FMP_PSearch) Ptr->Data;
    if(Sch->UserTag == UserTag)
      return Sch;
    Ptr = Ptr->Next;
  }
  return NULL;
}

/* FMP_Sem_Search must be locked */
FMP_PSearch FMP_AddSearch(FFSS_LongField UserTag)
{
  FMP_PSearch Sch;

  Sch = (FMP_PSearch) malloc(sizeof(FMP_TSearch));
  memset(Sch,0,sizeof(FMP_TSearch));
  Sch->UserTag = UserTag;
  FMP_Searchs = SU_AddElementHead(FMP_Searchs,Sch);
  return Sch;
}

/* FMP_Sem_Search must be locked */
FMP_PFile FMP_SearchFile(FMP_PSearch Sch,const char Name[],FFSS_LongField Size,FFSS_Field Chksum)
{
  SU_PList Ptr;
  FMP_PFile File;

  if(Chksum == 0) /* If no Chksum, must not be assigned to another file */
    return NULL;
  Ptr = Sch->Files;
  while(Ptr != NULL)
  {
    File = (FMP_PFile) Ptr->Data;
    if((File->Chksum == Chksum) && (File->Size == Size) && (strcmp(File->Name,Name) == 0))
      return File;
    Ptr = Ptr->Next;
  }
  return NULL;
}

/* FMP_Sem_Search must be locked */
FMP_PFile FMP_AddFile(FMP_PSearch Sch,const char Name[],FFSS_LongField Size,FFSS_Field Chksum)
{
  FMP_PFile File;

  File = (FMP_PFile) malloc(sizeof(FMP_TFile));
  memset(File,0,sizeof(FMP_TFile));
  File->Name = strdup(Name);
  File->Size = Size;
  File->Chksum = Chksum;
  File->RefCount = 1;
#ifdef DEBUG
  printf("FMP_AddFile : New File : %s - %lld - %ld\n",Name,Size,Chksum);
#endif
  Sch->Files = SU_AddElementHead(Sch->Files,File);
  return File;
}

/* FMP_Sem_Search must be locked */
void FMP_AddPath(FMP_PFile File,char *IP,const char Path[])
{
  SU_PList Ptr;
  FMP_PPath Pth;

  Ptr = File->Paths;
  while(Ptr != NULL)
  {
    Pth = (FMP_PPath) Ptr->Data;
#ifndef DEBUG
    if(strcmp(Pth->IP,IP) == 0)
    {
      printf("WARNING : FMP_AddPath : Duplicate IP (%s) for %s\n",IP,File->Name);
      return;
    }
#endif /* !DEBUG */
    if(strcmp(Pth->FullPath,Path) == 0)
    {
      printf("WARNING : FMP_AddPath : Duplicate Path : %s\n",Path);
      return;
    }
    Ptr = Ptr->Next;
  }
  Pth = (FMP_PPath) malloc(sizeof(FMP_TPath));
  memset(Pth,0,sizeof(FMP_TPath));
  Pth->IP = IP;
  Pth->FullPath = strdup(Path);
  Pth->State = FMP_PATH_STATE_NOT_CONNECTED;
  Pth->File = File;
  FMP_GetShareAndPath(Pth->FullPath,Pth->Share,sizeof(Pth->Share),Pth->Path,sizeof(Pth->Path));
#ifdef DEBUG
  printf("FMP_AddPath : New Path for %s : %s - %s\n",File->Name,Pth->IP,Pth->Path);
#endif
  File->Paths = SU_AddElementHead(File->Paths,Pth);
}

/* FMP_Sem_Search must be locked */
void FMP_FreePath(FMP_PPath Path)
{
  free(Path->FullPath);
  free(Path->IP);
  free(Path);
}

void FMP_FreeFile(FMP_PFile File)
{
  SU_PList Ptr;

#ifdef DEBUG
  printf("FMP_FreeFile : %s\n",File->Name);
#endif
  SU_SEM_WAIT(FMP_Sem_Search);
  File->RefCount--;
  if(File->RefCount <= 0)
  {
#ifdef DEBUG
    printf("No more RefCount for File %s\n",File->Name);
#endif
    Ptr = File->Paths;
    while(Ptr != NULL)
    {
      FMP_FreePath((FMP_PPath)Ptr->Data);
      Ptr = Ptr->Next;
    }
    SU_FreeList(File->Paths);
    free(File->Name);
    free(File);
  }
#ifdef DEBUG
  else
    printf("Remaining %ld RefCount to File %s\n",File->RefCount,File->Name);
#endif
  SU_SEM_POST(FMP_Sem_Search);
}

/* FMP_Sem_Search must be locked */
void FMP_FreeFile_internal(FMP_PFile File)
{
  SU_PList Ptr;

  File->RefCount--;
  if(File->RefCount <= 0)
  {
#ifdef DEBUG
    printf("No more RefCount for File %s\n",File->Name);
#endif
    Ptr = File->Paths;
    while(Ptr != NULL)
    {
      FMP_FreePath((FMP_PPath)Ptr->Data);
      Ptr = Ptr->Next;
    }
    SU_FreeList(File->Paths);
    free(File->Name);
    free(File);
  }
#ifdef DEBUG
  else
    printf("Remaining %ld RefCount to File %s\n",File->RefCount,File->Name);
#endif
}

void FMP_FreeSearch(struct FMP_SSearch *Sch)
{
  SU_PList Ptr;

#ifdef DEBUG
  printf("FMP_FreeSearch\n");
#endif
  SU_SEM_WAIT(FMP_Sem_Search);
  Ptr = Sch->Files;
  while(Ptr != NULL)
  {
    FMP_FreeFile_internal((FMP_PFile)Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Sch->Files);
  free(Sch);
  SU_SEM_POST(FMP_Sem_Search);
}

/* ******************* FFSS CALLBACKS ******************* */
/* FFSS UDP callbacks */
/* Each IP from IPs table is dupped internaly, and if you don't use it, you MUST free it !! */
void FFSS_OnSearchAnswer(const char Query[],const char Domain[],const char **Answers,char **IPs,FFSS_Field *ChkSums,FFSS_LongField *Sizes,int NbAnswers,FFSS_LongField User)
{
  int i;
  FMP_PFile File;
  char *name;
  FMP_PSearch Sch;
  SU_PList Ptr;

  SU_SEM_WAIT(FMP_Sem_Search);
  Sch = FMP_SearchSearch(User);
  if(Sch == NULL)
  {
    printf("WARNING : FFSS_OnSearchAnswer : No FMP_PSearch found for tag %lld\n",User);
    SU_SEM_POST(FMP_Sem_Search);
    return;
  }
  if(FMP_CB.OnSearchAnswerStart != NULL)
    FMP_CB.OnSearchAnswerStart(Sch,User);
  for(i=0;i<NbAnswers;i++)
  {
    if(((Answers[i][0] & FFSS_SEARCH_IS_FILE) == FFSS_SEARCH_IS_FILE) /* Answer is a file */
      && ((Answers[i][0] & FFSS_SEARCH_IS_SAMBA) == 0)) /* Answer is NOT on a samba share */
    {
      name = strrchr(Answers[i],'/');
      if(name != NULL)
      {
        File = FMP_SearchFile(Sch,name+1,Sizes[i],ChkSums[i]);
        if(File == NULL)
          File = FMP_AddFile(Sch,name+1,Sizes[i],ChkSums[i]);
        name[0] = 0;
        FMP_AddPath(File,IPs[i],Answers[i]+1);
      }
      else
      {
        free(IPs[i]);
#ifdef DEBUG
        printf("FFSS_OnSearchAnswer : not trailing / found for %s\n",Answers[i]);
#endif
      }
    }
    else
      free(IPs[i]);
  }
  Ptr = Sch->Files;
  while(Ptr != NULL)
  {
    File = (FMP_PFile) Ptr->Data;
    if(FMP_CB.OnSearchAnswerItem != NULL)
    {
      File->RefCount++;
      SU_SEM_POST(FMP_Sem_Search);
      FMP_CB.OnSearchAnswerItem(Sch,User,File->Name,File->Size,SU_ListCount(File->Paths),File);
      SU_SEM_WAIT(FMP_Sem_Search);
    }
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FMP_Sem_Search);
  if(FMP_CB.OnSearchAnswerEnd != NULL)
    FMP_CB.OnSearchAnswerEnd(Sch,User);
}

/* FFSS TCP callbacks */
bool FFSS_OnError(SU_PClientSocket Server,FFSS_Field Code,const char Descr[],FFSS_LongField Value,FFSS_LongField User)
{
  FMP_PPath Path = (FMP_PPath) (FFSS_Field)User;

  printf("%x : Got error : %d (%s)\n",SU_THREAD_SELF,Code,Descr);
  if(Path == NULL)
  {
    printf("Warning : Path is NULL... getting from TS\n");
    Path = FMP_GetThreadSpecific();
#ifdef DEBUG
    if(Path == NULL)
      printf("WARNING : PATH STILL NULL.. crash soon :p\n");
#endif
  }
  else if(FMP_GetThreadSpecific() == NULL)
  {
#ifdef DEBUG
    printf("Setting TS : %x\n",Path);
#endif
    SU_THREAD_SET_SPECIFIC(FMP_tskey,Path);
  }
  switch(Code)
  {
    case FFSS_ERROR_NO_ERROR :
      Path->State = FMP_PATH_STATE_CONNECTED;
      break;
    default :
      Path->State = FMP_PATH_STATE_NOT_CONNECTED;
      Path->Server = NULL;
      if(Path->Handle != 0)
      {
        SU_SEM_WAIT(FMP_Sem_Blocs);
        Path->File->Blocs[Path->Idx].State = FMP_BLOC_STATE_NOT_GOT;
        Path->Handle = 0;
        SU_SEM_POST(FMP_Sem_Blocs);
      }
  }
  if(Path->Locked)
    SU_SEM_POST(Path->Sem);
  else
    printf("WARNING : FFSS_OnError : Path->Locked is not locked !!\n");
  return true;
}

void FFSS_OnEndTCPThread(SU_PClientSocket Server)
{
  FMP_PPath Path = FMP_GetThreadSpecific();
  if(Path != NULL)
  {
    Path->Server = NULL;
    if(Path->Handle != 0)
    {
      SU_SEM_WAIT(FMP_Sem_Blocs);
      Path->File->Blocs[Path->Idx].State = FMP_BLOC_STATE_NOT_GOT;
      Path->Handle = 0;
      SU_SEM_POST(FMP_Sem_Blocs);
    }
    Path->State = FMP_PATH_STATE_NOT_CONNECTED;
  }
  printf("End TCP Thread\n");
}

void FFSS_OnStrmOpenAnswer(SU_PClientSocket Client,const char Path[],FFSS_Field Code,FFSS_Field Handle,FFSS_LongField FileSize,FFSS_LongField User)
{
  FMP_PPath Pth = (FMP_PPath) (FFSS_Field)User;

  Pth->Error = true;
  Pth->Handle = Handle;
  switch(Code)
  {
    case FFSS_ERROR_SERVER_IS_QUIET :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Pth->File->UserTag,0,Pth->IP,Pth->FullPath,Pth->File->Name,FMP_ERRCODE_SERVER_IS_QUIET);
      break;
    case FFSS_ERROR_ACCESS_DENIED :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Pth->File->UserTag,0,Pth->IP,Pth->FullPath,Pth->File->Name,FMP_ERRCODE_ACCESS_DENIED);
      break;
    case FFSS_ERROR_FILE_NOT_FOUND :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Pth->File->UserTag,0,Pth->IP,Pth->FullPath,Pth->File->Name,FMP_ERRCODE_FILE_NOT_FOUND);
      break;
    case FFSS_ERROR_NO_ERROR :
      Pth->Error = false;
      break;
    default :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Pth->File->UserTag,0,Pth->IP,Pth->FullPath,Pth->File->Name,FMP_ERRCODE_UNKNOWN_ERROR);
  }
  if(Pth->Locked)
    SU_SEM_POST(Pth->Sem);
  else
    printf("WARNING : FFSS_OnStrmOpenAnswer : Path->Locked is not locked !!\n");
}

void FFSS_OnStrmReadAnswer(SU_PClientSocket Client,FFSS_Field Handle,const char Bloc[],long int BlocSize,FFSS_Field ErrorCode,FFSS_LongField User)
{
  FMP_PPath Path = (FMP_PPath) (FFSS_Field)User;
  bool Error = false;

  switch(ErrorCode)
  {
    case FFSS_ERROR_END_OF_FILE :
      break;
    case FFSS_ERROR_NO_ERROR :
      break;
    case FFSS_ERROR_BAD_HANDLE :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File->UserTag,Path->Idx,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_BAD_FILE_HANDLE);
      Error = true;
    case FFSS_ERROR_IO_ERROR :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File->UserTag,Path->Idx,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_IO_ERROR);
      Error = true;
    default :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File->UserTag,Path->Idx,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_UNKNOWN_ERROR);
      Error = true;
  }
  if(Error)
  {
    SU_SEM_WAIT(FMP_Sem_Blocs);
    Path->File->Blocs[Path->Idx].State = FMP_BLOC_STATE_NOT_GOT;
    SU_SEM_POST(FMP_Sem_Blocs);
    if(Path->Locked)
      SU_SEM_POST(Path->Sem);
    else
      printf("WARNING : FFSS_OnStrmReadAnswer : Path->Locked is not locked !!\n");
    return;
  }

  if((FMP_CB.OnPacketReceived != NULL) && (BlocSize != 0))
    FMP_CB.OnPacketReceived(Path->File->UserTag,Path->Idx,Path->IP,Path->FullPath,Path->File->Name,BlocSize);

  SU_SEM_WAIT(FMP_Sem_Blocs);
  Path->State = FMP_PATH_STATE_TRANSFERING;
  if(BlocSize != 0)
  {
#ifdef DEBUG
    printf("FFSS_OnStrmReadAnswer : Writing %ld to file, starting at %ld\n",BlocSize,(Path->File->BlocSize*Path->Idx)+Path->File->Blocs[Path->Idx].Pos);
#endif
    fseek(Path->File->fp,(Path->File->BlocSize*Path->Idx)+Path->File->Blocs[Path->Idx].Pos,SEEK_SET);
    if(fwrite(Bloc,1,BlocSize,Path->File->fp) != BlocSize)
    {
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File->UserTag,Path->Idx,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_WRITE_ERROR);
      Path->File->Blocs[Path->Idx].State = FMP_BLOC_STATE_NOT_GOT;
      SU_SEM_POST(FMP_Sem_Blocs);
      if(Path->Locked)
        SU_SEM_POST(Path->Sem);
      else
        printf("WARNING : FFSS_OnStrmReadAnswer : Path->Locked is not locked !!\n");
      return;
    }
  }
  Path->File->Blocs[Path->Idx].Pos += BlocSize;
  if(Path->File->Blocs[Path->Idx].Pos >= Path->File->BlocSize) /* End of bloc, simulate EOF */
    BlocSize = 0;
  SU_SEM_POST(FMP_Sem_Blocs);
  if(BlocSize == 0) /* End of file or bloc */
  {
    if(FMP_CB.OnBlocComplete != NULL)
      FMP_CB.OnBlocComplete(Path->File->UserTag,Path->Idx,Path->IP,Path->FullPath,Path->File->Name);
    SU_SEM_WAIT(FMP_Sem_Blocs);
    Path->File->Blocs[Path->Idx].State = FMP_BLOC_STATE_GOT;
    SU_SEM_POST(FMP_Sem_Blocs);
    if(Path->Locked)
      SU_SEM_POST(Path->Sem);
    else
      printf("WARNING : FFSS_OnStrmReadAnswer : Path->Locked is not locked !!\n");
    return;
  }
  /* Read next bloc - Optimize network card by sending request now */
  FC_SendMessage_StrmRead(Path->Server,Path->Handle,(Path->File->BlocSize*Path->Idx)+Path->File->Blocs[Path->Idx].Pos,FMP_BUFFER_SIZE,(FFSS_Field)Path);
}

/* False if no more blocs == Transfer complete (or completing) */
bool FMP_GetNextIdx(FMP_PPath Path,FFSS_Field *Idx)
{
  FFSS_Field i;

  printf("FMP_GetNextIdx : Searching for a Bloc to download\n");
  SU_SEM_WAIT(FMP_Sem_Blocs);
  for(i=0;i<Path->File->NbBlocs;i++)
  {
    if(Path->File->Blocs[i].State == FMP_BLOC_STATE_NOT_GOT)
    {
      *Idx = i;
      Path->File->Blocs[i].State = FMP_BLOC_STATE_GETTING;
      SU_SEM_POST(FMP_Sem_Blocs);
      printf("FMP_GetNextIdx : Found Bloc %ld\n",i);
      return true;
    }
  }
  SU_SEM_POST(FMP_Sem_Blocs);
  printf("FMP_GetNextIdx : No more Bloc\n");
  return false;
}

bool FMP_IsDownloadComplete(FMP_PFile File)
{
  int i;

  SU_SEM_WAIT(FMP_Sem_Blocs);
  for(i=0;i<File->NbBlocs;i++)
  {
    if(File->Blocs[i].State != FMP_BLOC_STATE_GOT)
    {
      SU_SEM_POST(FMP_Sem_Blocs);
      return false;
    }
  }
  SU_SEM_POST(FMP_Sem_Blocs);
  return true;
}

SU_THREAD_ROUTINE(FMP_StreamingRoutine,User)
{
  FMP_PPath Path = (FMP_PPath) User;
  char Buf[512];

  while(SU_CreateSem(&Path->Sem,1,1,NULL) == false)
  {
    if(FMP_CB.OnError != NULL)
      FMP_CB.OnError(Path->File->UserTag,Path->Idx,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_SEMAPHORE_CREATE_ERROR);
    SU_SLEEP(FMP_DELAY_RETRY_SEM); /* Retry in 30 sec */
  }
  Path->Locked = false;

  while(1)
  {
    while(Path->State == FMP_PATH_STATE_NOT_CONNECTED)
    {
      /* Trying to connect to the host */
      printf("FNP : Sending Connection message to %s/%s\n",Path->IP,Path->Share);

      if(Path->Locked == false)
        SU_SEM_WAIT(Path->Sem);
      else
        printf("WARNING : FMP_StreamingRoutine : Path->Locked is already locked !!\n");
      Path->Locked = true;

      Path->Server = FC_SendMessage_ShareConnect(Path->IP,Path->Share,NULL,NULL,(FFSS_Field)Path);
      if(Path->Server == NULL)
      {
        Path->Locked = false;
        SU_SEM_POST(Path->Sem);
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File->UserTag,Path->Idx,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_HOST_DOWN);
        SU_SLEEP(FMP_DELAY_RETRY_RECO); /* Retry in 2 min */
        continue;
      }
      SU_SEM_WAIT(Path->Sem);
      Path->Locked = false;
      SU_SEM_POST(Path->Sem);
      if(Path->State != FMP_PATH_STATE_NOT_CONNECTED)
        break;
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File->UserTag,Path->Idx,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_SHARE_CONNECT_FAILED);
      FC_SendMessage_Disconnect(Path->Server);
      Path->Server = NULL;
      Path->Handle = 0;
      SU_SLEEP(FMP_DELAY_RETRY_RECO); /* Retry in 2 min */
    }
    /* Try to open the file */
    if(Path->Locked == false)
      SU_SEM_WAIT(Path->Sem);
    else
      printf("WARNING : FMP_StreamingRoutine : Path->Locked is already locked !!\n");
    Path->Locked = true;

    Path->Error = false;
    snprintf(Buf,sizeof(Buf),"%s/%s",Path->Path,Path->File->Name);
    FC_SendMessage_StrmOpen(Path->Server,Buf,FFSS_STRM_OPEN_READ | FFSS_STRM_OPEN_BINARY,(FFSS_Field)Path);

    SU_SEM_WAIT(Path->Sem);
    Path->Locked = false;
    SU_SEM_POST(Path->Sem);

    if(Path->Error) /* Error occured while opening file */
    {
      Path->State = FMP_PATH_STATE_NOT_CONNECTED;
      FC_SendMessage_Disconnect(Path->Server);
      Path->Server = NULL;
      Path->Handle = 0;
      SU_SLEEP(FMP_DELAY_RETRY_RECO); /* Retry in 2 min */
      continue;
    }

    while(FMP_GetNextIdx(Path,&Path->Idx)) /* Get next bloc to download from this Path */
    {
      if(Path->Locked == false)
        SU_SEM_WAIT(Path->Sem);
      else
        printf("WARNING : FMP_StreamingRoutine : Path->Locked is already locked !!\n");
      Path->Locked = true;

      Path->Error = false;
      /* Request first bloc from host */
      FC_SendMessage_StrmRead(Path->Server,Path->Handle,(Path->File->BlocSize*Path->Idx)+Path->File->Blocs[Path->Idx].Pos,FMP_BUFFER_SIZE,(FFSS_Field)Path);

      /* Wait for EndOfBloc */
      SU_SEM_WAIT(Path->Sem);
      Path->Locked = false;
      SU_SEM_POST(Path->Sem);
      if(Path->File->Blocs[Path->Idx].State != FMP_BLOC_STATE_GOT)
      {
        printf("Couldn't get my bloc, error might have occured... disconnecting\n");
        Path->File->Blocs[Path->Idx].State = FMP_BLOC_STATE_NOT_GOT;
        break;
      }
    }

    /* EOF - Disconnecting */
    Path->State = FMP_PATH_STATE_NOT_CONNECTED;
    FC_SendMessage_Disconnect(Path->Server);
    Path->Server = NULL;
    Path->Handle = 0;

    if(FMP_IsDownloadComplete(Path->File)) /* All parts completed */
    {
      SU_FreeSem(&Path->Sem);
      SU_SEM_WAIT(FMP_Sem_Search);
      if(!Path->File->Complete)
      {
        Path->File->Complete = true;
	fclose(Path->File->fp);
        SU_SEM_POST(FMP_Sem_Search);
        if(FMP_CB.OnDownloadComplete != NULL)
          FMP_CB.OnDownloadComplete(Path->File->UserTag,Path->File->Name,Path->File);
        SU_SEM_WAIT(FMP_Sem_Search);
      }
      FMP_FreeFile_internal(Path->File);
      SU_SEM_POST(FMP_Sem_Search);
      printf("FMP_StreamingRoutine Terminating\n");
      return;
    }
    else
    {
      printf("Try to find another bloc in 2min\n");
      SU_SLEEP(FMP_DELAY_RETRY_RECO); /* Else, retry in 2 min */
      continue;
    }
  }
  printf("FMP_StreamingRoutine Terminating\n");
}

/* ***************** INIT AND UNINIT ****************** */
bool FMP_Init(const char Master[])
{
  if(!SU_CreateSem(&FMP_Sem_Search,1,1,"FMP_Sem_Search"))
  {
    snprintf(FMP_Error,sizeof(FMP_Error),"Couldn't allocate semaphore FMP_Sem_Search");
    return false;
  }
  if(!SU_CreateSem(&FMP_Sem_Blocs,1,1,"FMP_Sem_Blocs"))
  {
    snprintf(FMP_Error,sizeof(FMP_Error),"Couldn't allocate semaphore FMP_Sem_Path");
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
    snprintf(FMP_Error,sizeof(FMP_Error),"FFSS library init failed");
    return false;
  }
  if(Master != NULL)
    FMP_Master = strdup(Master);

  return true;
}

bool FMP_Uninit(void)
{
  /* Shutting down server */
  FC_UnInit();
  return true;
}

void FMP_SetMaster(const char Master[])
{
  if(Master != NULL)
    FMP_Master = strdup(Master);
}

void FMP_SetBlocSize(FFSS_Field BS)
{
  FMP_BlocSize = BS;
}

/* ********************** QUERY INFOS ********************** */
char *FMP_GetLastError()
{
  return FMP_Error;
}

struct FMP_SSearch *FMP_SearchFiles(const char Key[],FFSS_LongField UserTag)
{
  FMP_PSearch Sch;
  SU_SEM_WAIT(FMP_Sem_Search);
  Sch = FMP_AddSearch(UserTag);
  SU_SEM_POST(FMP_Sem_Search);
  FC_SendMessage_Search(FMP_Master,NULL,Key,UserTag);
  return Sch;
}

void FMP_ListPaths(struct FMP_SFile *File,void (*CB_ListPaths)(const struct FMP_SFile *File,const char IP[],const char Path[],FFSS_Field State))
{
  SU_PList Ptr;
  FMP_PPath Pth;

  SU_SEM_WAIT(FMP_Sem_Search);
  Ptr = File->Paths;
  while(Ptr != NULL)
  {
    Pth = (FMP_PPath) Ptr->Data;
    CB_ListPaths(File,Pth->IP,Pth->FullPath,Pth->State);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FMP_Sem_Search);
}

FFSS_Field FMP_GetBlocsCount(struct FMP_SFile *File)
{
  return File->NbBlocs;
}

FFSS_Field FMP_GetBlocSize(struct FMP_SFile *File)
{
  return File->BlocSize;
}

bool FMP_GetBlocInfos(struct FMP_SFile *File,FFSS_Field Idx,FFSS_Field *State,FFSS_LongField *Pos)
{
  SU_SEM_WAIT(FMP_Sem_Blocs);
  if(Idx >= File->NbBlocs)
  {
    snprintf(FMP_Error,sizeof(FMP_Error),"Index %ld for file '%s' (max is %ld)",Idx,File->Name,File->NbBlocs-1);
    SU_SEM_POST(FMP_Sem_Blocs);
    return false;
  }
  if(State != NULL)
    *State = File->Blocs[Idx].State;
  if(Pos != NULL)
    *Pos = File->Blocs[Idx].Pos;
  SU_SEM_POST(FMP_Sem_Blocs);
  return true;
}

/* **************** DOWNLOAD FUNCTIONS *********************** */
bool FMP_StartDownload(struct FMP_SFile *File,const char DestFileName[],FFSS_LongField UserTag)
{
  SU_PList Ptr;
  FMP_PPath Pth;
  FILE *fp;

  fp = fopen(DestFileName,"wb");
  if(fp == NULL)
  {
    snprintf(FMP_Error,sizeof(FMP_Error),"Couldn't open '%s' for writing",DestFileName);
    return false;
  }

  SU_SEM_WAIT(FMP_Sem_Search);
  File->UserTag = UserTag;
  File->fp = fp;
  /* Compute and allocate blocs */
  File->BlocSize = FMP_BlocSize;
  File->NbBlocs = File->Size / File->BlocSize;
  if(File->Size % File->BlocSize)
    File->NbBlocs++;
  File->Blocs = (FMP_TBloc *) malloc(sizeof(FMP_TBloc)*File->NbBlocs);
  memset(File->Blocs,0,sizeof(FMP_TBloc)*File->NbBlocs);
  /* Start threads */
  Ptr = File->Paths;
  while(Ptr != NULL)
  {
    Pth = (FMP_PPath) Ptr->Data;
    if(!SU_CreateThread(&Pth->HThr,FMP_StreamingRoutine,(void *)Pth,true))
    {
      snprintf(FMP_Error,sizeof(FMP_Error),"Couldn't create a new thread (yet, download may have started). Out of system resources ?");
      SU_SEM_POST(FMP_Sem_Search);
      return false;
    }
    File->RefCount++;
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FMP_Sem_Search);
  return true;
}
