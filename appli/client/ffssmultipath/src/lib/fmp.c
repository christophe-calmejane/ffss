/*
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
/*
  TO DO :
   - Creer un thread qui verifie regulierement l'etat des semaphores des Path
     -> Si locked, regarder la derniere date de mise a jour (ajouter un champ time_t dans le PPath), si delai > 60s, liberer le sem (mettre l'etat a not_connected par ex)
     -> Mise a jour du time_t dans les packets receive
*/

#include "fmp.h"
#define FMP_NAME "FFSS Multi Path Library"
#define FMP_VERSION "v1.1"
#define FMP_COPYRIGHT "(c) Christophe Calmejane 2003"
#define FMP_DEFAULT_BLOC_SIZE 5*1024*1024

#define FMP_DELAY_RETRY_SEM  30
#define FMP_DELAY_RETRY_RECO 2*60
#define FMP_MIN_RECONNECT_DELAY 20

/* ************* */
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
  SU_SEM_HANDLE Sem;  /* FMP_PPath sem for synchronisation */
  bool Locked;        /* Boolean value used with Sem */
  FFSS_PTransfer FT;  /* FileTransfer structure */
  FFSS_Field Idx;     /* Current bloc Idx to download */
  bool HaveIdx;       /* True if an Idx has been assigned to me */
  bool Error;
  bool MustCancel;    /* True if download is canceled */
  bool MustPause;     /* True if thread must be suspended */
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
FFSS_Field FMP_ReconnectDelay = FMP_DELAY_RETRY_RECO;

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
  bool Error = false;

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
  if(((Path->State != FMP_PATH_STATE_CONNECTED) && (Path->State != FMP_PATH_STATE_TRANSFERING)) && (Code == FFSS_ERROR_REMOTE_CLOSED))
  {
#ifdef DEBUG
    printf("FFSS_OnError : Not connected, ignoring error message\n");
#endif
    return true;
  }
  switch(Code)
  {
    case FFSS_ERROR_NO_ERROR :
      Path->State = FMP_PATH_STATE_CONNECTED;
      break;
    case FFSS_ERROR_XFER_MODE_NOT_SUPPORTED :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_XFER_MODE_NOT_SUPPORTED);
      Error = true;
      break;
    case FFSS_ERROR_SERVER_IS_QUIET :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_SERVER_IS_QUIET);
      Error = true;
      break;
    case FFSS_ERROR_TOO_MANY_TRANSFERS :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TOO_MANY_TRANSFERS);
      Error = true;
      break;
    case FFSS_ERROR_INTERNAL_ERROR :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_SERVER_INTERNAL_ERROR);
      Error = true;
      break;
    case FFSS_ERROR_FILE_NOT_FOUND :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_FILE_NOT_FOUND);
      Error = true;
      break;
    case FFSS_ERROR_CANNOT_CONNECT :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_SERVER_CANNOT_CONNECT);
      Error = true;
      break;
    case FFSS_ERROR_ACCESS_DENIED :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_ACCESS_DENIED);
      Error = true;
      break;
    case FFSS_ERROR_RESOURCE_NOT_AVAIL :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_RESOURCE_NOT_AVAIL);
      Error = true;
      break;
    case FFSS_ERROR_SHARE_DISABLED :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_SHARE_DISABLED);
      Error = true;
      break;
    case FFSS_ERROR_TOO_MANY_CONNECTIONS :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TOO_MANY_CONNECTIONS);
      Error = true;
      break;
    case FFSS_ERROR_NEED_LOGIN_PASS :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_NEED_LOGIN_PASS);
      Error = true;
      break;
    case FFSS_ERROR_REMOTE_CLOSED :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_REMOTE_CLOSED);
      Error = true;
      break;
    case FFSS_ERROR_SHARE_EJECTED :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_SHARE_EJECTED);
      Error = true;
      break;
    case FFSS_ERROR_SOCKET_ERROR :
      if(Path->HaveIdx == false)
        break;
      /* else do default case */
    default :
      if(FMP_CB.OnError != NULL)
        FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_UNKNOWN_ERROR);
      Error = true;
  }
  if(Error)
  {
    SU_SEM_WAIT(FMP_Sem_Blocs);
    Path->State = FMP_PATH_STATE_NOT_CONNECTED;
    FC_SendMessage_Disconnect(Path->Server);
    Path->Server = NULL;
    if(Path->HaveIdx)
    {
      Path->File->Blocs[Path->Idx].State = FMP_BLOC_STATE_NOT_GOT;
      Path->File->Blocs[Path->Idx].Pos = 0;
      if(FMP_CB.OnUnAssignBloc != NULL)
        FMP_CB.OnUnAssignBloc(Path->File,Path->File->UserTag,Path->Idx);
      Path->HaveIdx = false;
    }
    SU_SEM_POST(FMP_Sem_Blocs);
  }

  if(Path->Locked)
    SU_SEM_POST(Path->Sem);
  else
    printf("WARNING : FFSS_OnError : Path->Locked is not locked !!\n");
  return true;
}

void FFSS_OnTransferFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Descr[],bool Download)
{
  FMP_PPath Path = (FMP_PPath) (FFSS_Field)(FT->UserInfo);

  printf("FFSS_OnTransferFailed [%x] : Got error : %d (%s)\n",SU_THREAD_SELF,ErrorCode,Descr);
  if(Path == NULL)
  {
    printf("WARNING : PATH IS NULL.. crash soon :p\n");
  }
  if(Path->MustCancel)
  {
    Path->State = FMP_PATH_STATE_CANCELED;
    if(FMP_CB.OnError != NULL)
      FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_CANCELED);
  }
  else if(Path->MustPause)
  {
    Path->State = FMP_PATH_STATE_PAUSED;
    if(FMP_CB.OnError != NULL)
      FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_SUSPENDED);
  }
  else
  {
    switch(ErrorCode)
    {
      case FFSS_ERROR_TRANSFER_MALLOC :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_MALLOC);
        break;
      case FFSS_ERROR_TRANSFER_TIMEOUT :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_TIMEOUT);
        break;
      case FFSS_ERROR_TRANSFER_SEND :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_SEND);
        break;
      case FFSS_ERROR_TRANSFER_EOF :
        if((FMP_CB.OnError != NULL) && Path->HaveIdx)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_EOF);
        break;
      case FFSS_ERROR_TRANSFER_READ_FILE :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_READ_FILE);
        break;
      case FFSS_ERROR_TRANSFER_ACCEPT :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_ACCEPT);
        break;
      case FFSS_ERROR_TRANSFER_OPENING :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_OPENING);
        break;
      case FFSS_ERROR_TRANSFER_RECV :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_RECV);
        break;
      case FFSS_ERROR_TRANSFER_WRITE_FILE :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_WRITE_FILE);
        break;
      case FFSS_ERROR_TRANSFER_FILE_BIGGER :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_FILE_BIGGER);
        break;
      case FFSS_ERROR_TRANSFER_CHECKSUM :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_CHECKSUM);
        break;
      case FFSS_ERROR_TRANSFER_CANCELED :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_TRANSFER_CANCELED);
        break;
      default :
        if(FMP_CB.OnError != NULL)
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_UNKNOWN_ERROR);
    }
  }

  if(Path->Locked)
    SU_SEM_POST(Path->Sem);
  else
    printf("WARNING : FFSS_OnTransferFailed : Path->Locked is not locked !!\n");
}

void FFSS_OnTransferSuccess(FFSS_PTransfer FT,bool Download)
{
  FMP_PPath Path = (FMP_PPath) (FFSS_Field)(FT->UserInfo);

  printf("FFSS_OnTransferSuccess [%x]\n",SU_THREAD_SELF);
  if(Path == NULL)
  {
    printf("WARNING : PATH IS NULL.. crash soon :p\n");
  }

  if(FMP_CB.OnBlocComplete != NULL)
    FMP_CB.OnBlocComplete(Path->File,Path->File->UserTag,Path->Idx);
  SU_SEM_WAIT(FMP_Sem_Blocs);
  Path->File->Blocs[Path->Idx].State = FMP_BLOC_STATE_GOT;
  if(FMP_CB.OnUnAssignBloc != NULL)
    FMP_CB.OnUnAssignBloc(Path->File,Path->File->UserTag,Path->Idx);
  Path->HaveIdx = false;
  SU_SEM_POST(FMP_Sem_Blocs);
  if(Path->Locked)
    SU_SEM_POST(Path->Sem);
  else
    printf("WARNING : FFSS_OnTransferSuccess : Path->Locked is not locked !!\n");
  return;
}

bool FFSS_OnTransferFileWrite(FFSS_PTransfer FT,const char Buf[],FFSS_Field Size,FFSS_LongField Offset) /* 'Offset' from FT->StartingPos */ /* True on success */
{
  FMP_PPath Path = (FMP_PPath) (FFSS_Field)(FT->UserInfo);

  if(Path == NULL)
  {
    printf("WARNING : (FFSS_OnTransferFileWrite) PATH IS NULL.. crash soon :p\n");
  }

  if(Path->MustCancel)
  {
    Path->State = FMP_PATH_STATE_CANCELED;
    if(Path->Locked)
      SU_SEM_POST(Path->Sem);
    else
      printf("WARNING : FFSS_OnTransferFileWrite : Path->Locked is not locked !!\n");
    return false;
  }
  if(Path->MustPause)
  {
    Path->State = FMP_PATH_STATE_PAUSED;
    if(Path->Locked)
      SU_SEM_POST(Path->Sem);
    else
      printf("WARNING : FFSS_OnTransferFileWrite : Path->Locked is not locked !!\n");
    return false;
  }
  SU_SEM_WAIT(FMP_Sem_Blocs);
  if(Path->State != FMP_PATH_STATE_NOT_CONNECTED)
    Path->State = FMP_PATH_STATE_TRANSFERING;
  fseek(Path->File->fp,FT->StartingPos+Offset,SEEK_SET);
  if(fwrite(Buf,1,Size,Path->File->fp) != Size)
  {
    SU_SEM_POST(FMP_Sem_Blocs);
    return false;
  }
  Path->File->Blocs[Path->Idx].Pos += Size;
  SU_SEM_POST(FMP_Sem_Blocs);

  if(FMP_CB.OnPacketReceived != NULL)
    FMP_CB.OnPacketReceived(Path->File,Path->File->UserTag,Path->Idx,Size);
  return true;
}

/* False if no more blocs == Transfer complete (or completing) */
bool FMP_GetNextIdx(FMP_PPath Path)
{
  FFSS_Field i;

  printf("FMP_GetNextIdx : Searching for a Bloc to download\n");
  SU_SEM_WAIT(FMP_Sem_Blocs);
  for(i=0;i<Path->File->NbBlocs;i++)
  {
    if(Path->File->Blocs[i].State == FMP_BLOC_STATE_NOT_GOT)
    {
      Path->File->Blocs[i].State = FMP_BLOC_STATE_GETTING;
      Path->File->Blocs[i].Pos = 0;
      Path->Idx = i;
      Path->HaveIdx = true;
      if(FMP_CB.OnAssignBloc != NULL)
        FMP_CB.OnAssignBloc(Path->File,Path->File->UserTag,Path->Idx,Path->IP,Path->FullPath,Path->File->Name);
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
      FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_SEMAPHORE_CREATE_ERROR);
    SU_SLEEP(FMP_DELAY_RETRY_SEM); /* Retry in 30 sec */
  }
  Path->Locked = false;

  while(1)
  {
    while((Path->State == FMP_PATH_STATE_NOT_CONNECTED) || (Path->MustPause))
    {
      /* Check for cancel/pause */
      while(Path->MustPause)
      {
#ifdef DEBUG
        printf("[%x] THREAD PAUSED... SLEEPING\n",SU_THREAD_SELF);
#endif
        SU_SLEEP(1);
      }
      if(Path->MustCancel)
      {
        SU_SEM_WAIT(FMP_Sem_Search);
        FMP_FreeFile_internal(Path->File);
        SU_SEM_POST(FMP_Sem_Search);
        printf("FMP_StreamingRoutine CANCELED\n");
        return;
      }

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
          FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_HOST_DOWN);
        SU_SLEEP(FMP_ReconnectDelay); /* Retry in 2 min */
        continue;
      }
      SU_SEM_WAIT(Path->Sem);
      Path->Locked = false;
      SU_SEM_POST(Path->Sem);
      if(Path->State != FMP_PATH_STATE_NOT_CONNECTED)
        break;
      //if(FMP_CB.OnError != NULL)
        //FMP_CB.OnError(Path->File,Path->File->UserTag,Path->IP,Path->FullPath,Path->File->Name,FMP_ERRCODE_SHARE_CONNECT_FAILED);
      FC_SendMessage_Disconnect(Path->Server);
      Path->Server = NULL;
      SU_SLEEP(FMP_ReconnectDelay); /* Retry in 2 min */
    }

    while(FMP_GetNextIdx(Path)) /* Get next bloc to download from this Path */
    {
      FFSS_LongField StartPos;

      /* Try to download the file */
      if(Path->Locked == false)
        SU_SEM_WAIT(Path->Sem);
      else
        printf("WARNING : FMP_StreamingRoutine : Path->Locked is already locked !!\n");
      Path->Locked = true;

      Path->Error = false;
      snprintf(Buf,sizeof(Buf),"%s/%s",Path->Path,Path->File->Name);
      StartPos = (Path->File->BlocSize*Path->Idx)+Path->File->Blocs[Path->Idx].Pos;
      FFSS_DownloadFile(Path->Server,Buf,NULL,StartPos,StartPos + Path->File->BlocSize - 1,NULL,false,(FFSS_Field)Path,&Path->FT);

      /* Wait for EndOfBloc */
      SU_SEM_WAIT(Path->Sem);
      Path->Locked = false;
      SU_SEM_POST(Path->Sem);

      if(Path->File->Blocs[Path->Idx].State != FMP_BLOC_STATE_GOT) /* Error occured while downloading file */
      {
        printf("Couldn't get my bloc, error might have occured... disconnecting\n");
        SU_SEM_WAIT(FMP_Sem_Blocs);
        Path->File->Blocs[Path->Idx].State = FMP_BLOC_STATE_NOT_GOT;
        Path->File->Blocs[Path->Idx].Pos = 0;
        if(Path->HaveIdx)
        {
          if(FMP_CB.OnUnAssignBloc != NULL)
            FMP_CB.OnUnAssignBloc(Path->File,Path->File->UserTag,Path->Idx);
          Path->HaveIdx = false;
        }
        SU_SEM_POST(FMP_Sem_Blocs);
        break;
      }
    }

    /* EOF - Disconnecting */
    if(!Path->MustCancel && !Path->MustPause)
      Path->State = FMP_PATH_STATE_NOT_CONNECTED;
    FC_SendMessage_Disconnect(Path->Server);
    Path->Server = NULL;

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
          FMP_CB.OnDownloadComplete(Path->File,Path->File->UserTag,Path->File->Name);
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
      if(!Path->MustCancel && !Path->MustPause)
        SU_SLEEP(FMP_ReconnectDelay); /* Else, retry in 2 min */
      else
        Path->State = FMP_PATH_STATE_NOT_CONNECTED;
      continue;
    }
  }
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
  FFSS_CB.CCB.OnTransferFailed = FFSS_OnTransferFailed;
  FFSS_CB.CCB.OnTransferSuccess = FFSS_OnTransferSuccess;
  FFSS_CB.CCB.OnTransferFileWrite = FFSS_OnTransferFileWrite;
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

void FMP_SetReconnectDelay(FFSS_Field Delay) /* Delay in sec */
{
  if(Delay < FMP_MIN_RECONNECT_DELAY)
    Delay = FMP_MIN_RECONNECT_DELAY;
  FMP_ReconnectDelay = Delay;
}

/* ********************** QUERY INFOS ********************** */
char *FMP_GetLastError(void)
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

void FMP_ListPaths(struct FMP_SFile *File,FFSS_LongField UserTag,FMP_LISTPATHS_CB Func)
{
  SU_PList Ptr;
  FMP_PPath Pth;

  SU_SEM_WAIT(FMP_Sem_Search);
  Ptr = File->Paths;
  while(Ptr != NULL)
  {
    Pth = (FMP_PPath) Ptr->Data;
    Func(File,Pth,Pth->IP,Pth->FullPath,Pth->State,UserTag);
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

FFSS_Field FMP_GetPathState(struct FMP_SPath *Path)
{
  return Path->State;
}

char *FMP_GetName(void)
{
  return FMP_NAME;
}

char *FMP_GetVersion(void)
{
  return FMP_VERSION;
}

char *FMP_GetCopyright(void)
{
  return FMP_COPYRIGHT;
}


/* **************** DOWNLOAD FUNCTIONS *********************** */
bool FMP_StartDownload(struct FMP_SFile *File,const char DestFileName[],FFSS_LongField UserTag)
{
  SU_PList Ptr;
  FMP_PPath Pth;
  FILE *fp;

  if(File == NULL)
  {
    snprintf(FMP_Error,sizeof(FMP_Error),"FMP_SFile is NULL",DestFileName);
    return false;
  }
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

void FMP_CancelDownload(struct FMP_SFile *File)
{
  SU_PList Ptr;
  FMP_PPath Pth;

  SU_SEM_WAIT(FMP_Sem_Search);
  Ptr = File->Paths;
  while(Ptr != NULL)
  {
    Pth = (FMP_PPath) Ptr->Data;
    Pth->State = FMP_PATH_STATE_CANCELED;
    Pth->MustCancel = true;
    Pth->MustPause = false;
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FMP_Sem_Search);
}

void FMP_PauseDownload(struct FMP_SFile *File)
{
  SU_PList Ptr;
  FMP_PPath Pth;

  SU_SEM_WAIT(FMP_Sem_Search);
  Ptr = File->Paths;
  while(Ptr != NULL)
  {
    Pth = (FMP_PPath) Ptr->Data;
    FMP_PausePath(Pth);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FMP_Sem_Search);
}

void FMP_ResumeDownload(struct FMP_SFile *File)
{
  SU_PList Ptr;
  FMP_PPath Pth;

  SU_SEM_WAIT(FMP_Sem_Search);
  Ptr = File->Paths;
  while(Ptr != NULL)
  {
    Pth = (FMP_PPath) Ptr->Data;
    FMP_ResumePath(Pth);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FMP_Sem_Search);
}

void FMP_PausePath(struct FMP_SPath *Path)
{
  if(Path == NULL)
    return;
  Path->MustPause = true;
}

void FMP_ResumePath(struct FMP_SPath *Path)
{
  if(Path == NULL)
    return;
  Path->MustPause = false;
  Path->State = FMP_PATH_STATE_NOT_CONNECTED;
}

