#include "server.h"
#ifdef __unix__
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif /* __unix__ */
#include <assert.h>

SU_PList FS_Index=NULL; /* FS_PShare */

#ifdef __unix__
FFSS_LongField FS_BuildIndex_rec(FS_PShare Share,FS_PNode Node,const char Path[])
{
  FS_PDir Dir;
  FS_PFile File;
  char name[2048];
  DIR *dir;
  struct dirent *ent;
  struct stat st,st2;
  FFSS_LongField total_dir_size = 0;

  FFSS_PrintDebug(5,"\tBuilding index for sub-dir %s\n",Path);
  /* List all files in Path, and fill Node with it */
  dir = opendir(Path);
  if(dir == NULL)
  {
    FFSS_PrintDebug(4,"Error opening dir %s\n",Path);
    return 0;
  }
  ent = readdir(dir);
  while(ent != NULL)
  {
    if((strcmp(ent->d_name,".") == 0) || (strcmp(ent->d_name,"..") == 0))
    {
      ent = readdir(dir);
      continue;
    }
    snprintf(name,sizeof(name),"%s/%s",Path,ent->d_name);
    stat(name,&st);
    lstat(name,&st2);
    if(S_ISDIR(st.st_mode)) /* If the entry is a directory */
    {
      Dir = (FS_PDir) malloc(sizeof(FS_TDir));
      memset(Dir,0,sizeof(FS_TDir));
      Dir->DirName = strdup(ent->d_name);
      Dir->Flags = FFSS_FILE_DIRECTORY | ((st.st_mode&S_IXUSR)?FFSS_FILE_EXECUTABLE:0) | (S_ISLNK(st2.st_mode)?FFSS_FILE_LINK:0);
      Dir->Time = st.st_ctime;
      Dir->Size = FS_BuildIndex_rec(Share,&Dir->Files,name);
      total_dir_size += Dir->Size;
      Node->Dirs = SU_AddElementHead(Node->Dirs,Dir);
      Share->NbDirs++;
      Node->NbDirs++;
    }
    else
    {
      File = (FS_PFile) malloc(sizeof(FS_TFile));
      memset(File,0,sizeof(FS_TFile));
      File->FileName = strdup(ent->d_name);
      File->Flags = ((st.st_mode&S_IXUSR)?FFSS_FILE_EXECUTABLE:0) | (S_ISLNK(st2.st_mode)?FFSS_FILE_LINK:0);
      File->Size = st.st_size;
      File->Time = st.st_ctime;
      total_dir_size += File->Size;
      Node->Files = SU_AddElementHead(Node->Files,File);
      Share->NbFiles++;
      Node->NbFiles++;
    }
    ent = readdir(dir);
  }
  closedir(dir);
  return total_dir_size;
}
#else /* !__unix__ */

time_t FS_ConvertTime(FILETIME ft)
{
  struct tm t;
  SYSTEMTIME st;

  FileTimeToSystemTime(&ft,&st);
  t.tm_year = st.wYear - 1900;
  t.tm_mon = st.wMonth - 1;
  t.tm_mday = st.wDay;
  t.tm_wday = st.wDayOfWeek;
  t.tm_hour = st.wHour;
  t.tm_min = st.wMinute;
  t.tm_sec = st.wSecond;
  return mktime(&t);
}

FFSS_LongField FS_BuildIndex_rec(FS_PShare Share,FS_PNode Node,const char Path[])
{
  FS_PDir Dir;
  FS_PFile File;
  char name[2048];
  HANDLE dir;
  WIN32_FIND_DATA ent;

  FFSS_PrintDebug(5,"\tBuilding index for sub-dir %s\n",Path);
  /* List all files in Path, and fill Node with it */
  snprintf(name,sizeof(name),"%s\\*.*",Path);
  dir = FindFirstFile(name,&ent);
  if(dir == INVALID_HANDLE_VALUE)
  {
    FFSS_PrintDebug(4,"Error opening dir %s (%d)\n",name,GetLastError());
    return;
  }
  do
  {
    if((strcmp(ent.cFileName,".") == 0) || (strcmp(ent.cFileName,"..") == 0))
    {
      continue;
    }
    if(ent.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) /* If the entry is a directory */
    {
      Share->NbDirs++;
      Node->NbDirs++;
      Dir = (FS_PDir) malloc(sizeof(FS_TDir));
      memset(Dir,0,sizeof(FS_TDir));
      Dir->DirName = strdup(ent.cFileName);
      Dir->Flags = FFSS_FILE_DIRECTORY | FFSS_FILE_EXECUTABLE;
      Dir->Time = FS_ConvertTime(ent.ftCreationTime);
      snprintf(name,sizeof(name),"%s\\%s",Path,ent.cFileName);
      FS_BuildIndex_rec(Share,&Dir->Files,name);
      Node->Dirs = SU_AddElementHead(Node->Dirs,Dir);
    }
    else
    {
      Share->NbFiles++;
      Node->NbFiles++;
      File = (FS_PFile) malloc(sizeof(FS_TFile));
      memset(File,0,sizeof(FS_TFile));
      File->FileName = strdup(ent.cFileName);
      File->Flags = FFSS_FILE_EXECUTABLE;
      File->Size = (ent.nFileSizeHigh * MAXDWORD) + ent.nFileSizeLow;
      File->Time = FS_ConvertTime(ent.ftCreationTime);
      Node->Files = SU_AddElementHead(Node->Files,File);
    }
  } while(FindNextFile(dir,&ent));
  FindClose(dir);
}
#endif /* __unix__ */

void FS_BuildIndex(const char Path[],const char ShareName[],const char ShareComment[],bool Writeable,bool Private,int MaxConnections,SU_PList Users,bool do_it_now)
{
  FS_PShare Share;
  SU_PList Ptr;
#ifdef _WIN32
  char Pat[]="x:";
#endif /* _WIN32 */

  Share = (FS_PShare) malloc(sizeof(FS_TShare));
  memset(Share,0,sizeof(FS_TShare));
  Share->Path = strdup(Path);
  Share->ShareName = strdup(ShareName);
  Share->Comment = strdup(ShareComment);
  Share->Writeable = Writeable;
  Share->Private = Private;
  Share->MaxConnections = MaxConnections;
  Share->Users = Users;
  Ptr = Share->Users;
  while(Ptr != NULL)
  {
    FFSS_PrintDebug(5,"Adding user %s with password %s to this share (%s)\n",((FS_PUser)Ptr->Data)->Login,((FS_PUser)Ptr->Data)->Password,(((FS_PUser)Ptr->Data)->Writeable == true)?"writeable":"read-only");
    Ptr = Ptr->Next;
  }
  if(do_it_now)
  {
    FFSS_PrintDebug(5,"Start building index for share %s : %s\n",Share->ShareName,Share->Comment);
#ifdef _WIN32
    if(strlen(Share->Path) == 3)
    {
      Pat[0] = Share->Path[0];
      FS_BuildIndex_rec(Share,&Share->Root,Pat);
    }
    else
#endif /* _WIN32 */
      FS_BuildIndex_rec(Share,&Share->Root,Share->Path);
    FFSS_PrintDebug(5,"Done building %s (%ld files, %ld directories)\n",Share->ShareName,Share->NbFiles,Share->NbDirs);
  }

  FS_Index = SU_AddElementHead(FS_Index,Share);
}

void FS_RealBuildIndex(void)
{
  SU_PList Ptr;
  FS_PShare Share;
#ifdef _WIN32
  char Pat[]="x:";
#endif /* _WIN32 */

  Ptr = FS_Index;
  while(Ptr != NULL)
  {
    Share = (FS_PShare) Ptr->Data;
    FFSS_PrintDebug(5,"Start building index for share %s : %s\n",Share->ShareName,Share->Comment);
#ifdef _WIN32
    if(strlen(Share->Path) == 3)
    {
      Pat[0] = Share->Path[0];
      FS_BuildIndex_rec(Share,&Share->Root,Pat);
    }
    else
#endif /* _WIN32 */
      FS_BuildIndex_rec(Share,&Share->Root,Share->Path);
    FFSS_PrintDebug(5,"Done building %s (%ld files, %ld directories)\n",Share->ShareName,Share->NbFiles,Share->NbDirs);
    Ptr = Ptr->Next;
  }
}

void FS_FreeFiles(FS_PFile File)
{
  if(File->FileName != NULL)
    free(File->FileName);
  free(File);
}

void FS_FreeDirs(FS_PDir Dir)
{
  SU_PList Ptr;

  if(Dir->DirName != NULL)
    free(Dir->DirName);
  Ptr = Dir->Files.Dirs;
  while(Ptr != NULL)
  {
    FS_FreeDirs(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Dir->Files.Dirs);
  Ptr = Dir->Files.Files;
  while(Ptr != NULL)
  {
    FS_FreeFiles(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Dir->Files.Files);
  free(Dir);
}

void FS_FreeUser(FS_PUser Usr)
{
  if(Usr->Login != NULL)
    free(Usr->Login);
  if(Usr->Password != NULL)
    free(Usr->Password);
  free(Usr);
}

void FS_FreeShare(FS_PShare Share)
{
  SU_PList Ptr;

  if(Share->Conns != NULL)
  {
    FFSS_PrintDebug(5,"Share %s not empty, waiting for connections to terminate\n",Share->ShareName);
    FS_EjectFromShare(Share,true);
    Share->Remove = true;
    return;
  }
  FFSS_PrintDebug(5,"Freeing share %s\n",Share->ShareName);
  FS_RemoveShare(Share);
  if(Share->Path != NULL)
    free(Share->Path);
  if(Share->ShareName != NULL)
    free(Share->ShareName);
  if(Share->Comment != NULL)
    free(Share->Comment);
  Ptr = Share->Users;
  while(Ptr != NULL)
  {
    FS_FreeUser(Ptr->Data);
    Ptr = Ptr->Next;
  }
  Ptr = Share->Root.Dirs;
  while(Ptr != NULL)
  {
    FS_FreeDirs(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Share->Root.Dirs);
  Ptr = Share->Root.Files;
  while(Ptr != NULL)
  {
    FS_FreeFiles(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Share->Root.Files);
  free(Share);
}

void FS_FreeIndex(void)
{
  SU_PList Ptr;

  Ptr = FS_Index;
  while(Ptr != NULL)
  {
    FS_FreeShare(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(FS_Index);
  FS_Index = NULL;
}

void FS_RescanShare(FS_PShare Share)
{
  SU_PList Ptr;
#ifdef _WIN32
  char Pat[]="x:";
#endif /* _WIN32 */

  Ptr = Share->Root.Dirs;
  while(Ptr != NULL)
  {
    FS_FreeDirs(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Share->Root.Dirs);
  Ptr = Share->Root.Files;
  while(Ptr != NULL)
  {
    FS_FreeFiles(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Share->Root.Files);
  Share->Root.Dirs = NULL;
  Share->Root.NbDirs = 0;
  Share->Root.Files = NULL;
  Share->Root.NbFiles = 0;
  FFSS_PrintDebug(5,"Start re-building index for share %s : %s\n",Share->ShareName,Share->Comment);
#ifdef _WIN32
  if(strlen(Share->Path) == 3)
  {
    Pat[0] = Share->Path[0];
    FS_BuildIndex_rec(Share,&Share->Root,Pat);
  }
  else
#endif /* _WIN32 */
    FS_BuildIndex_rec(Share,&Share->Root,Share->Path);
}

/* Returns a buffer to be sent then freed, or NULL if the path is incorrect */
char *FS_BuildDirectoryBuffer(FS_PShare Share,const char Dir[],long int *size_out)
{
  char *buf;
  SU_PList Ptr;
  char *tmp,*tok;
#ifdef __unix__
  char *p;
#endif /* __unix__ */
  FS_PNode Node;
  long int buf_size,len,pos,i;

  assert(Share);
  if(Share == NULL)
    return NULL;
  Node = &Share->Root;
  if(Dir[0] != 0)
  {
    tmp = strdup(Dir);
    tok = strtok_r(tmp,"/",&p);
    while(tok != NULL)
    {
      Ptr = Node->Dirs;
      while(Ptr != NULL)
      {
        if(SU_strcasecmp(((FS_PDir)Ptr->Data)->DirName,tok))
          break;
        Ptr = Ptr->Next;
      }
      if(Ptr == NULL) /* Path not found */
      {
        free(tmp);
        return NULL;
      }
      Node = &((FS_PDir)Ptr->Data)->Files;
      tok = strtok_r(NULL,"/",&p);
    }
    free(tmp);
  }
  /* Node points to the requested directory - Now building buffer */
  if((Node->NbFiles+Node->NbDirs) == 0) /* If the directory is empty */
  {
    buf = (char *) malloc(sizeof(FFSS_Field));
    *(FFSS_Field *)buf = 0;
    *size_out = sizeof(FFSS_Field);
    return buf;
  }
  buf_size = FS_AVERAGE_FILE_LENGTH*(Node->NbFiles+Node->NbDirs);
  buf = (char *) malloc(buf_size);
  pos = 0;

  /* Flush number of entries */
  *(FFSS_Field *)(buf+pos) = Node->NbFiles+Node->NbDirs;
  pos += sizeof(FFSS_Field);

  /* Flush directories */
  Ptr = Node->Dirs;
  i = 0;
  while(Ptr != NULL)
  {
    len = strlen(((FS_PDir)Ptr->Data)->DirName)+1;
    while((pos+len) >= buf_size)
    {
      buf_size += (Node->NbDirs-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    SU_strcpy(buf+pos,((FS_PDir)Ptr->Data)->DirName,len);
    pos += len;

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbDirs-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    *(FFSS_Field *)(buf+pos) = ((FS_PDir)Ptr->Data)->Flags;
    pos += sizeof(FFSS_Field);

    if((pos+sizeof(FFSS_LongField)) >= buf_size)
    {
      buf_size += (Node->NbDirs-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    *(FFSS_LongField *)(buf+pos) = ((FS_PDir)Ptr->Data)->Size;
    pos += sizeof(FFSS_LongField);

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbDirs-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    *(FFSS_Field *)(buf+pos) = ((FS_PDir)Ptr->Data)->Time;
    pos += sizeof(FFSS_Field);

    i++;
    Ptr = Ptr->Next;
  }

  /* Flush files */
  Ptr = Node->Files;
  i = 0;
  while(Ptr != NULL)
  {
    len = strlen(((FS_PFile)Ptr->Data)->FileName)+1;
    while((pos+len) >= buf_size)
    {
      buf_size += (Node->NbFiles-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    SU_strcpy(buf+pos,((FS_PFile)Ptr->Data)->FileName,len);
    pos += len;

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbFiles-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    *(FFSS_Field *)(buf+pos) = ((FS_PFile)Ptr->Data)->Flags;
    pos += sizeof(FFSS_Field);

    if((pos+sizeof(FFSS_LongField)) >= buf_size)
    {
      buf_size += (Node->NbFiles-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    *(FFSS_LongField *)(buf+pos) = ((FS_PFile)Ptr->Data)->Size;
    pos += sizeof(FFSS_LongField);

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbFiles-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    *(FFSS_Field *)(buf+pos) = ((FS_PFile)Ptr->Data)->Time;
    pos += sizeof(FFSS_Field);

    i++;
    Ptr = Ptr->Next;
  }

  *size_out = pos;
  return buf;
}

/* Returns a buffer to be sent then freed */
char *FS_BuildIndexBuffer(FS_PNode Node,char *buf_in,long int *buf_pos,long int total,long int *buf_size,FM_TFTNode *TabNodes,long int *NodePos,unsigned char *Tags,long int father,long int total_node)
{
  SU_PList Ptr;
  char *buf;
  unsigned char tags,tg;
  long int size,pos,len,i,node,old;

  buf = buf_in;
  pos = *buf_pos;
  size = *buf_size;
  node = *NodePos;
  tags = *Tags;

  if((Node->NbFiles+Node->NbDirs) == 0) /* If the directory is empty */
  {
    return buf;
  }

  size += FS_AVERAGE_FILE_LENGTH*(Node->NbFiles+Node->NbDirs);
  buf = (char *) realloc(buf,size);

  /* Flush directories */
  Ptr = Node->Dirs;
  i = 0;
  while(Ptr != NULL)
  {
    /* Flush Directory Name */
    TabNodes[node].Pos = pos + total;
    TabNodes[node].Father = total_node + father;
    old = node;
    node++;
    len = strlen(((FS_PDir)Ptr->Data)->DirName)+1;
    while((pos+len) >= size)
    {
      size += (Node->NbDirs-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,size);
    }
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    SU_strcpy(buf+pos,((FS_PDir)Ptr->Data)->DirName,len);
    pos += len;
    tg = FFSS_FILE_TAGS_NOTHING;

    /* Flush sub directory */
    buf = FS_BuildIndexBuffer(&((FS_PDir)Ptr->Data)->Files,buf,&pos,total,&size,TabNodes,&node,&tg,old,total_node);
    TabNodes[old].Last = total_node + node - 1;
    TabNodes[old].Tags = tg;
    tags |= tg;

    /* Next directory */
    i++;
    Ptr = Ptr->Next;
  }

  /* Flush files */
  Ptr = Node->Files;
  i = 0;
  while(Ptr != NULL)
  {
    /* Flush File Name */
    TabNodes[node].Pos = pos + total;
    TabNodes[node].Father = total_node + father;
    TabNodes[node].Last = total_node + node;
    tg = FFSS_GetFileTags(((FS_PFile)Ptr->Data)->FileName);
    tags |= tg;
    TabNodes[node].Tags = tg;
    node++;
    len = strlen(((FS_PFile)Ptr->Data)->FileName)+1;
    while((pos+len) >= size)
    {
      size += (Node->NbFiles-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,size);
    }
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    SU_strcpy(buf+pos,((FS_PFile)Ptr->Data)->FileName,len);
    pos += len;

    i++;
    Ptr = Ptr->Next;
  }

  *Tags = tags;
  *NodePos = node;
  *buf_pos = pos;
  *buf_size = size;
  return buf;
}

bool FS_SendIndex(const char Host[],const char Port[])
{
  char *buf;
  SU_PList Ptr,Bufs,Sizes;
  long int pos,size,len,total,total_node;
  FS_PShare Share;
  bool res;
  int comp;
  FM_TFTNode *TabNodes;
  long int NodePos;
  long int NodeSize;
  unsigned char Tags;

  Bufs = NULL;
  Sizes = NULL;
  Ptr = FS_Index;
  total = 0;
  total_node = 0;
  while(Ptr != NULL)
  {
    Share = (FS_PShare) Ptr->Data;
    if(!Share->Private)
    {
      NodeSize = sizeof(FM_TFTNode)*(Share->NbFiles+Share->NbDirs+1);
      TabNodes = (FM_TFTNode *) malloc(NodeSize);
      TabNodes[0].Pos = total;
      TabNodes[0].Father = -1;
      NodePos = 1;

      Tags = FFSS_FILE_TAGS_NOTHING;
      size = FFSS_MAX_SHARENAME_LENGTH+1;
      buf = (char *) malloc(size);

      /* Flush Share Name */
      len = strlen(Share->ShareName)+1;
      if(len > FFSS_MAX_SHARENAME_LENGTH)
        len = FFSS_MAX_SHARENAME_LENGTH;
      SU_strcpy(buf,Share->ShareName,len);
      pos = len;

      buf = FS_BuildIndexBuffer(&Share->Root,buf,&pos,total,&size,TabNodes,&NodePos,&Tags,0,total_node);
      TabNodes[0].Last = total_node + NodePos - 1;
      TabNodes[0].Tags = Tags;
      Bufs = SU_AddElementTail(Bufs,buf);
      Sizes = SU_AddElementTail(Sizes,(void *)pos);
      Bufs = SU_AddElementTail(Bufs,TabNodes);
      Sizes = SU_AddElementTail(Sizes,(void *)NodeSize);
      total += pos;
      total_node += NodePos;
    }

    Ptr = Ptr->Next;
  }

/*  SaveState = FS_MyState;
  FS_MyState = FFSS_STATE_OFF;*/
#ifdef HAVE_BZLIB
  if(total >= FS_COMPRESSION_TRIGGER_BZLIB)
    comp = FFSS_COMPRESSION_BZLIB;
  else
#endif /* HAVE_BZLIB */
  if(total >= FS_COMPRESSION_TRIGGER_ZLIB)
    comp = FFSS_COMPRESSION_ZLIB;
  else
    comp = FFSS_COMPRESSION_NONE;
  res = FS_SendMessage_IndexAnswer(Host,Port,Bufs,Sizes,comp);
/*  FS_MyState = SaveState;*/

  SU_FreeListElem(Bufs);
  SU_FreeList(Sizes);

  return res;
}

bool FS_CaseFilePath(FS_PShare Share,char Path[])
{
  SU_PList Ptr;
  char *tmp,*tok,*tok2;
#ifdef __unix__
  char *p;
#endif /* __unix__ */
  FS_PNode Node;
  long int pos=1,len;

  Node = &Share->Root;
  if(Path[0] != 0)
  {
    tmp = strdup(Path);
    tok = strtok_r(tmp,"/",&p);
    tok2 = NULL;
    while(tok != NULL)
    {
      tok2 = strtok_r(NULL,"/",&p);
      if(tok2 == NULL)
        break;
      Ptr = Node->Dirs;
      while(Ptr != NULL)
      {
        if(SU_strcasecmp(((FS_PDir)Ptr->Data)->DirName,tok))
        {
          len = strlen(((FS_PDir)Ptr->Data)->DirName);
          memcpy(Path+pos,((FS_PDir)Ptr->Data)->DirName,len);
          pos += len + 1;
          break;
        }
        Ptr = Ptr->Next;
      }
      if(Ptr == NULL) /* Path not found */
      {
        free(tmp);
        return false;
      }
      Node = &((FS_PDir)Ptr->Data)->Files;
      tok = tok2;
    }
    /* Node points to the requested directory - Now checking file */
    Ptr = Node->Files;
    while(Ptr != NULL)
    {
      if(SU_strcasecmp(((FS_PFile)Ptr->Data)->FileName,tok))
      {
        len = strlen(((FS_PFile)Ptr->Data)->FileName);
        memcpy(Path+pos,((FS_PFile)Ptr->Data)->FileName,len);
        free(tmp);
        return true;
      }
      Ptr = Ptr->Next;
    }
    free(tmp);
  }
  return false;
}
