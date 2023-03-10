/*
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
#include "server.h"
#ifdef __unix__
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif /* __unix__ */
#include <assert.h>

SU_PList FS_Index=NULL; /* FS_PShare */

bool FS_GetChecksumForFile(FS_PFile File,const char Path[])
{
  /* Load checksum from file. Set it in File->ChkSum, and return true */
  return false;
}

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
  FILE *fp;
  char ChksumBuf[1024];
  int siz;

  SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"\tBuilding index for sub-dir %s",Path);
  /* List all files in Path, and fill Node with it */
  dir = opendir(Path);
  if(dir == NULL)
  {
    SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"Error opening dir %s",Path);
    return 0;
  }
  ent = readdir(dir);
  while(ent != NULL)
  {
    if((strcmp(ent->d_name,".") == 0) || (strcmp(ent->d_name,"..") == 0))
    {
      if(strcmp(Share->Path,Path) == 0)
      {
        char *ttt;
        snprintf(name,sizeof(name),"%s/.",Share->Path);
        stat(name,&st);
        Share->Time = st.st_ctime;
        ttt = ctime(&Share->Time);
        ttt[strlen(ttt)-1] = 0;
        SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"\tStating share main directory... time = %s",ttt);
      }
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
      if((Share->NoChksum == false) && (File->Size >= FS_MIN_FILE_CHKSUM_SIZE))
      {
        if(!FS_GetChecksumForFile(File,name))
        {
          fp = fopen(name,"rb");
          if(fp != NULL)
          {
            siz = sizeof(ChksumBuf);
            if(siz > File->Size)
              siz = File->Size;
            fread(ChksumBuf,1,siz,fp);
            fclose(fp);
            File->ChkSum = FFSS_ComputeChecksum(0,ChksumBuf,siz);
          }
        }
      }
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
  FFSS_LongField total_dir_size = 0;
  FILE *fp;
  char ChksumBuf[1024];
  size_t siz;

  SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"\tBuilding index for sub-dir %s",Path);
  /* List all files in Path, and fill Node with it */
  snprintf(name,sizeof(name),"%s\\*.*",Path);
  dir = FindFirstFile(name,&ent);
  if(dir == INVALID_HANDLE_VALUE)
  {
    SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"Error opening dir %s (%d)",name,GetLastError());
    return 0;
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
      Dir->Size = FS_BuildIndex_rec(Share,&Dir->Files,name);
      total_dir_size += Dir->Size;
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
      if((Share->NoChksum == false) && (File->Size >= FS_MIN_FILE_CHKSUM_SIZE))
      {
        snprintf(name,sizeof(name),"%s\\%s",Path,ent.cFileName);
        if(!FS_GetChecksumForFile(File,name))
        {
          fp = fopen(name,"rb");
          if(fp != NULL)
          {
            siz = sizeof(ChksumBuf);
            if(siz > File->Size)
              siz = (size_t)File->Size;
            fread(ChksumBuf,1,siz,fp);
            fclose(fp);
            File->ChkSum = FFSS_ComputeChecksum(0,ChksumBuf,siz);
          }
        }
      }
      total_dir_size += File->Size;
      Node->Files = SU_AddElementHead(Node->Files,File);
    }
  } while(FindNextFile(dir,&ent));
  FindClose(dir);
  return total_dir_size;
}
#endif /* __unix__ */

/* Locks FS_SemShr */
void FS_BuildIndex(const char Path[],const char ShareName[],const char ShareComment[],bool Writeable,bool Private,bool NoChksum,int MaxConnections,SU_PList Users,bool do_it_now)
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
  Share->NoChksum = NoChksum;
  Share->MaxConnections = MaxConnections;
  Share->Users = Users;
  Ptr = Share->Users;
  while(Ptr != NULL)
  {
    SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"Adding user %s with password %s to this share (%s)",((FS_PUser)Ptr->Data)->Login,((FS_PUser)Ptr->Data)->Password,(((FS_PUser)Ptr->Data)->Writeable == true)?"writeable":"read-only");
    Ptr = Ptr->Next;
  }
  if(do_it_now)
  {
    SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"Start building index for share %s : %s",Share->ShareName,Share->Comment);
#ifdef _WIN32
    if(strlen(Share->Path) == 3)
    {
      Pat[0] = Share->Path[0];
      FS_BuildIndex_rec(Share,&Share->Root,Pat);
    }
    else
#endif /* _WIN32 */
      FS_BuildIndex_rec(Share,&Share->Root,Share->Path);
    SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"Done building %s (%ld files, %ld directories)",Share->ShareName,Share->NbFiles,Share->NbDirs);
  }

  SU_SEM_WAIT(FS_SemShr);
  FS_Index = SU_AddElementHead(FS_Index,Share);
  SU_SEM_POST(FS_SemShr);
}

/* Locks FS_SemShr */
void FS_RealBuildIndex(void)
{
  SU_PList Ptr;
  FS_PShare Share;
#ifdef _WIN32
  char Pat[]="x:";
#endif /* _WIN32 */

  SU_SEM_WAIT(FS_SemShr);
  Ptr = FS_Index;
  while(Ptr != NULL)
  {
    Share = (FS_PShare) Ptr->Data;
    SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"Start building index for share %s : %s",Share->ShareName,Share->Comment);
#ifdef _WIN32
    if(strlen(Share->Path) == 3)
    {
      Pat[0] = Share->Path[0];
      FS_BuildIndex_rec(Share,&Share->Root,Pat);
    }
    else
#endif /* _WIN32 */
      FS_BuildIndex_rec(Share,&Share->Root,Share->Path);
    SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"Done building %s (%ld files, %ld directories)",Share->ShareName,Share->NbFiles,Share->NbDirs);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemShr);
}

void FS_FreeFiles(FS_PShare Share,FS_PFile File)
{
  if(File->FileName != NULL)
    free(File->FileName);
  free(File);
  Share->NbFiles--;
}

void FS_FreeDirs(FS_PShare Share,FS_PDir Dir)
{
  SU_PList Ptr;

  if(Dir->DirName != NULL)
    free(Dir->DirName);
  Ptr = Dir->Files.Dirs;
  while(Ptr != NULL)
  {
    FS_FreeDirs(Share,Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Dir->Files.Dirs);
  Ptr = Dir->Files.Files;
  while(Ptr != NULL)
  {
    FS_FreeFiles(Share,Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Dir->Files.Files);
  free(Dir);
  Share->NbDirs--;
}

void FS_FreeUser(FS_PUser Usr)
{
  if(Usr->Login != NULL)
    free(Usr->Login);
  if(Usr->Password != NULL)
    free(Usr->Password);
  free(Usr);
}

/* Assumes FS_SemShr is locked */
void FS_FreeShare(FS_PShare Share)
{
  SU_PList Ptr;

  if(Share->Conns != NULL)
  {
    SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"Share %s not empty, waiting for connections to terminate",Share->ShareName);
    FS_EjectFromShare(Share,true);
    Share->Remove = true;
    return;
  }
  SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"Freeing share %s",Share->ShareName);
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
  SU_FreeList(Share->Users);
  Ptr = Share->Root.Dirs;
  while(Ptr != NULL)
  {
    FS_FreeDirs(Share,Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Share->Root.Dirs);
  Ptr = Share->Root.Files;
  while(Ptr != NULL)
  {
    FS_FreeFiles(Share,Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Share->Root.Files);
  free(Share);
}

/* Locks FS_SemShr */
void FS_FreeIndex(void)
{
  SU_PList Ptr;

  SU_SEM_WAIT(FS_SemShr);
  Ptr = FS_Index;
  while(Ptr != NULL)
  {
    FS_FreeShare(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(FS_Index);
  FS_Index = NULL;
  SU_SEM_POST(FS_SemShr);
}

/* FS_SemShr must has been locked */
void FS_RescanShare(FS_PShare Share)
{
  SU_PList Ptr;
  bool Old;
#ifdef _WIN32
  char Pat[]="x:";
#endif /* _WIN32 */

  Old = Share->Disabled;
  Share->Disabled = true;
  Ptr = Share->Root.Dirs;
  while(Ptr != NULL)
  {
    FS_FreeDirs(Share,Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Share->Root.Dirs);
  Ptr = Share->Root.Files;
  while(Ptr != NULL)
  {
    FS_FreeFiles(Share,Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(Share->Root.Files);
  Share->Root.Dirs = NULL;
  Share->Root.NbDirs = 0;
  Share->Root.Files = NULL;
  Share->Root.NbFiles = 0;
  SU_DBG_PrintDebug(FS_DBGMSG_INDEX,"Start re-building index for share %s : %s",Share->ShareName,Share->Comment);
#ifdef _WIN32
  if(strlen(Share->Path) == 3)
  {
    Pat[0] = Share->Path[0];
    FS_BuildIndex_rec(Share,&Share->Root,Pat);
  }
  else
#endif /* _WIN32 */
    FS_BuildIndex_rec(Share,&Share->Root,Share->Path);
  Share->Disabled = Old;
}

/* Assumes FS_SemShr is locked */
/* Returns a buffer to be sent then freed, or NULL if the path is incorrect */
char *FS_BuildDirectoryBuffer(FS_PShare Share, const char Dir[], size_t *size_out)
{
  char *buf;
  SU_PList Ptr;
  char *tmp,*tok;
#ifdef __unix__
  char *p;
#endif /* __unix__ */
  FS_PNode Node;
  size_t buf_size,len,pos,i;

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
  pos = FFSS_PackField(buf,pos,Node->NbFiles+Node->NbDirs);

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
    pos = FFSS_PackString(buf,pos,((FS_PDir)Ptr->Data)->DirName,len);

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbDirs-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    pos = FFSS_PackField(buf,pos,((FS_PDir)Ptr->Data)->Flags);

    if((pos+sizeof(FFSS_LongField)) >= buf_size)
    {
      buf_size += (Node->NbDirs-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    pos = FFSS_PackLongField(buf,pos,((FS_PDir)Ptr->Data)->Size);

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbDirs-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    pos = FFSS_PackField(buf,pos,(FFSS_Field)((FS_PDir)Ptr->Data)->Time);

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
    pos = FFSS_PackString(buf,pos,((FS_PFile)Ptr->Data)->FileName,len);

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbFiles-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    pos = FFSS_PackField(buf,pos,((FS_PFile)Ptr->Data)->Flags);

    if((pos+sizeof(FFSS_LongField)) >= buf_size)
    {
      buf_size += (Node->NbFiles-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    pos = FFSS_PackLongField(buf,pos,((FS_PFile)Ptr->Data)->Size);

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbFiles-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
	pos = FFSS_PackField(buf, pos, (FFSS_Field)((FS_PFile)Ptr->Data)->Time);

    i++;
    Ptr = Ptr->Next;
  }

  *size_out = pos;
  return buf;
}

char *FS_BuildRecursiveDirectoryBuffer_rec(FS_PNode Node,const char Dir[],const char Relative[],char *buf,size_t *buf_size_in_out,unsigned int *nb_entries_in_out,size_t *size_in_out)
{
  char NewDir[512],NewRelative[512];
  size_t newdir_eos,newrelative_eos;
  ptrdiff_t pos;
	unsigned int nb_entries;
	size_t buf_size, i, len;
  SU_PList Ptr;
  FS_PNode NewNode;

  if(Dir[1] == 0) /* Root directory */
    SU_strcpy(NewDir,Dir,sizeof(NewDir));
  else
    snprintf(NewDir,sizeof(NewDir),"%s/",Dir);
  newdir_eos = strlen(NewDir); /* Offset to end of string */
  if(Relative[0] == 0)
    NewRelative[0] = 0;
  else
    snprintf(NewRelative,sizeof(NewRelative),"%s/",Relative);
  newrelative_eos = strlen(NewRelative); /* Offset to end of string */
  buf_size = *buf_size_in_out;
  nb_entries = *nb_entries_in_out;
  pos = *size_in_out;

  /* Flush number of entries */
  nb_entries += Node->NbFiles + Node->NbDirs;

  /* Flush directories */
  Ptr = Node->Dirs;
  i = 0;
  while(Ptr != NULL)
  {
    NewNode = &((FS_PDir)Ptr->Data)->Files;
    SU_strcat(NewRelative,((FS_PDir)Ptr->Data)->DirName,sizeof(NewRelative));
    len = strlen(NewRelative)+1;
    while((pos+len) >= buf_size)
    {
      buf_size += (Node->NbDirs-i+1)*FS_REC_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    pos = FFSS_PackString(buf,pos,NewRelative,len);

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbDirs-i+1)*FS_REC_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    pos = FFSS_PackField(buf,pos,((FS_PDir)Ptr->Data)->Flags);

    if((pos+sizeof(FFSS_LongField)) >= buf_size)
    {
      buf_size += (Node->NbDirs-i+1)*FS_REC_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    pos = FFSS_PackLongField(buf,pos,((FS_PDir)Ptr->Data)->Size);

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbDirs-i+1)*FS_REC_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
	pos = FFSS_PackField(buf, pos, (FFSS_Field)((FS_PDir)Ptr->Data)->Time);

    if((NewNode->NbFiles+NewNode->NbDirs) != 0) /* If the directory is not empty */
    {
      SU_strcat(NewDir,((FS_PDir)Ptr->Data)->DirName,sizeof(NewDir));
      buf = FS_BuildRecursiveDirectoryBuffer_rec(NewNode,NewDir,NewRelative,buf,&buf_size,&nb_entries,&pos);
      NewDir[newdir_eos] = 0; /* Reset string to current directory */
    }
    NewRelative[newrelative_eos] = 0; /* Reset string to current relative */
    i++;
    Ptr = Ptr->Next;
  }

  /* Flush files */
  Ptr = Node->Files;
  i = 0;
  while(Ptr != NULL)
  {
    SU_strcat(NewDir,((FS_PFile)Ptr->Data)->FileName,sizeof(NewDir));
    len = strlen(NewDir)+1;
    while((pos+len) >= buf_size)
    {
      buf_size += (Node->NbFiles-i+1)*FS_REC_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    pos = FFSS_PackString(buf,pos,NewDir,len);
    NewDir[newdir_eos] = 0; /* Reset string to current directory */

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbFiles-i+1)*FS_REC_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    pos = FFSS_PackField(buf,pos,((FS_PFile)Ptr->Data)->Flags);

    if((pos+sizeof(FFSS_LongField)) >= buf_size)
    {
      buf_size += (Node->NbFiles-i+1)*FS_REC_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
    pos = FFSS_PackLongField(buf,pos,((FS_PFile)Ptr->Data)->Size);

    if((pos+sizeof(FFSS_Field)) >= buf_size)
    {
      buf_size += (Node->NbFiles-i+1)*FS_REC_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,buf_size);
    }
	pos = FFSS_PackField(buf, pos, (FFSS_Field)((FS_PFile)Ptr->Data)->Time);

    i++;
    Ptr = Ptr->Next;
  }

  *buf_size_in_out = buf_size;
  *nb_entries_in_out = nb_entries;
  *size_in_out = pos;
  return buf;
}

/* Assumes FS_SemShr is locked */
/* Returns a buffer to be sent then freed, or NULL if the path is incorrect */
char *FS_BuildRecursiveDirectoryBuffer(FS_PShare Share,const char Dir[],size_t *size_out)
{
  char *buf;
  SU_PList Ptr;
  char *tmp,*tok;
#ifdef __unix__
  char *p;
#endif /* __unix__ */
  FS_PNode Node;
  unsigned int nb_entries = 0;
	size_t buf_size, pos;

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
  pos = sizeof(FFSS_Field); /* Keep space for nb entries */

  if(Dir[0] == 0)
    buf = FS_BuildRecursiveDirectoryBuffer_rec(Node,"/","",buf,&buf_size,&nb_entries,&pos);
  else
    buf = FS_BuildRecursiveDirectoryBuffer_rec(Node,Dir,"",buf,&buf_size,&nb_entries,&pos);

  /* Flush number of entries */
  FFSS_PackField(buf,0,nb_entries);

  *size_out = pos;
  return buf;
}

/* Assumes FS_SemShr is locked */
/* Returns a buffer to be sent then freed */
char *FS_BuildIndexBuffer(FS_PNode Node, char *buf_in, size_t *buf_pos, size_t total, size_t *buf_size, FM_TFTNode *TabNodes, FFSS_LongField *NodePos, FFSS_BitField *Tags, FFSS_LongField father, FFSS_LongField total_node, size_t *fsize_out)
{
  SU_PList Ptr;
  char *buf;
  size_t size,pos,len,i;
	FFSS_LongField node, old;
	FFSS_BitField tags, tg;
  size_t total_fsize = 0,fsize;

  buf = buf_in;
  pos = *buf_pos;
  size = *buf_size;
  node = *NodePos;
  tags = *Tags;

  if((Node->NbFiles+Node->NbDirs) == 0) /* If the directory is empty */
  {
    *fsize_out = 0;
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
    pos = FFSS_PackString(buf,pos,((FS_PDir)Ptr->Data)->DirName,len);
    tg = FFSS_FILE_TAGS_NOTHING;

    /* Flush sub directory */
    buf = FS_BuildIndexBuffer(&((FS_PDir)Ptr->Data)->Files,buf,&pos,total,&size,TabNodes,&node,&tg,old,total_node,&fsize);
    total_fsize += fsize;
    TabNodes[old].Last = total_node + node - 1;
    TabNodes[old].Tags = tg;
    TabNodes[old].Size = fsize;
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
    TabNodes[node].Size = ((FS_PFile)Ptr->Data)->Size;
    TabNodes[node].ChkSum = ((FS_PFile)Ptr->Data)->ChkSum;
    total_fsize += (size_t)((FS_PFile)Ptr->Data)->Size;
    node++;
    len = strlen(((FS_PFile)Ptr->Data)->FileName)+1;
    while((pos+len) >= size)
    {
      size += (Node->NbFiles-i+1)*FS_AVERAGE_FILE_LENGTH;
      buf = (char *) realloc(buf,size);
    }
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    pos = FFSS_PackString(buf,pos,((FS_PFile)Ptr->Data)->FileName,len);

    i++;
    Ptr = Ptr->Next;
  }

  *Tags = tags;
  *NodePos = node;
  *buf_pos = pos;
  *buf_size = size;
  *fsize_out = total_fsize;
  return buf;
}

/* Locks FS_SemShr */
bool FS_SendIndex(const char Host[],const char Port[])
{
  char *buf;
  SU_PList Ptr,Bufs,Sizes,Plugs;
	size_t pos, size, len, total;
	FFSS_LongField total_node;
  FS_PShare Share;
  bool res,do_it,checked;
  int comp;
  FM_TFTNode *TabNodes;
	FFSS_LongField NodePos;
  size_t NodeSize;
	FFSS_BitField Tags;
  size_t fsize;

  if(Host == NULL)
    return true;
  Bufs = NULL;
  Sizes = NULL;
  SU_SEM_WAIT(FS_SemShr);
  Ptr = FS_Index;
  total = 0;
  total_node = 0;
  while(Ptr != NULL)
  {
    Share = (FS_PShare) Ptr->Data;
    do_it = true;
    checked = false;
    SU_SEM_WAIT(FS_SemPlugin);
    Plugs = FS_Plugins;
    while(Plugs != NULL)
    {
      if(((FS_PPlugin)Plugs->Data)->OnCheckShowShare != NULL)
      {
        do_it &= ((FS_PPlugin)Plugs->Data)->OnCheckShowShare(Share);
        checked = true;
      }
      Plugs = Plugs->Next;
    }
    SU_SEM_POST(FS_SemPlugin);
    if((!Share->Private && !checked) || (checked && do_it))
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
      pos = FFSS_PackString(buf,0,Share->ShareName,len);

      buf = FS_BuildIndexBuffer(&Share->Root,buf,&pos,total,&size,TabNodes,&NodePos,&Tags,0,total_node,&fsize);
      TabNodes[0].Last = total_node + NodePos - 1;
      TabNodes[0].Tags = Tags;
      TabNodes[0].Size = fsize;
      Bufs = SU_AddElementTail(Bufs,buf);
      Sizes = SU_AddElementTail(Sizes,(void *)pos);
      Bufs = SU_AddElementTail(Bufs,TabNodes);
      Sizes = SU_AddElementTail(Sizes,(void *)NodeSize);
      total += pos;
      total_node += NodePos;
    }

    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemShr);

#ifdef HAVE_BZLIB
  if(total >= FS_COMPRESSION_TRIGGER_BZLIB)
    comp = FFSS_COMPRESSION_BZLIB;
  else
#endif /* HAVE_BZLIB */
#ifndef DISABLE_ZLIB
  if(total >= FS_COMPRESSION_TRIGGER_ZLIB)
    comp = FFSS_COMPRESSION_ZLIB;
  else
#endif /* !DISABLE_ZLIB */
    comp = FFSS_COMPRESSION_NONE;
  res = FS_SendMessage_IndexAnswer(Host,Port,Bufs,Sizes,comp);

  SU_FreeListElem(Bufs);
  SU_FreeList(Sizes);

  return res;
}

/* Assumes FS_SemShr is locked */
bool FS_CaseFilePath(FS_PShare Share,char Path[])
{
  SU_PList Ptr;
  char *tmp,*tok,*tok2;
#ifdef __unix__
  char *p;
#endif /* __unix__ */
  FS_PNode Node;
  size_t pos=1,len;

  Node = &Share->Root;
  if((Path[0] != 0) && (Path[0] == '/'))
  {
    if(Path[1] == 0)
      return false;
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
          pos += len;
          Path[pos++] = '/';
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
        pos += len;
        Path[pos] = 0;
        free(tmp);
        return true;
      }
      Ptr = Ptr->Next;
    }
    free(tmp);
  }
  return false;
}
