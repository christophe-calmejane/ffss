#include "server.h"

// bool FS_LoadConfig() is defined in parser.y as this file is only used under unix
// But must be defined here for other architectures

void FS_MainThread(void)
{

  pthread_join(FS_THR_UDP,NULL);
  pthread_join(FS_THR_TCP,NULL);
  printf("joined!\n");

}

bool FS_SaveConfig(const char FileName[])
{
  return true;
}

void FS_RemoveShare(FS_PShare Share) {}

bool FS_CheckDirectoryChanged(FS_PShare Share)
{
  struct stat st;
  char name[1024];

  FFSS_PrintDebug(5,"Checking for a change in share %s (%s)\n",Share->ShareName,Share->Path);
  snprintf(name,sizeof(name),"%s/.",Share->Path);
  stat(name,&st);
  if(st.st_ctime != Share->Time)
    return true;
  else
    return false;
}
