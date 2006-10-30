#include "client.h"
#include "interface.h"

/*
  TODO : Envoyer un StrmClose qd on change de chanson !!!
*/

int FNP_CurrentSong = 0;

void PlayNextFile(bool Lock)
{
  gchar *path;
  gchar *ip;
  char Share[100];
  char File[512];

  if(Lock)
    gdk_threads_enter();
  printf("Playing song %d\n",FNP_CurrentSong);
  gtk_clist_get_text(FNP_clist,FNP_CurrentSong,0,&path);
  gtk_clist_get_text(FNP_clist,FNP_CurrentSong,1,&ip);
  if(path == NULL)
  {
    printf("Looping songs\n");
    FNP_CurrentSong = 0;
    gtk_clist_get_text(FNP_clist,FNP_CurrentSong,0,&path);
    gtk_clist_get_text(FNP_clist,FNP_CurrentSong,1,&ip);
    if(path == NULL)
    {
      printf("Ending... no more valid songs\n");
      if(Lock)
        gdk_threads_leave();
      return;
    }
  }
  assert(path);
  assert(ip);
  FNP_PlayFile_Path(ip,path);
  if(Lock)
    gdk_threads_leave();
}

void OnEndOfFile()
{
  printf("End of file... playing next one\n");
  FNP_CurrentSong++;
  PlayNextFile(true);
}

void OnEndTCPThread(void)
{
  printf("Connection with ffss server lost\n");
}

void OnError(int Code)
{
  printf("Received an error code : %d\n",Code);
}

void OnSearchAnswerStart(void)
{
  gdk_threads_enter();
}

void OnSearchAnswerItem(const char IP[],const char Path[])
{
  gchar *strings[2];

  printf("Adding : %s (%s)\n",Path,IP);
  strings[0] = strdup(Path);
  strings[1] = strdup(IP);
  gtk_clist_append(FNP_clist,strings);
}

void OnSearchAnswerEnd(void)
{
  gdk_threads_leave();
}

