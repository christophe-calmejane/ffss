#include "client.h"
#include "interface.h"

/*
  TODO : Attention, le send du StrmClose ne passe pas (bad file descriptor sur la socket)
         Envoyer un StrmClose qd on change de chanson !!!
*/

void PlayNextFile(bool Lock)
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
    FNP_PlayFile_Path(ip,path);
  }
  if(Lock)
    gdk_threads_leave();
}

void OnEndOfFile()
{
  GList *items;
  gdk_threads_enter();

  items = FNP_clist->selection;
  if(items != NULL)
  {
    gtk_clist_unselect_row(FNP_clist,(gint)items->data,0);
    gtk_clist_select_row(FNP_clist,(gint)(items->data)+1,0);
    if(FNP_clist->selection == NULL)
      gtk_clist_select_row(FNP_clist,0,0);
  }
  gdk_threads_leave();

  printf("End of file... playing next one\n");
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

