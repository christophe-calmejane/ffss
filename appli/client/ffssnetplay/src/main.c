/*
 * Initial main.c file generated by Glade. Edit as required.
 * Glade will not overwrite this file.
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <gtk/gtk.h>

#include "interface.h"
#include "support.h"
#include "client.h"

GtkWidget *wnd_main;
GtkCList *FNP_clist;
SU_THREAD_HANDLE FNP_Thread;

int main (int argc, char *argv[])
{
  g_thread_init(NULL);
  gtk_set_locale ();
  gtk_init (&argc, &argv);

  /*
   * The following code was added by Glade to create one of each component
   * (except popup menus), just so that you see something after building
   * the project. Delete any components that you don't want shown initially.
   */
  wnd_main = create_window1 ();
  gtk_widget_show (wnd_main);
  FNP_clist = (GtkCList *) lookup_widget(wnd_main,"clist1");

  FNP_CB.OnEndOfFile = OnEndOfFile;
  FNP_CB.OnEndTCPThread = OnEndTCPThread;
  FNP_CB.OnError = OnError;
  FNP_CB.OnSearchAnswerStart = OnSearchAnswerStart;
  FNP_CB.OnSearchAnswerItem = OnSearchAnswerItem;
  FNP_CB.OnSearchAnswerEnd = OnSearchAnswerEnd;
  if(!FNP_Init("ffss"))
  {
    printf("Error : %s\n",FNP_GetLastError());
    return -2;
  }
  SU_DBG_SetOutput(SU_DBG_OUTPUT_PRINTF);
  SU_DBG_SetOptions(true,true);
#ifdef DEBUG
  SU_DBG_SetFlags(FFSS_DBGMSG_ALL);
#endif /* DEBUG */

  gdk_threads_enter();
  gtk_main();
  gdk_threads_leave();

  FNP_Uninit();
  return 0;
}

