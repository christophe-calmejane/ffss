/*
 * Initial main.c file generated by Glade. Edit as required.
 * Glade will not overwrite this file.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtk.h>

#include "interface.h"
#include "support.h"
#include "client.h"

GtkWidget *wnd_main;
GtkListStore *store_dwl,*store_fail;

G_LOCK_DEFINE(gbl_conns_lock);
GList *gbl_conns = NULL;
char *MyMaster = NULL;

#if defined(_WIN32) && !defined(_CONSOLE)
int APIENTRY WinMain(HINSTANCE hInstance,HINSTANCE hPrevInstance,LPSTR lpCmdLine,int nCmdShow)
#else
int main (int argc, char *argv[])
#endif
{

  g_thread_init(NULL);
  gtk_set_locale();
  gtk_init (NULL,NULL);
  gtk_rc_parse("ffssclient.gtkrc");

  /*
   * The following code was added by Glade to create one of each component
   * (except popup menus), just so that you see something after building
   * the project. Delete any components that you don't want shown initially.
   */
  wnd_main = create_window1();
  gtk_widget_show (wnd_main);
  store_dwl = (GtkListStore *) lookup_widget(wnd_main,"store4");
  store_fail = (GtkListStore *) lookup_widget(wnd_main,"store5");
  assert(store_dwl);
  assert(store_fail);

  /* FFSS Initialization */
  memset(&FFSS_CB,0,sizeof(FFSS_CB));
  /* UDP CALLBACKS */
  FFSS_CB.CCB.OnNewState = OnNewState;
  FFSS_CB.CCB.OnSharesListing = OnSharesListing;
  FFSS_CB.CCB.OnServerListingAnswer = OnServerListingAnswer;
  FFSS_CB.CCB.OnEndServerListingAnswer = OnEndServerListingAnswer;
  FFSS_CB.CCB.OnDomainListingAnswer = OnDomainListingAnswer;
  FFSS_CB.CCB.OnMasterSearchAnswer = OnMasterSearchAnswer;
  FFSS_CB.CCB.OnError = OnError;
  FFSS_CB.CCB.OnDirectoryListingAnswer = OnDirectoryListingAnswer;
  /* TCP CALLBACKS */
//  void (*OnBeginTCPThread)(SU_PClientSocket Server);
  FFSS_CB.CCB.OnError = OnError;
  FFSS_CB.CCB.OnDirectoryListingAnswer = OnDirectoryListingAnswer;
  FFSS_CB.CCB.OnEndTCPThread = OnEndTCPThread;
  FFSS_CB.CCB.OnIdleTimeout = OnIdleTimeout;
  FFSS_CB.CCB.OnTransferFailed = OnTransfertFailed;
  FFSS_CB.CCB.OnTransferSuccess = OnTransfertSuccess;
  FFSS_CB.CCB.OnTransferActive = OnTransfertActive;
//  FFSS_CB.CCB.OnInitXFer = OnInitXFer;
//  FFSS_CB.CCB.OnData = OnData;
  if(!FC_Init())
    return -1;

#ifdef HAVE_MASTER
  MyMaster = strdup(CLT_MASTER);
#endif /* HAVE_MASTER */
  if(MyMaster != NULL)
  {
    if(FC_SendMessage_DomainListing(MyMaster))
      FC_SendMessage_ServerList(MyMaster,NULL,NULL);
    else
      FC_SendMessage_ServerSearch();
  }
  else
  {
    FC_SendMessage_MasterSearch();
    SU_SLEEP(4);
    if(MyMaster == NULL)
      FC_SendMessage_ServerSearch();
  }

  gdk_threads_enter();
  gtk_main();
  gdk_threads_leave();

  /* Shutting down server */
  FC_UnInit();
  return 0;
}

