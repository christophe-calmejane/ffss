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
GtkCList *clist_dwl,*clist_fail;

G_LOCK_DEFINE(gbl_conns_lock);
GList *gbl_conns = NULL;

int main (int argc, char *argv[])
{
#ifdef ENABLE_NLS
  bindtextdomain (PACKAGE, PACKAGE_LOCALE_DIR);
  textdomain (PACKAGE);
#endif

  g_thread_init(NULL);
  gtk_set_locale();
  gtk_init (&argc, &argv);

  add_pixmap_directory (PACKAGE_DATA_DIR "/pixmaps");
  add_pixmap_directory (PACKAGE_SOURCE_DIR "/pixmaps");

  /*
   * The following code was added by Glade to create one of each component
   * (except popup menus), just so that you see something after building
   * the project. Delete any components that you don't want shown initially.
   */
  wnd_main = create_window1();
  gtk_widget_show (wnd_main);
  clist_dwl = (GtkCList *) lookup_widget(wnd_main,"clist5");
  clist_fail = (GtkCList *) lookup_widget(wnd_main,"clist6");
  assert(clist_dwl);
  assert(clist_fail);

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
  /* Sending server request message to broadcast */
#ifdef HAVE_MASTER
  if(FC_SendMessage_DomainListing(CLT_MASTER))
    FC_SendMessage_ServerList(CLT_MASTER,NULL,NULL);
  else
    FC_SendMessage_ServerSearch();
#else
  FC_SendMessage_MasterSearch();
  //FC_SendMessage_ServerSearch();
#endif

  gdk_threads_enter();
  gtk_main();
  gdk_threads_leave();

  /* Shutting down server */
  FC_UnInit();
  return 0;
}

