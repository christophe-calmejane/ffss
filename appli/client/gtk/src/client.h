#ifndef _CLIENT_H_
#define _CLIENT_H_

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
//#include <unistd.h>
#include <string.h>

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <ffss.h>
#include <assert.h>

#ifdef __unix__
#define MKDIR(x) mkdir(x,493)
#else /* __unix__ */
#include <direct.h>
#define MKDIR(x) _mkdir(x)
#endif /* __unix__ */

/* TEMPO */
//#define HAVE_MASTER
#ifdef HAVE_MASTER
#define CLT_MASTER "ffss"
#endif

extern char *MyMaster;

typedef struct
{
  GtkWidget *wnd;           /* Window managing this connection     */
  gchar *host_share;        /* Host and share name                 */
  gchar *path;              /* Current browsing path in the server */
  SU_PClientSocket Server;  /* Connection with Server              */
  bool Active;              /* Says if Server is valid or not      */
  guint ctx_id;             /* Context id for statusbar            */
  GtkStatusbar *sb;         /* Shortcut to statusbar               */
  //GtkCList *list;           /* Shortcut to clist                   */
  GtkTreeModel *store;      /* Shortcut to GtkListStore            */
  GtkTreeView *view;        /* Shortcut to GtkTreeView             */
  int ref_count;            /* Number of usage of this structure   */
  bool recursif;            /* If recursive listing is in progress */
  int rec_dir_count;        /* Number of directories to list       */
  gchar *rec_path;
  gchar *rec_local_path;
} TConn, *PConn;

void FC_DQ_AddOpToList_AddFileToDownload(PConn Conn,const char Remote[],const char Local[],const char Size[]);
void add_conn_count(PConn Conn);
bool remove_conn(PConn Conn,bool clear_wnd);
PConn lookup_conn(SU_PClientSocket Server,bool even_null_wnd);

/* UDP callbacks */
void OnNewState(FFSS_Field State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[]);
void OnSharesListing(const char IP[],const char **Names,const char **Comments,int NbShares,FFSS_LongField User);
/* Except for the FM_PHost->IP that is dupped internaly, and if you don't use it, you MUST free it !! */
void OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList,FFSS_LongField User); /* SU_PList of FM_PHost */
void OnEndServerListingAnswer(void);
void OnDomainListingAnswer(const char **Domains,int NbDomains,FFSS_LongField User); /* First domain is assumed to be domain from the answering master */
void OnMasterSearchAnswer(struct sockaddr_in Master,FFSS_Field ProtocolVersion,const char Domain[],FFSS_LongField User);

/* TCP callbacks */
bool OnError(SU_PClientSocket Server,FFSS_Field ErrorCode,const char Descr[],FFSS_LongField Value,FFSS_LongField User);
bool OnDirectoryListingAnswer(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries,FFSS_LongField User); /* FC_PEntry */
void OnEndTCPThread(SU_PClientSocket Server);
void OnIdleTimeout(SU_PClientSocket Server);
void OnTransfertFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download);
void OnTransfertSuccess(FFSS_PTransfer FT,bool Download);
void OnTransfertActive(FFSS_PTransfer FT,long int Amount,bool Download);


#endif /* _CLIENT_H_ */
