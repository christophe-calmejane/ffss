#ifndef _CLIENT_H_
#define _CLIENT_H_

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <ffss.h>
#include <assert.h>

#ifdef __unix__
#define MKDIR(x) mkdir(x,493)
#else /* __unix__ */
#define MKDIR(x) mkdir(x)
#endif /* __unix__ */

/* TEMPO */
#define HAVE_MASTER
#ifdef HAVE_MASTER
#define CLT_MASTER "ffss"
#endif

typedef struct
{
  GtkWidget *wnd;           /* Window managing this connection     */
  gchar *host_share;        /* Host and share name                 */
  gchar *path;              /* Current browsing path in the server */
  SU_PClientSocket Server;  /* Connection with Server              */
  bool Active;              /* Says if Server is valid or not      */
  guint ctx_id;             /* Context id for statusbar            */
  GtkStatusbar *sb;         /* Shortcut to statusbar               */
  GtkCList *list;           /* Shortcut to clist                   */
  int ref_count;            /* Number of usage of this structure   */
  bool recursif;            /* If recursive listing is in progress */
  int rec_dir_count;        /* Number of directories to list       */
  gchar *rec_path;
  gchar *rec_local_path;
} TConn, *PConn;

void add_conn_count(PConn Conn);
bool remove_conn(PConn Conn,bool clear_wnd);
PConn lookup_conn(SU_PClientSocket Server,bool even_null_wnd);
void launch_next_download(void);
void add_file_to_download(PConn Conn,char *remote,char *local,char *size,bool lock);
void move_file_to_failed(char *host_share,char *remote,char *local,char *size,char *error);

/* UDP callbacks */
void OnNewState(long int State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[]);
void OnSharesListing(const char IP[],const char **Names,const char **Comments,int NbShares);
void OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList);
void OnEndServerListingAnswer(void);
void OnDomainListingAnswer(const char **Domains,int NbDomains);
void OnMasterSearchAnswer(struct sockaddr_in Master,FFSS_Field MasterVersion,const char Domain[]);
bool OnError(SU_PClientSocket Server,int Code,const char Descr[]);
bool OnDirectoryListingAnswer(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries);

/* TCP callbacks */
bool OnError(SU_PClientSocket Server,int Code,const char Descr[]);
bool OnDirectoryListingAnswer(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries);
void OnEndTCPThread(SU_PClientSocket Server);
void OnIdleTimeout(SU_PClientSocket Server);
void OnTransfertFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download);
void OnTransfertSuccess(FFSS_PTransfer FT,bool Download);
void OnTransfertActive(FFSS_PTransfer FT,long int Amount,bool Download);


#endif /* _CLIENT_H_ */