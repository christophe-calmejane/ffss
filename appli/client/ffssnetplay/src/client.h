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

#define FNP_NAME "FFSSNetPlay"
#define FNP_VERSION "0.3"
#define FNP_PORT 8080
#define FNP_BUFFER_SIZE 65536

/* TEMPO */
#define HAVE_MASTER
#ifdef HAVE_MASTER
#define CLT_MASTER "orion"
#endif

extern GtkWidget *wnd_main;
extern GtkCList *FNP_clist;
extern SU_SEM_HANDLE FNP_Sem;
extern FFSS_LongField FNP_CurrentFilePos;
extern FFSS_LongField FNP_CurrentFileSize;

SU_THREAD_ROUTINE(StreamingRoutine,User);
void FNP_PlayNextFile(bool Lock);
void FNP_ConnectServer(const char IP[],const char Share[]);

/* UDP callbacks */
void OnSearchAnswer(const char Query[],const char Domain[],const char **Answers,char **IPs,int NbAnswers);

/* TCP callbacks */
bool OnError(SU_PClientSocket Server,int Code,const char Descr[],FFSS_LongField Value);
void OnEndTCPThread(SU_PClientSocket Server);
void OnStrmOpenAnswer(SU_PClientSocket Client,const char Path[],int Code,long int Handle,FFSS_LongField FileSize);
void OnStrmReadAnswer(SU_PClientSocket Client,long int Handle,const char Bloc[],long int BlocSize);


#endif /* _CLIENT_H_ */
