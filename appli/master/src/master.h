#ifndef __MASTER_H__
#define __MASTER_H__

#ifdef __unix__
#include <ffss.h>
#include <sys/ioctl.h>
#include <net/if.h>
#else /* __unix__ */
#include "../ffss.h"
#include "../utils.h"
#endif /* __unix__ */

#define FFSS_MASTER_VERSION "1.0-pre51"
#define FM_COMPRESSION_TRIGGER_ZLIB 1000
#define FM_COMPRESSION_TRIGGER_BZLIB 5000
#define FM_TOO_MANY_ANSWERS_TRIGGER 20000

#ifdef DEBUG
#define FM_MYHOSTS_FILE "Hosts.dat"
#define FM_MYINDEX_FILE "Index.dat"
#define CONFIG_FILE_NAME "./Master.conf"
#define FM_SEARCHLOG_FILE "Search.log"
#define FM_PID_FILE "./Master.pid"
#define FM_INDEX_DUMP_INTERVAL_PING 0
#else /* !DEBUG */
#define FM_MYHOSTS_FILE "/var/lib/ffss/hosts.dat"
#define FM_MYINDEX_FILE "/var/lib/ffss/index.dat"
#define CONFIG_FILE_NAME "/etc/ffss/master.conf"
#define FM_SEARCHLOG_FILE "/var/lib/ffss/search.log"
#define FM_PID_FILE "/var/lib/ffss/master.pid"
#define FM_INDEX_DUMP_INTERVAL_PING 30
#endif /* DEBUG */
#define FM_INDEX_MAX_FATHER_RECURSION 100

typedef struct
{
  char *Name;          /* Name of the domain */
  char *Master;        /* Address of the master of the domain */
  SU_PList Hosts;      /* FM_PHost */ /* Hosts of the domain */
  SU_PClientSocket CS; /* NULL if not connected */
} FM_TDomain, *FM_PDomain;

typedef struct
{ /* Do NOT free those pointeurs !! */
  FM_PDomain Domain;
  FM_PHost Host;
} FM_TQueue, *FM_PQueue;

typedef struct
{
  struct sockaddr_in Client;
  int Port;
  char *Domain;
  char *KeyWords;
  long int Compressions;
  bool Master;
} FM_TSearch, *FM_PSearch;


extern FM_TDomain FM_MyDomain;
extern SU_PList FM_Domains; /* FM_PDomain */
extern SU_PList FM_MyQueue; /* FM_PQueue */
extern SU_PList FM_OtherQueue; /* FM_PQueue */
extern SU_PList FM_SearchQueue; /* FM_PSearch */
extern FILE *FM_SearchLogFile;
extern SU_SEM_HANDLE FM_MySem;  /* Semaphore to protect the use of FM_MyQueue */
extern SU_SEM_HANDLE FM_MySem2; /* Semaphore to protect the use of the Hosts of FM_MyDomain */
extern SU_SEM_HANDLE FM_MySem3; /* Semaphore to protect the use of FM_OtherQueue */
extern SU_SEM_HANDLE FM_MySem4; /* Semaphore to protect the use of FM_SearchQueue */
extern SU_SEM_HANDLE FM_MySem5; /* Semaphore to protect the use of the index */

extern char *FM_User,*FM_Group;

SU_THREAD_ROUTINE(FM_ThreadPing,User);
SU_THREAD_ROUTINE(FM_ThreadQueue,User);
SU_THREAD_ROUTINE(FM_ThreadSearch,User);

FM_PHost FM_SearchHostByIP(FM_PDomain Domain,const char IP[]);

/* Returns a buffer to be sent then freed, or NULL if queue is empty */
char *FM_BuildStatesBuffer(SU_PList Queue,long int *size_out);
/* Returns a buffer to be sent then freed */
char *FM_BuildServerListing(const char Domain[],const char OS[],long int *size_out);

void FM_AddStateToMyQueue(FM_PDomain Domain,FM_PHost Hst);
void FM_AddStateToOtherQueue(FM_PDomain Domain,FM_PHost Hst);
void FM_FreeHost(FM_PHost Hst);

bool FM_LoadConfigFile(const char FileName[],bool UserGroup);

SU_PList FM_LoadHosts(const char FileName[]);
bool FM_SaveHosts(SU_PList Hosts,const char FileName[]);

/* Returns a buffer to be sent then freed, or NULL if request failed */
char *FM_Search(FM_PSearch Sch,long int *size_out);

void FM_BeginIndexAnswerParse(struct sockaddr_in Client);
/* Must return TRUE on error */
bool FM_IndexAnswerParse(struct sockaddr_in Client,const char Buf[],long int Len);

void FM_SetHostStateInIndex(const char Host[],FFSS_Field State);

#endif /* !__MASTER_H__ */
