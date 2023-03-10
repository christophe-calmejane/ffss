/*
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
#ifndef __MASTER_H__
#define __MASTER_H__

#include <ffss/ffss.h>
#ifdef __unix__
#include <sys/ioctl.h>
#include <net/if.h>
#endif /* __unix__ */

#define FFSS_MASTER_VERSION "1.0-pre65"
#define FM_COMPRESSION_TRIGGER_ZLIB 1000
#define FM_COMPRESSION_TRIGGER_BZLIB 5000
#define FM_TOO_MANY_ANSWERS_TRIGGER 20000

#ifdef DEBUG
#ifdef __unix__
#define STATS
#endif /* __unix__ */
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
#define FM_INDEX_DUMP_INTERVAL_PING 60
#endif /* DEBUG */
#define FM_INDEX_MAX_FATHER_RECURSION 100

/* Debug defines */
  /* Master specific */
#define FM_DBGMSG_GLOBAL      0x00010
#define FM_DBGMSG_STATES      0x00020
#define FM_DBGMSG_SEARCH      0x00040
#define FM_DBGMSG_INDEX       0x00100

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
  bool Removed; /* If removed before emptying queue */
} FM_TQueue, *FM_PQueue;

typedef struct
{
  struct sockaddr_in Client;
  int Port;
  char *Domain;
  char *KeyWords;
  FFSS_BitField Compressions;
  bool Master;
  FFSS_LongField User;
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
extern SU_SEM_HANDLE FM_TmpSem; /* Temporary semaphore */

extern char *FM_User,*FM_Group,*FM_Iface;

SU_THREAD_ROUTINE(FM_ThreadPing,User);
SU_THREAD_ROUTINE(FM_ThreadSearch,User);

bool FM_IsMyDomain(FM_PDomain Dom);
FM_PHost FM_SearchHostByIP(FM_PDomain Domain,const char IP[]);

/* Returns a buffer to be sent then freed, or NULL if queue is empty */
char *FM_BuildStatesBuffer(SU_PList Queue, size_t *size_out);
/* Returns a buffer to be sent then freed */
char *FM_BuildServerListing(const char Domain[], const char OS[], size_t *size_out);

void FM_AddStateToMyQueue(FM_PDomain Domain,FM_PHost Hst);
void FM_AddStateToOtherQueue(FM_PDomain Domain,FM_PHost Hst);
void FM_FreeHost(FM_PHost Hst);

bool FM_LoadConfigFile(const char FileName[],bool UserGroup);

SU_PList FM_LoadHosts(const char FileName[]);
bool FM_SaveHosts(SU_PList Hosts,const char FileName[]);

/* Returns a buffer to be sent then freed, or NULL if request failed */
char *FM_Search(FM_PSearch Sch, size_t *size_out);

void FM_BeginIndexAnswerParse(struct sockaddr_in Client);
/* Must return TRUE on error */
bool FM_IndexAnswerParse(struct sockaddr_in Client, const char Buf[], size_t Len);

void FM_SetHostStateInIndex(const char Host[],FFSS_Field State);

#endif /* !__MASTER_H__ */
