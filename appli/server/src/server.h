#ifndef __SERVER_H__
#define __SERVER_H__

#include <ffss.h>
#ifdef __unix__
#include <sys/ioctl.h>
#include <net/if.h>
#define FS_PLUGIN_EXPORT
#else /* !__unix__ */
#define FS_PLUGIN_EXPORT __declspec(dllexport)
#endif /* __unix__ */

#ifdef USE_CRYPT
#include <crypt.h>
#endif

#define FS_OPCODE_ADDSHARE       1
#define FS_OPCODE_DELSHARE       2
#define FS_OPCODE_GETGLOBAL      3
#define FS_OPCODE_GETSHARE       4
#define FS_OPCODE_UPDTSHARE      5
#define FS_OPCODE_UPDTGLOBAL     6
#define FS_OPCODE_SETSTATE       7
#define FS_OPCODE_GETSTATE       8
#define FS_OPCODE_GETSHRLIST     9
#define FS_OPCODE_RESCAN        10
#define FS_OPCODE_SETSHARESTATE 11
#define FS_OPCODE_EJECT         12
#define FS_OPCODE_GETSHRCONNS   13
#define FS_OPCODE_EJECTIP       14
#define FS_OPCODE_GETNAMEAVAIL  15
#define FS_OPCODE_ACK           20
#define FS_OPCODE_NACK          21
#define FS_OPCODE_PL_LOAD       30
#define FS_OPCODE_PL_UNLOAD     31
#define FS_OPCODE_PL_CONFIGURE  32
#define FS_OPCODE_PL_ENUM       33


#define FS_AVERAGE_FILE_LENGTH 25
#define FS_REC_AVERAGE_FILE_LENGTH 80
#define FS_COMPRESSION_TRIGGER_ZLIB 1400
#define FS_COMPRESSION_TRIGGER_BZLIB 5000

#define FS_ON_DOWNLOAD_MAX_RETRIES 5
#define FS_ON_DOWNLOAD_SLEEP_RETRY 100
#define FS_CHECK_EVERY_X_PING 10

#define FFSS_SERVER_VERSION "1.0-pre83"

#ifdef DEBUG
#define CONFIG_FILE_NAME "./Server.conf"
#else /* !DEBUG */
#define CONFIG_FILE_NAME "/etc/ffss/server.conf"
#endif /* DEBUG */

typedef struct
{
  SU_PList Dirs;      /* FS_PDir */
  FFSS_Field NbDirs;  /* Number of dirs in this node */
  SU_PList Files;     /* FS_PFile */
  FFSS_Field NbFiles; /* Number of files in this node */
} FS_TNode, *FS_PNode;

typedef struct
{
  char *DirName;       /* Name of the directory */
  FFSS_Field Flags;    /* Flags of the dir */
  FFSS_LongField Size; /* Size of the dir */
  time_t Time;         /* Time of the dir */
  FS_TNode Files;      /* Files/Dirs in this dir */
} FS_TDir, *FS_PDir;

typedef struct
{
  char *FileName;      /* Name of the file */
  FFSS_Field Flags;    /* Flags of the file */
  FFSS_LongField Size; /* Size of the file */
  time_t Time;         /* Time of the file */
} FS_TFile, *FS_PFile;

typedef struct
{
  char *Login;     /* The login */
  char *Password;  /* The password */
  bool Writeable;  /* If the share is writeable for this user */
} FS_TUser, *FS_PUser;

typedef struct
{
  char *Path;                  /* Path of the share on disk */
  char *ShareName;             /* Name of the share */
  char *Comment;               /* Comment of the share */
  bool Writeable;              /* If the share is writeable */
  bool Private;                /* If we need to be a REGISTERED user (login/pwd) */
  unsigned int MaxConnections; /* Max simultaneous connections to this share */
  SU_PList Users;              /* FS_PUser */

  FS_TNode Root;               /* Files/Dirs in the root dir of the share */
  FFSS_Field NbFiles;          /* Total number of files in the share */
  FFSS_Field NbDirs;           /* Total number of directories in the share */
  time_t Time;                 /* Time of Shared directory (Path/.) */
  SU_PList Conns;              /* FS_PConn */
  bool Disabled;               /* Temporarly disable the share */
  bool Remove;                 /* If the share is to be removed (close all active connections) */
#ifdef _WIN32
  HANDLE NotifyHandle;         /* Notification handle */
#endif /* _WIN32 */
} FS_TShare, *FS_PShare;

typedef struct
{
  /* Shared variables */
  bool Writeable;
  FS_PShare Share;                 /* DO NOT FREE THIS, ONLY A POINTER TO THE REAL SHARE */
  SU_PClientSocket Client;         /* SU_PClientSocket structure of the share connection */ /* Do NOT free this, only a pointer !! */

  /* FFSS variables */
  char *ShareName;                 /* NULL if FTP connection */
  bool Remove;                     /* If this connection is to be removed */
  FFSS_Field Comps;                /* Supported compressions */

  /* FTP variables */
  char Path[FFSS_MAX_PATH_LENGTH]; /* Only used for FTP connections - Current path */
  char Type;
} FS_TThreadSpecific, *FS_PThreadSpecific;

typedef struct
{
  char *Remote;          /* Remote ip of connection */
  FS_PUser User;         /* DO NOT FREE THIS, ONLY A POINTER TO THE REAL USER */
  FS_PThreadSpecific ts; /* DO NOT FREE THIS, ONLY A POINTER TO THE REAL TS */
  FS_PShare Share;       /* DO NOT FREE THIS, ONLY A POINTER TO THE REAL SHARE */
  SU_PList XFers;        /* FFSS_PTransfer */ /* DO NOT FREE THESE, ONLY A POINTER TO THE REAL XFERS */
  bool XFerInConn;       /* If Xfer in connection socket (once in this mode... can't go back until share reconnection) */
  int CurrentXFer;       /* Only used for xfer in connection socket */
  char *TransferBuffer;  /* Only used for xfer in connection socket */
  SU_PList Strms;        /* FS_PStreaming */
  bool ToRemove;         /* Must be remove when last xfer is done */
} FS_TConn, *FS_PConn;

typedef struct
{
  char *Name;               /* Name of my server */
  char *Comment;            /* Comment of my server */
  char *Master;             /* Host name of my master */
  char *MasterIP;           /* IP of my master */
  char *MyIP;               /* My IP */
  unsigned int Idle;        /* Idle time allowed on a share */
  unsigned int MaxConn;     /* Global Max connections (except FTP) */
  unsigned int Conn;        /* Number of current global connections (except FTP) */
  unsigned int MaxXFerPerConn; /* Global Max XFer per connection */
  unsigned int FTPMaxConn;  /* FTP Max connections */
  unsigned int FTPConn;     /* Number of current FTP connections */
  bool  FTP;                /* FTP compatibility */
  bool  ConfSock;           /* Using a local socket for realtime configuration */
  bool  XFerInConn ;        /* If xfer are sent using connection socket */
  bool  LimitedBind;        /* If limited bind mode is active */
} FS_TGlobal, *FS_PGlobal;

typedef struct
{
  SU_SOCKET  sock;            /* Socket for commands */
  FILE *fp;                   /* Opened file for reading/writing */
  char *LocalPath;            /* Local path of file used for fopen */
  FFSS_LongField StartingPos; /* Reading/Writing starting pos in the file */
  SU_PClientSocket Client;    /* SU_PClientSocket structure for file transfer */
} FS_TTransferFTP, *FS_PTransferFTP;

typedef struct
{
  FILE *fp;
  char *FileName;
  FFSS_LongField Position;
  FFSS_Field Handle;
  FFSS_LongField fsize;
} FS_TStreaming, *FS_PStreaming;

typedef struct /* Allocated by Plugin / Freed by Server */
{
  FFSS_Field size; /* Size of this structure... must be set by plugin */
  char *Path;      /* Set and freed by server */
  char *Name;      /* Set and freed by plugin */
  char *Copyright; /* Set and freed by plugin */
  char *Version;   /* Set and freed by plugin */
  bool Startup;    /* If plugin is loaded at server startup */
  SU_DL_HANDLE Handle;
  FFSS_TServerCallbacks CB;
  bool (*OnCheckConfConn)(SU_PClientSocket Client);
} FS_TPlugin, *FS_PPlugin;

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Order of Semaphore locking */
extern SU_SEM_HANDLE FS_SemGbl;   /* Semaphore to protect the use of MyGlobal */
extern SU_SEM_HANDLE FS_SemShr;   /* Semaphore to protect the use of FS_Index and all sub structs */
extern SU_SEM_HANDLE FS_SemPlugin;/* Semaphore to protect the use of FS_Plugins */
extern SU_THREAD_KEY_HANDLE FS_tskey;
extern SU_THREAD_ONCE_HANDLE FS_once;

extern FS_TGlobal FS_MyGlobal;
extern SU_PList FS_Index; /* FS_PShare */
extern char *FS_MyDomain;
extern char *FS_MyIntName;
extern int FS_MyState;
extern char *FS_TimeTable[];
extern SU_PList FS_Plugins; /* FS_PPlugin */

/* Locks FS_SemShr */
void FS_BuildIndex(const char Path[],const char ShareName[],const char ShareComment[],bool Writeable,bool Private,int MaxConnections,SU_PList Users,bool do_it_now);
/* Locks FS_SemShr */
void FS_RealBuildIndex(void);
void FS_FreeUser(FS_PUser Usr);
/* Assumes FS_SemShr is locked */
void FS_FreeShare(FS_PShare Share);
/* Locks FS_SemShr */
void FS_FreeIndex(void);
/* Assumes FS_SemShr is locked */
void FS_RescanShare(FS_PShare Share);
/* Assumes FS_SemShr is locked */
/* Returns a buffer to be sent then freed (its size in size_out), or NULL if the path is incorrect */
char *FS_BuildDirectoryBuffer(FS_PShare Share,const char Dir[],long int *size_out);
/* Assumes FS_SemShr is locked */
/* Returns a buffer to be sent then freed, or NULL if the path is incorrect */
char *FS_BuildRecursiveDirectoryBuffer(FS_PShare Share,const char Dir[],long int *size_out);
/* Locks FS_SemShr */
bool FS_SendIndex(const char Host[],const char Port[]);
/* Assumes FS_SemShr is locked */
bool FS_CaseFilePath(FS_PShare Share,char Path[]);

/* Assumes FS_SemGbl is locked */
char *FS_CheckGlobal(void);
bool FS_ShutDown();

/* Assumes FS_SemShr is locked */
FS_PShare FS_GetShareFromName(const char Name[]);
/* Assumes FS_SemShr is locked */
FS_PShare FS_GetShareFromPath(const char Path[]);
/* Locks FS_SemShr */
void FS_EjectAll(bool EjectXFers);
/* Assumes FS_SemShr is locked */
void FS_EjectFromShare(FS_PShare Share,bool EjectXFers);
/* Assumes FS_SemShr is locked */
void FS_EjectFromShareByIP(FS_PShare Share,const char IP[],bool EjectXFers);
/* Locks FS_SemPlugin */
FS_PPlugin FS_LoadPlugin(const char Name[]);
/* Locks FS_SemPlugin */
void FS_UnLoadPlugin(SU_DL_HANDLE Handle);
/* Locks FS_SemPlugin */
void FS_UnLoadAllPlugin(void);
bool FS_ConfigurePlugin(SU_DL_HANDLE Handle);
/* Locks FS_SemPlugin */
bool FS_IsPluginValid(FS_PPlugin Plugin);

/* Functions from arch dependant file */
bool FS_IsAlreadyRunning(void);
bool FS_LoadConfig(const char FileName[]);
/* Locks FS_SemShr & FS_SemGbl */
bool FS_SaveConfig(const char FileName[]);
void FS_MainThread(void);
/* Assumes FS_SemShr is locked */
void FS_RemoveShare(FS_PShare Share);
/* Assumes FS_SemShr is locked */
bool FS_CheckDirectoryChanged(FS_PShare Share);
void FS_AddPluginToStartup(FS_PPlugin Plugin);
void FS_RemovePluginFromStartup(FS_PPlugin Plugin);


extern FFSS_Field FS_CurrentXFerTag;
bool FS_InitXFerUpload(SU_PClientSocket Client,FFSS_PTransfer FT,const char Path[],bool Download);
bool FS_TransferBloc(FFSS_PTransfer FT,FS_PConn Conn); /* False on END OF TRANSFER */


/* Included from MASTER's index.h */

typedef struct /* 16 bytes */
{
  int Pos;                     /* Pos is the offset in 'char *FileTree' for the string of this node        */ /* -1 if not used */
  int Father;                  /* Father is the index in 'FM_TFTNode *FTNodes' for the father of this node */ /* -1 if no father */
  int Last;                    /* Last index in host's nodes table for this directory                      */
  unsigned char Tags;          /* Bit field for tags of file                                               */
} FM_TFTNode, *FM_PFTNode;


/* End included */

/*#undef SU_SEM_WAIT
#undef SU_SEM_POST
#define SU_SEM_WAIT(x) { printf("Locking %d (%s:%d)\n",x,__FILE__,__LINE__);WaitForSingleObject(x,INFINITE);printf("Got %d\n",x); }
#define SU_SEM_POST(x) { printf("Releasing %d (%s:%d)\n",x,__FILE__,__LINE__);ReleaseSemaphore(x,1,NULL); }*/

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* !__SERVER_H__ */
