#ifndef __SERVER_H__
#define __SERVER_H__

#ifdef __unix__
#include <ffss.h>
#include <sys/ioctl.h>
#include <net/if.h>
#else /* !__unix__ */
#include "../ffss.h"
#include "../utils.h"
#include "../transfer.h"
#endif /* __unix__ */

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
#define FS_OPCODE_ACK           20
#define FS_OPCODE_NACK          21

#define FS_AVERAGE_FILE_LENGTH 25
#define FS_COMPRESSION_TRIGGER_ZLIB 1400
#define FS_COMPRESSION_TRIGGER_BZLIB 5000

#define FS_ON_DOWNLOAD_MAX_RETRIES 5
#define FS_ON_DOWNLOAD_SLEEP_RETRY 100

#define FFSS_SERVER_VERSION "1.0-pre72"

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
  char *Path;         /* Path of the share on disk */
  char *ShareName;    /* Name of the share */
  char *Comment;      /* Comment of the share */
  bool Writeable;     /* If the share is writeable */
  bool Private;       /* If we need to be a REGISTERED user (login/pwd) */
  int MaxConnections; /* Max simultaneous connections to this share */
  SU_PList Users;     /* FS_PUser */

  FS_TNode Root;      /* Files/Dirs in the root dir of the share */
  FFSS_Field NbFiles; /* Total number of files in the share */
  FFSS_Field NbDirs;  /* Total number of directories in the share */
  SU_PList Conns;     /* FS_PConn */
  bool Disabled;      /* Temporarly disable the share */
  bool Remove;        /* If the share is to be removed (close all active connections) */
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
  FS_PUser User;         /* DO NOT FREE THIS, ONLY A POINTER TO THE REAL SHARE */
  FS_PThreadSpecific ts; /* DO NOT FREE THIS, ONLY A POINTER TO THE REAL SHARE */
  SU_PList XFers;        /* FFSS_PTransfer */ /* DO NOT FREE THESE, ONLY A POINTER TO THE REAL XFERS */
  int CurrentXFer;       /* Only used for xfer in connection socket */
  char *TransferBuffer;  /* Only used for xfer in connection socket */
  SU_PList Strms;        /* FS_PStreaming */
} FS_TConn, *FS_PConn;

typedef struct
{
  char *Name;        /* Name of my server */
  char *Comment;     /* Comment of my server */
  char *Master;      /* Host name of my master */
  char *MasterIP;    /* IP of my master */
  char *MyIP;        /* My IP */
  int   Idle;        /* Idle time allowed on a share */
  int   MaxConn;     /* Global Max connections (except FTP) */
  int   Conn;        /* Number of current global connections (except FTP) */
  int   MaxXFerPerConn; /* Global Max XFer per connection */
  int   FTPMaxConn ; /* FTP Max connections */
  int   FTPConn;     /* Number of current FTP connections */
  bool  FTP;         /* FTP compatibility */
  bool  ConfSock;    /* Using a local socket for realtime configuration */
  bool  XFerInConn ; /* If xfer are sent using connection socket */
  bool  LimitedBind; /* If limited bind mode is active */
} FS_TGlobal, *FS_PGlobal;

typedef struct
{
  int  sock;                  /* Socket for commands */
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

typedef struct
{
  char *Name;
  void *Handle;
  FFSS_TServerCallbacks CB;
  bool (*OnCheckConfConn)(SU_PClientSocket Client);
} FS_TPlugin, *FS_PPlugin;

extern SU_SEM_HANDLE FS_SemConn;  /* Semaphore to protect the use of Conns in a FS_PShare */
extern SU_SEM_HANDLE FS_SemGbl;   /* Semaphore to protect the use of MyGlobal */
extern SU_SEM_HANDLE FS_SemShr;   /* Semaphore to protect the use of FS_Index */
extern SU_SEM_HANDLE FS_SemXFer;  /* Semaphore to protect the use of FFSS_PTransfer */
extern SU_THREAD_KEY_HANDLE FS_tskey;
extern SU_THREAD_ONCE_HANDLE FS_once;

extern FS_TGlobal FS_MyGlobal;
extern SU_PList FS_Index; /* FS_PShare */
extern char *FS_MyDomain;
extern char *FS_MyIntName;
extern int FS_MyState;
extern char *FS_TimeTable[];
extern SU_PList FS_Plugins;

void FS_RealBuildIndex(void);
void FS_BuildIndex(const char Path[],const char ShareName[],const char ShareComment[],bool Writeable,bool Private,int MaxConnections,SU_PList Users,bool do_it_now);
void FS_FreeShare(FS_PShare Share);
void FS_FreeIndex(void);
void FS_RescanShare(FS_PShare Share);
/* Returns a buffer to be sent then freed (its size in size_out), or NULL if the path is incorrect */
char *FS_BuildDirectoryBuffer(FS_PShare Share,const char Dir[],long int *size_out);
bool FS_SendIndex(const char Host[],const char Port[]);
bool FS_CaseFilePath(FS_PShare Share,char Path[]);

char *FS_CheckGlobal(void);

FS_PShare FS_GetShareFromName(const char Name[]);
FS_PShare FS_GetShareFromPath(const char Path[]);
void FS_EjectFromShare(FS_PShare Share,bool EjectXFers);
void FS_EjectFromShareByIP(FS_PShare Share,const char IP[],bool EjectXFers);

/* Functions from arch dependant file */
bool FS_LoadConfig(const char FileName[]);
bool FS_SaveConfig(const char FileName[]);
void FS_MainThread(void);
void FS_RemoveShare(FS_PShare Share);
bool FS_LoadPlugin(const char Name[]);
void FS_UnLoadPlugin(void *Handle);

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


#endif /* __SERVER_H__ */
