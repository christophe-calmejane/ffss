#ifndef __FFSS_H__
#define __FFSS_H__

#ifdef __linux__
#define FFSS_SERVER_OS "Linux"
#elif __FreeBSD__
#define FFSS_SERVER_OS "FreeBSD"
#define __BSD__
#elif __NetBSD__
#define FFSS_SERVER_OS "NetBSD"
#define __BSD__
#elif __OpenBSD__
#define FFSS_SERVER_OS "OpenBSD"
#define __BSD__
#elif _WIN32
#define FFSS_SERVER_OS "Win32"
#elif __MACH__
#define FFSS_SERVER_OS "MacOS"
#define __unix__
#elif __CYGWIN32__
#define FFSS_SERVER_OS "CygWin"
#else
#error "Unknown OS... contact devel team"
#endif /* __linux__ */

#ifdef _WIN32
#define HAVE_BZLIB 1
#endif /* _WIN32 */

#include <skyutils.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#ifndef NO_ZLIB_INCLUDE
#include <zlib.h>
#endif /* NO_ZLIB_INCLUDE */
#ifdef HAVE_BZLIB
#include <bzlib.h>
#endif /* HAVE_BZLIB */

#ifdef _WIN32
#include <time.h>
#define strtok_r(a,b,c) strtok(a,b)
#define LOG_INFO    2
#define LOG_WARNING 1
#define LOG_ERR     0
#else /* !_WIN32 */
#include <sys/time.h>
#include <unistd.h>
#include <syslog.h>
#endif /* _WIN32 */

#define FFSS_VERSION "1.0.0-pre73"
#define FFSS_COPYRIGHT "FFSS library v" FFSS_VERSION " (c) Ze KiLleR / SkyTech 2001'02"
#define FFSS_FTP_SERVER "FFSS FTP compatibility v" FFSS_VERSION

#define FFSS_PROTOCOL_VERSION                  0x0010002
#define FFSS_PROTOCOL_VERSION_LEAST_COMPATIBLE 0x0010002

#define FFSS_MASTER_PORT 10001
#define FFSS_SERVER_PORT 10002
#define FFSS_SERVER_FTP_PORT 10003
#define FFSS_SERVER_CONF_PORT 10004
#define FFSS_SERVER_CONF_PORT_S "10004"
#define FFSS_MASTER_PORT_S "10001"
#define FFSS_SERVER_PORT_S "10002"

/*#define FFSS_MASTER_PORT 695
#define FFSS_SERVER_PORT 696
#define FFSS_SERVER_FTP_PORT 697
#define FFSS_SERVER_CONF_PORT 698
#define FFSS_SERVER_CONF_PORT_S "698"
#define FFSS_MASTER_PORT_S "695"
#define FFSS_SERVER_PORT_S "696"*/

#define FFSS_IP_V4 1
#define FFSS_IP_V6 2
#define FFSS_IP_FIELD_SIZE 16

#define FFSS_IP_TYPE FFSS_IP_V4

#define FFSS_MAX_SERVERNAME_LENGTH 15
#define FFSS_MAX_SERVEROS_LENGTH 15
#define FFSS_MAX_SERVERCOMMENT_LENGTH 50
#define FFSS_MAX_DOMAIN_LENGTH 20
#define FFSS_MAX_SHARENAME_LENGTH 20
#define FFSS_MAX_SHARECOMMENT_LENGTH 50
#define FFSS_MAX_ERRORMSG_LENGTH 100
#define FFSS_MAX_LOGIN_LENGTH 20
#define FFSS_MAX_PASSWORD_LENGTH 20
#define FFSS_MAX_PATH_LENGTH 1024
#define FFSS_MAX_FILEPATH_LENGTH 1024
#define FFSS_MAX_KEYWORDS_LENGTH 2048
#ifdef __BSD__
#define FFSS_MAX_UDP_PACKET_LENGTH 4000
#else /* !__BSD__ */
#define FFSS_MAX_UDP_PACKET_LENGTH 60000
#endif /* __BSD__ */
#define FFSS_MIN_SEARCH_REQUEST_LENGTH 4
#define FFSS_UDP_CLIENT_BUFFER_SIZE 500000
#define FFSS_UDP_SERVER_BUFFER_SIZE 100000
#define FFSS_UDP_MASTER_BUFFER_SIZE 100000
#define FFSS_TCP_CLIENT_BUFFER_SIZE 100000
#define FFSS_TCP_SERVER_BUFFER_SIZE 60000
#ifdef _WIN32
#define FFSS_TRANSFER_BUFFER_SIZE 1460
#else /* !_WIN32 */
#ifdef __BSD__
#define FFSS_TRANSFER_BUFFER_SIZE 1460
#else /* !__BSD__ */
#define FFSS_TRANSFER_BUFFER_SIZE 1480
#endif /* __BSD__ */
#endif /* _WIN32 */
#define FFSS_STREAMING_BUFFER_SIZE 65536
#define FFSS_TRANSFER_READ_BUFFER_SIZE 65536
#define FFSS_TIMEOUT_INDEX_ACCEPT 5
#define FFSS_TIMEOUT_INDEX_XFER 60
#define FFSS_TIMEOUT_ACCEPT 10
#define FFSS_TIMEOUT_UDP_LOCK 15
#define FFSS_TIMEOUT_TRANSFER 60
#define FFSS_TIMEOUT_UDP_MESSAGE 5
#define FFSS_TIMEOUT_TCP_MESSAGE 15
#define FFSS_TIMEOUT_FTP 60
#define FFSS_PING_INTERVAL 90
#define FFSS_PING_TIMEOUT FFSS_PING_INTERVAL*2
#define FFSS_INDEX_INTERVAL 60*10               /* 10 min */
#define FFSS_KEEP_HOST_DELAY 60*60*24*7         /* 7 days */
#define FFSS_STATE_BROADCAST_INTERVAL 120
#define FFSS_UDP_MAX_ERRORS 20
#define FFSS_DEFAULT_MAX_CONN 10
#define FFSS_DEFAULT_MAX_XFER_PER_CONN 2

#define FFSS_MESSAGE_STATE                      1
#define FFSS_MESSAGE_STATE_ANSWER               2
#define FFSS_MESSAGE_NEW_STATES                 3
#define FFSS_MESSAGE_INDEX_REQUEST              4
#define FFSS_MESSAGE_INDEX_ANSWER               5
#define FFSS_MESSAGE_SERVER_LISTING             6
#define FFSS_MESSAGE_SERVER_LISTING_ANSWER      7
#define FFSS_MESSAGE_CLIENT_SERVER_FAILED       8
#define FFSS_MESSAGE_PING                       9
#define FFSS_MESSAGE_PONG                      10
#define FFSS_MESSAGE_SERVER_SEARCH             11
#define FFSS_MESSAGE_SHARES_LISTING            12
#define FFSS_MESSAGE_SHARES_LISTING_ANSWER     13
#define FFSS_MESSAGE_SHARE_CONNECTION          14
#define FFSS_MESSAGE_DIRECTORY_LISTING         15
#define FFSS_MESSAGE_DIRECTORY_LISTING_ANSWER  16
#define FFSS_MESSAGE_DOWNLOAD                  17
#define FFSS_MESSAGE_UPLOAD                    18
#define FFSS_MESSAGE_MOVE                      19
#define FFSS_MESSAGE_COPY                      20
#define FFSS_MESSAGE_DELETE                    21
#define FFSS_MESSAGE_MKDIR                     22
#define FFSS_MESSAGE_DOMAINS_LISTING           30
#define FFSS_MESSAGE_DOMAINS_LISTING_ANSWER    31
#define FFSS_MESSAGE_SEARCH                    50
#define FFSS_MESSAGE_SEARCH_ANSWER             51
#define FFSS_MESSAGE_SEARCH_FW                 52
#define FFSS_MESSAGE_SEARCH_MASTER             55
#define FFSS_MESSAGE_SEARCH_MASTER_ANSWER      56
#define FFSS_MESSAGE_INDEX_ANSWER_SAMBA        57
#define FFSS_MESSAGE_STREAMING_OPEN            80
#define FFSS_MESSAGE_STREAMING_OPEN_ANSWER     81
#define FFSS_MESSAGE_STREAMING_CLOSE           82
#define FFSS_MESSAGE_STREAMING_READ            83
#define FFSS_MESSAGE_STREAMING_READ_ANSWER     84
#define FFSS_MESSAGE_STREAMING_WRITE           85
#define FFSS_MESSAGE_STREAMING_WRITE_ANSWER    86
#define FFSS_MESSAGE_STREAMING_SEEK            87
#define FFSS_MESSAGE_INIT_XFER                 97
#define FFSS_MESSAGE_CANCEL_XFER               98
#define FFSS_MESSAGE_DATA                      99
#define FFSS_MESSAGE_DISCONNECT               100
#define FFSS_MESSAGE_ERROR                    200
#define FFSS_MESSAGE_GATEWAY                 1000

#define FFSS_MESSAGESIZE_STATE                      4
#define FFSS_MESSAGESIZE_STATE_ANSWER               2
#define FFSS_MESSAGESIZE_NEW_STATES                 3
#define FFSS_MESSAGESIZE_NEW_STATES_2               3
#define FFSS_MESSAGESIZE_INDEX_REQUEST              3
#define FFSS_MESSAGESIZE_INDEX_ANSWER               7
#define FFSS_MESSAGESIZE_INDEX_ANSWER_2             5
#define FFSS_MESSAGESIZE_SERVER_LISTING             3
#define FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER      3
#define FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER_2    1
#define FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER_3    2
#define FFSS_MESSAGESIZE_CLIENT_SERVER_FAILED       3
#define FFSS_MESSAGESIZE_PING                       2
#define FFSS_MESSAGESIZE_PONG                       3
#define FFSS_MESSAGESIZE_SERVER_SEARCH              2
#define FFSS_MESSAGESIZE_SHARES_LISTING             2
#define FFSS_MESSAGESIZE_SHARES_LISTING_ANSWER      4
#define FFSS_MESSAGESIZE_SHARE_CONNECTION           4
#define FFSS_MESSAGESIZE_DIRECTORY_LISTING          2
#define FFSS_MESSAGESIZE_DIRECTORY_LISTING_ANSWER   4
#define FFSS_MESSAGESIZE_DIRECTORY_LISTING_ANSWER_2 3
#define FFSS_MESSAGESIZE_DOWNLOAD                   (3+1*2)
#define FFSS_MESSAGESIZE_UPLOAD                     (3+1*2)
#define FFSS_MESSAGESIZE_MOVE                       2
#define FFSS_MESSAGESIZE_COPY                       2
#define FFSS_MESSAGESIZE_DELETE                     2
#define FFSS_MESSAGESIZE_MKDIR                      2
#define FFSS_MESSAGESIZE_DOMAINS_LISTING            2
#define FFSS_MESSAGESIZE_DOMAINS_LISTING_ANSWER     3
#define FFSS_MESSAGESIZE_SEARCH                     4
#define FFSS_MESSAGESIZE_SEARCH_ANSWER              3
#define FFSS_MESSAGESIZE_SEARCH_FW                  5
#define FFSS_MESSAGESIZE_SEARCH_MASTER              3
#define FFSS_MESSAGESIZE_SEARCH_MASTER_ANSWER       3
#define FFSS_MESSAGESIZE_INDEX_ANSWER_SAMBA         7
#define FFSS_MESSAGESIZE_INDEX_ANSWER_SAMBA_2       5
#define FFSS_MESSAGESIZE_STREAMING_OPEN             3
#define FFSS_MESSAGESIZE_STREAMING_OPEN_ANSWER      (4+1*2)
#define FFSS_MESSAGESIZE_STREAMING_CLOSE            3
#define FFSS_MESSAGESIZE_STREAMING_READ             (4+1*2)
#define FFSS_MESSAGESIZE_STREAMING_READ_ANSWER      3
#define FFSS_MESSAGESIZE_STREAMING_WRITE            (3+1*2)
#define FFSS_MESSAGESIZE_STREAMING_WRITE_ANSWER     4
#define FFSS_MESSAGESIZE_STREAMING_SEEK             (4+1*2)
#define FFSS_MESSAGESIZE_INIT_XFER                  3
#define FFSS_MESSAGESIZE_CANCEL_XFER                3
#define FFSS_MESSAGESIZE_DATA                       3
#define FFSS_MESSAGESIZE_DISCONNECT                 2
#define FFSS_MESSAGESIZE_ERROR                      3

#define FFSS_STATE_ON     1
#define FFSS_STATE_OFF    2
#define FFSS_STATE_QUIET  4
#define FFSS_STATE_ALL    7

#define FFSS_SEEK_SET  1
#define FFSS_SEEK_CUR  2
#define FFSS_SEEK_END  3

#define FFSS_STRM_OPEN_READ    1
#define FFSS_STRM_OPEN_WRITE   2
#define FFSS_STRM_OPEN_TEXT    4
#define FFSS_STRM_OPEN_BINARY  8

#define FFSS_THREAD_SERVER 1
#define FFSS_THREAD_CLIENT 2
#define FFSS_THREAD_MASTER 3

#define FFSS_ERROR_PROTOCOL_VERSION_ERROR     1
#define FFSS_ERROR_RESOURCE_NOT_AVAIL         2
#define FFSS_ERROR_NEED_LOGIN_PASS            3
#define FFSS_ERROR_TOO_MANY_CONNECTIONS       4
#define FFSS_ERROR_FILE_NOT_FOUND             5
#define FFSS_ERROR_ACCESS_DENIED              6
#define FFSS_ERROR_NOT_ENOUGH_SPACE           7
#define FFSS_ERROR_CANNOT_CONNECT             8
#define FFSS_ERROR_INTERNAL_ERROR             9
#define FFSS_ERROR_TOO_MANY_TRANSFERS        10
#define FFSS_ERROR_DIRECTORY_NOT_EMPTY       11
#define FFSS_ERROR_FILE_ALREADY_EXISTS       12
#define FFSS_ERROR_IDLE_TIMEOUT              13
#define FFSS_ERROR_SERVER_IS_QUIET           14
#define FFSS_ERROR_SHARE_DISABLED            15
#define FFSS_ERROR_SHARE_EJECTED             16
#define FFSS_ERROR_BUFFER_OVERFLOW           17
#define FFSS_ERROR_XFER_MODE_NOT_SUPPORTED   18
#define FFSS_ERROR_RESEND_LAST_UDP           19
#define FFSS_ERROR_BAD_SEARCH_REQUEST        20
#define FFSS_ERROR_TOO_MANY_ANSWERS          21
#define FFSS_ERROR_NOT_IMPLEMENTED          100
#define FFSS_ERROR_NO_ERROR                 666

#define FFSS_ERROR_TRANSFER_MALLOC       1
#define FFSS_ERROR_TRANSFER_TIMEOUT      2
#define FFSS_ERROR_TRANSFER_SEND         3
#define FFSS_ERROR_TRANSFER_EOF          4
#define FFSS_ERROR_TRANSFER_READ_FILE    5
#define FFSS_ERROR_TRANSFER_ACCEPT       6
#define FFSS_ERROR_TRANSFER_OPENING      7
#define FFSS_ERROR_TRANSFER_RECV         8
#define FFSS_ERROR_TRANSFER_WRITE_FILE   9
#define FFSS_ERROR_TRANSFER_FILE_BIGGER 10
#define FFSS_ERROR_TRANSFER_CHECKSUM    11
#define FFSS_ERROR_TRANSFER_CANCELED    12

#define FFSS_FILE_DIRECTORY  1
#define FFSS_FILE_EXECUTABLE 2
#define FFSS_FILE_LINK       4

#define FFSS_COMPRESSION_NONE  0
#define FFSS_COMPRESSION_ZLIB  1
#define FFSS_COMPRESSION_BZLIB 2

#define FFSS_BZLIB_BLOCK100K 4
#define FFSS_BZLIB_SMALL 1

#define FFSS_FILE_TAGS_NOTHING  0
#define FFSS_FILE_TAGS_MUSIC    1
#define FFSS_FILE_TAGS_VIDEO    2
#define FFSS_FILE_TAGS_IMAGE    4
#define FFSS_FILE_TAGS_DOC      8
#define FFSS_FILE_TAGS_EXE     16
#define FFSS_FILE_TAGS_ZIP     32

#define FFSS_MUSIC_NB_EXT  9
#define FFSS_VIDEO_NB_EXT  8
#define FFSS_IMAGE_NB_EXT  8
#define FFSS_DOC_NB_EXT    8
#define FFSS_EXE_NB_EXT    5
#define FFSS_ZIP_NB_EXT   10

#define FFSS_OPTIONS_DEBUG        1
#define FFSS_OPTIONS_BZLIB        2
#define FFSS_OPTIONS_CONTEXT      4
#define FFSS_OPTIONS_MALLOC_TRACE 8
#define FFSS_OPTIONS_FTP          16
#define FFSS_OPTIONS_NO_CHECKSUM  32

#define CRLF "\xD\xA"
#define FFSS_SUPER_MAGIC 0xFF55

/* ************************************************ */
/*                   DATA STRUCTURE                 */
/* ************************************************ */
typedef long int FFSS_Field;
#ifdef __unix__
typedef long long FFSS_LongField;
#else /* !__unix__ */
typedef __int64 FFSS_LongField;
#endif /* __unix__ */

typedef struct
{
  char *Name;
  FFSS_Field Flags;
  FFSS_LongField Size;
  FFSS_Field Stamp;
} FC_TEntry, *FC_PEntry;

typedef struct
{
  char *Name;
  char *OS;
  char *Comment;
  char *IP;
  FFSS_Field State;
  time_t LastPong;
  time_t OffSince;
  /* Ajouter un pointeur sur la table d'index */
  time_t LastIndex; /* For Master use */
} FM_THost, *FM_PHost;

typedef struct
{
  char *Name;      /* Name of the domain */
  char *Master;    /* Address of the master of the domain */
  SU_PList Hosts;  /* FM_PHost */ /* Hosts of the domain */
  bool Listed;     /* Servers already listed ? */
} FM_TDomain, *FM_PDomain;

typedef struct
{
  FFSS_LongField fsize,total;
  FFSS_Field Checksum;
  FFSS_Field XFerTag;
  bool Download;
  bool UseConnSock;
} FFSS_TXFerInfo, *FFSS_PXFerInfo;

typedef struct
{
  int  sock;                  /* Opened socket for file transfer */
  FILE *fp;                   /* Opened file for reading/writing */
  char *FileName;             /* Remote file name */ /* NULL on server side */
  char *LocalPath;            /* Local path of file used for fopen */
  FFSS_LongField StartingPos; /* Reading/Writing starting pos in the file */
  FFSS_LongField FileSize;    /* Size of the file */
  FFSS_LongField XFerPos;     /* Current xfer pos */
  int  ThreadType;            /* Type of the thread (SERVER / CLIENT) */
  SU_PClientSocket Client;    /* SU_PClientSocket structure of the share connection we transfer from */ /* Do NOT free this, only a pointer !! */
  bool Cancel;                /* If the transfer is to be canceled */
  void *User;                 /* User information */
  FFSS_TXFerInfo XI;          /* XFer info for xfer using connection socket */
} FFSS_TTransfer, *FFSS_PTransfer;

typedef struct
{
  /* UDP callbacks */
  void (*OnPing)(struct sockaddr_in Master);
  void (*OnStateAnswer)(const char Domain[]);
  void (*OnServerSearch)(struct sockaddr_in Client);
  void (*OnSharesListing)(struct sockaddr_in Client);
  void (*OnIndexRequest)(struct sockaddr_in Master,FFSS_Field Port);
  void (*OnError)(FFSS_Field ErrorCode,const char Description[]);
  void (*OnMasterSearchAnswer)(struct sockaddr_in Master,FFSS_Field ProtocolVersion,const char Domain[]);

  /* TCP callbacks */
  bool (*OnCheckConnection)(SU_PClientSocket Client);
  bool (*OnShareConnection)(SU_PClientSocket Client,const char ShareName[],const char Login[],const char Password[],long int Compressions);
  bool (*OnDirectoryListing)(SU_PClientSocket Client,const char Path[]); /* Path IN the share (without share name) */
  bool (*OnDownload)(SU_PClientSocket Client,const char Path[],FFSS_LongField StartPos,int Port); /* Path IN the share (without share name) */
  bool (*OnUpload)(SU_PClientSocket Client,const char Path[],FFSS_LongField Size,int Port); /* Path IN the share (without share name) */
  bool (*OnRename)(SU_PClientSocket Client,const char Path[],const char NewPath[]); /* Path IN the share (without share name) */
  bool (*OnCopy)(SU_PClientSocket Client,const char Path[],const char NewPath[]); /* Path IN the share (without share name) */
  bool (*OnDelete)(SU_PClientSocket Client,const char Path[]); /* Path IN the share (without share name) */
  bool (*OnMkDir)(SU_PClientSocket Client,const char Path[]); /* Path IN the share (without share name) */
  void (*OnEndTCPThread)(void);
  int (*OnSelect)(void); /* 0=Do timed-out select ; 1=don't do timed-out select, but sleep ; 2=don't do timed-out select, and continue */
  void (*OnIdleTimeout)(SU_PClientSocket Client);
  void (*OnTransferFailed)(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download);
  void (*OnTransferSuccess)(FFSS_PTransfer FT,bool Download);
  void (*OnTransferActive)(FFSS_PTransfer FT,long int Amount,bool Download);
  void (*OnCancelXFer)(SU_PClientSocket Server,FFSS_Field XFerTag);
  void (*OnStrmOpen)(SU_PClientSocket Client,long int Flags,const char Path[]); /* Path IN the share (without share name) */
  void (*OnStrmClose)(SU_PClientSocket Client,long int Handle);
  void (*OnStrmRead)(SU_PClientSocket Client,long int Handle,FFSS_LongField StartPos,long int Length);
  void (*OnStrmWrite)(SU_PClientSocket Client,long int Handle,FFSS_LongField StartPos,const char Bloc[],long int BlocSize);
  void (*OnStrmSeek)(SU_PClientSocket Client,long int Handle,long int Flags,FFSS_LongField Pos);

  /* FTP callbacks */
  bool (*OnConnectionFTP)(SU_PClientSocket Client);
  void (*OnPWDFTP)(SU_PClientSocket Client);
  void (*OnTypeFTP)(SU_PClientSocket Client,const char Type);
  void (*OnModeFTP)(SU_PClientSocket Client,const char Mode);
  bool (*OnDirectoryListingFTP)(SU_PClientSocket Client,SU_PClientSocket Data,const char Path[]);
  void (*OnCWDFTP)(SU_PClientSocket Client,const char Path[]);

  void (*OnDownloadFTP)(SU_PClientSocket Client,const char Path[],FFSS_LongField StartPos,const char Host[],const char Port[]); /* Path from the root of all shares */
  bool (*OnUploadFTP)(SU_PClientSocket Client,const char Path[],FFSS_LongField Size,int Port); /* Path from the root of all shares */
  bool (*OnRenameFTP)(SU_PClientSocket Client,const char Path[],const char NewPath[]); /* Path from the root of all shares */
  bool (*OnDeleteFTP)(SU_PClientSocket Client,const char Path[]); /* Path from the root of all shares */
  bool (*OnMkDirFTP)(SU_PClientSocket Client,const char Path[]); /* Path from the root of all shares */
  void (*OnEndTCPThreadFTP)(void);
  void (*OnIdleTimeoutFTP)(SU_PClientSocket Client);
} FFSS_TServerCallbacks, *FFSS_PServerCallbacks;

typedef struct
{
  /* UDP callbacks */
  void (*OnNewState)(FFSS_Field State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[]);
  void (*OnSharesListing)(const char IP[],const char **Names,const char **Comments,int NbShares);
  /* WARNING !! (char *) of the FM_PHost structure are pointers to STATIC buffer, and must be dupped ! */
  /* Except for the FM_PHost->IP that is dupped internaly, and if you don't use it, you MUST free it !! */
  void (*OnServerListingAnswer)(const char Domain[],int NbHost,SU_PList HostList); /* SU_PList of FM_PHost */
  void (*OnEndServerListingAnswer)(void);
  void (*OnDomainListingAnswer)(const char **Domains,int NbDomains); /* First domain is assumed to be domain from the answering master */
  void (*OnMasterSearchAnswer)(struct sockaddr_in Master,FFSS_Field ProtocolVersion,const char Domain[]);
  void (*OnSearchAnswer)(const char Query[],const char Domain[],const char **Answers,int NbAnswers);
  void (*OnUDPError)(int ErrNum);
  void (*OnMasterError)(int Code,const char Descr[]);

  /* TCP callbacks */
  void (*OnBeginTCPThread)(SU_PClientSocket Server);
  bool (*OnError)(SU_PClientSocket Server,int Code,const char Descr[]);
  bool (*OnDirectoryListingAnswer)(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries); /* FC_PEntry */
  void (*OnEndTCPThread)(SU_PClientSocket Server);
  void (*OnIdleTimeout)(SU_PClientSocket Server);
  void (*OnTransferFailed)(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download);
  void (*OnTransferSuccess)(FFSS_PTransfer FT,bool Download);
  void (*OnTransferActive)(FFSS_PTransfer FT,long int Amount,bool Download);
  FFSS_PTransfer (*OnInitXFer)(SU_PClientSocket Server,const char RequestedFileName[]); /* Returns PTransfer from RequestedFileName */
  FFSS_PTransfer (*OnData)(SU_PClientSocket Server,FFSS_Field XFerTag); /* Returns PTransfer from XFerTag */
  void (*OnStrmOpenAnswer)(SU_PClientSocket Client,const char Path[],int Code,long int Handle,FFSS_LongField FileSize);
  void (*OnStrmReadAnswer)(SU_PClientSocket Client,long int Handle,const char Bloc[],long int BlocSize);
  void (*OnStrmWriteAnswer)(SU_PClientSocket Client,long int Handle,int Code);

  /* Fatal error */
  void (*OnFatalError)(void);
} FFSS_TClientCallbacks, *FFSS_PClientCallbacks;

typedef struct
{
  /* UDP callbacks */
  void (*OnState)(struct sockaddr_in Server,FFSS_Field State,const char Name[],const char OS[],const char Comment[]);
  void (*OnNewState)(FFSS_Field State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[]);
  void (*OnServerListing)(struct sockaddr_in Client,const char OS[],const char Domain[],long int Compressions);
//  void (*OnServerListingAnswer)(const char Domain[],const char NbHost,SU_PList HostList); /* SU_PList of FM_PHost */ /* WARNING !! (char *) of the FM_PHost structure are pointers to STATIC buffer, and must be dupped ! */
  void (*OnClientServerFailed)(const char IP[]);
  void (*OnPong)(struct sockaddr_in Server,FFSS_Field State);
  void (*OnDomainListing)(struct sockaddr_in Client);
  void (*OnSearch)(struct sockaddr_in Client,int Port,const char Domain[],const char KeyWords[],long int Compressions);
  void (*OnSearchForward)(struct sockaddr_in Master,const char ClientIP[],int Port,const char KeyWords[],long int Compressions);
  void (*OnMasterSearch)(struct sockaddr_in Client,bool Server);
  void (*OnIndexAnswer)(struct sockaddr_in Client,FFSS_Field CompressionType,FFSS_Field IndexSize,FFSS_Field FileTreeSize,FFSS_Field NodesSize,int Port);
  void (*OnIndexAnswerSamba)(struct sockaddr_in Client,FFSS_Field CompressionType,FFSS_Field IndexSize,FFSS_Field FileTreeSize,FFSS_Field NodesSize,int Port);
} FFSS_TMasterCallbacks, *FFSS_PMasterCallbacks;

typedef struct
{
  FFSS_TServerCallbacks SCB;
  FFSS_TClientCallbacks CCB;
  FFSS_TMasterCallbacks MCB;
} FFSS_TCallbacks, *FFSS_PCallbacks;

typedef struct
{
  long int Handle;
  char *Buf;
  long int BufSize;
} FC_THandle, *FC_PHandle;

/* ************************************************ */
/*                 EXTERN VARIABLES                 */
/* ************************************************ */
extern SU_THREAD_HANDLE FS_THR_UDP,FS_THR_TCP,FS_THR_TCP_FTP;
extern SU_THREAD_HANDLE FC_THR_UDP;
extern SU_THREAD_HANDLE FM_THR_UDP;
extern FFSS_TCallbacks FFSS_CB;
extern char *FFSS_MyIP;

extern char *FFSS_ErrorTable[];

void FFSS_PrintSyslog(int Level,char *Txt, ...);
void FFSS_PrintDebug(int Level,char *Txt, ...);


/* ************************************************ */
/*              FFSS SERVER FUNCTIONS               */
/* ************************************************ */

/* FFSS Server : Init */
/* Initialisation of the FFSS Server - Must be called before any other FFSS function */
/* Returns true on success, false otherwise */
bool FS_Init(int ServerPort,bool FTP);

/* FFSS Server : UnInit */
/* Uninitialisation of the FFSS Server - Must be called at the end of the main */
/* Returns true on success, false otherwise */
bool FS_UnInit(void);


/* ************************************************ */
/*              FFSS CLIENT FUNCTIONS               */
/* ************************************************ */

/* FFSS Client : Init */
/* Initialisation of the FFSS Client - Must be called before any other FFSS function */
/* Returns true on success, false otherwise */
bool FC_Init(void);

/* FFSS Client : UnInit */
/* Uninitialisation of the FFSS Client - Must be called at the end of the main */
/* Returns true on success, false otherwise */
bool FC_UnInit(void);


/* ************************************************ */
/*              FFSS MASTER FUNCTIONS               */
/* ************************************************ */

/* FFSS Master : Init */
/* Initialisation of the FFSS Master - Must be called before any other FFSS function */
/* Returns true on success, false otherwise */
bool FM_Init(int MasterPort);

/* FFSS Master : UnInit */
/* Uninitialisation of the FFSS Master - Must be called at the end of the main */
/* Returns true on success, false otherwise */
bool FM_UnInit(void);

/* ************************************************ */
/*              FFSS DRIVER FUNCTIONS               */
/* ************************************************ */
FC_PHandle FC_CreateHandle(void);
void FC_FreeHandle(FC_PHandle Hdl);
/* Return true on success, false otherwise. If error, the socket is closed */
bool FC_WaitDataTCP(SU_PClientSocket Client,FC_PHandle Hdl);
/*
  The 'bool FC_Init(void)' and 'bool FC_UnInit(void)' functions have the same prototype than CLIENT's one
*/

/* ************************************************ */
/*               FFSS UTILS FUNCTIONS               */
/* ************************************************ */

/* FFSS_ComputeChecksum                                     */
/* Computes and updates the Checksum of a buffer            */
/* Set Buf to NULL for the first call, to init the checksum */
FFSS_Field FFSS_ComputeChecksum(FFSS_Field Old,const char Buf[],long int Len);

/* Retrieve local IP of interface named IntName */
bool FFSS_GetMyIP(SU_PServerInfo SI,const char IntName[]);

/*
 * FFSS_GetFileTags
 *   Returns the tags of a file
 */
unsigned char FFSS_GetFileTags(const char *FileName);  /* <-- Full name of the file */

/*
 * FM_GetWordTags
 *   Returns the tags of a word
 */
unsigned char FFSS_GetWordTags(const char *Word);  /* <-- word to check for extension */


/* ************************************************ */
/*              FFSS TRANSFER FUNCTIONS             */
/* ************************************************ */

/* If FT_out is NULL, not filled */
bool FFSS_UploadFile(SU_PClientSocket Client,const char FilePath[],FFSS_LongField StartingPos,int Port,void *User,bool UseConnSock,FFSS_PTransfer *FT_out);

/* RemotePath in the share */
/* If FT_out is NULL, not filled */
/* If LocalPath is NULL, stdout is used for local writing */
bool FFSS_DownloadFile(SU_PClientSocket Server,const char RemotePath[],const char LocalPath[],FFSS_LongField StartingPos,void *User,bool UseConnSock,FFSS_PTransfer *FT_out);


/* ************************************************ */
/*                  SERVER MESSAGES                 */
/* ************************************************ */

/* FS_SendMessage_State Function                    */
/* Sends a STATE message to a master                */
/*  Master : The name of my master, or NULL if none */
/*  Name : The name of my server                    */
/*  OS : The os of my server                        */
/*  Comment : The comment of my server              */
/*  State : The new state of my server              */
bool FS_SendMessage_State(const char Master[],const char Name[],const char OS[],const char Comment[],int State);

/* FS_SendMessage_ServerSearchAnswer Function       */
/* Sends a STATE message to a client                */
/*  Domain : The domain of my master                */
/*  Name : The name of my server                    */
/*  OS : The os of my server                        */
/*  Comment : The comment of my server              */
/*  State : The state of my server                  */
/*  IP : The IP address of my server                */
/*  MasterIP : The IP address of my server          */
bool FS_SendMessage_ServerSearchAnswer(struct sockaddr_in Client,const char Domain[],const char Name[],const char OS[],const char Comment[],int State,const char IP[],const char MasterIP[]);

/* FS_SendMessage_ServerSharesAnswer Function                */
/* Sends a SHARES ANSWER message to a client                 */
/*  IP : The IP address of my server                         */
/*  ShareNames : A tab of the share names of my server       */
/*  ShareComments : A tab of the share comments of my server */
/*  NbShares : The number of shares of my server             */
bool FS_SendMessage_ServerSharesAnswer(struct sockaddr_in Client,const char IP[],const char **ShareNames,const char **ShareComments,int NbShares);

/* FS_SendMessage_Pong Function     */
/* Sends a PONG message to a master */
/*  Master : The sin of my master   */
/*  State : The state of my server  */
bool FS_SendMessage_Pong(struct sockaddr_in Master,int State);

/* FS_SendMessage_Error Function              */
/* Sends an ERROR message to a client         */
/*  Client : The socket of the client         */
/*  Code : The error code to send             */
/*  Descr : The description of the error code */
bool FS_SendMessage_Error(int Client,FFSS_Field Code,const char Descr[]);

/* FS_SendMessage_DirectoryListingAnswer Function                     */
/* Sends a DIRECTORY LISTING ANSWER message to a client               */
/*  Client : The socket of the client                                 */
/*  Path : The path of the directory IN the share                     */
/*  Buffer : The buffer containing the nb of entries, and the entries */
/*  BufSize : The size of the buffer                                  */
/*  Compression : The type of compression to be applied to Buffer     */
bool FS_SendMessage_DirectoryListingAnswer(int Client,const char Path[],const char *Buffer,long int BufSize,int Compression);

/* FS_SendMessage_InitXFer Function                        */
/* Sends an INIT XFER message to a client                  */
/*  Client : The socket of the client                      */
/*  Tag : The xfer tag that will be used when sending data */
/*  FileName : The name of the requested file              */
bool FS_SendMessage_InitXFer(int Client,FFSS_Field Tag,const char FileName[]);

/* FS_SendMessage_MasterSearch Function      */
/* Sends a MASTER SEARCH message to broadcast */
bool FS_SendMessage_MasterSearch();

/* FS_SendMessage_IndexAnswer Function                             */
/* Sends an INDEX ANSWER message to someone                        */
/*  Host : The name of the host to send to                         */
/*  Port : The port to use                                         */
/*  Buffers : Chained list of share buffers                        */
/*  Sizes : Chained list of sizes of share buffers                 */
/*  Compression : The type of compression to be applied to Buffers */
bool FS_SendMessage_IndexAnswer(const char Host[],const char Port[],SU_PList Buffers,SU_PList Sizes,int Compression);

/* FS_SendMessage_StrmOpenAnswer Function             */
/* Sends an STREAMING OPEN answer message to a client */
/*  Client : The socket of the client                 */
/*  Path : The path of the file requested             */
/*  Code : The error code to send                     */
/*  Handle : The handle of the file if successfull    */
/*  FileSize : Size of the file                       */
bool FS_SendMessage_StrmOpenAnswer(int Client,const char Path[],FFSS_Field Code,long int Handle,FFSS_LongField FileSize);

/* FS_SendMessage_StrmReadAnswer Function             */
/* Sends an STREAMING READ answer message to a client */
/*  Client : The socket of the client                 */
/*  Handle : The handle of the file                   */
/*  Buf : The buffer of datas                         */
/*  BlocLen : The length of the datas                 */
bool FS_SendMessage_StrmReadAnswer(int Client,long int Handle,char *Buf,long int BlocLen);

/* FS_SendMessage_StrmWriteAnswer Function             */
/* Sends an STREAMING WRITE answer message to a client */
/*  Client : The socket of the client                  */
/*  Handle : The handle of the file                    */
/*  Code : The error code to send                      */
bool FS_SendMessage_StrmWriteAnswer(int Client,long int Handle,FFSS_Field Code);


/* ************************************************ */
/*                  CLIENT MESSAGES                 */
/* ************************************************ */

/* FC_SendMessage_ServerSearch Function       */
/* Sends a SERVER SEARCH message to broadcast */
bool FC_SendMessage_ServerSearch(void);

/* FC_SendMessage_SharesListing Function                       */
/* Sends a SHARES LISTING message to a server                  */
/*  Server : The name of the server we want the shares listing */
bool FC_SendMessage_SharesListing(const char Server[]);

/* NC_SendMessage_ServerList Function                      */
/* Sends a SERVER LIST message to a master                 */
/*  Master : The name of my master, or NULL if none        */
/*  OS : The desired OS, or NULL if requesting all         */
/*  Domain : The desired domain, or NULL if requesting all */
bool FC_SendMessage_ServerList(const char Master[],const char OS[],const char Domain[]);

/* FC_SendMessage_ShareConnect Function                  */
/* Sends a SHARE CONNECTION message to a server          */
/*  Server : The name of Server we wish to connect to    */
/*  ShareName : The Share Name we wish to connect to     */
/*  Login : The Login we may use (or NULL if none)       */
/*  Password : The Password we may use (or NULL if none) */
SU_PClientSocket FC_SendMessage_ShareConnect(const char Server[],const char ShareName[],const char Login[],const char Password[]);

/* FC_SendMessage_DirectoryListing Function             */
/* Sends a DIRECTORY LISTING message to a server        */
/*  Server : The Server's structure we are connected to */
/*  Path : The path we request a listing                */
bool FC_SendMessage_DirectoryListing(SU_PClientSocket Server,const char Path[]);

/* FC_SendMessage_Download Function                                 */
/* Sends a DOWNLOAD message to a server                             */
/*  Server : The Server's structure we are connected to             */
/*  Path : The path of requested file (in the share)                */
/*  StartingPos : The pos we want to download the file starting at  */
/*  UseConnSock : Use a separate socket/thread, or use the existing */
int FC_SendMessage_Download(SU_PClientSocket Server,const char Path[],FFSS_LongField StartingPos,bool UseConnSock);

/* FC_SendMessage_Disconnect Function                   */
/* Sends an DISCONNECT message to a server              */
/*  Server : The Server's structure we are connected to */
void FC_SendMessage_Disconnect(SU_PClientSocket Server);

/* FC_SendMessage_CancelXFer Function                   */
/* Sends an CANCEL XFER message to a server             */
/*  Server : The Server's structure we are connected to */
/*  XFerTag : The tag of the xfer we want to cancel     */
void FC_SendMessage_CancelXFer(SU_PClientSocket Server,FFSS_Field XFerTag);

/* FC_SendMessage_DomainListing Function   */
/* Sends a DOMAIN LIST message to a master */
/*  Master : The name of my master         */
bool FC_SendMessage_DomainListing(const char Master[]);

/* FC_SendMessage_Search Function                          */
/* Sends a SEARCH message to a master                      */
/*  Master : The name of my master                         */
/*  Domain : The desired domain, or NULL if requesting all */
/*  Keys   : A String of keywords                          */
bool FC_SendMessage_Search(const char Master[],const char Domain[],const char Key[]);

/* FC_SendMessage_MasterSearch Function       */
/* Sends a MASTER SEARCH message to broadcast */
bool FC_SendMessage_MasterSearch();

/* FC_SendMessage_StrmOpen Function                     */
/* Sends an STREAMING OPEN message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Path : The path of the requested file               */
/*  Flags : The opening mode flags                      */
bool FC_SendMessage_StrmOpen(SU_PClientSocket Server,const char Path[],int Flags);

/* FC_SendMessage_StrmClose Function                     */
/* Sends an STREAMING CLOSE message to a server          */
/*  Server : The Server's structure we are connected to  */
/*  Handle : The handle of the file to close             */
bool FC_SendMessage_StrmClose(SU_PClientSocket Server,long int Handle);

/* FC_SendMessage_StrmRead Function                     */
/* Sends an STREAMING READ message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  StartPos : The start position of the requested bloc */
/*  Length : Indicative length requested                */
bool FC_SendMessage_StrmRead(SU_PClientSocket Server,long int Handle,FFSS_LongField StartPos,long int Length);

/* FC_SendMessage_StrmWrite Function                    */
/* Sends an STREAMING WRITE message to a server         */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  StartPos : The start position of the requested bloc */
/*  Buf : The buffer of datas                           */
/*  BlocLen : The length of the datas                   */
bool FC_SendMessage_StrmWrite(SU_PClientSocket Server,long int Handle,FFSS_LongField StartPos,char *Buf,long int BlocLen);

/* FC_SendMessage_StrmSeek Function                     */
/* Sends an STREAMING SEEK message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  Flags : The flags for the seek operation            */
/*  StartPos : The position of the seek                 */
bool FC_SendMessage_StrmSeek(SU_PClientSocket Server,long int Handle,int Flags,FFSS_LongField StartPos);


/* ************************************************ */
/*                  MASTER MESSAGES                 */
/* ************************************************ */

/* FM_SendMessage_Ping Function    */
/* Sends a PING message to servers */
bool FM_SendMessage_Ping();

/* FM_SendMessage_NewStatesClients Function                         */
/* Sends a NEW STATES message to clients                            */
/*  Buffer : The buffer containing the nb of states, and the states */
/*  BufSize : The size of the buffer                                */
/*  Compression : The type of compression to be applied to Buffer   */
  /* !!!!!!!!!!!!!!!!!!!!! USELESS FUNCTION !!!!!!!!!!!!!!!!!!!!!!!!!!! */
  /* Client should NOT be listening on a fixed port, or we won't be able to run multiple clients on the same host */
//bool FM_SendMessage_NewStatesClients(const char *Buffer,long int BufSize,int Compression);

/* FM_SendMessage_NewStatesMaster Function                          */
/* Sends a NEW STATES message to a master                           */
/*  Master : The name of the master we want to send states          */
/*  Buffer : The buffer containing the nb of states, and the states */
/*  BufSize : The size of the buffer                                */
/*  Compression : The type of compression to be applied to Buffer   */
bool FM_SendMessage_NewStatesMaster(const char Master[],const char *Buffer,long int BufSize,int Compression);

/* FM_SendMessage_ServerListing Function                            */
/* Sends a NEW STATES message to client                             */
/*  Client : The sin of the client                                  */
/*  Buffer : The buffer containing the nb of domains, and the hosts */
/*  BufSize : The size of the buffer                                */
/*  Compression : The type of compression to be applied to Buffer   */
bool FM_SendMessage_ServerListing(struct sockaddr_in Client,const char *Buffer,long int BufSize,int Compression);

/* FM_SendMessage_Error Function              */
/* Sends an ERROR message to a server         */
/*  Server : The name of the server           */
/*  Code : The error code to send             */
/*  Descr : The description of the error code */
bool FM_SendMessage_Error(const char Server[],FFSS_Field Code,const char Descr[]);

/* FM_SendMessage_ErrorClient Function        */
/* Sends an ERROR message to a client         */
/*  Client : The sin of the client            */
/*  Code : The error code to send             */
/*  Descr : The description of the error code */
bool FM_SendMessage_ErrorClient(struct sockaddr_in Client,FFSS_Field Code,const char Descr[]);

/* FM_SendMessage_ServerList Function              */
/* Sends a SERVER LIST message to a foreign master */
/*  Master : The name of the foreign master        */
bool FM_SendMessage_ServerList(const char Master[]);

/* FM_SendMessage_DomainListingAnswer Function */
/* Sends a DOMAIN ANSWER message to client     */
/*  Client : The sin of the client             */
/*  Domains : A SU_PList of FM_PDomain         */
bool FM_SendMessage_DomainListingAnswer(struct sockaddr_in Client,SU_PList Domains);

/* FM_SendMessage_MasterSearchAnswer Function               */
/* Sends a MASTER SEARCH ANSWER message to client or server */
/*  Client : The sin of the client or the server            */
/*  Server : True if from server                            */
/*  Domain : The name of my domain                          */
bool FM_SendMessage_MasterSearchAnswer(struct sockaddr_in Client,bool Server,const char Domain[]);

/* FM_SendMessage_SearchAnswer Function                                          */
/* Sends a SEARCH ANSWER message to client                                       */
/*  Client : The sin of the client                                               */
/*  Buffer : The buffer containing the query, the nb of answers, and the answers */
/*  BufSize : The size of the buffer                                             */
/*  Compression : The type of compression to be applied to Buffer                */
bool FM_SendMessage_SearchAnswer(struct sockaddr_in Client,const char *Buffer,long int BufSize,int Compression);

/* FM_SendMessage_SearchForward Function                 */
/* Sends a SEARCH message to a foreign master            */
/*  Master : The name of the master                      */
/*  Client : The sin of the client                       */
/*  Compression   : Compressions supported by the client */
/*  Keys   : A String of keywords                        */
bool FM_SendMessage_SearchForward(const char Master[],struct sockaddr_in Client,int Compression,const char Key[]);



/* ************************************************************************* */
/* TRANSFER.H                                                                */
/* ************************************************************************* */
void FFSS_FreeTransfer(FFSS_PTransfer T);
bool FFSS_SendData(FFSS_PTransfer FT,FFSS_Field Tag,char *Buf,int len);
void FFSS_OnDataDownload(FFSS_PTransfer FT,const char Buf[],int Len);
void FFSS_InitXFerDownload(FFSS_PTransfer FT,FFSS_Field XFerTag);
extern char *FFSS_TransferErrorTable[];
extern long int FFSS_TransferBufferSize;
extern long int FFSS_TransferReadBufferSize;

/* ************************************************************************* */
/* UTILS.H                                                                   */
/* ************************************************************************* */
/* Unpacks a string from a message, checking if the string really terminates (prevents DoS attacks) */
/*  Returns the string, or NULL if there is a problem */
char *FFSS_UnpackString(const char beginning[],const char buf[],int len,long int *new_pos);
/* Unpacks a FFSS_Field from a message, checking if the FFSS_Field is fully in the message (prevents DoS attacks) */
/*  Returns the FFSS_Field, or 0 if there is a problem */
FFSS_Field FFSS_UnpackField(const char beginning[],const char buf[],int len,long int *new_pos);
/* Same with LongField */
FFSS_LongField FFSS_UnpackLongField(const char beginning[],const char buf[],int len,long int *new_pos);
void FFSS_UnpackIP(const char beginning[],char *buf,int len,long int *new_pos,char buf_out[],int Type);
void FFSS_PackIP(char *buf,const char IP[],int Type);

bool FFSS_CompresseZlib(char *in,long int len_in,char *out,long int *len_out);
char *FFSS_UncompresseZlib(char *in,long int len_in,long int *len_out);
#ifdef HAVE_BZLIB
bool FFSS_CompresseBZlib(char *in,long int len_in,char *out,long int *len_out);
char *FFSS_UncompresseBZlib(char *in,long int len_in,long int *len_out);
#endif /* HAVE_BZLIB */
/* Adds a broadcast address to send to, when using bcast send */
void FFSS_AddBroadcastAddr(const char Addr[]);
/* Returns FFSS library compilation options (FFSS_OPTIONS_xx) */
int FFSS_GetFFSSOptions(void);

extern char *FFSS_MusicExt[FFSS_MUSIC_NB_EXT];
extern char *FFSS_VideoExt[FFSS_VIDEO_NB_EXT];
extern char *FFSS_ImageExt[FFSS_IMAGE_NB_EXT];
extern char *FFSS_DocExt[FFSS_DOC_NB_EXT];
extern char *FFSS_ExeExt[FFSS_EXE_NB_EXT];
extern char *FFSS_ZipExt[FFSS_ZIP_NB_EXT];

extern int N_DebugLevel;
extern bool N_SyslogOn;
extern SU_PServerInfo FS_SI_UDP,FS_SI_OUT_UDP,FS_SI_TCP,FS_SI_TCP_FTP;
extern SU_PServerInfo FC_SI_OUT_UDP;
extern SU_PServerInfo FM_SI_UDP,FM_SI_OUT_UDP;
#ifdef _WIN32
extern FILE *FFSS_LogFile;
#endif /* _WIN32 */

#ifndef DEBUG
#ifdef __unix__
#define FFSS_PrintDebug(x,...) /* */
#define SYSLOG_FN(x,y) syslog(x,y)
#else /* !__unix__ */
#define FFSS_PrintDebug() /* */
#define SYSLOG_FN(x,y) SU_WriteToLogFile(FFSS_LogFile,y)
#endif /* __unix__ */
#else /* DEBUG */
#define SYSLOG_FN(x,y) printf(y)
#endif /* !DEBUG */

#ifdef FFSS_CONTEXT
void FFSS_handle_SIGNAL(int signal);
#define context set_context(__FILE__, __LINE__)
void set_context(char *file, int line);
#else /* !FFSS_CONTEXT */
#define context
#endif /* FFSS_CONTEXT */

#endif /* !__FFSS_H__ */
