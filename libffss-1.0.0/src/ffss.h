/*
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
#ifndef __FFSS_H__
#define __FFSS_H__

/* Undefine c++ bool type (unsigned char ?)
   Use SU_BOOL type in your appli, every time you use a ffss prototype
 */
#ifdef __cplusplus
#define bool SU_BOOL
#endif /* __cplusplus */

/* Define the target OS */
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
extern char FFSS_WinServerVersion[20];
#elif defined __MACH__ && defined __APPLE__
#define FFSS_SERVER_OS "MacOS"
#define __unix__
#elif __CYGWIN32__
#define FFSS_SERVER_OS "CygWin"
extern char FFSS_WinServerVersion[20];
#elif __sun__
#define FFSS_SERVER_OS "SunOS"
#else
#error "Unknown OS... contact devel team"
#endif /* __linux__ */

/* Define the target arch word alignment (not needed on x86) */
#ifdef __arm__
#define USE_ALIGNED_WORD
#endif /* __arm__ */

#ifndef DISABLE_BZLIB
#ifdef _WIN32
#define HAVE_BZLIB 1
#endif /* _WIN32 */
#endif /* !DISABLE_BZLIB */

#ifdef FFSS_DRIVER
#include <kffss.h>
#endif /* FFSS_DRIVER */

#include <skyutils.h>
#ifndef FFSS_DRIVER
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#ifndef DISABLE_ZLIB
#include <zlib.h>
#endif /* !DISABLE_ZLIB */
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
#ifdef __sun__
#include <sys/sockio.h>
#endif /* __sun__ */
#endif /* _WIN32 */
#endif /* !FFSS_DRIVER */

#define FFSS_VERSION "1.0.0-pre86"
#define FFSS_COPYRIGHT "FFSS library v" FFSS_VERSION " (c) Christophe Calmejane 2001'03"
#define FFSS_FTP_SERVER "FFSS FTP compatibility v" FFSS_VERSION

#define FFSS_PROTOCOL_VERSION                  0x0010007
#define FFSS_PROTOCOL_VERSION_LEAST_COMPATIBLE 0x0010007

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
#define FFSS_MAX_SERVEROS_LENGTH 10
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
#define FFSS_MIN_SEARCH_REQUEST_LENGTH 3
#define FFSS_UDP_CLIENT_BUFFER_SIZE 500000
#define FFSS_UDP_SERVER_BUFFER_SIZE 100000
#define FFSS_UDP_MASTER_BUFFER_SIZE 100000
#define FFSS_TCP_MASTER_BUFFER_SIZE 100000
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
#define FFSS_UDP_MAX_ERRORS 20
#define FFSS_DEFAULT_MAX_CONN 10
#define FFSS_DEFAULT_MAX_XFER_PER_CONN 2

/* FFSS Message types */
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
#define FFSS_MESSAGE_REC_DIR_LISTING           23
#define FFSS_MESSAGE_REC_DIR_LISTING_ANSWER    24
#define FFSS_MESSAGE_DOMAINS_LISTING           30
#define FFSS_MESSAGE_DOMAINS_LISTING_ANSWER    31
#define FFSS_MESSAGE_SHORT_MESSAGE             32
#define FFSS_MESSAGE_SEARCH                    50
#define FFSS_MESSAGE_SEARCH_ANSWER             51
#define FFSS_MESSAGE_SEARCH_FW                 52
#define FFSS_MESSAGE_SEARCH_MASTER             55
#define FFSS_MESSAGE_SEARCH_MASTER_ANSWER      56
#define FFSS_MESSAGE_INDEX_ANSWER_SAMBA        57
#define FFSS_MESSAGE_MASTER_CONNECTION         58
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

/* FFSS Message minimal size */
#define FFSS_MESSAGESIZE_STATE                      4
#define FFSS_MESSAGESIZE_STATE_ANSWER               2
#define FFSS_MESSAGESIZE_NEW_STATES                 3
#define FFSS_MESSAGESIZE_NEW_STATES_2               3
#define FFSS_MESSAGESIZE_INDEX_REQUEST              3
#define FFSS_MESSAGESIZE_INDEX_ANSWER               7
#define FFSS_MESSAGESIZE_INDEX_ANSWER_2             5
#define FFSS_MESSAGESIZE_SERVER_LISTING             5
#define FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER      5
#define FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER_2    1
#define FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER_3    2
#define FFSS_MESSAGESIZE_CLIENT_SERVER_FAILED       3
#define FFSS_MESSAGESIZE_PING                       3
#define FFSS_MESSAGESIZE_PONG                       4
#define FFSS_MESSAGESIZE_SERVER_SEARCH              2
#define FFSS_MESSAGESIZE_SHARES_LISTING             4
#define FFSS_MESSAGESIZE_SHARES_LISTING_ANSWER      6
#define FFSS_MESSAGESIZE_SHARE_CONNECTION           6
#define FFSS_MESSAGESIZE_DIRECTORY_LISTING          4
#define FFSS_MESSAGESIZE_DIRECTORY_LISTING_ANSWER   6
#define FFSS_MESSAGESIZE_DIRECTORY_LISTING_ANSWER_2 3
#define FFSS_MESSAGESIZE_DOWNLOAD                   (3+2*2)
#define FFSS_MESSAGESIZE_UPLOAD                     (3+2*2)
#define FFSS_MESSAGESIZE_MOVE                       4
#define FFSS_MESSAGESIZE_COPY                       4
#define FFSS_MESSAGESIZE_DELETE                     4
#define FFSS_MESSAGESIZE_MKDIR                      4
#define FFSS_MESSAGESIZE_REC_DIR_LISTING            4
#define FFSS_MESSAGESIZE_REC_DIR_LISTING_ANSWER     6
#define FFSS_MESSAGESIZE_REC_DIR_LISTING_ANSWER_2   3
#define FFSS_MESSAGESIZE_DOMAINS_LISTING            4
#define FFSS_MESSAGESIZE_DOMAINS_LISTING_ANSWER     5
#define FFSS_MESSAGESIZE_SHORT_MESSAGE              2
#define FFSS_MESSAGESIZE_SEARCH                     6
#define FFSS_MESSAGESIZE_SEARCH_ANSWER              5
#define FFSS_MESSAGESIZE_SEARCH_FW                  7
#define FFSS_MESSAGESIZE_SEARCH_MASTER              5
#define FFSS_MESSAGESIZE_SEARCH_MASTER_ANSWER       5
#define FFSS_MESSAGESIZE_INDEX_ANSWER_SAMBA         7
#define FFSS_MESSAGESIZE_INDEX_ANSWER_SAMBA_2       5
#define FFSS_MESSAGESIZE_MASTER_CONNECTION          3
#define FFSS_MESSAGESIZE_STREAMING_OPEN             5
#define FFSS_MESSAGESIZE_STREAMING_OPEN_ANSWER      (4+2*2)
#define FFSS_MESSAGESIZE_STREAMING_CLOSE            3
#define FFSS_MESSAGESIZE_STREAMING_READ             (4+2*2)
#define FFSS_MESSAGESIZE_STREAMING_READ_ANSWER      (4+1*2)
#define FFSS_MESSAGESIZE_STREAMING_WRITE            (3+2*2)
#define FFSS_MESSAGESIZE_STREAMING_WRITE_ANSWER     (4+1*2)
#define FFSS_MESSAGESIZE_STREAMING_SEEK             (4+1*2)
#define FFSS_MESSAGESIZE_INIT_XFER                  3
#define FFSS_MESSAGESIZE_CANCEL_XFER                3
#define FFSS_MESSAGESIZE_DATA                       3
#define FFSS_MESSAGESIZE_DISCONNECT                 2
#define FFSS_MESSAGESIZE_ERROR                      7

#define FFSS_THREAD_SERVER 1
#define FFSS_THREAD_CLIENT 2
#define FFSS_THREAD_MASTER 3

/* FFSS State and Search answer defines */
#define FFSS_STATE_ON     1
#define FFSS_STATE_OFF    2
#define FFSS_STATE_QUIET  4
#define FFSS_STATE_ALL    7
#define FFSS_SEARCH_IS_FILE  32
#define FFSS_SEARCH_IS_SAMBA 64

/* FFSS Streaming defines */
#define FFSS_SEEK_SET  1
#define FFSS_SEEK_CUR  2
#define FFSS_SEEK_END  3
#define FFSS_STRM_OPEN_READ    1
#define FFSS_STRM_OPEN_WRITE   2
#define FFSS_STRM_OPEN_TEXT    4
#define FFSS_STRM_OPEN_BINARY  8

/* FFSS Error defines */
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
#define FFSS_ERROR_SOCKET_ERROR              22
#define FFSS_ERROR_ATTACK                    23
#define FFSS_ERROR_END_OF_FILE               24
#define FFSS_ERROR_IO_ERROR                  25
#define FFSS_ERROR_BAD_HANDLE                26
#define FFSS_ERROR_REMOTE_CLOSED             27
#define FFSS_ERROR_NOT_IMPLEMENTED          100
#define FFSS_ERROR_NO_ERROR                 666

/* FFSS Transfer Error defines */
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

/* FC_PEntry->Flags defines */
#define FFSS_FILE_DIRECTORY  1
#define FFSS_FILE_EXECUTABLE 2
#define FFSS_FILE_LINK       4

/* FFSS Compression defines */
#define FFSS_COMPRESSION_NONE  0
#define FFSS_COMPRESSION_ZLIB  1
#define FFSS_COMPRESSION_BZLIB 2
#define FFSS_BZLIB_BLOCK100K 4
#define FFSS_BZLIB_SMALL 1

/* FFSS Master index defines */
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

/* FFSS_GetFFSSOptions defines */
#define FFSS_OPTIONS_DEBUG        1
#define FFSS_OPTIONS_BZLIB        2
#define FFSS_OPTIONS_CONTEXT      4
#define FFSS_OPTIONS_MALLOC_TRACE 8
#define FFSS_OPTIONS_FTP          16
#define FFSS_OPTIONS_NO_CHECKSUM  32

/* Filter defines */
  /* Server */
#define FFSS_FILTER_CHAINS_SERVER_UDP_PACKET            0
#define FFSS_FILTER_CHAINS_SERVER_TCP_CONNECTION        1
#define FFSS_FILTER_CHAINS_SERVER_TCP_FTP_CONNECTION    2
  /* Client */
#define FFSS_FILTER_CHAINS_CLIENT_UDP_PACKET            0
  /* Master */
#define FFSS_FILTER_CHAINS_MASTER_UDP_PACKET            0
#define FFSS_FILTER_CHAINS_MASTER_TCP_CONNECTION_MASTER 1
  /* Actions */
#define FFSS_FILTER_ACTION_ACCEPT   1
#define FFSS_FILTER_ACTION_REJECT   2

/* QoS defines */
#define FFSS_QOS_CHECK_DELAY 50
  /* Chains */
#define FFSS_QOS_CHAINS_TRAFFIC_UPLOAD   0
#define FFSS_QOS_CHAINS_TRAFFIC_DOWNLOAD 1
#define FFSS_QOS_CHAINS_TRAFFIC_GLOBAL   2
  /* Criteria */
#define FFSS_QOS_CRITERIA_BYTES_PER_MSEC     1
#define FFSS_QOS_CRITERIA_BANDWIDTH_PER_CENT 2

/* Debug defines */
  /* Commons */
#define FFSS_DBGMSG_GLOBAL      0x00001
#define FFSS_DBGMSG_WARNING     0x00002
#define FFSS_DBGMSG_OUT_MSG     0x00004
#define FFSS_DBGMSG_PARSE_PROTO 0x00008
#define FFSS_DBGMSG_FATAL       0x00080
#define FFSS_DBGMSG_ALL         0x0FFFF

#define CRLF "\xD\xA"
#define FFSS_SUPER_MAGIC 0xFF55
#define FFSS_SHORT_MESSAGE_MAX 1024

/* ************************************************ */
/*                   DATA STRUCTURE                 */
/* ************************************************ */
typedef unsigned long int FFSS_Field;
#ifdef __unix__
typedef unsigned long long FFSS_LongField;
#else /* !__unix__ */
typedef unsigned __int64 FFSS_LongField;
#endif /* __unix__ */
typedef unsigned short int FFSS_THREAD_TYPE;

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

#ifndef FFSS_DRIVER
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
  SU_SOCKET sock;               /* Opened socket for file transfer */
  int  Port;                    /* Port sock is listening to (download) */
  FILE *fp;                     /* Opened file for reading/writing */
  char *FileName;               /* Remote file name */ /* NULL on server side */
  char *LocalPath;              /* Local path of file used for fopen */
  FFSS_LongField StartingPos;   /* Reading/Writing starting pos in the file */
  FFSS_LongField FileSize;      /* Size of the file */ /* Or size of the partial file if requested */
  FFSS_LongField XFerPos;       /* User readable : Current xfer pos (from 0 to FileSize, even if partial file is requested) */
  FFSS_LongField EndingPos;     /* Reading/Writing ending pos in the file */ /* 0 if full size requested */
  FFSS_THREAD_TYPE ThreadType;  /* Type of the thread (SERVER / CLIENT) */
  SU_PClientSocket Client;      /* SU_PClientSocket structure of the share connection we transfer from */ /* Do NOT free this, only a pointer !! */
  bool Cancel;                  /* If the transfer is to be canceled */
  void *User;                   /* User information */
  FFSS_TXFerInfo XI;            /* XFer info for xfer using connection socket */
  unsigned long int Throughput; /* Throughput in bytes/msec */
  FFSS_LongField UserInfo;      /* UserInfo passed to Download/Upload function, and passed back in Transfer Callbacks */
} FFSS_TTransfer, *FFSS_PTransfer;
#endif /* !FFSS_DRIVER */

typedef struct
{
  /* UDP callbacks */
  void (*OnPing)(struct sockaddr_in Master);
  void (*OnStateAnswer)(const char Domain[]);
  void (*OnServerSearch)(struct sockaddr_in Client);
  void (*OnSharesListing)(struct sockaddr_in Client,FFSS_LongField User);
  void (*OnIndexRequest)(struct sockaddr_in Master,FFSS_Field Port);
  void (*OnError)(FFSS_Field ErrorCode,const char Description[]);
  void (*OnMasterSearchAnswer)(struct sockaddr_in Master,FFSS_Field ProtocolVersion,const char Domain[],FFSS_LongField User);
  void (*OnShortMessage)(struct sockaddr_in Client,const char Message[]);

  /* TCP callbacks */
  void *(*OnShareConnection)(SU_PClientSocket Client,const char ShareName[],const char Login[],const char Password[],long int Compressions,FFSS_LongField User);
  void (*OnBeginTCPThread)(SU_PClientSocket Client,void *Info); /* Info is the (void *) returned by OnShareConnection */
  bool (*OnDirectoryListing)(SU_PClientSocket Client,const char Path[],FFSS_LongField User); /* Path IN the share (without share name) */
  bool (*OnRecursiveDirectoryListing)(SU_PClientSocket Client,const char Path[],FFSS_LongField User); /* Path IN the share (without share name) */
  bool (*OnDownload)(SU_PClientSocket Client,const char Path[],FFSS_LongField StartPos,FFSS_LongField EndingPos,int Port,FFSS_LongField User); /* Path IN the share (without share name) */
  bool (*OnUpload)(SU_PClientSocket Client,const char Path[],FFSS_LongField Size,int Port,FFSS_LongField User); /* Path IN the share (without share name) */
  bool (*OnRename)(SU_PClientSocket Client,const char Path[],const char NewPath[],FFSS_LongField User); /* Path IN the share (without share name) */
  bool (*OnCopy)(SU_PClientSocket Client,const char Path[],const char NewPath[],FFSS_LongField User); /* Path IN the share (without share name) */
  bool (*OnDelete)(SU_PClientSocket Client,const char Path[],FFSS_LongField User); /* Path IN the share (without share name) */
  bool (*OnMkDir)(SU_PClientSocket Client,const char Path[],FFSS_LongField User); /* Path IN the share (without share name) */
  void (*OnEndTCPThread)(void); /* Last callback raised before ending thread and freeing Client struct */
  bool (*OnDisconnect)(SU_PClientSocket Client);
  int (*OnSelect)(void); /* 0=Do timed-out select ; 1=don't do timed-out select, but sleep ; 2=don't do timed-out select, and continue */
  void (*OnIdleTimeout)(SU_PClientSocket Client);
  bool (*OnTransferFileWrite)(FFSS_PTransfer FT,const char Buf[],FFSS_Field Size,FFSS_LongField Offset); /* 'Offset' from FT->StartingPos */ /* True on success */
  void (*OnTransferFailed)(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download); /* UserInfo passed to Download/Upload function is in FT->UserInfo */
  void (*OnTransferSuccess)(FFSS_PTransfer FT,bool Download); /* UserInfo passed to Download/Upload function is in FT->UserInfo */
  void (*OnTransferActive)(FFSS_PTransfer FT,long int Amount,bool Download); /* UserInfo passed to Download/Upload function is in FT->UserInfo */
  void (*OnCancelXFer)(SU_PClientSocket Server,FFSS_Field XFerTag);
  void (*OnStrmOpen)(SU_PClientSocket Client,long int Flags,const char Path[],FFSS_LongField User); /* Path IN the share (without share name) */
  void (*OnStrmClose)(SU_PClientSocket Client,FFSS_Field Handle);
  void (*OnStrmRead)(SU_PClientSocket Client,FFSS_Field Handle,FFSS_LongField StartPos,long int Length,FFSS_LongField User);
  void (*OnStrmWrite)(SU_PClientSocket Client,FFSS_Field Handle,FFSS_LongField StartPos,const char Bloc[],long int BlocSize,FFSS_LongField User);
  void (*OnStrmSeek)(SU_PClientSocket Client,FFSS_Field Handle,long int Flags,FFSS_LongField Pos);

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
  void (*OnSharesListing)(const char IP[],const char **Names,const char **Comments,int NbShares,FFSS_LongField User);
  /* WARNING !! (char *) of the FM_PHost structure are pointers to STATIC buffer, and must be dupped ! */
  /* Except for the FM_PHost->IP that is dupped internaly, and if you don't use it, you MUST free it !! */
  void (*OnServerListingAnswer)(const char Domain[],int NbHost,SU_PList HostList,FFSS_LongField User); /* SU_PList of FM_PHost */
  void (*OnEndServerListingAnswer)(void);
  void (*OnDomainListingAnswer)(const char **Domains,int NbDomains,FFSS_LongField User); /* First domain is assumed to be domain from the answering master */
  void (*OnMasterSearchAnswer)(struct sockaddr_in Master,FFSS_Field ProtocolVersion,const char Domain[],FFSS_LongField User);
  /* Each IP from IPs table is dupped internaly, and if you don't use it, you MUST free it !! */
  void (*OnSearchAnswer)(const char Query[],const char Domain[],const char **Answers,char **IPs,FFSS_Field *ChkSums,FFSS_LongField *Sizes,int NbAnswers,FFSS_LongField User);
  void (*OnUDPError)(int ErrNum);
  void (*OnMasterError)(FFSS_Field ErrorCode,const char Descr[]);
  void (*OnShortMessage)(struct sockaddr_in Server,const char Message[]);

  /* TCP callbacks */
  void (*OnBeginTCPThread)(SU_PClientSocket Server);
  bool (*OnError)(SU_PClientSocket Server,FFSS_Field ErrorCode,const char Descr[],FFSS_LongField Value,FFSS_LongField User);
  bool (*OnDirectoryListingAnswer)(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries,FFSS_LongField User); /* FC_PEntry */
  bool (*OnRecursiveDirectoryListingAnswer)(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries,FFSS_LongField User); /* FC_PEntry */
  void (*OnEndTCPThread)(SU_PClientSocket Server); /* Last callback raised before ending thread and freeing Server struct */
  void (*OnIdleTimeout)(SU_PClientSocket Server);
  bool (*OnTransferFileWrite)(FFSS_PTransfer FT,const char Buf[],FFSS_Field Size,FFSS_LongField Offset); /* 'Offset' from FT->StartingPos */ /* True on success */
  void (*OnTransferFailed)(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download);
  void (*OnTransferSuccess)(FFSS_PTransfer FT,bool Download);
  void (*OnTransferActive)(FFSS_PTransfer FT,long int Amount,bool Download);
  FFSS_PTransfer (*OnInitXFer)(SU_PClientSocket Server,const char RequestedFileName[],FFSS_Field XFerTag); /* Returns PTransfer from RequestedFileName */
  FFSS_PTransfer (*OnData)(SU_PClientSocket Server,FFSS_Field XFerTag); /* Returns PTransfer from XFerTag */
  void (*OnStrmOpenAnswer)(SU_PClientSocket Client,const char Path[],FFSS_Field ErrorCode,FFSS_Field Handle,FFSS_LongField FileSize,FFSS_LongField User);
  void (*OnStrmReadAnswer)(SU_PClientSocket Client,FFSS_Field Handle,const char Bloc[],long int BlocSize,FFSS_Field ErrorCode,FFSS_LongField User);
  void (*OnStrmWriteAnswer)(SU_PClientSocket Client,FFSS_Field Handle,FFSS_Field ErrorCode,FFSS_LongField User);

  /* Fatal error */
  void (*OnFatalError)(void);
} FFSS_TClientCallbacks, *FFSS_PClientCallbacks;

typedef struct
{
  /* UDP callbacks */
  void (*OnState)(struct sockaddr_in Server,FFSS_Field State,const char Name[],const char OS[],const char Comment[]);
  void (*OnServerListing)(struct sockaddr_in Client,const char OS[],const char Domain[],long int Compressions,FFSS_LongField User);
  void (*OnClientServerFailed)(const char IP[]);
  void (*OnPong)(struct sockaddr_in Server,FFSS_Field State);
  void (*OnDomainListing)(struct sockaddr_in Client,FFSS_LongField User);
  void (*OnSearch)(struct sockaddr_in Client,int Port,const char Domain[],const char KeyWords[],long int Compressions,FFSS_LongField User);
  void (*OnMasterSearch)(struct sockaddr_in Client,bool Server,FFSS_LongField User);
  void (*OnIndexAnswer)(struct sockaddr_in Client,FFSS_Field CompressionType,FFSS_Field IndexSize,FFSS_Field FileTreeSize,FFSS_Field NodesSize,int Port);
  void (*OnIndexAnswerSamba)(struct sockaddr_in Client,FFSS_Field CompressionType,FFSS_Field IndexSize,FFSS_Field FileTreeSize,FFSS_Field NodesSize,int Port);

  /* TCP callbacks */
  void (*OnMasterConnected)(SU_PClientSocket Master);
  void (*OnMasterDisconnected)(SU_PClientSocket Master);
  void (*OnNewState)(FFSS_Field State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[]);
  void (*OnServerListingMaster)(SU_PClientSocket Master,const char OS[],const char Domain[],long int Compressions,FFSS_LongField User);
  void (*OnSearchForward)(SU_PClientSocket Master,const char ClientIP[],int Port,const char KeyWords[],long int Compressions,FFSS_LongField User);
} FFSS_TMasterCallbacks, *FFSS_PMasterCallbacks;

typedef struct
{
  FFSS_TServerCallbacks SCB;
  FFSS_TClientCallbacks CCB;
  FFSS_TMasterCallbacks MCB;
} FFSS_TCallbacks, *FFSS_PCallbacks;

typedef struct
{
  FFSS_Field Handle;
  char *Buf;
  long int BufSize;
} FC_THandle, *FC_PHandle;


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* ************************************************ */
/*                 EXTERN VARIABLES                 */
/* ************************************************ */
#ifndef FFSS_DRIVER
extern SU_THREAD_HANDLE FS_THR_UDP,FS_THR_TCP,FS_THR_TCP_FTP;
extern SU_THREAD_HANDLE FC_THR_UDP;
extern SU_THREAD_HANDLE FM_THR_UDP,FM_THR_TCP;
#endif /* !FFSS_DRIVER */
extern FFSS_TCallbacks FFSS_CB;
extern char *FFSS_MyIP;

extern char *FFSS_ErrorTable[];

void FFSS_PrintSyslog(int Level,char *Txt, ...);


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
bool FM_Init(int MasterPort,const char *User,const char *Group,const char *Interface);

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

#ifndef FFSS_DRIVER
/* Retrieve local IP of interface named IntName */
bool FFSS_GetMyIP(SU_PServerInfo SI,const char IntName[]);
#endif /* !FFSS_DRIVER */

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

/*
 * FFSS_GetOS
 *   Returns the tags of a word
 */
char *FFSS_GetOS(void);

/*
 * FFSS_CheckSizeofTTransfer
 *   Checks the size of structure in library and application.
 *   Should be called in every application using FFSS library to check size of data types
 *   If it returns false, you should check that struct member alignment is set to 4 (not the default in MSVC)
 */
bool FFSS_CheckSizeofTTransfer(int Size);


#ifndef FFSS_DRIVER
/* ************************************************ */
/*              FFSS TRANSFER FUNCTIONS             */
/* ************************************************ */

/* If FT_out is NULL, not filled */
/* EndingPos must be set to 0 for full file upload */
bool FFSS_UploadFile(SU_PClientSocket Client,const char FilePath[],FFSS_LongField StartingPos,FFSS_LongField EndingPos,int Port,void *User,bool UseConnSock,FFSS_LongField UserInfo,FFSS_PTransfer *FT_out);

/* RemotePath in the share */
/* If FT_out is NULL, not filled */
/* If LocalPath is NULL, OnTransferFileWrite will be called when a bloc is to be written to file */
/* EndingPos must be set to 0 for full file download */
bool FFSS_DownloadFile(SU_PClientSocket Server,const char RemotePath[],const char LocalPath[],FFSS_LongField StartingPos,FFSS_LongField EndingPos,void *User,bool UseConnSock,FFSS_LongField UserInfo,FFSS_PTransfer *FT_out);


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
/*  Client : The sin of the client                  */
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
/*  Client : The sin of the client                           */
/*  IP : The IP address of my server                         */
/*  ShareNames : A tab of the share names of my server       */
/*  ShareComments : A tab of the share comments of my server */
/*  NbShares : The number of shares of my server             */
/*  User : User pointer returned in message answer           */
bool FS_SendMessage_ServerSharesAnswer(struct sockaddr_in Client,const char IP[],const char **ShareNames,const char **ShareComments,int NbShares,FFSS_LongField User);

/* FS_SendMessage_Pong Function     */
/* Sends a PONG message to a master */
/*  Master : The sin of my master   */
/*  State : The state of my server  */
bool FS_SendMessage_Pong(struct sockaddr_in Master,int State);

/* FS_SendMessage_Error Function                   */
/* Sends an ERROR message to a client              */
/*  Client : The socket of the client              */
/*  Code : The error code to send                  */
/*  Descr : The description of the error code      */
/*  Value : Extra value depending on error code    */
/*  User : User pointer returned in message answer */
bool FS_SendMessage_Error(SU_SOCKET Client,FFSS_Field Code,const char Descr[],FFSS_LongField Value,FFSS_LongField User);

/* FS_SendMessage_DirectoryListingAnswer Function                     */
/* Sends a DIRECTORY LISTING ANSWER message to a client               */
/*  Client : The socket of the client                                 */
/*  Path : The path of the directory IN the share                     */
/*  Buffer : The buffer containing the nb of entries, and the entries */
/*  BufSize : The size of the buffer                                  */
/*  Compression : The type of compression to be applied to Buffer     */
/*  User : User pointer returned in message answer                    */
bool FS_SendMessage_DirectoryListingAnswer(SU_SOCKET Client,const char Path[],const char *Buffer,long int BufSize,int Compression,FFSS_LongField User);

/* FS_SendMessage_RecursiveDirectoryListingAnswer Function            */
/* Sends a RECURSIVE DIRECTORY LISTING ANSWER message to a client     */
/*  Client : The socket of the client                                 */
/*  Path : The path of the directory IN the share                     */
/*  Buffer : The buffer containing the nb of entries, and the entries */
/*  BufSize : The size of the buffer                                  */
/*  Compression : The type of compression to be applied to Buffer     */
/*  User : User pointer returned in message answer                    */
bool FS_SendMessage_RecursiveDirectoryListingAnswer(SU_SOCKET Client,const char Path[],const char *Buffer,long int BufSize,int Compression,FFSS_LongField User);

/* FS_SendMessage_InitXFer Function                        */
/* Sends an INIT XFER message to a client                  */
/*  Client : The socket of the client                      */
/*  Tag : The xfer tag that will be used when sending data */
/*  FileName : The name of the requested file              */
bool FS_SendMessage_InitXFer(SU_SOCKET Client,FFSS_Field Tag,const char FileName[]);

/* FS_SendMessage_MasterSearch Function            */
/* Sends a MASTER SEARCH message to broadcast      */
/*  User : User pointer returned in message answer */
bool FS_SendMessage_MasterSearch(FFSS_LongField User);

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
/*  User : User pointer returned in message answer    */
bool FS_SendMessage_StrmOpenAnswer(SU_SOCKET Client,const char Path[],FFSS_Field Code,FFSS_Field Handle,FFSS_LongField FileSize,FFSS_LongField User);

/* FS_SendMessage_StrmReadAnswer Function             */
/* Sends an STREAMING READ answer message to a client */
/*  Client : The socket of the client                 */
/*  Handle : The handle of the file                   */
/*  Buf : The buffer of datas                         */
/*  BlocLen : The length of the datas                 */
/*  Code : The error code to send                     */
/*  User : User pointer returned in message answer    */
bool FS_SendMessage_StrmReadAnswer(SU_SOCKET Client,FFSS_Field Handle,char *Buf,long int BlocLen,FFSS_Field Code,FFSS_LongField User);

/* FS_SendMessage_StrmWriteAnswer Function             */
/* Sends an STREAMING WRITE answer message to a client */
/*  Client : The socket of the client                  */
/*  Handle : The handle of the file                    */
/*  Code : The error code to send                      */
/*  User : User pointer returned in message answer     */
bool FS_SendMessage_StrmWriteAnswer(SU_SOCKET Client,FFSS_Field Handle,FFSS_Field Code,FFSS_LongField User);

/* FS_SendMessage_ShortMessage Function           */
/* Sends a SHORT MESSAGE message back to a client */
/*  Server : The sin of the client to respond to  */
/*  Message : Message to be sent to the server    */
bool FS_SendMessage_ShortMessage(struct sockaddr_in Client,const char Message[]);

#endif /* !FFSS_DRIVER */
/* ************************************************ */
/*                  CLIENT MESSAGES                 */
/* ************************************************ */

/* FC_SendMessage_ServerSearch Function       */
/* Sends a SERVER SEARCH message to broadcast */
bool FC_SendMessage_ServerSearch(void);

/* FC_SendMessage_SharesListing Function                       */
/* Sends a SHARES LISTING message to a server                  */
/*  Server : The name of the server we want the shares listing */
/*  User : User pointer returned in message answer             */
bool FC_SendMessage_SharesListing(const char Server[],FFSS_LongField User);

/* FC_SendMessage_ServerList Function                      */
/* Sends a SERVER LIST message to a master                 */
/*  Master : The name of my master, or NULL if none        */
/*  OS : The desired OS, or NULL if requesting all         */
/*  Domain : The desired domain, or NULL if requesting all */
/*  User : User pointer returned in message answer         */
bool FC_SendMessage_ServerList(const char Master[],const char OS[],const char Domain[],FFSS_LongField User);

/* FC_SendMessage_ShareConnect Function                  */
/* Sends a SHARE CONNECTION message to a server          */
/*  Server : The name of Server we wish to connect to    */
/*  ShareName : The Share Name we wish to connect to     */
/*  Login : The Login we may use (or NULL if none)       */
/*  Password : The Password we may use (or NULL if none) */
/*  User : User pointer returned in message answer       */
SU_PClientSocket FC_SendMessage_ShareConnect(const char Server[],const char ShareName[],const char Login[],const char Password[],FFSS_LongField User);

/* FC_SendMessage_DirectoryListing Function             */
/* Sends a DIRECTORY LISTING message to a server        */
/*  Server : The Server's structure we are connected to */
/*  Path : The path we request a listing                */
/*  User : User pointer returned in message answer      */
bool FC_SendMessage_DirectoryListing(SU_PClientSocket Server,const char Path[],FFSS_LongField User);

/* FC_SendMessage_RecursiveDirectoryListing Function       */
/* Sends a RECURSIVE DIRECTORY LISTING message to a server */
/*  Server : The Server's structure we are connected to    */
/*  Path : The path we request a listing                   */
/*  User : User pointer returned in message answer         */
bool FC_SendMessage_RecursiveDirectoryListing(SU_PClientSocket Server,const char Path[],FFSS_LongField User);

/* FC_SendMessage_Download Function                                 */
/* Sends a DOWNLOAD message to a server                             */
/*  Server : The Server's structure we are connected to             */
/*  Path : The path of requested file (in the share)                */
/*  StartingPos : The pos we want to download the file starting at  */
/*  EndingPos : The pos we want the download to stop (0=full file)  */
/*  UseConnSock : Use a separate socket/thread, or use the existing */
/*  User : User pointer returned in message answer                  */
int FC_SendMessage_Download(SU_PClientSocket Server,const char Path[],FFSS_LongField StartingPos,FFSS_LongField EndingPos,bool UseConnSock,FFSS_LongField User);

/* FC_SendMessage_Disconnect Function                   */
/* Sends an DISCONNECT message to a server              */
/*  Server : The Server's structure we are connected to */
void FC_SendMessage_Disconnect(SU_PClientSocket Server);

/* FC_SendMessage_CancelXFer Function                   */
/* Sends an CANCEL XFER message to a server             */
/*  Server : The Server's structure we are connected to */
/*  XFerTag : The tag of the xfer we want to cancel     */
void FC_SendMessage_CancelXFer(SU_PClientSocket Server,FFSS_Field XFerTag);

/* FC_SendMessage_DomainListing Function           */
/* Sends a DOMAIN LIST message to a master         */
/*  Master : The name of my master                 */
/*  User : User pointer returned in message answer */
bool FC_SendMessage_DomainListing(const char Master[],FFSS_LongField User);

/* FC_SendMessage_Search Function                          */
/* Sends a SEARCH message to a master                      */
/*  Master : The name of my master                         */
/*  Domain : The desired domain, or NULL if requesting all */
/*  Keys   : A String of keywords                          */
/*  User : User pointer returned in message answer         */
bool FC_SendMessage_Search(const char Master[],const char Domain[],const char Key[],FFSS_LongField User);

/* FC_SendMessage_MasterSearch Function       */
/* Sends a MASTER SEARCH message to broadcast */
/*  User : User pointer returned in message answer */
bool FC_SendMessage_MasterSearch(FFSS_LongField User);

/* FC_SendMessage_StrmOpen Function                     */
/* Sends an STREAMING OPEN message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Path : The path of the requested file               */
/*  Flags : The opening mode flags                      */
/*  User : User pointer returned in message answer      */
bool FC_SendMessage_StrmOpen(SU_PClientSocket Server,const char Path[],int Flags,FFSS_LongField User);

/* FC_SendMessage_StrmClose Function                     */
/* Sends an STREAMING CLOSE message to a server          */
/*  Server : The Server's structure we are connected to  */
/*  Handle : The handle of the file to close             */
bool FC_SendMessage_StrmClose(SU_PClientSocket Server,FFSS_Field Handle);

/* FC_SendMessage_StrmRead Function                     */
/* Sends an STREAMING READ message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  StartPos : The start position of the requested bloc */
/*  Length : Indicative length requested                */
/*  User : User pointer returned in message answer      */
bool FC_SendMessage_StrmRead(SU_PClientSocket Server,FFSS_Field Handle,FFSS_LongField StartPos,long int Length,FFSS_LongField User);

/* FC_SendMessage_StrmWrite Function                    */
/* Sends an STREAMING WRITE message to a server         */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  StartPos : The start position of the requested bloc */
/*  Buf : The buffer of datas                           */
/*  BlocLen : The length of the datas                   */
/*  User : User pointer returned in message answer      */
bool FC_SendMessage_StrmWrite(SU_PClientSocket Server,FFSS_Field Handle,FFSS_LongField StartPos,char *Buf,long int BlocLen,FFSS_LongField User);

/* FC_SendMessage_StrmSeek Function                     */
/* Sends an STREAMING SEEK message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  Flags : The flags for the seek operation            */
/*  StartPos : The position of the seek                 */
bool FC_SendMessage_StrmSeek(SU_PClientSocket Server,FFSS_Field Handle,int Flags,FFSS_LongField StartPos);

/* FC_SendMessage_ShortMessage Function                        */
/* Sends a SHORT MESSAGE message to a server                   */
/*  Server : The name of the server we want the shares listing */
/*  Message : Message to be sent to the server                 */
bool FC_SendMessage_ShortMessage(const char Server[],const char Message[]);


#ifndef FFSS_DRIVER
/* ************************************************ */
/*                  MASTER MESSAGES                 */
/* ************************************************ */

/* FM_SendMessage_Connect Function  */
/* Connects to a foreign master     */
/*  Master : The name of the Master */
SU_PClientSocket FM_SendMessage_Connect(const char Master[]);

/* FM_SendMessage_MasterConnection Function       */
/* Sends a MASTER CONNECTION message to a master  */
/* Must be the first message sent upon connection */
/*  Master : The socket of the Master             */
bool FM_SendMessage_MasterConnection(SU_SOCKET Master);

/* FM_SendMessage_Ping Function    */
/* Sends a PING message to servers */
bool FM_SendMessage_Ping();

/* FM_SendMessage_NewStatesMaster Function                          */
/* Sends a NEW STATES message to a master                           */
/*  Master : The socket of the master                               */
/*  Buffer : The buffer containing the nb of states, and the states */
/*  BufSize : The size of the buffer                                */
/*  Compression : The type of compression to be applied to Buffer   */
bool FM_SendMessage_NewStatesMaster(SU_SOCKET Master,const char *Buffer,long int BufSize,int Compression);

/* FM_SendMessage_ServerListing Function                            */
/* Sends a NEW STATES message to client                             */
/*  Client : The sin of the client                                  */
/*  Buffer : The buffer containing the nb of domains, and the hosts */
/*  BufSize : The size of the buffer                                */
/*  Compression : The type of compression to be applied to Buffer   */
/*  User : User pointer returned in message answer                  */
bool FM_SendMessage_ServerListing(struct sockaddr_in Client,const char *Buffer,long int BufSize,int Compression,FFSS_LongField User);

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

/* FM_SendMessage_ErrorMaster Function        */
/* Sends an ERROR message to a master         */
/*  Master : The socket of the Master         */
/*  Code : The error code to send             */
/*  Descr : The description of the error code */
bool FM_SendMessage_ErrorMaster(SU_SOCKET Master,FFSS_Field Code,const char Descr[]);

/* FM_SendMessage_ServerList Function              */
/* Sends a SERVER LIST message to a foreign master */
/*  Master : The socket of the Master              */
/*  User : User pointer returned in message answer */
bool FM_SendMessage_ServerList(SU_SOCKET Master,FFSS_LongField User);

/* FM_SendMessage_DomainListingAnswer Function     */
/* Sends a DOMAIN ANSWER message to client         */
/*  Client : The sin of the client                 */
/*  NbDomains : Nomber of domains                  */
/*  Domains : Array of strings (name of domains)   */
/*  User : User pointer returned in message answer */
bool FM_SendMessage_DomainListingAnswer(struct sockaddr_in Client,int NbDomains,char *Domains[],FFSS_LongField User);

/* FM_SendMessage_MasterSearchAnswer Function               */
/* Sends a MASTER SEARCH ANSWER message to client or server */
/*  Client : The sin of the client or the server            */
/*  Server : True if from server                            */
/*  Domain : The name of my domain                          */
/*  User : User pointer returned in message answer          */
bool FM_SendMessage_MasterSearchAnswer(struct sockaddr_in Client,bool Server,const char Domain[],FFSS_LongField User);

/* FM_SendMessage_SearchAnswer Function                                          */
/* Sends a SEARCH ANSWER message to client                                       */
/*  Client : The sin of the client                                               */
/*  Buffer : The buffer containing the query, the nb of answers, and the answers */
/*  BufSize : The size of the buffer                                             */
/*  Compression : The type of compression to be applied to Buffer                */
/*  User : User pointer returned in message answer                               */
bool FM_SendMessage_SearchAnswer(struct sockaddr_in Client,const char *Buffer,long int BufSize,int Compression,FFSS_LongField User);

/* FM_SendMessage_SearchForward Function                 */
/* Sends a SEARCH message to a foreign master            */
/*  Master : The socket of the Master                    */
/*  Client : The sin of the client                       */
/*  Compression   : Compressions supported by the client */
/*  Keys   : A String of keywords                        */
/*  User : User pointer returned in message answer       */
bool FM_SendMessage_SearchForward(SU_SOCKET Master,struct sockaddr_in Client,int Compression,const char Key[],FFSS_LongField User);


#endif /* !FFSS_DRIVER */
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
/* Packs a string (with len max char) into a message */
/*  Returns the new pos in the message buffer */
long int FFSS_PackString(char buf[],int pos,const char strn[],int len);
/* Packs a FFSS_Field into a message */
/*  Returns the new pos in the message buffer */
long int FFSS_PackField(char buf[],int pos,FFSS_Field val);
/* Same with LongField */
long int FFSS_PackLongField(char buf[],int pos,FFSS_LongField val);
void FFSS_PackIP(char *buf,const char IP[],int Type);

#ifndef DISABLE_ZLIB
bool FFSS_CompresseZlib(char *in,long int len_in,char *out,long int *len_out);
char *FFSS_UncompresseZlib(char *in,long int len_in,long int *len_out);
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
bool FFSS_CompresseBZlib(char *in,long int len_in,char *out,long int *len_out);
char *FFSS_UncompresseBZlib(char *in,long int len_in,long int *len_out);
#endif /* HAVE_BZLIB */
/* Adds a broadcast address to send to, when using bcast send */
void FFSS_AddBroadcastAddr(const char Addr[]);
/* Returns FFSS library compilation options (FFSS_OPTIONS_xx) */
int FFSS_GetFFSSOptions(void);


/* ************************************************************************* */
/* FILTER                                                                    */
/* ************************************************************************* */
#ifdef _WIN32
#define INADDR_GET_IP(x) x.S_un.S_addr
#else /* !_WIN32 */
#define INADDR_GET_IP(x) x.s_addr
#endif /* _WIN32 */
typedef unsigned int FFSS_FILTER_CHAIN;
typedef unsigned int FFSS_FILTER_ACTION;
/* Do NOT call any Filter function within the callback, or it will result as a dead lock */
typedef void (*FFSS_FILTER_CHAINS_ENUM_CB)(FFSS_FILTER_CHAIN Chain,const char Name[],FFSS_FILTER_ACTION Default); /* Strings are temporary buffers... copy them */
typedef void (*FFSS_FILTER_RULES_ENUM_CB)(const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[]); /* Strings are temporary buffers... copy them */
bool FFSS_Filter_AddRuleToChain_Head(FFSS_FILTER_CHAIN Chain,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[]);
bool FFSS_Filter_AddRuleToChain_Tail(FFSS_FILTER_CHAIN Chain,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[]);
bool FFSS_Filter_AddRuleToChain_Pos(FFSS_FILTER_CHAIN Chain,unsigned int Pos,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[]);
bool FFSS_Filter_SetDefaultActionOfChain(FFSS_FILTER_CHAIN Chain,FFSS_FILTER_ACTION Action);
bool FFSS_Filter_GetDefaultActionOfChain(FFSS_FILTER_CHAIN Chain,FFSS_FILTER_ACTION *Action);
bool FFSS_Filter_DelRuleFromChain_Pos(FFSS_FILTER_CHAIN Chain,unsigned int Pos);
bool FFSS_Filter_DelRuleFromChain_Name(FFSS_FILTER_CHAIN Chain,const char Name[]);
bool FFSS_Filter_ClearChain(FFSS_FILTER_CHAIN Chain);
bool FFSS_Filter_GetRuleOfChain_Pos(FFSS_FILTER_CHAIN Chain,unsigned int Pos,char **IP,char **Mask,FFSS_FILTER_ACTION *Action,char **Name); /* You must free returned strings */
bool FFSS_Filter_GetRuleOfChain_Name(FFSS_FILTER_CHAIN Chain,const char Name[],char **IP,char **Mask,FFSS_FILTER_ACTION *Action); /* You must free returned strings */
bool FFSS_Filter_EnumChains(FFSS_FILTER_CHAINS_ENUM_CB EnumCB);
bool FFSS_Filter_EnumRulesOfChain(FFSS_FILTER_CHAIN Chain,FFSS_FILTER_RULES_ENUM_CB EnumCB);
FFSS_FILTER_ACTION FFSS_Filter_GetActionOfChainFromIP(unsigned int Chain,unsigned long IP);
bool FFSS_Filter_Init(FFSS_THREAD_TYPE ThreadType);

typedef struct
{
  bool Initialized;
  bool (*AddRuleToChain_Head)(FFSS_FILTER_CHAIN Chain,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[]);
  bool (*AddRuleToChain_Tail)(FFSS_FILTER_CHAIN Chain,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[]);
  bool (*AddRuleToChain_Pos)(FFSS_FILTER_CHAIN Chain,unsigned int Pos,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[]);
  bool (*SetDefaultActionOfChain)(FFSS_FILTER_CHAIN Chain,FFSS_FILTER_ACTION Action);
  bool (*GetDefaultActionOfChain)(FFSS_FILTER_CHAIN Chain,FFSS_FILTER_ACTION *Action);
  bool (*DelRuleFromChain_Pos)(FFSS_FILTER_CHAIN Chain,unsigned int Pos);
  bool (*DelRuleFromChain_Name)(FFSS_FILTER_CHAIN Chain,const char Name[]);
  bool (*ClearChain)(FFSS_FILTER_CHAIN Chain);
  bool (*GetRuleOfChain_Pos)(FFSS_FILTER_CHAIN Chain,unsigned int Pos,char **IP,char **Mask,FFSS_FILTER_ACTION *Action,char **Name); /* You must free returned strings */
  bool (*GetRuleOfChain_Name)(FFSS_FILTER_CHAIN Chain,const char Name[],char **IP,char **Mask,FFSS_FILTER_ACTION *Action); /* You must free returned strings */
  bool (*EnumChains)(FFSS_FILTER_CHAINS_ENUM_CB EnumCB);
  bool (*EnumRulesOfChain)(FFSS_FILTER_CHAIN Chain,FFSS_FILTER_RULES_ENUM_CB EnumCB);
} FFSS_Filter_TApi, *FFSS_Filter_PApi;
extern FFSS_Filter_TApi FFSS_Filter_Api;


/* ************************************************************************* */
/* QOS                                                                       */
/* ************************************************************************* */
typedef unsigned int FFSS_QOS_CHAIN;
typedef unsigned int FFSS_QOS_CRITERIA;
typedef unsigned int FFSS_QOS_VALUE;
/* Do NOT call any QoS function within the callback, or it will result as a dead lock */
typedef void (*FFSS_QOS_CHAINS_ENUM_CB)(FFSS_QOS_CHAIN Chain,const char Name[]); /* Strings are temporary buffers... copy them */
typedef void (*FFSS_QOS_RULES_ENUM_CB)(const char IP[],const char Mask[],FFSS_QOS_CRITERIA Criteria,FFSS_QOS_VALUE Value,const char Name[]); /* Strings are temporary buffers... copy them */
bool FFSS_QoS_AddRuleToChain_Head(FFSS_QOS_CHAIN Chain,const char IP[],const char Mask[],FFSS_QOS_CRITERIA Criteria,FFSS_QOS_VALUE Value,const char Name[]);
bool FFSS_QoS_AddRuleToChain_Tail(FFSS_QOS_CHAIN Chain,const char IP[],const char Mask[],FFSS_QOS_CRITERIA Criteria,FFSS_QOS_VALUE Value,const char Name[]);
bool FFSS_QoS_AddRuleToChain_Pos(FFSS_QOS_CHAIN Chain,unsigned int Pos,const char IP[],const char Mask[],FFSS_QOS_CRITERIA Criteria,FFSS_QOS_VALUE Value,const char Name[]);
bool FFSS_QoS_DelRuleFromChain_Pos(FFSS_QOS_CHAIN Chain,unsigned int Pos);
bool FFSS_QoS_DelRuleFromChain_Name(FFSS_QOS_CHAIN Chain,const char Name[]);
bool FFSS_QoS_ClearChain(FFSS_QOS_CHAIN Chain);
bool FFSS_QoS_GetRuleOfChain_Pos(FFSS_QOS_CHAIN Chain,unsigned int Pos,char **IP,char **Mask,FFSS_QOS_CRITERIA *Criteria,FFSS_QOS_VALUE *Value,char **Name); /* You must free returned strings */
bool FFSS_QoS_GetRuleOfChain_Name(FFSS_QOS_CHAIN Chain,const char Name[],char **IP,char **Mask,FFSS_QOS_CRITERIA *Criteria,FFSS_QOS_VALUE *Value); /* You must free returned strings */
bool FFSS_QoS_EnumChains(FFSS_QOS_CHAINS_ENUM_CB EnumCB);
bool FFSS_QoS_EnumRulesOfChain(FFSS_QOS_CHAIN Chain,FFSS_QOS_RULES_ENUM_CB EnumCB);
unsigned long int FFSS_QoS_UpdateRate(FFSS_QOS_CHAIN Chain,unsigned long IP,signed long int ThroughputDelta,unsigned long int TimeDelta);
bool FFSS_QoS_Init(FFSS_LongField BandWidth); /* Bytes/msec */

typedef struct
{
  bool Initialized;
  FFSS_LongField BandWidth; /* Bytes/msec */
  bool (*AddRuleToChain_Head)(FFSS_QOS_CHAIN Chain,const char IP[],const char Mask[],FFSS_QOS_CRITERIA Criteria,FFSS_QOS_VALUE Value,const char Name[]);
  bool (*AddRuleToChain_Tail)(FFSS_QOS_CHAIN Chain,const char IP[],const char Mask[],FFSS_QOS_CRITERIA Criteria,FFSS_QOS_VALUE Value,const char Name[]);
  bool (*AddRuleToChain_Pos)(FFSS_QOS_CHAIN Chain,unsigned int Pos,const char IP[],const char Mask[],FFSS_QOS_CRITERIA Criteria,FFSS_QOS_VALUE Value,const char Name[]);
  bool (*DelRuleFromChain_Pos)(FFSS_QOS_CHAIN Chain,unsigned int Pos);
  bool (*DelRuleFromChain_Name)(FFSS_QOS_CHAIN Chain,const char Name[]);
  bool (*ClearChain)(FFSS_QOS_CHAIN Chain);
  bool (*GetRuleOfChain_Pos)(FFSS_QOS_CHAIN Chain,unsigned int Pos,char **IP,char **Mask,FFSS_QOS_CRITERIA *Criteria,FFSS_QOS_VALUE *Value,char **Name); /* You must free returned strings */
  bool (*GetRuleOfChain_Name)(FFSS_QOS_CHAIN Chain,const char Name[],char **IP,char **Mask,FFSS_QOS_CRITERIA *Criteria,FFSS_QOS_VALUE *Value); /* You must free returned strings */
  bool (*EnumChains)(FFSS_QOS_CHAINS_ENUM_CB EnumCB);
  bool (*EnumRulesOfChain)(FFSS_QOS_CHAIN Chain,FFSS_QOS_RULES_ENUM_CB EnumCB);
} FFSS_QoS_TApi, *FFSS_QoS_PApi;
extern FFSS_QoS_TApi FFSS_QoS_Api;


/* ************************************************************************* */
/* GLOBAL VARIABLES                                                          */
/* ************************************************************************* */
extern char *FFSS_MusicExt[FFSS_MUSIC_NB_EXT];
extern char *FFSS_VideoExt[FFSS_VIDEO_NB_EXT];
extern char *FFSS_ImageExt[FFSS_IMAGE_NB_EXT];
extern char *FFSS_DocExt[FFSS_DOC_NB_EXT];
extern char *FFSS_ExeExt[FFSS_EXE_NB_EXT];
extern char *FFSS_ZipExt[FFSS_ZIP_NB_EXT];

extern bool N_SyslogOn;
#ifndef FFSS_DRIVER
extern SU_PServerInfo FS_SI_UDP,FS_SI_OUT_UDP,FS_SI_TCP,FS_SI_TCP_FTP;
extern SU_PServerInfo FC_SI_OUT_UDP;
extern SU_PServerInfo FM_SI_UDP,FM_SI_OUT_UDP,FM_SI_TCP;
extern SU_THREAD_HANDLE FFSS_MainThread;
#endif /* !FFSS_DRIVER */
#ifdef _WIN32
#define FFSS_CU_REGISTRY_PATH "HKEY_CURRENT_USER\\Software\\FFSS\\"
#define FFSS_LM_REGISTRY_PATH "HKEY_LOCAL_MACHINE\\Software\\FFSS\\"
extern FILE *FFSS_LogFile;
#endif /* _WIN32 */

#ifndef DEBUG
#ifdef __unix__
#define SU_DBG_PrintDebug(x,...) /* */
#define SYSLOG_FN(x,y) syslog(x,y)
#else /* !__unix__ */
#define SU_DBG_PrintDebug() /* */
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

#if !defined(SU_MALLOC_TRACE) && !defined(FFSS_DRIVER)
void *FFSS_malloc(size_t size);
#define malloc FFSS_malloc
#endif /* !SU_MALLOC_TRACE && !FFSS_DRIVER */

#ifdef __cplusplus
}
/* Redefine c++ bool type */
#undef bool
#endif /* __cplusplus */

#endif /* !__FFSS_H__ */
