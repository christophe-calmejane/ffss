#ifndef __FMP_H__
#define __FMP_H__

#include <ffss.h>

#define FMP_PATH_STATE_NOT_CONNECTED 0
#define FMP_PATH_STATE_CONNECTED     1
#define FMP_PATH_STATE_TRANSFERING   2
#define FMP_PATH_STATE_COMPLETED     3
#define FMP_PATH_STATE_PAUSED        4
#define FMP_PATH_STATE_CANCELED      5

#define FMP_BLOC_STATE_NOT_GOT  0
#define FMP_BLOC_STATE_GETTING  1
#define FMP_BLOC_STATE_GOT      2

#define FMP_ERRCODE_UNKNOWN_ERROR           1
#define FMP_ERRCODE_SEMAPHORE_CREATE_ERROR  2
#define FMP_ERRCODE_RESOURCE_NOT_AVAIL      3
#define FMP_ERRCODE_SHARE_DISABLED          4
#define FMP_ERRCODE_TOO_MANY_CONNECTIONS    5
#define FMP_ERRCODE_NEED_LOGIN_PASS         6
#define FMP_ERRCODE_HOST_DOWN               7
#define FMP_ERRCODE_SHARE_CONNECT_FAILED    8
#define FMP_ERRCODE_FILE_NOT_FOUND          9
#define FMP_ERRCODE_SERVER_IS_QUIET         10
#define FMP_ERRCODE_ACCESS_DENIED           11
#define FMP_ERRCODE_XFER_MODE_NOT_SUPPORTED 12
#define FMP_ERRCODE_TOO_MANY_TRANSFERS      13
#define FMP_ERRCODE_SERVER_INTERNAL_ERROR   14
#define FMP_ERRCODE_SERVER_CANNOT_CONNECT   15
#define FMP_ERRCODE_TRANSFER_MALLOC         16
#define FMP_ERRCODE_TRANSFER_TIMEOUT        17
#define FMP_ERRCODE_TRANSFER_SEND           18
#define FMP_ERRCODE_TRANSFER_EOF            19
#define FMP_ERRCODE_TRANSFER_READ_FILE      20
#define FMP_ERRCODE_TRANSFER_ACCEPT         21
#define FMP_ERRCODE_TRANSFER_OPENING        22
#define FMP_ERRCODE_TRANSFER_RECV           23
#define FMP_ERRCODE_TRANSFER_WRITE_FILE     24
#define FMP_ERRCODE_TRANSFER_FILE_BIGGER    25
#define FMP_ERRCODE_TRANSFER_CHECKSUM       26
#define FMP_ERRCODE_TRANSFER_CANCELED       27
#define FMP_ERRCODE_TRANSFER_SUSPENDED      28

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct FMP_SSearch;
struct FMP_SFile;
struct FMP_SPath;

typedef struct
{
  /* Search callbacks */
  void (*OnSearchAnswerStart)(struct FMP_SSearch *Sch,FFSS_LongField UserTag);
  void (*OnSearchAnswerItem)(struct FMP_SSearch *Sch,FFSS_LongField UserTag,const char Name[],FFSS_LongField Size,FFSS_Field Count,struct FMP_SFile *File); /* Use 'File' with StartDownload & FreeFile functions */
  void (*OnSearchAnswerEnd)(struct FMP_SSearch *Sch,FFSS_LongField UserTag);
  /* Download callbacks */
  void (*OnAssignBloc)(struct FMP_SFile *File,FFSS_LongField UserTag,const FFSS_Field Idx,const char IP[],const char Path[],const char Name[]); /* 'UserTag' and 'File' passed to FMP_StartDownload, 'Idx' of FileBloc, 'IP' from where we got the packet,'Path' in its share, 'Name' of the file */
  void (*OnUnAssignBloc)(struct FMP_SFile *File,FFSS_LongField UserTag,const FFSS_Field Idx); /* 'UserTag' and 'File' passed to FMP_StartDownload, 'Idx' of FileBloc */
  void (*OnError)(struct FMP_SFile *File,FFSS_LongField UserTag,const char IP[],const char Path[],const char Name[],FFSS_Field ErrorCode); /* 'UserTag' and 'File' passed to FMP_StartDownload */
  void (*OnPacketReceived)(struct FMP_SFile *File,FFSS_LongField UserTag,const FFSS_Field Idx,FFSS_Field SizeOfPacket); /* 'UserTag' and 'File' passed to FMP_StartDownload, 'Idx' of FileBloc */
  void (*OnBlocComplete)(struct FMP_SFile *File,FFSS_LongField UserTag,const FFSS_Field Idx); /* 'UserTag' and 'File' passed to FMP_StartDownload, 'Idx' of FileBloc */
  void (*OnDownloadComplete)(struct FMP_SFile *File,FFSS_LongField UserTag,const char Name[]); /* 'UserTag' and 'File' passed to FMP_StartDownload */
} FMP_TCB, *FMP_PCB;

typedef void (* FMP_LISTPATHS_CB)(const struct FMP_SFile *File,const struct FMP_SPath *Pth,const char IP[],const char Path[],FFSS_Field State,FFSS_LongField UserTag);

/* INIT / UNINIT FUNCTIONS */
bool FMP_Init(const char Master[]); /* False if failed : Use FMP_GetLastError for more infos */ /* Overrides all FFSS_CB.CCB values */
bool FMP_Uninit(void); /* False if failed : Use FMP_GetLastError for more infos */
void FMP_SetMaster(const char Master[]);
void FMP_SetBlocSize(FFSS_Field BS);
void FMP_SetReconnectDelay(FFSS_Field Delay); /* Delay in sec */

/* SEARCH FUNCTIONS */
struct FMP_SSearch *FMP_SearchFiles(const char Key[],FFSS_LongField UserTag); /* 'UserTag' is passed back in OnSearchAnswerXXX callbacks */
void FMP_FreeSearch(struct FMP_SSearch *Sch); /* When you free a Search, Files returned by OnSearchAnswerItem cannot be used anymore for StartDownload and ListPaths, but active File download still go on */
void FMP_FreeFile(struct FMP_SFile *File); /* Must be called after download finished (or canceled) */

/* DOWNLOAD FUNCTIONS */
bool FMP_StartDownload(struct FMP_SFile *File,const char DestFileName[],FFSS_LongField UserTag); /* 'UserTag' is passed back in OnPacketReceived */ /* False if failed : Use FMP_GetLastError for more infos */
void FMP_CancelDownload(struct FMP_SFile *File);
void FMP_PauseDownload(struct FMP_SFile *File);
void FMP_ResumeDownload(struct FMP_SFile *File);
void FMP_PausePath(struct FMP_SPath *Path);
void FMP_ResumePath(struct FMP_SPath *Path);

/* QUERY INFOS FUNCTIONS */
char *FMP_GetLastError(void);
void FMP_ListPaths(struct FMP_SFile *File,FFSS_LongField UserTag,FMP_LISTPATHS_CB Func); /* 'UserTag' is passed back in Func */
FFSS_Field FMP_GetBlocsCount(struct FMP_SFile *File);
bool FMP_GetBlocInfos(struct FMP_SFile *File,FFSS_Field Idx,FFSS_Field *State,FFSS_LongField *Pos); /* False if failed : Use FMP_GetLastError for more infos */
char *FMP_GetName(void);
char *FMP_GetVersion(void);
char *FMP_GetCopyright(void);

extern FMP_TCB FMP_CB;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* !__FMP_H__ */
