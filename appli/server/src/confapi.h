#ifndef __CONFAPI_H__
#define __CONFAPI_H__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "server.h"

typedef struct
{
  char *Name;
  char *Comment;
  char *Master;
  int  Idle;
  int  MaxConn;
  int  MaxXFerPerConn;
  bool FTP;
  int  FTP_MaxConn;
} FSCA_TGlobal, *FSCA_PGlobal;

typedef struct
{
  char *Name;
  char *Comment;
  bool  Writeable;
  bool  Private;
  int   MaxConn;
} FSCA_TShare, *FSCA_PShare;

typedef struct
{
  char *Name;
  char *Path;
  bool  Active;
  int   NbConns;
  int   NbXfers;
} FSCA_TShareLst, *FSCA_PShareLst;

typedef struct
{
  char *IP;
  int NbXfers;
  int NbStrms;
  SU_PList Xfers; /* FSCA_PFileInfo */
  SU_PList Strms; /* FSCA_PFileInfo */
} FSCA_TConn, *FSCA_PConn;

typedef struct
{
  char *Name;
  char *Pct;
} FSCA_TFileInfo, *FSCA_PFileInfo;

/* Get Infos */
FSCA_PGlobal FSCA_RequestGlobalInfo(SU_PClientSocket Client);
FSCA_PShare FSCA_RequestShareInfo(SU_PClientSocket Client,const char ShareName[],const char SharePath[]);
int FSCA_RequestStateInfo(SU_PClientSocket Client); /* -1 on error */
SU_PList FSCA_RequestConns(SU_PClientSocket Client,const char Path[]); /* FSCA_PConn */
SU_PList FSCA_RequestSharesList(SU_PClientSocket Client); /* FSCA_PShareLst */
bool FSCA_RequestEject(SU_PClientSocket Client,const char ShareName[]);

/* Set Infos */
bool FSCA_SetGlobalInfo(SU_PClientSocket Client,FSCA_PGlobal Gbl);
bool FSCA_SetStateInfo(SU_PClientSocket Client,int State);
bool FSCA_SetShareState(SU_PClientSocket Client,const char ShareName[],bool Active);
bool FSCA_RescanQuery(SU_PClientSocket Client,const char ShareName[]);
bool FSCA_AddShare(SU_PClientSocket Client,const char SharePath[],FSCA_PShare Share);
bool FSCA_DelShare(SU_PClientSocket Client,const char SharePath[]);
bool FSCA_SetShareInfo(SU_PClientSocket Client,const char SharePath[],FSCA_PShare Share);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* !__CONFAPI_H__ */
