#ifndef __CONFAPI_H__
#define __CONFAPI_H__

/* Undefine c++ bool type (unsigned char ?)
   Use SU_BOOL type in your appli, every time you use a ffss prototype
 */
#ifdef __cplusplus
#define bool SU_BOOL
#endif /* __cplusplus */

#include "server.h"

typedef struct
{
  char *Name;    /* Free it */
  char *Comment; /* Free it */
  char *Master;  /* Free it */
  int  Idle;
  int  MaxConn;
  int  MaxXFerPerConn;
  bool FTP;
  int  FTP_MaxConn;
} FSCA_TGlobal, *FSCA_PGlobal;

typedef struct
{
  char *Name;      /* Free it */
  char *Comment;   /* Free it */
  bool  Writeable;
  bool  Private;
  int   MaxConn;
  SU_PList Users; /* FS_PUser */
} FSCA_TShare, *FSCA_PShare;

typedef struct
{
  char *Name;     /* Free it */
  char *Path;     /* Free it */
  bool  Disabled;
  int   NbConns;
  int   NbXfers;
} FSCA_TShareLst, *FSCA_PShareLst;

typedef struct
{
  char *IP;       /* Free it */
  int NbXfers;
  int NbStrms;
  SU_PList Xfers; /* FSCA_PFileInfo */
  SU_PList Strms; /* FSCA_PFileInfo */
} FSCA_TConn, *FSCA_PConn;

typedef struct
{
  char *Name; /* Free it */
  char *Pct;  /* Free it */
} FSCA_TFileInfo, *FSCA_PFileInfo;

typedef struct
{
  void *Handle;    /* DO NOT FREE */
  char *Name;      /* Free it */
  char *Copyright; /* Free it */
  char *Version;   /* Free it */
  bool Startup;    /* If plugin is loaded at server startup */
} FSCA_TPluginInfo, *FSCA_PPluginInfo;

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* *************** Authentification *************** */
bool FSCA_RequestAuth(SU_PClientSocket Client,const char Login[],const char Pwd[]);

/* *************** Get Infos *************** */
/* You are responsible for freeing the lists and the structs */
FSCA_PGlobal FSCA_RequestGlobalInfo(SU_PClientSocket Client);
FSCA_PShare FSCA_RequestShareInfo(SU_PClientSocket Client,const char SharePath[]);
bool FSCA_RequestShareNameAvailable(SU_PClientSocket Client,const char ShareName[]);
int FSCA_RequestStateInfo(SU_PClientSocket Client); /* -1 on error */
SU_PList FSCA_RequestConns(SU_PClientSocket Client,const char Path[]); /* FSCA_PConn */
SU_PList FSCA_RequestSharesList(SU_PClientSocket Client); /* FSCA_PShareLst */
bool FSCA_RequestEject(SU_PClientSocket Client,const char ShareName[]);

/* *************** Set Infos *************** */
bool FSCA_SetGlobalInfo(SU_PClientSocket Client,FSCA_PGlobal Gbl);
bool FSCA_SetStateInfo(SU_PClientSocket Client,int State);
bool FSCA_SetShareState(SU_PClientSocket Client,const char ShareName[],bool Active);
bool FSCA_RescanQuery(SU_PClientSocket Client,const char ShareName[]);
bool FSCA_AddShare(SU_PClientSocket Client,const char SharePath[],FSCA_PShare Share);
bool FSCA_DelShare(SU_PClientSocket Client,const char SharePath[]);
bool FSCA_SetShareInfo(SU_PClientSocket Client,const char SharePath[],FSCA_PShare Share);

/* ***************  Plugins  *************** */
void *FSCA_Plugin_Load(SU_PClientSocket Client,const char Path[],bool AddToStartup);
bool FSCA_Plugin_Unload(SU_PClientSocket Client,void *Handle,bool RemoveFromStartup);
bool FSCA_Plugin_Configure(SU_PClientSocket Client,void *Handle,void *User);
SU_PList FSCA_Plugin_Enum(SU_PClientSocket Client); /* FSCA_PPluginInfo */ /* You are responsible for freeing the list and the structs */

#ifdef __cplusplus
}
/* Redefine c++ bool type */
#undef bool
#endif /* __cplusplus */

#endif /* !__CONFAPI_H__ */
