#ifndef _DRAW_H_
#define _DRAW_H_

typedef struct
{
  char *Name;
  char *OS;
  char *Comment;
  char *IP;
  char *Domain;
  FFSS_Field State;
} FCQ_THost, *FCQ_PHost;

typedef struct
{
  char *Name;
  char *Comment;
} FCQ_TShare, *FCQ_PShare;

#define FCQ_MAX_STRINGS   5

#define FCQ_OP_STATUS      1
#define FCQ_OP_DIRLIST     2
#define FCQ_OP_ADDFILEDWL  3
#define FCQ_OP_ENDTHREAD   4
#define FCQ_OP_XFERFAILED  5
#define FCQ_OP_XFERSUCCESS 6
#define FCQ_OP_XFERACTIVE  7

typedef struct
{
  int Type;
  PConn Conn;
  char *Strings[FCQ_MAX_STRINGS];
  SU_PList Entries; /* FC_PEntry */
  long int Amount;
} FCQ_TOperation, *FCQ_POperation;

extern SU_SEM_HANDLE FCQ_Sem;
extern SU_PList FCQ_Hosts; /* FCQ_PHost */
extern SU_PList FCQ_Domains; /* char * */
extern SU_PList FCQ_Shares; /* FCQ_PShare */
extern SU_PList FCQ_Ops; /* FCQ_POperation */

gboolean FCQ_IdleFunc(gpointer data);
void FCQ_MoveFileToFailed(char *host_share,char *remote,char *local,char *size,char *error);
void FCQ_LaunchNextDownload(void);

#endif /* !_DRAW_H_ */
