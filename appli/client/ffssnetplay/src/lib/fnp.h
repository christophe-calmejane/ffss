#ifndef __FNP_H__
#define __FNP_H__

#include <ffss.h>

#define FNP_NAME "FFSS Net Play Library"
#define FNP_VERSION "0.1"
#define FNP_PORT 10005
#define FNP_BUFFER_SIZE 65536

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef struct
{
  void (*OnEndOfFile)(void);
  void (*OnEndTCPThread)(void);
  void (*OnError)(int Code);
  /* Search answers */
  void (*OnSearchAnswerStart)(void);
  void (*OnSearchAnswerItem)(const char IP[],const char Path[]);
  void (*OnSearchAnswerEnd)(void);
} FNP_TCB, *FNP_PCB;

bool FNP_Init(const char Master[]);
bool FNP_Uninit(void);
void FNP_SearchFiles(const char Key[]);
char *FNP_GetLastError();
FFSS_LongField FNP_GetCurrentFilePos(void);
FFSS_LongField FNP_GetCurrentFileSize(void);
void FNP_PlayFile_Path(const char IP[],const char Path[]); /* Path is something like HOST/Share/Path/File */
void FNP_PlayFile(const char IP[],const char Share[],const char File[]);

extern FNP_TCB FNP_CB;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* !__FNP_H__ */
