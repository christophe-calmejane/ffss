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
