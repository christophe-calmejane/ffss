/*
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
#include <fmp.h>

struct FMP_SFile *fil = NULL;
struct FMP_SSearch *sch = NULL;
volatile bool Complete = false;

void SearchAnswerStart(struct FMP_SSearch *Sch,FFSS_LongField UserTag)
{
  printf("Got answers for Tag %lld\n",UserTag);
}

void SearchAnswerItem(struct FMP_SSearch *Sch,FFSS_LongField UserTag,const char Name[],FFSS_LongField Size,FFSS_Field Count,struct FMP_SFile *File)
{
  printf("\tName=%50s - Size=%8lld - %d occurences\n",Name,Size,Count);
  if(fil == NULL)
  {
    fil = File;
  }
  else
    FMP_FreeFile(File);
}

void SearchAnswerEnd(struct FMP_SSearch *Sch,FFSS_LongField UserTag)
{
  printf("End of List for Tag %lld\n",UserTag);
  if(!FMP_StartDownload(fil,"./toto.out",1))
  {
    printf("Start download failed : %s\n",FMP_GetLastError());
    exit(-1);
  }
}


void OnError(struct FMP_SFile *File,FFSS_LongField UserTag,const char IP[],const char Path[],const char Name[],FFSS_Field ErrorCode) /* 'UserTag' and 'File' passed to FMP_StartDownload */
{
  printf("Error : %ld from %s for %s\n",ErrorCode,IP,Name);
}

void OnPacketReceived(struct FMP_SFile *File,FFSS_LongField UserTag,const FFSS_Field Idx,FFSS_Field SizeOfPacket) /* 'UserTag' and 'File' passed to FMP_StartDownload, 'Idx' of FileBloc */
{
  //printf("Packet received for Idx %ld from %s - Size = %ld\n",Idx,IP,SizeOfPacket);
}

void OnBlocComplete(struct FMP_SFile *File,FFSS_LongField UserTag,const FFSS_Field Idx) /* 'UserTag' and 'File' passed to FMP_StartDownload, 'Idx' of FileBloc */
{
  printf("Bloc complete for Idx %ld\n",Idx);
}

void OnDownloadComplete(struct FMP_SFile *File,FFSS_LongField UserTag,const char Name[]) /* 'UserTag' and 'File' passed to FMP_StartDownload */
{
  printf("Download complete for file %s\n",Name);
  FMP_FreeFile(File);
  FMP_FreeSearch(sch);
  Complete = true;
}


void handint(int sig)
{
  static bool done = false;

  if(!done)
  {
    FMP_Uninit();
    exit(0);
  }
}

/* Main */
int main(int argc, char *argv[])
{
  FMP_CB.OnSearchAnswerStart = SearchAnswerStart;
  FMP_CB.OnSearchAnswerItem = SearchAnswerItem;
  FMP_CB.OnSearchAnswerEnd = SearchAnswerEnd;

  FMP_CB.OnError = OnError;
  FMP_CB.OnPacketReceived = OnPacketReceived;
  FMP_CB.OnBlocComplete = OnBlocComplete;
  FMP_CB.OnDownloadComplete = OnDownloadComplete;

  SU_DBG_SetOutput(SU_DBG_OUTPUT_PRINTF);
  SU_DBG_SetOptions(true,true);
#ifdef DEBUG
  SU_DBG_SetFlags(FFSS_DBGMSG_ALL);
#endif /* DEBUG */

  if(argc != 2)
  {
    printf("Usage : %s \"<key words>\"\n",argv[0]);
    return -1;
  }
  if(!FMP_Init("orion"))
  {
    printf("Error : %s\n",FMP_GetLastError());
    return -2;
  }
#ifdef __unix__
  signal(SIGTSTP,handint);
#endif /* __unix__ */
  signal(SIGINT,handint);
  signal(SIGTERM,handint);

  sch = FMP_SearchFiles(argv[1],0);
  while(!Complete)
  {
    SU_SLEEP(1);
    /*printf("CALLING PAUSE\n");
    FMP_PauseDownload(fil);
    SU_SLEEP(15);
    printf("CALLING RESUME\n");
    FMP_ResumeDownload(fil);
    SU_SLEEP(20);*/
  }
  return 0;
}

