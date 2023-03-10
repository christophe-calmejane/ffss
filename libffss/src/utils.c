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
#ifndef FFSS_DRIVER
#include <ffss/ffss.h>
#include "utils.h"
#include <stdarg.h>

#else /* FFSS_DRIVER */
#include <ffss_tdi.h>
#define bool SU_BOOL
#undef FILE
#include <stdio.h>
#define sscanf FFSS_sscanf
void FFSS_sscanf(const char IP[],const char [],int *a,int *b,int *c,int *d)
{
  int tt[4],nbp=0,nbi=0;
  char *p=(char *)IP;

  if(!p)
    return;

  while(*p)
  {
    if(*p>='0' && *p<='9')
	{
      nbi++;tt[nbp]*=10;tt[nbp]+=*p-'0';
	}
    else if(*p=='.' && nbi)
	{
      nbp++;nbi=0;
	}
    if(nbi>3 || nbp>3)
      break;
    p++;
  }
  *a = tt[0];*b = tt[1];*c = tt[2];*d = tt[3];
}
#endif /* !FFSS_DRIVER */

FFSS_TCallbacks FFSS_CB;
bool N_SyslogOn = true;
char *FFSS_MyIP = NULL;

char *FFSS_MusicExt[FFSS_MUSIC_NB_EXT] = {"mp3","wav","au","wma","snd","xm","mid","mod","ra"} ; /* 9 */
char *FFSS_VideoExt[FFSS_VIDEO_NB_EXT] = {"mpg","mpeg","avi","vob","asf","rm","ram","mov"} ; /* 8 */
char *FFSS_ImageExt[FFSS_IMAGE_NB_EXT] = {"bmp","tiff","jpg","jpeg","png","gif","pcx","pcd"} ; /* 8 */
char *FFSS_DocExt[FFSS_DOC_NB_EXT] = {"htm","html","txt","doc","pdf","nfo","tex","ps"} ; /* 8 */
char *FFSS_ExeExt[FFSS_EXE_NB_EXT] = {"exe","com","bat","sys","dll"} ; /* 5 */
char *FFSS_ZipExt[FFSS_ZIP_NB_EXT] = {"zip","arj","rar","tar","gz","jar","ace","bz2","deb","rpm"} ; /* 10 */

char *FFSS_ErrorTable[]={"Nothing","Protocol version mismatch","Resource not available","Wrong login/password, or not specified","Too many connections","File or directory not found","Access denied","Not enough space","Cannot connect","Internal error","Too many active transfers","Directory not empty","File already exists","Idle time out","Quiet mode","Share is disabled","Ejected from share","Your message will overflow my receipt buffer","Requested transfer mode not supported","Please resend last UDP message","Bad search request","Too many answers","Socket Error","Possible DoS attack","End of file","I/O error","Bad handle","Remote host closed connection"};

#ifndef FFSS_DRIVER
SU_THREAD_HANDLE FFSS_MainThread;
SU_THREAD_KEY_HANDLE FFSS_Context_tskey;
SU_THREAD_ONCE_HANDLE FFSS_Context_once = SU_THREAD_ONCE_INIT;
SU_PList FFSS_Broadcast = NULL; /* char * */

typedef struct
{
  int Line;
  char File[512];
} FFSS_TContextSpecific, *FFSS_PContextSpecific;
#ifdef _WIN32
FILE *FFSS_LogFile = NULL;
#endif /* _WIN32 */

void FFSS_Context_destroyts(void *ptr)
{
  FFSS_PContextSpecific ts = (FFSS_PContextSpecific) ptr;

  free(ts);
}

void FFSS_Context_tsinitkey(void)
{
  SU_CreateThreadKey(&FFSS_Context_tskey,&FFSS_Context_once,FFSS_Context_destroyts);
}

FFSS_PContextSpecific FFSS_Context_GetThreadSpecific(void)
{
  FFSS_PContextSpecific ts;

  SU_THREAD_ONCE(FFSS_Context_once,FFSS_Context_tsinitkey);
  ts = (FFSS_PContextSpecific) SU_THREAD_GET_SPECIFIC(FFSS_Context_tskey);
  if(ts == NULL)
  {
    ts = (FFSS_PContextSpecific) malloc(sizeof(FFSS_TContextSpecific));
    memset(ts,0,sizeof(FFSS_TContextSpecific));
    SU_THREAD_SET_SPECIFIC(FFSS_Context_tskey,ts);
  }
  return ts;
}

void set_context(char *file, int line)
{
  FFSS_PContextSpecific ts = FFSS_Context_GetThreadSpecific();
  ts->Line = line;
  SU_strcpy(ts->File,file,sizeof(ts->File));
}

void FFSS_handle_SIGNAL(int signal)
{
  if(signal == SIGSEGV)
  {
    FFSS_PContextSpecific ts = FFSS_Context_GetThreadSpecific();
    FFSS_PrintSyslog(LOG_ERR,"FFSS Crashed in thread %x (%d) :-(. Check: %s:%d\n",SU_THREAD_SELF,getpid(),ts->File,ts->Line);
    if(FFSS_MainThread != (SU_THREAD_HANDLE)SU_THREAD_SELF)
    {
      FFSS_PrintSyslog(LOG_ERR,"I (%d) am not the main thread (%d). Signaling the main thread\n",SU_THREAD_SELF,FFSS_MainThread);
#ifdef __linux__
      //pthread_kill_other_threads_np();
#endif /* __linux__ */
      SU_TermThread(FFSS_MainThread);
    }
#ifdef _WIN32
    SU_CloseLogFile(FFSS_LogFile);
#endif /* _WIN32 */
    abort();
  }
}
#endif /* !FFSS_DRIVER */

bool FFSS_CheckSizeofTTransfer(int Size)
{
  int InternalSize = sizeof(FFSS_TTransfer);
  return Size == InternalSize;
}

/* Unpacks a string from a message, checking if the string really terminates (prevents DoS attacks) */
/*  Returns the string, or NULL if there is a problem */
char *FFSS_UnpackString(const char beginning[], const char buf[], size_t len, ptrdiff_t *new_pos)
{
  ptrdiff_t pos = buf - beginning;
	while(pos < (ptrdiff_t)len)
  {
    if(beginning[pos] == 0)
    {
      *new_pos = pos + 1;
      return (char *)buf;
    }
    pos++;
  }
  FFSS_PrintSyslog(LOG_WARNING,"String out of message... DoS attack ?\n");
  return NULL;
}

/* Unpacks a FFSS_Field from a message, checking if the FFSS_Field is fully in the message (prevents DoS attacks) */
/*  Returns the FFSS_Field, or 0 if there is a problem */
FFSS_Field FFSS_UnpackField(const char beginning[], const char buf[], size_t len, ptrdiff_t *new_pos)
{
	ptrdiff_t pos = buf - beginning;
  FFSS_Field ret = 0;

  if((pos+sizeof(FFSS_Field)) <= len)
  {
    *new_pos = pos + sizeof(FFSS_Field);
#ifdef WORDS_BIGENDIAN
    *(((char *)&ret)+0) = buf[3];
    *(((char *)&ret)+1) = buf[2];
    *(((char *)&ret)+2) = buf[1];
    *(((char *)&ret)+3) = buf[0];
#else /* !WORDS_BIGENDIAN */
#ifdef USE_ALIGNED_WORD
    *(((char *)&ret)+0) = buf[0];
    *(((char *)&ret)+1) = buf[1];
    *(((char *)&ret)+2) = buf[2];
    *(((char *)&ret)+3) = buf[3];
#else /* !USE_ALIGNED_WORD */
    ret = *(FFSS_Field *)buf;
#endif /* USE_ALIGNED_WORD */
#endif /* WORDS_BIGENDIAN */
  }
  else
    FFSS_PrintSyslog(LOG_WARNING,"LongInt out of message... DoS attack ?\n");
  return ret;
}

/* Unpacks a FFSS_LongField from a message, checking if the FFSS_Field is fully in the message (prevents DoS attacks) */
/*  Returns the FFSS_Field, or 0 if there is a problem */
FFSS_LongField FFSS_UnpackLongField(const char beginning[], const char buf[], size_t len, ptrdiff_t *new_pos)
{
	ptrdiff_t pos = buf - beginning;
  FFSS_LongField ret = 0;

  if((pos+sizeof(FFSS_LongField)) <= len)
  {
    *new_pos = pos + sizeof(FFSS_LongField);
#ifdef WORDS_BIGENDIAN
    *(((char *)&ret)+0) = buf[7];
    *(((char *)&ret)+1) = buf[6];
    *(((char *)&ret)+2) = buf[5];
    *(((char *)&ret)+3) = buf[4];
    *(((char *)&ret)+4) = buf[3];
    *(((char *)&ret)+5) = buf[2];
    *(((char *)&ret)+6) = buf[1];
    *(((char *)&ret)+7) = buf[0];
#else /* !WORDS_BIGENDIAN */
#ifdef USE_ALIGNED_WORD
    *(((char *)&ret)+0) = buf[0];
    *(((char *)&ret)+1) = buf[1];
    *(((char *)&ret)+2) = buf[2];
    *(((char *)&ret)+3) = buf[3];
    *(((char *)&ret)+4) = buf[4];
    *(((char *)&ret)+5) = buf[5];
    *(((char *)&ret)+6) = buf[6];
    *(((char *)&ret)+7) = buf[7];
#else /* !USE_ALIGNED_WORD */
    ret = *(FFSS_LongField *)buf;
#endif /* USE_ALIGNED_WORD */
#endif /* WORDS_BIGENDIAN */
  }
  else
    FFSS_PrintSyslog(LOG_WARNING,"LongLongInt out of message... DoS attack ?\n");
  return ret;
}

void FFSS_UnpackIP(const char beginning[], char *buf, size_t len, ptrdiff_t *new_pos, char buf_out[], int Type)
{
  int a,b,c,d;
	ptrdiff_t pos = buf - beginning;

  buf_out[0] = 0;
	if((pos + FFSS_IP_FIELD_SIZE) <= (ptrdiff_t)len)
  {
    switch(Type)
    {
      case FFSS_IP_V4 :
        if(buf != NULL)
        {
          a = (unsigned char)buf[0];
          b = (unsigned char)buf[1];
          c = (unsigned char)buf[2];
          d = (unsigned char)buf[3];
          snprintf(buf_out,16,"%d.%d.%d.%d",a,b,c,d);
          buf_out[15] = 0;
        }
        break;
      case FFSS_IP_V6 :
        break;
      default :
        SU_DBG_PrintDebug(FFSS_DBGMSG_FATAL,"Unknown IP type : %d",Type);
    }
    *new_pos = pos + FFSS_IP_FIELD_SIZE;
  }
  else
  {
    FFSS_PrintSyslog(LOG_WARNING,"IP out of message... DoS attack ?\n");
  }
}

/* Packs a string (with len max char) into a message */
/*  Returns the new pos in the message buffer */
ptrdiff_t FFSS_PackString(char buf[], ptrdiff_t pos, const char strn[], size_t len)
{
  SU_strcpy(buf+pos,strn,len);
  return pos + len;
}

/* Packs a FFSS_Field into a message */
/*  Returns the new pos in the message buffer */
ptrdiff_t FFSS_PackField(char buf[],ptrdiff_t pos,FFSS_Field val)
{
#ifdef WORDS_BIGENDIAN
  buf[pos+0] = *(((char *)&val)+3);
  buf[pos+1] = *(((char *)&val)+2);
  buf[pos+2] = *(((char *)&val)+1);
  buf[pos+3] = *(((char *)&val)+0);
#else /* !WORDS_BIGENDIAN */
#ifdef USE_ALIGNED_WORD
  buf[pos+0] = *(((char *)&val)+0);
  buf[pos+1] = *(((char *)&val)+1);
  buf[pos+2] = *(((char *)&val)+2);
  buf[pos+3] = *(((char *)&val)+3);
#else /* !USE_ALIGNED_WORD */
  *(FFSS_Field *)(buf+pos) = val;
#endif /* USE_ALIGNED_WORD */
#endif /* WORDS_BIGENDIAN */
  return pos + sizeof(FFSS_Field);
}

/* Packs a FFSS_LongField into a message */
/*  Returns the new pos in the message buffer */
ptrdiff_t FFSS_PackLongField(char buf[],ptrdiff_t pos,FFSS_LongField val)
{
#ifdef WORDS_BIGENDIAN
  buf[pos+0] = *(((char *)&val)+7);
  buf[pos+1] = *(((char *)&val)+6);
  buf[pos+2] = *(((char *)&val)+5);
  buf[pos+3] = *(((char *)&val)+4);
  buf[pos+4] = *(((char *)&val)+3);
  buf[pos+5] = *(((char *)&val)+2);
  buf[pos+6] = *(((char *)&val)+1);
  buf[pos+7] = *(((char *)&val)+0);
#else /* !WORDS_BIGENDIAN */
#ifdef USE_ALIGNED_WORD
  buf[pos+0] = *(((char *)&val)+0);
  buf[pos+1] = *(((char *)&val)+1);
  buf[pos+2] = *(((char *)&val)+2);
  buf[pos+3] = *(((char *)&val)+3);
  buf[pos+4] = *(((char *)&val)+4);
  buf[pos+5] = *(((char *)&val)+5);
  buf[pos+6] = *(((char *)&val)+6);
  buf[pos+7] = *(((char *)&val)+7);
#else /* !USE_ALIGNED_WORD */
  *(FFSS_LongField *)(buf+pos) = val;
#endif /* USE_ALIGNED_WORD */
#endif /* WORDS_BIGENDIAN */
  return pos + sizeof(FFSS_LongField);
}

void FFSS_PackIP(char *buf,const char IP[],int Type)
{
  int a,b,c,d;

  switch(Type)
  {
    case FFSS_IP_V4 :
      if(IP != NULL)
      {
        sscanf(IP,"%d.%d.%d.%d",&a,&b,&c,&d);
      }
      else
      {
        a = b = c = d = 0;
      }
      buf[0] = (unsigned char)a;
      buf[1] = (unsigned char)b;
      buf[2] = (unsigned char)c;
      buf[3] = (unsigned char)d;
      break;
    case FFSS_IP_V6 :
      break;
    default :
      SU_DBG_PrintDebug(FFSS_DBGMSG_FATAL,"Unknown IP type : %d",Type);
  }
}

FFSS_Field FFSS_ComputeChecksum(FFSS_Field Old, const char Buf[], size_t Len)
{
#ifdef DISABLE_CHECKSUM
  return 1;
#else /* !DISABLE_CHECKSUM */
  if(Buf == NULL)
    return adler32(0L, Z_NULL, 0);
  return adler32(Old, Buf, Len);
#endif /* DISABLE_CHECKSUM */
}

#ifndef FFSS_DRIVER
bool FFSS_GetMyIP(SU_PServerInfo SI,const char IntName[])
{
#ifdef __unix__
  struct ifconf ic;
  int i,soi;

  soi = sizeof(struct ifreq); /* Size of an interface */
  ic.ifc_len = 10 * soi;      /* Allocate buffer for 10 interfaces */
  ic.ifc_buf = (CADDR_T) malloc(ic.ifc_len);
  if(ioctl(SI->sock,SIOCGIFCONF,&ic) == -1)
    return false;
  for(i=0;i<(ic.ifc_len/soi);i++)
  {
    if(strcmp(ic.ifc_req[i].ifr_name,IntName) == 0)
    {
      FFSS_MyIP = strdup(inet_ntoa(((struct sockaddr_in *)(&ic.ifc_req[i].ifr_addr))->sin_addr));
      SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Using interface %s, with ip %s",IntName,FFSS_MyIP);
      return true;
    }
  }
  return false;
#else /* !__unix__ */
  char Buf[1024];

  if(gethostname(Buf,sizeof(Buf)) != 0)
    return false;
  FFSS_MyIP = strdup(SU_AdrsOfPort(Buf));
  return true;
#endif /* __unix__ */
}

void FFSS_AddBroadcastAddr(const char Addr[])
{
  FFSS_Broadcast = SU_AddElementTail(FFSS_Broadcast,strdup(Addr));
}

int FFSS_SendBroadcast(SU_PServerInfo SI, char *Text, size_t len, char *port)
{
  int res=(int)len,v;
  SU_PList Ptr;

  if(FFSS_Broadcast == NULL)
    return SU_UDPSendBroadcast(SI,Text,(int)len,port);
  else
  {
    Ptr = FFSS_Broadcast;
    while(Ptr != NULL)
    {
      v = SU_UDPSendToAddr(SI,Text,(int)len,(char *)Ptr->Data,port);
      if(v == SOCKET_ERROR)
        res = v;
      Ptr = Ptr->Next;
    }
  }
  return res;
}
#endif /* !FFSS_DRIVER */

#ifndef DISABLE_ZLIB
bool FFSS_CompresseZlib(char *in,size_t len_in,char *out,size_t *len_out)
{
  int res;

  res = compress(out,len_out,in,len_in);
  SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Using Z compression (before=%ld - after=%ld)",len_in,*len_out);
  return (res == Z_OK);
}

char *FFSS_UncompresseZlib(char *in,size_t len_in,size_t *len_out)
{
  char *out,*old_out;
	size_t len;
  int res;

  len = len_in*3;

  out = (char *) malloc(len);
  if(out == NULL)
    return NULL;

  *len_out = len;
  res = uncompress(out,len_out,in,len_in);
  while(res != Z_OK)
  {
    if((res == Z_MEM_ERROR) || (res == Z_DATA_ERROR))
    {
      free(out);
      return NULL;
    }
    len *= 2;
    old_out = out;
    out = (char *) realloc(out,len);
    if(out == NULL)
    {
      free(old_out);
      return NULL;
    }
    *len_out = len;
    res = uncompress(out,len_out,in,len_in);
  }
  return out;
}
#endif /* !DISABLE_ZLIB */

#ifdef HAVE_BZLIB
bool FFSS_CompresseBZlib(char *in,size_t len_in,char *out,size_t *len_out)
{
  int res;

  res = BZ2_bzBuffToBuffCompress(out,(unsigned int *)len_out,in,len_in,FFSS_BZLIB_BLOCK100K,0,0);
  SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Using BZ compression (before=%ld - after=%ld)",len_in,*len_out);
  return (res == BZ_OK);
}

char *FFSS_UncompresseBZlib(char *in,size_t len_in,size_t *len_out)
{
  char *out,*old_out;
	size_t len;
  int res;

  len = len_in*3;

  out = (char *) malloc(len);
  if(out == NULL)
    return NULL;

  *len_out = len;
  res = BZ2_bzBuffToBuffDecompress(out,(unsigned int *)len_out,in,len_in,FFSS_BZLIB_SMALL,0);
  while(res != BZ_OK)
  {
    if((res == BZ_MEM_ERROR) || (res == BZ_DATA_ERROR) || (res == BZ_DATA_ERROR_MAGIC) || (res == BZ_UNEXPECTED_EOF))
    {
      free(out);
      return NULL;
    }
    len *= 2;
    old_out = out;
    out = (char *) realloc(out,len);
    if(out == NULL)
    {
      free(old_out);
      return NULL;
    }
    *len_out = len;
    res = BZ2_bzBuffToBuffDecompress(out,(unsigned int *)len_out,in,len_in,FFSS_BZLIB_SMALL,0);
  }
  return out;
}
#endif /* HAVE_BZLIB */

/*
 * FFSS_GetFileTags
 *   Returns the tags of a file
 */
unsigned char FFSS_GetFileTags(const char *FileName)  /* <-- Full name of the file */
{
  char *dotPos;
  int i;

  dotPos = strrchr(FileName,'.');
  if(dotPos != NULL)
  {
    dotPos += 1; /* next one */
    for(i=0;i<FFSS_MUSIC_NB_EXT;i++)
      if(SU_strcasecmp(FFSS_MusicExt[i],dotPos)) return FFSS_FILE_TAGS_MUSIC;
    for(i=0;i<FFSS_VIDEO_NB_EXT;i++)
      if(SU_strcasecmp(FFSS_VideoExt[i],dotPos)) return FFSS_FILE_TAGS_VIDEO;
    for(i=0;i<FFSS_IMAGE_NB_EXT;i++)
      if(SU_strcasecmp(FFSS_ImageExt[i],dotPos)) return FFSS_FILE_TAGS_IMAGE;
    for(i=0;i<FFSS_DOC_NB_EXT;i++)
      if(SU_strcasecmp(FFSS_DocExt[i],dotPos)) return FFSS_FILE_TAGS_DOC;
    for(i=0;i<FFSS_EXE_NB_EXT;i++)
      if(SU_strcasecmp(FFSS_ExeExt[i],dotPos)) return FFSS_FILE_TAGS_EXE;
    for(i=0;i<FFSS_ZIP_NB_EXT;i++)
      if(SU_strcasecmp(FFSS_ZipExt[i],dotPos)) return FFSS_FILE_TAGS_ZIP;
  }
  /* nothing found matching */
  return FFSS_FILE_TAGS_NOTHING;
}

/*
 * FM_GetWordTags
 *   Returns the tags of a word
 */
unsigned char FFSS_GetWordTags(const char *Word)  /* <-- word to check for extension */
{
  int i;

  for(i=0;i<FFSS_MUSIC_NB_EXT;i++)
    if(SU_strcasecmp(FFSS_MusicExt[i],Word)) return FFSS_FILE_TAGS_MUSIC;
  for(i=0;i<FFSS_VIDEO_NB_EXT;i++)
    if(SU_strcasecmp(FFSS_VideoExt[i],Word)) return FFSS_FILE_TAGS_VIDEO;
  for(i=0;i<FFSS_IMAGE_NB_EXT;i++)
    if(SU_strcasecmp(FFSS_ImageExt[i],Word)) return FFSS_FILE_TAGS_IMAGE;
  for(i=0;i<FFSS_DOC_NB_EXT;i++)
    if(SU_strcasecmp(FFSS_DocExt[i],Word)) return FFSS_FILE_TAGS_DOC;
  for(i=0;i<FFSS_EXE_NB_EXT;i++)
    if(SU_strcasecmp(FFSS_ExeExt[i],Word)) return FFSS_FILE_TAGS_EXE;
  for(i=0;i<FFSS_ZIP_NB_EXT;i++)
    if(SU_strcasecmp(FFSS_ZipExt[i],Word)) return FFSS_FILE_TAGS_ZIP;
  /* nothing found matching */
  return FFSS_FILE_TAGS_NOTHING;
}

#ifndef FFSS_DRIVER
#undef malloc
void *FFSS_malloc(size_t size)
{
  void *ptr;

  ptr = malloc(size);
  if(ptr == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Malloc of size %d failed ! Not enough memory... exiting\n",size);
    if(FFSS_MainThread != (SU_THREAD_HANDLE)SU_THREAD_SELF)
    {
#ifdef __linux__
      //pthread_kill_other_threads_np();
#endif /* __linux__ */
      SU_TermThread(FFSS_MainThread);
    }
    abort();
  }
  return ptr;
}
#endif /* !FFSS_DRIVER */

#ifdef FFSS_DRIVER
#include <stdarg.h>
ULONG _cdecl DbgPrint(PCH Format,...);
#define printf DbgPrint
char FFSS_SlogStr[4096];
void FFSS_PrintSyslog(int Level,char *Txt, ...)
{
  va_list argptr;

  va_start(argptr,Txt);
  _vsnprintf(FFSS_SlogStr,sizeof(FFSS_SlogStr),Txt,argptr);
  va_end(argptr);
  printf("FFSS(SYSLOG) : %s",FFSS_SlogStr);
}
#else /* !FFSS_DRIVER */

void FFSS_PrintSyslog(int Level,char *Txt, ...)
{
  va_list argptr;
  char Str[4096];

#ifdef DEBUG
  if(!N_SyslogOn)
    return;
#endif /* DEBUG */
  va_start(argptr,Txt);
#ifdef _WIN32
  _vsnprintf(Str,sizeof(Str),Txt,argptr);
#else /* !_WIN32 */
  vsnprintf(Str,sizeof(Str),Txt,argptr);
#endif /* _WIN32 */
  va_end(argptr);
  SYSLOG_FN(Level,Str);
}

#endif /* FFSS_DRIVER */

int FFSS_GetFFSSOptions(void)
{
  int Flags = 0;
#ifdef DEBUG
  Flags |= FFSS_OPTIONS_DEBUG;
#endif /* DEBUG */
#ifdef HAVE_BZLIB
  Flags |= FFSS_OPTIONS_BZLIB;
#endif /* HAVE_BZLIB */
#ifdef FFSS_CONTEXT
  Flags |= FFSS_OPTIONS_CONTEXT;
#endif /* FFSS_CONTEXT */
#ifdef SU_MALLOC_TRACE
  Flags |= FFSS_OPTIONS_MALLOC_TRACE;
#endif /* SU_MALLOC_TRACE */
#ifdef FFSS_FTP
  Flags |= FFSS_OPTIONS_FTP;
#endif /* FFSS_FTP */
#ifdef DISABLE_CHECKSUM
  Flags |= FFSS_OPTIONS_NO_CHECKSUM;
#endif /* DISABLE_CHECKSUM */
  return Flags;
}

#ifndef FFSS_DRIVER
#if defined(_WIN32) | defined(__CYGWIN32__)
char FFSS_WinServerVersion[20] = {0,};
#endif /* _WIN32 | __CYGWIN32__ */

#ifdef _WIN32
#include <VersionHelpers.h>
#endif /* _WIN32 */
char *FFSS_GetOS(void)
{
#ifdef _WIN32
  if(FFSS_WinServerVersion[0] == 0)
  {
	  if (IsWindows8Point1OrGreater())
	  {
		  SU_strcpy(FFSS_WinServerVersion, "Win8.1", sizeof(FFSS_WinServerVersion));
	  }

	  else if (IsWindows8OrGreater())
	  {
		  SU_strcpy(FFSS_WinServerVersion, "Win8", sizeof(FFSS_WinServerVersion));
	  }

	  else if (IsWindows7OrGreater())
	  {
		  SU_strcpy(FFSS_WinServerVersion, "Win7", sizeof(FFSS_WinServerVersion));
	  }

	  else if (IsWindowsVistaOrGreater())
	  {
		  SU_strcpy(FFSS_WinServerVersion, "WinVista", sizeof(FFSS_WinServerVersion));
	  }

	  else if (IsWindowsXPOrGreater())
	  {
		  SU_strcpy(FFSS_WinServerVersion, "WinXP", sizeof(FFSS_WinServerVersion));
	  }

	  else
	  {
		  SU_strcpy(FFSS_WinServerVersion, "WinUnknown", sizeof(FFSS_WinServerVersion));
	  }
  }
  return FFSS_WinServerVersion;
#else /* !_WIN32 */
  return FFSS_SERVER_OS;
#endif /* _WIN32 */
}
#endif /* !FFSS_DRIVER */
