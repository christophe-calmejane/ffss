#include "ffss.h"
#include "utils.h"
#include <stdarg.h>

FFSS_TCallbacks FFSS_CB;
#ifdef DEBUG
int N_DebugLevel = 6;
#else
int N_DebugLevel = 0;
#endif

char *FFSS_MusicExt[FFSS_MUSIC_NB_EXT] = {"mp3","wav","au","wma","snd","xm","mid","mod","ra"} ; /* 9 */
char *FFSS_VideoExt[FFSS_VIDEO_NB_EXT] = {"mpg","mpeg","avi","vob","asf","rm","ram","mov"} ; /* 8 */
char *FFSS_ImageExt[FFSS_IMAGE_NB_EXT] = {"bmp","tiff","jpg","jpeg","png","gif","pcx","pcd"} ; /* 8 */
char *FFSS_DocExt[FFSS_DOC_NB_EXT] = {"htm","html","txt","doc","pdf","nfo","tex","ps"} ; /* 8 */
char *FFSS_ExeExt[FFSS_EXE_NB_EXT] = {"exe","com","bat","sys","dll"} ; /* 5 */
char *FFSS_ZipExt[FFSS_ZIP_NB_EXT] = {"zip","arj","rar","tar","gz","jar","ace","bz2","deb","rpm"} ; /* 10 */

int FFSS_ContextLine;
char FFSS_ContextFile[512];
#ifdef _WIN32
FILE *FFSS_LogFile = NULL;
#endif /* _WIN32 */

void set_context(char *file, int line)
{
  FFSS_ContextLine = line;
  strncpy(FFSS_ContextFile, file, sizeof(FFSS_ContextFile)-1);
  FFSS_ContextFile[sizeof(FFSS_ContextFile)-1] = 0;
}

void FFSS_handle_SIGNAL(int signal)
{
  if(signal == SIGSEGV)
  {
    FFSS_PrintSyslog(LOG_ERR,"FFSS Crashed :-(. Check: %s:%d\n", FFSS_ContextFile,FFSS_ContextLine);
    abort();
  }
}

/* Unpacks a string from a message, checking if the string really terminates (prevents DoS attacks) */
/*  Returns the string, or NULL if there is a problem */
char *FFSS_UnpackString(const char beginning[],const char buf[],int len,long int *new_pos)
{
  long int pos = buf - beginning;
  while(pos < len)
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
FFSS_Field FFSS_UnpackField(const char beginning[],const char buf[],int len,long int *new_pos)
{
  int pos = buf - beginning;
  if((pos+sizeof(FFSS_Field)) <= len)
  {
    *new_pos = pos + sizeof(FFSS_Field);
    return *(FFSS_Field *)buf;
  }
  else
  {
    FFSS_PrintSyslog(LOG_WARNING,"Longint out of message... DoS attack ?\n");
    return 0;
  }
}

void FFSS_UnpackIP(const char beginning[],char *buf,int len,long int *new_pos,char buf_out[],int Type)
{
  int a,b,c,d;
  int pos = buf - beginning;

  buf_out[0] = 0;
  if((pos+FFSS_IP_FIELD_SIZE) <= len)
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
        FFSS_PrintDebug(1,"Unknown IP type : %d\n",Type);
    }
    *new_pos = pos + FFSS_IP_FIELD_SIZE;
  }
  else
  {
    FFSS_PrintSyslog(LOG_WARNING,"IP out of message... DoS attack ?\n");
  }
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
      FFSS_PrintDebug(1,"Unknown IP type : %d\n",Type);
  }
}

FFSS_Field FFSS_ComputeChecksum(FFSS_Field Old,const char Buf[],long int Len)
{
#ifdef DISABLE_CHECKSUM
  return 1;
#else
  if(Buf == NULL)
    return adler32(0L, Z_NULL, 0);
  return adler32(Old, Buf, Len);
#endif
}

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
#ifdef DEBUG
      printf("Using interface %s, with ip %s\n",IntName,FFSS_MyIP);
#endif
      return true;
    }
  }
  return false;
#else
  char Buf[1024];

  if(gethostname(Buf,sizeof(Buf)) != 0)
    return false;
  FFSS_MyIP = strdup(SU_AdrsOfPort(Buf));
  return true;
#endif
}


bool FFSS_CompresseZlib(char *in,long int len_in,char *out,long int *len_out)
{
  int res;

  res = compress(out,len_out,in,len_in);
#ifdef DEBUG
  printf("Using Z compression (before=%ld - after=%ld)\n",len_in,*len_out);
#endif
  return (res == Z_OK);
}

char *FFSS_UncompresseZlib(char *in,long int len_in,long int *len_out)
{
  char *out,*old_out;
  long int len;
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

#ifdef HAVE_BZLIB
bool FFSS_CompresseBZlib(char *in,long int len_in,char *out,long int *len_out)
{
  int res;

  res = BZ2_bzBuffToBuffCompress(out,(unsigned int *)len_out,in,len_in,FFSS_BZLIB_BLOCK100K,0,0);
#ifdef DEBUG
  printf("Using BZ compression (before=%ld - after=%ld)\n",len_in,*len_out);
#endif
  return (res == BZ_OK);
}

char *FFSS_UncompresseBZlib(char *in,long int len_in,long int *len_out)
{
  char *out,*old_out;
  long int len;
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
#endif

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

void FFSS_PrintSyslog(int Level,char *Txt, ...)
{
  va_list argptr;
  char Str[4096];

  va_start(argptr,Txt);
#ifdef _WIN32
  _vsnprintf(Str,sizeof(Str),Txt,argptr);
#else /* _WIN32 */
  vsnprintf(Str,sizeof(Str),Txt,argptr);
#endif /* _WIN32 */
  va_end(argptr);
  SYSLOG_FN(Level,Str);
}

#undef FFSS_PrintDebug
void FFSS_PrintDebug(int Level,char *Txt, ...)
{
  va_list argptr;
  char Str[4096];

  if(Level <= N_DebugLevel)
  {
    va_start(argptr,Txt);
#ifdef _WIN32
    _vsnprintf(Str,sizeof(Str),Txt,argptr);
#else /* _WIN32 */
    vsnprintf(Str,sizeof(Str),Txt,argptr);
#endif /* _WIN32 */
    va_end(argptr);
    printf("FFSS(%d) : %s",Level,Str);
  }
}

