#ifdef FFSS_DRIVER

/* Functions of this driver are NOT reentrant */

#include <ffss_tdi.h>
#define bool SU_BOOL
#define strdup FFSS_strdup
#undef FILE
#include <stdio.h>

SU_PList SU_AddElementHead(SU_PList List,void *Elem)
{
  SU_PList El;

  El = (SU_PList) malloc(sizeof(SU_TList));
  El->Next = List;
  El->Data = Elem;
  return El;
}

void SU_FreeListElem(SU_PList List)
{
  SU_PList Ptr,Ptr2;

  Ptr = List;
  while(Ptr != NULL)
  {
    Ptr2 = Ptr->Next;
    free(Ptr->Data);
    free(Ptr);
    Ptr = Ptr2;
  }
}

char *SU_strcpy(char *dest,const char *src,int len)
{
  int pos=0;

  if(src != NULL)
  {
    while(pos < (len-1))
    {
      dest[pos] = src[pos];
      pos++;
      if(src[pos] == 0)
        break;
    }
  }
  dest[pos] = 0;
  return dest;
}

unsigned char SU_toupper(unsigned char c)
{
  if((c >= 'a') && (c <= 'z'))
    return (c-32);
  if((c >= 224) && (c <= 255))
    return (c-32);
  return c;
}

bool SU_strcasecmp(const char *s,const char *p)
{
  while((*s != 0) && (*p != 0))
  {
    if(SU_toupper(*s) != SU_toupper(*p))
      return false;
    s++;p++;
  }
  return ((*s == 0) && (*p == 0));
}

char *FFSS_strdup(const char *in)
{
  char *s;
  long int len;

  len = strlen(in) + 1;
  s = (char *) malloc(len);
  if(s == NULL)
    return NULL;
  strncpy(s,in,len);
  return s;
}

void FC_AnalyseUDP(struct sockaddr_in Client,char Buf[],long int Len)
{
  int Type;
  char *str,*str2,*str3,*str4;
  char **Names,**Comments;
  char **answers,**ips;
  long int pos;
  FFSS_Field val,val2,val3;
  FFSS_LongField lval;
  FFSS_Field i,j,state,type_ip,type_ip2;
  char IP[512], IP2[512];
  FM_PHost Hst;
  SU_PList HostList;
  bool do_it,error,free_it;
  char *u_Buf;
  long int u_pos,u_Len;

  Type = *(FFSS_Field *)(Buf+sizeof(FFSS_Field));
  pos = sizeof(FFSS_Field)*2;
  free_it = false;
  u_Buf = Buf;
  u_Len = Len;
  switch(Type)
  {
    case FFSS_MESSAGE_NEW_STATES :
      context;
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      error = false;
      switch(val2)
      {
        case FFSS_COMPRESSION_NONE:
          u_pos = pos;
          break;
#ifndef DISABLE_ZLIB
        case FFSS_COMPRESSION_ZLIB:
          u_Buf = FFSS_UncompresseZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_NEW_STATES,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n","");
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
        case FFSS_COMPRESSION_BZLIB:
          u_Buf = FFSS_UncompresseBZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_NEW_STATES,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted BZ compressed buffer (%s) ... DoS attack ?\n","");
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#endif
        default:
          FFSS_PrintSyslog(LOG_WARNING,"Unknown compression type (%s) : %ld ... DoS attack ?\n","",val2);
          error = true;
          break;
      }
      if(error)
        break;
      val = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
      FFSS_PrintDebug(3,"Received a new state message (%d states)\n",val);
      for(i=0;i<val;i++)
      {
        state = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        type_ip = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        FFSS_UnpackIP(u_Buf,u_Buf+u_pos,u_Len,&u_pos,IP,type_ip);
        str = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        str2 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        str3 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        str4 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        type_ip2 = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        FFSS_UnpackIP(u_Buf,u_Buf+u_pos,u_Len,&u_pos,IP2,type_ip2);
        if((state == 0) || (type_ip == 0) || (IP[0] == 0) || (str == NULL) || (str2 == NULL) || (str3 == NULL) || (str4 == NULL) || (type_ip2 == 0) || (IP2[0] == 0))
        {
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
          break;
        }
        if(FFSS_CB.CCB.OnNewState != NULL)
          FFSS_CB.CCB.OnNewState(state,IP2,str,str2,str3,str4,IP);
      }
      break;
    case FFSS_MESSAGE_SHARES_LISTING_ANSWER :
      context;
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      type_ip = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      FFSS_UnpackIP(Buf,Buf+pos,Len,&pos,IP,type_ip);
      if((type_ip == 0) || (IP[0] == 0))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
        break;
      }
      val = FFSS_UnpackField (Buf, Buf + pos, Len, &pos);
      if(val == 0)
      {
        FFSS_PrintDebug(3,"Received a shares listing message, but server has no shares\n");
        if(FFSS_CB.CCB.OnSharesListing != NULL)
          FFSS_CB.CCB.OnSharesListing(IP,NULL,NULL,0,lval);
        break;
      }
      FFSS_PrintDebug(3,"Received a shares listing message (%d shares)\n",val);
      Names = (char **) malloc(val*sizeof(char *));
      Comments = (char **) malloc(val*sizeof (char *));
      for(i=0;i<val;i++)
      {
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        str2 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if((str == NULL) || (str2 == NULL))
        {
          free (Names);
          free (Comments);
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
          break;
        }
        Names[i] = str;
        Comments[i] = str2;
      }
      if(FFSS_CB.CCB.OnSharesListing != NULL)
        FFSS_CB.CCB.OnSharesListing (IP,(const char **)Names,(const char **)Comments,val);
      free(Names);
      free(Comments);
      break;
    case FFSS_MESSAGE_SERVER_LISTING_ANSWER :
      context;
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      error = false;
      switch(val2)
      {
        case FFSS_COMPRESSION_NONE:
          u_pos = pos;
          break;
#ifndef DISABLE_ZLIB
        case FFSS_COMPRESSION_ZLIB:
          u_Buf = FFSS_UncompresseZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n","");
            FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
        case FFSS_COMPRESSION_BZLIB:
          u_Buf = FFSS_UncompresseBZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted BZ compressed buffer (%s) ... DoS attack ?\n","");
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#endif
        default:
          FFSS_PrintSyslog(LOG_WARNING,"Unknown compression type (%s) : %ld ... DoS attack ?\n","",val2);
          error = true;
          break;
      }
      if(error)
        break;
      val = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
      FFSS_PrintDebug(4,"Received a server listing answer (%d domains)\n",val);
      for(i=0;i<val;i++)
      {
        str = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        val2 = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        if(str == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
          break;
        }
        FFSS_PrintDebug(3,"\t%d hosts in domain %s\n",val2,str);
        HostList = NULL;
        do_it = true;
        if(val2 != 0)
        {
          for (j=0;j<val2;j++)
          {
            val3 = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
            str2 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
            str3 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
            str4 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
            type_ip = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
            FFSS_UnpackIP(u_Buf,u_Buf+u_pos,u_Len,&u_pos,IP,type_ip);
            if((val3 == 0) || (str2 == NULL) || (str3 == NULL) || (str4 == NULL) || (type_ip == 0) || (IP[0] == 0))
            {
              FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
              do_it = false;
              break;
            }
            Hst = (FM_PHost) malloc(sizeof(FM_THost));
            memset (Hst,0,sizeof(FM_THost));
            Hst->State = val3;
            Hst->Name = str2;
            Hst->OS = str3;
            Hst->Comment = str4;
            Hst->IP = strdup(IP);
            HostList = SU_AddElementHead(HostList,Hst);
          }
        }
        if(do_it)
        {
          if(FFSS_CB.CCB.OnServerListingAnswer != NULL)
            FFSS_CB.CCB.OnServerListingAnswer(str,val2,HostList,lval);
        }
        SU_FreeListElem(HostList);
      }
      if(FFSS_CB.CCB.OnEndServerListingAnswer != NULL)
        FFSS_CB.CCB.OnEndServerListingAnswer();
      break;
    case FFSS_MESSAGE_DOMAINS_LISTING_ANSWER :
      context;
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if(val == 0)
      {
        FFSS_PrintDebug(3,"Received a domains listing answer, but master has no domains\n");
        if (FFSS_CB.CCB.OnDomainListingAnswer != NULL)
          FFSS_CB.CCB.OnDomainListingAnswer(NULL,0,lval);
        break;
      }
      FFSS_PrintDebug(3,"Received a domains listing answer (%d domains)\n",val);
      Names = (char **) malloc(val*sizeof(char *));
      for(i=0;i<val;i++)
      {
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if(str == NULL)
        {
          free (Names);
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
          break;
        }
        Names[i] = str;
      }
      if(FFSS_CB.CCB.OnDomainListingAnswer != NULL)
        FFSS_CB.CCB.OnDomainListingAnswer((const char **)Names,val,lval);
      free (Names);
      break;
    case FFSS_MESSAGE_SEARCH_MASTER_ANSWER :
      context;
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (str == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
        break;
      }
      if(FFSS_CB.CCB.OnMasterSearchAnswer != NULL)
        FFSS_CB.CCB.OnMasterSearchAnswer(Client,val,str,lval);
      break;
    case FFSS_MESSAGE_SEARCH_ANSWER :
      context;
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      error = false;
      switch(val2)
      {
        case FFSS_COMPRESSION_NONE:
          u_pos = pos;
          break;
#ifndef DISABLE_ZLIB
        case FFSS_COMPRESSION_ZLIB:
          u_Buf = FFSS_UncompresseZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_ANSWER,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n","");
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
        case FFSS_COMPRESSION_BZLIB:
          u_Buf = FFSS_UncompresseBZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_ANSWER,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted BZ compressed buffer (%s) ... DoS attack ?\n","");
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#endif
        default:
          FFSS_PrintSyslog(LOG_WARNING,"Unknown compression type (%s) : %ld ... DoS attack ?\n","",val2);
          error = true;
          break;
      }
      if(error)
        break;
      str = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
      if(str == NULL)
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
        break;
      }
      val = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
      if(val == 0)
      {
        FFSS_PrintDebug(3,"Received a search answer message, but master has found nothing\n");
        if(FFSS_CB.CCB.OnSearchAnswer != NULL)
          FFSS_CB.CCB.OnSearchAnswer(str,NULL,NULL,NULL,0,lval);
        break;
      }
      FFSS_PrintDebug(3,"Received a search answer message (%d domains)\n",val);
      for(i=0;i<val;i++)
      {
        str2 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        if(str2 == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
          break;
        }
        val2 = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        if(val2 == 0)
        {
          FFSS_PrintDebug(3,"Master has found nothing for domain %s\n",str2);
          if(FFSS_CB.CCB.OnSearchAnswer != NULL)
            FFSS_CB.CCB.OnSearchAnswer(str,str2,NULL,NULL,0,lval);
          continue;
        }
        answers = (char **) malloc(val2*sizeof(char *));
        ips = (char **) malloc(val2*sizeof(char *));
        for(j=0;j<val2;j++)
        {
          type_ip = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
          FFSS_UnpackIP(u_Buf,u_Buf+u_pos,u_Len,&u_pos,IP,type_ip);
          str3 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
          if((str3 == NULL) || (type_ip == 0) || (IP[0] == 0))
          {
            FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
            error = true;
            break;
          }
          ips[j] = strdup(IP);
          answers[j] = str3;
        }
        if(!error)
        {
          if(FFSS_CB.CCB.OnSearchAnswer != NULL)
            FFSS_CB.CCB.OnSearchAnswer(str,str2,(const char **)answers,(char **)ips,val2,lval);
        }
        free(ips);
        free(answers);
      }
      break;
    case FFSS_MESSAGE_ERROR :
      context;
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (str == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n","");
        break;
      }
      FFSS_PrintDebug(3,"Received a master error message (%d:%s)\n",val,str);
      if(FFSS_CB.CCB.OnMasterError != NULL)
        FFSS_CB.CCB.OnMasterError(val,str);
      break;
    default:
      FFSS_PrintSyslog(LOG_WARNING,"Unknown message type (%s) : %d ... DoS attack ?\n","",Type);
  }
  if(free_it)
    free(u_Buf);
}


/* FFSS Client : UnInit */
/* Initialisation of the FFSS Client - Must be called before any other FFSS function */
/* Returns true on success, false otherwise */
bool FC_Init(void)
{
  KdPrint(("FFSS driver running\n"));
  return true;
}

/* FFSS Client : UnInit */
/* Uninitialisation of the FFSS Client - Must be called at the end of the main */
/* Returns true on success, false otherwise */
bool FC_UnInit(void)
{
  KdPrint(("FFSS driver shut down\n"));
  return true;
}

#endif
