#ifndef FFSS_DRIVER

#include "ffss.h"
#include "utils.h"
#include "common.h"
#include "transfer.h"

SU_PServerInfo FC_SI_OUT_UDP;
SU_THREAD_HANDLE FC_THR_UDP;

void FC_AnalyseUDP(struct sockaddr_in Client,char Buf[],long int Len)
{
  int Type;
  char *str,*str2,*str3,*str4;
  char **Names,**Comments;
  char **answers;
  long int pos;
  FFSS_Field val,val2,val3;
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
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      error = false;
      switch(val2)
      {
        case FFSS_COMPRESSION_NONE:
          u_pos = pos;
          break;
        case FFSS_COMPRESSION_ZLIB:
          u_Buf = FFSS_UncompresseZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_NEW_STATES,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#ifdef HAVE_BZLIB
        case FFSS_COMPRESSION_BZLIB:
          u_Buf = FFSS_UncompresseBZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_NEW_STATES,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted BZ compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#endif
        default:
          FFSS_PrintSyslog(LOG_WARNING,"Unknown compression type (%s) : %ld ... DoS attack ?\n",inet_ntoa(Client.sin_addr),val2);
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
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
          break;
        }
        if(FFSS_CB.CCB.OnNewState != NULL)
          FFSS_CB.CCB.OnNewState(state,IP2,str,str2,str3,str4,IP);
      }
      break;
    case FFSS_MESSAGE_SHARES_LISTING_ANSWER :
      type_ip = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      FFSS_UnpackIP(Buf,Buf+pos,Len,&pos,IP,type_ip);
      if((type_ip == 0) || (IP[0] == 0))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      val = FFSS_UnpackField (Buf, Buf + pos, Len, &pos);
      if(val == 0)
      {
        FFSS_PrintDebug(3,"Received a shares listing message, but server has no shares\n");
        if(FFSS_CB.CCB.OnSharesListing != NULL)
          FFSS_CB.CCB.OnSharesListing(IP,NULL,NULL,0);
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
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
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
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      error = false;
      switch(val2)
      {
        case FFSS_COMPRESSION_NONE:
          u_pos = pos;
          break;
        case FFSS_COMPRESSION_ZLIB:
          u_Buf = FFSS_UncompresseZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
            FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#ifdef HAVE_BZLIB
        case FFSS_COMPRESSION_BZLIB:
          u_Buf = FFSS_UncompresseBZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted BZ compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#endif
        default:
          FFSS_PrintSyslog(LOG_WARNING,"Unknown compression type (%s) : %ld ... DoS attack ?\n",inet_ntoa(Client.sin_addr),val2);
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
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
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
              FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
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
            FFSS_CB.CCB.OnServerListingAnswer(str,val2,HostList);
        }
        SU_FreeListElem(HostList);
      }
      if(FFSS_CB.CCB.OnEndServerListingAnswer != NULL)
        FFSS_CB.CCB.OnEndServerListingAnswer();
      break;
    case FFSS_MESSAGE_DOMAINS_LISTING_ANSWER :
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if(val == 0)
      {
        FFSS_PrintDebug(3,"Received a domains listing answer, but master has no domains\n");
        if (FFSS_CB.CCB.OnDomainListingAnswer != NULL)
          FFSS_CB.CCB.OnDomainListingAnswer(NULL,0);
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
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
          break;
        }
        Names[i] = str;
      }
      if(FFSS_CB.CCB.OnDomainListingAnswer != NULL)
        FFSS_CB.CCB.OnDomainListingAnswer((const char **)Names,val);
      free (Names);
      break;
    case FFSS_MESSAGE_SEARCH_MASTER_ANSWER :
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (str == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.CCB.OnMasterSearchAnswer != NULL)
        FFSS_CB.CCB.OnMasterSearchAnswer(Client,val,str);
      break;
    case FFSS_MESSAGE_SEARCH_ANSWER :
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      error = false;
      switch(val2)
      {
        case FFSS_COMPRESSION_NONE:
          u_pos = pos;
          break;
        case FFSS_COMPRESSION_ZLIB:
          u_Buf = FFSS_UncompresseZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_ANSWER,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#ifdef HAVE_BZLIB
        case FFSS_COMPRESSION_BZLIB:
          u_Buf = FFSS_UncompresseBZlib(Buf+pos,Len-sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_ANSWER,&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted BZ compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
            error = true;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#endif
        default:
          FFSS_PrintSyslog(LOG_WARNING,"Unknown compression type (%s) : %ld ... DoS attack ?\n",inet_ntoa(Client.sin_addr),val2);
          error = true;
          break;
      }
      if(error)
        break;
      str = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
      if(str == NULL)
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      val = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
      if(val == 0)
      {
        FFSS_PrintDebug(3,"Received a search answer message, but master has found nothing\n");
        if(FFSS_CB.CCB.OnSearchAnswer != NULL)
          FFSS_CB.CCB.OnSearchAnswer(str,NULL,NULL,0);
        break;
      }
      FFSS_PrintDebug(3,"Received a search answer message (%d domains)\n",val);
      for(i=0;i<val;i++)
      {
        str2 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        if(str2 == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
          break;
        }
        val2 = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        if(val2 == 0)
        {
          FFSS_PrintDebug(3,"Master has found nothing for domain %s\n",str2);
          if(FFSS_CB.CCB.OnSearchAnswer != NULL)
            FFSS_CB.CCB.OnSearchAnswer(str,str2,NULL,0);
          continue;
        }
        answers = (char **) malloc(val2*sizeof(char *));
        for(j=0;j<val2;j++)
        {
          str3 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
          if(str3 == NULL)
          {
            free (answers);
            FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
            break;
          }
          answers[j] = str3;
        }
        if(FFSS_CB.CCB.OnSearchAnswer != NULL)
          FFSS_CB.CCB.OnSearchAnswer(str,str2,(const char **)answers,val2);
        free (answers);
      }
      break;
    case FFSS_MESSAGE_ERROR :
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (str == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      FFSS_PrintDebug(3,"Received a master error message (%d:%s)\n",val,str);
      if(FFSS_CB.CCB.OnMasterError != NULL)
        FFSS_CB.CCB.OnMasterError(val,str);
      break;
    default:
      FFSS_PrintSyslog(LOG_WARNING,"Unknown message type (%s) : %d ... DoS attack ?\n",inet_ntoa(Client.sin_addr),Type);
  }
  if(free_it)
    free(u_Buf);
}

bool FC_AnalyseTCP(SU_PClientSocket Server,char Buf[],long int Len)
{
  int Type, i;
  long int pos;
  FFSS_Field val,val2,val3,val4;
  char *str,*str2;
  SU_PList Ptr;
  FC_PEntry Ent;
  bool ret_val;
  FFSS_PTransfer FT;
  bool free_it;
  char *u_Buf;
  long int u_pos,u_Len;
  long int Length;

  Type = *(FFSS_Field *)(Buf+sizeof(FFSS_Field));
  pos = sizeof (FFSS_Field)*2;
  ret_val = true;
  free_it = false;
  u_Buf = Buf;
  u_Len = Len;
  switch (Type)
  {
    case FFSS_MESSAGE_ERROR :
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (str == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      FFSS_PrintDebug(3,"Received a server error message (%d:%s)\n",val,str);
      if(FFSS_CB.CCB.OnError != NULL)
        ret_val = FFSS_CB.CCB.OnError(Server,val,str);
      break;
    case FFSS_MESSAGE_DIRECTORY_LISTING_ANSWER :
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if(str == NULL)
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      switch (val2)
      {
        case FFSS_COMPRESSION_NONE:
          u_pos = pos;
          break;
        case FFSS_COMPRESSION_ZLIB:
          u_Buf = FFSS_UncompresseZlib(Buf+pos,Len-sizeof(FFSS_Field)*3-(strlen(str)+1),&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
            ret_val = false;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#ifdef HAVE_BZLIB
        case FFSS_COMPRESSION_BZLIB:
          u_Buf = FFSS_UncompresseBZlib(Buf+pos,Len-sizeof(FFSS_Field)*3-(strlen(str)+1),&u_Len);
          if(u_Buf == NULL)
          {
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted BZ compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
            ret_val = false;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#endif
        default:
          FFSS_PrintSyslog(LOG_WARNING,"Unknown compression type (%s) : %ld ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr),val2);
          ret_val = false;
          break;
      }
      if(!ret_val)
        break;
      val = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
      Ptr = NULL;
      for(i=0;i<val;i++)
      {
        Ent = (FC_PEntry) malloc(sizeof(FC_TEntry));
        memset(Ent,0,sizeof(FC_TEntry));
        str2 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        val2 = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        val3 = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        val4 = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        if(str2 == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
          free (Ent);
          SU_FreeListElem(Ptr);
          ret_val = false;
          break;
        }
        Ent->Name = str2;
        Ent->Flags = val2;
        Ent->Size = val3;
        Ent->Stamp = val4;
        Ptr = SU_AddElementTail(Ptr,Ent);
      }
      FFSS_PrintDebug(3,"Received a directory listing answer (%d entries)\n",val);
      if(FFSS_CB.CCB.OnDirectoryListingAnswer != NULL)
        ret_val = FFSS_CB.CCB.OnDirectoryListingAnswer(Server,str,val,Ptr);
      SU_FreeListElem(Ptr);
      break;
    case FFSS_MESSAGE_INIT_XFER :
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if(str == NULL)
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      FFSS_PrintDebug(3,"Received a init xfer message (%s)\n",str);
      if(FFSS_CB.CCB.OnInitXFer != NULL)
      {
        FT = FFSS_CB.CCB.OnInitXFer(Server,str);
        if(FT != NULL)
          FFSS_InitXFerDownload(FT,val);
      }
      break;
    case FFSS_MESSAGE_DATA :
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      Length = Len-FFSS_MESSAGESIZE_DATA*sizeof(FFSS_Field);
      if(Length < 0)
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      FFSS_PrintDebug(3,"Received a xfer data message (%d bytes)\n",val);
      if(FFSS_CB.CCB.OnData != NULL)
      {
        FT = FFSS_CB.CCB.OnData(Server,val);
        if(FT != NULL)
          FFSS_OnDataDownload(FT,Buf+pos,Length);
      }
      break;
    case FFSS_MESSAGE_STREAMING_OPEN_ANSWER :
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val3 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if((str == NULL) || (val == 0))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      FFSS_PrintDebug(3,"Received a streaming open answer message (%s)\n",val);
      if(FFSS_CB.CCB.OnStrmOpenAnswer != NULL)
        FFSS_CB.CCB.OnStrmOpenAnswer(Server,str,val,val2,val3);
      break;
    case FFSS_MESSAGE_STREAMING_READ_ANSWER :
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      Length = Len-FFSS_MESSAGESIZE_STREAMING_READ_ANSWER*sizeof(FFSS_Field);
      if((val == 0) || (Length < 0))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      FFSS_PrintDebug(3,"Received a streaming read answer message (%d bytes)\n",Length);
      if(FFSS_CB.CCB.OnStrmReadAnswer != NULL)
        FFSS_CB.CCB.OnStrmReadAnswer(Server,val,Buf+pos,Length);
      break;
    case FFSS_MESSAGE_STREAMING_WRITE_ANSWER :
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (val2 == 0))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      FFSS_PrintDebug(3,"Received a streaming write answer message\n");
      if(FFSS_CB.CCB.OnStrmWriteAnswer != NULL)
        FFSS_CB.CCB.OnStrmWriteAnswer(Server,val,val2);
      break;
    default:
      FFSS_PrintSyslog(LOG_WARNING,"Unknown message type (%s) : %d ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr),Type);
      ret_val = false;
  }
  if(free_it)
    free(u_Buf);
  return ret_val;
}

SU_THREAD_ROUTINE(FC_ClientThreadTCP,User)
{
  SU_PClientSocket Client = (SU_PClientSocket) User;
  int len,res;
  FFSS_Field Size;
  bool analyse;
  fd_set rfds;
  struct timeval tv;
  int retval;
  char *Buf;
  long int BufSize;

  SU_ThreadBlockSigs();
  BufSize = FFSS_TCP_CLIENT_BUFFER_SIZE;
  Buf = (char *) malloc(BufSize);
  if(Buf == NULL)
    SU_END_THREAD(NULL);
  if(FFSS_CB.CCB.OnBeginTCPThread != NULL)
    FFSS_CB.CCB.OnBeginTCPThread(Client);
  len = 0;
  while (1)
  {
    if(Client->User != 0)	/* Idle time out value */
    {
      FFSS_PrintDebug(3,"Client has defined a idle time out value of %d sec for this connection\n",(int)Client->User);
      FD_ZERO(&rfds);
      FD_SET(Client->sock,&rfds);
      tv.tv_sec = (int)Client->User;
      tv.tv_usec = 0;
      retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
      if(!retval)
      {
        if(FFSS_CB.CCB.OnIdleTimeout != NULL)
          FFSS_CB.CCB.OnIdleTimeout(Client);
        FFSS_PrintDebug(1,"Error on TCP port of the client (TIME OUT)\n");
        SU_FreeCS(Client);
        if(FFSS_CB.CCB.OnEndTCPThread != NULL)
          FFSS_CB.CCB.OnEndTCPThread(Client);
        free(Buf);
        SU_END_THREAD(NULL);
      }
    }

    if(len >= BufSize)
    {
      FFSS_PrintSyslog(LOG_INFO,"WARNING : Client's buffer too short for this message !!\n");
    }
    res = recv(Client->sock,Buf+len,BufSize-len,SU_MSG_NOSIGNAL);
    if(res == SOCKET_ERROR)
    {
      FFSS_PrintDebug(1,"Error on TCP port of the client (SOCKET_ERROR : %d)\n",errno);
      SU_FreeCS(Client);
      if(FFSS_CB.CCB.OnEndTCPThread != NULL)
	FFSS_CB.CCB.OnEndTCPThread(Client);
      free(Buf);
      SU_END_THREAD(NULL);
    }
    else if(res == 0)
    {
      SU_FreeCS(Client);
      if(FFSS_CB.CCB.OnEndTCPThread != NULL)
        FFSS_CB.CCB.OnEndTCPThread(Client);
      free(Buf);
      SU_END_THREAD(NULL);
    }
    len += res;
    FFSS_PrintDebug(6,"Data found on TCP port from %s (%s) ... analysing\n",inet_ntoa(Client->SAddr.sin_addr),SU_NameOfPort(inet_ntoa(Client->SAddr.sin_addr)));
    analyse = true;
    while (analyse)
    {
      if(len < 5)
      {
        FFSS_PrintSyslog(LOG_WARNING,"Length of the message is less than 5 (%d) (%s) ... DoS attack ?\n",len,inet_ntoa(Client->SAddr.sin_addr));
        SU_FreeCS(Client);
        if(FFSS_CB.CCB.OnEndTCPThread != NULL)
          FFSS_CB.CCB.OnEndTCPThread(Client);
        free(Buf);
        SU_END_THREAD(NULL);
      }
      Size = *(FFSS_Field *)Buf;
      if(Size > len)
      {
        FFSS_PrintDebug(5,"Warning, Size of the message is greater than received data (%d - %d)... Message splitted ?\n",Size,len);
        break;
        /* Keeps waiting for data */
      }
      else
      {
        if(!FC_AnalyseTCP(Client,Buf,Size))
	{
          SU_FreeCS(Client);
          if(FFSS_CB.CCB.OnEndTCPThread != NULL)
            FFSS_CB.CCB.OnEndTCPThread(Client);
          free(Buf);
          SU_END_THREAD(NULL);
        }
        if(len > Size)
        {
          FFSS_PrintDebug(5,"Warning, Size of the message is less than received data (%d - %d)... multiple messages ?\n",Size,len);
          memmove(Buf,Buf+Size,len-Size);
          len -= Size;
          /* Keeps analysing the buffer */
        }
        else
        {
          analyse = false;
          len = 0;
        }
      }
    }
  }
}

/* FFSS Client : Init */
/* Initialisation of the FFSS Client - Must be called before any other FFSS function */
/* Returns true on success, false otherwise */
bool FC_Init(void)
{
  int len;

#ifdef FFSS_CONTEXT
  signal(SIGSEGV,FFSS_handle_SIGNAL);
  context;
#endif
  signal(SIGTERM,FFSS_SignalHandler_Term);
#ifdef __unix__
  signal(SIGPIPE,FFSS_SignalHandler_BrokenPipe);
#endif
  FFSS_ShuttingDown = false;
  FC_SI_OUT_UDP = SU_CreateServer(0,SOCK_DGRAM,false);
  if(FC_SI_OUT_UDP == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating outgoing UDP socket (%d:%s)\n",errno,strerror(errno));
    return false;
  }
  len = sizeof(struct sockaddr_in);
  if(getsockname(FC_SI_OUT_UDP->sock,(struct sockaddr *)&(FC_SI_OUT_UDP->SAddr),&len) == -1)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error getting socket name\n",errno,strerror(errno));
    SU_FreeSI(FC_SI_OUT_UDP);
    return false;
  }
  if(SU_SetSocketOpt(FC_SI_OUT_UDP->sock,SO_BROADCAST,1) == -1)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error setting broadcast option to socket\n",errno,strerror(errno));
    SU_FreeSI(FC_SI_OUT_UDP);
    return false;
  }
  if(!SU_CreateThread(&FC_THR_UDP,F_ThreadUDP,(void *)FFSS_THREAD_CLIENT,false))
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating UDP thread\n");
    SU_FreeSI(FC_SI_OUT_UDP);
    return false;
  }
  FFSS_PrintSyslog(LOG_INFO,"FFSS client waiting on arbitrary UDP port\n");
  return true;
}

/* FFSS Client : UnInit */
/* Uninitialisation of the FFSS Client - Must be called at the end of the main */
/* Returns true on success, false otherwise */
bool FC_UnInit(void)
{
  FFSS_ShuttingDown = true;
  SU_TermThread(FC_THR_UDP);
  SU_ServerDisconnect(FC_SI_OUT_UDP);
  free(FC_SI_OUT_UDP);
  if(FFSS_MyIP != NULL)
    free(FFSS_MyIP);
  FFSS_PrintSyslog(LOG_INFO,"FFSS client shut down\n");
#ifdef _WIN32
  SU_CloseLogFile(FFSS_LogFile);
#endif /* _WIN32 */
  return true;
}

#endif /* FFSS_DRIVER */
