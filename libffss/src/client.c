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
#include "common.h"
#include "transfer.h"

SU_PServerInfo FC_SI_OUT_UDP=NULL;
SU_THREAD_HANDLE FC_THR_UDP;
SU_THREAD_ID FC_THRID_UDP;
static SU_THREAD_RET_TYPE threadwork_ret_zero = 0;

void FC_AnalyseUDP(struct sockaddr_in Client,char Buf[],size_t Len)
{
	FFSS_Field Type;
  char *str,*str2,*str3,*str4;
  char **Names,**Comments;
  char **answers,**ips;
  size_t pos;
  FFSS_Field val,val2,val3;
  FFSS_LongField lval,lval2,lval3;
  FFSS_Field i,j,state,type_ip,type_ip2;
  char IP[512], IP2[512];
  FM_PHost Hst;
  SU_PList HostList;
  bool do_it,error,free_it;
  char *u_Buf;
  size_t u_pos,u_Len;
  FFSS_LongField *chksums;
  FFSS_LongField *sizes;

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
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
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
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a new state message (%d states)",val);
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
      context;
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
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
        SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a shares listing message, but server has no shares");
        if(FFSS_CB.CCB.OnSharesListing != NULL)
          FFSS_CB.CCB.OnSharesListing(IP,NULL,NULL,0,lval);
        break;
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a shares listing message (%d shares)",val);
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
        FFSS_CB.CCB.OnSharesListing (IP,(const char **)Names,(const char **)Comments,val,lval);
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
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
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
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a server listing answer (%d domains)",val);
      for(i=0;i<val;i++)
      {
        str = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        val2 = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
        if(str == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
          break;
        }
        SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"\t%d hosts in domain %s",val2,str);
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
        SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a domains listing answer, but master has no domains");
        if (FFSS_CB.CCB.OnDomainListingAnswer != NULL)
          FFSS_CB.CCB.OnDomainListingAnswer(NULL,0,lval);
        break;
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a domains listing answer (%d domains)",val);
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
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
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
            FFSS_PrintSyslog(LOG_WARNING,"Corrupted Z compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
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
        SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a search answer message, but master has found nothing");
        if(FFSS_CB.CCB.OnSearchAnswer != NULL)
          FFSS_CB.CCB.OnSearchAnswer(str,NULL,NULL,NULL,NULL,NULL,0,lval);
        break;
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a search answer message (%d domains)",val);
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
          SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Master has found nothing for domain %s",str2);
          if(FFSS_CB.CCB.OnSearchAnswer != NULL)
            FFSS_CB.CCB.OnSearchAnswer(str,str2,NULL,NULL,NULL,NULL,0,lval);
          continue;
        }
        answers = (char **) malloc(val2*sizeof(char *));
        ips = (char **) malloc(val2*sizeof(char *));
				chksums = (FFSS_LongField *)malloc(val2*sizeof(FFSS_LongField));
        sizes = (FFSS_LongField *) malloc(val2*sizeof(FFSS_LongField));
        for(j=0;j<val2;j++)
        {
          type_ip = FFSS_UnpackField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
          FFSS_UnpackIP(u_Buf,u_Buf+u_pos,u_Len,&u_pos,IP,type_ip);
          lval3 = FFSS_UnpackLongField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
          lval2 = FFSS_UnpackLongField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
          str3 = FFSS_UnpackString(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
          if((str3 == NULL) || (type_ip == 0) || (IP[0] == 0))
          {
            FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
            error = true;
            break;
          }
          ips[j] = strdup(IP);
          answers[j] = str3;
          chksums[j] = lval3;
          sizes[j] = lval2;
        }
        if(!error)
        {
          if(FFSS_CB.CCB.OnSearchAnswer != NULL)
            FFSS_CB.CCB.OnSearchAnswer(str,str2,(const char **)answers,(char **)ips,chksums,sizes,val2,lval);
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
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a master error message (%d:%s)",val,str);
      if(FFSS_CB.CCB.OnMasterError != NULL)
        FFSS_CB.CCB.OnMasterError(val,str);
      break;
    case FFSS_MESSAGE_SHORT_MESSAGE :
      context;
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if(str == NULL)
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.CCB.OnShortMessage != NULL)
        FFSS_CB.CCB.OnShortMessage(Client,str);
      break;
    default:
      FFSS_PrintSyslog(LOG_WARNING,"Unknown UDP message type (%s) : %d ... DoS attack ?\n",inet_ntoa(Client.sin_addr),Type);
  }
  if(free_it)
    free(u_Buf);
}

bool FC_AnalyseTCP(SU_PClientSocket Server,char Buf[],size_t Len)
{
	FFSS_Field Type, i;
  size_t pos;
  FFSS_Field val,val2,val4;
  FFSS_LongField lval,lval2;
  char *str,*str2;
  SU_PList Ptr;
  FC_PEntry Ent;
  bool ret_val;
  FFSS_PTransfer FT;
  bool free_it;
  char *u_Buf;
  size_t u_pos,u_Len;
  size_t Length;

  Type = *(FFSS_Field *)(Buf+sizeof(FFSS_Field));
  pos = sizeof (FFSS_Field)*2;
  ret_val = true;
  free_it = false;
  u_Buf = Buf;
  u_Len = Len;
  switch (Type)
  {
    case FFSS_MESSAGE_ERROR :
      context;
      lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (str == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a server error message (%d:%s:%ld)",val,str,lval);
      if(FFSS_CB.CCB.OnError != NULL)
        ret_val = FFSS_CB.CCB.OnError(Server,val,str,lval,lval2);
      break;
    case FFSS_MESSAGE_DIRECTORY_LISTING_ANSWER :
      context;
      lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
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
#ifndef DISABLE_ZLIB
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
#endif /* !DISABLE_ZLIB */
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
        lval = FFSS_UnpackLongField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
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
        Ent->Size = lval;
        Ent->Stamp = val4;
        Ptr = SU_AddElementTail(Ptr,Ent);
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a directory listing answer (%d entries)",val);
      if(FFSS_CB.CCB.OnDirectoryListingAnswer != NULL)
        ret_val = FFSS_CB.CCB.OnDirectoryListingAnswer(Server,str,val,Ptr,lval2);
      SU_FreeListElem(Ptr);
      break;
    case FFSS_MESSAGE_REC_DIR_LISTING_ANSWER :
      context;
      lval2 = FFSS_UnpackLongField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
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
#ifndef DISABLE_ZLIB
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
#endif /* !DISABLE_ZLIB */
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
        lval = FFSS_UnpackLongField(u_Buf,u_Buf+u_pos,u_Len,&u_pos);
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
        Ent->Size = lval;
        Ent->Stamp = val4;
        Ptr = SU_AddElementTail(Ptr,Ent);
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a recursive directory listing answer (%d entries)",val);
      if(FFSS_CB.CCB.OnRecursiveDirectoryListingAnswer != NULL)
        ret_val = FFSS_CB.CCB.OnRecursiveDirectoryListingAnswer(Server,str,val,Ptr,lval2);
      SU_FreeListElem(Ptr);
      break;
    case FFSS_MESSAGE_INIT_XFER :
      context;
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if(str == NULL)
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a init xfer message (%s)",str);
      if(FFSS_CB.CCB.OnInitXFer != NULL)
      {
        FT = FFSS_CB.CCB.OnInitXFer(Server,str,val);
        if(FT != NULL)
          FFSS_InitXFerDownload(FT,val);
      }
      break;
    case FFSS_MESSAGE_DATA :
      context;
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      Length = Len-FFSS_MESSAGESIZE_DATA*sizeof(FFSS_Field);
      if(Length < 0)
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a xfer data message (%d bytes)",Length);
      if(FFSS_CB.CCB.OnData != NULL)
      {
        FT = FFSS_CB.CCB.OnData(Server,val);
        if(FT != NULL)
          FFSS_OnDataDownload(FT,Buf+pos,Length);
      }
      break;
    case FFSS_MESSAGE_STREAMING_OPEN_ANSWER :
      context;
      lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      if((str == NULL) || (val == 0))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a streaming open answer message (%d)",val);
      if(FFSS_CB.CCB.OnStrmOpenAnswer != NULL)
        FFSS_CB.CCB.OnStrmOpenAnswer(Server,str,val,val2,lval,lval2);
      break;
    case FFSS_MESSAGE_STREAMING_READ_ANSWER :
      context;
      lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      Length = Len-FFSS_MESSAGESIZE_STREAMING_READ_ANSWER*sizeof(FFSS_Field);
      if((val == 0) || (Length < 0) || (val2 == 0))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a streaming read answer message (%d bytes)",Length);
      if(FFSS_CB.CCB.OnStrmReadAnswer != NULL)
        FFSS_CB.CCB.OnStrmReadAnswer(Server,val,Buf+pos,Length,val2,lval2);
      break;
    case FFSS_MESSAGE_STREAMING_WRITE_ANSWER :
      context;
      lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (val2 == 0))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr));
        ret_val = false;
        break;
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a streaming write answer message");
      if(FFSS_CB.CCB.OnStrmWriteAnswer != NULL)
        FFSS_CB.CCB.OnStrmWriteAnswer(Server,val,val2,lval2);
      break;
    default:
      if(FFSS_CB.CCB.OnError != NULL)
        FFSS_CB.CCB.OnError(Server,FFSS_ERROR_ATTACK,FFSS_ErrorTable[FFSS_ERROR_ATTACK],0,0);
      FFSS_PrintSyslog(LOG_WARNING,"Unknown TCP message type (%s) : %d ... DoS attack ?\n",inet_ntoa(Server->SAddr.sin_addr),Type);
      ret_val = false;
  }
  if(free_it)
    free(u_Buf);
  return ret_val;
}

SU_THREAD_ROUTINE(FC_ClientThreadTCP,User)
{
  SU_PClientSocket Client = (SU_PClientSocket) User;
	size_t len;
  int res;
  FFSS_Field Size;
  bool analyse;
  fd_set rfds;
  struct timeval tv;
  int retval;
  char *Buf;
  size_t BufSize;

  SU_ThreadBlockSigs();
  BufSize = FFSS_TCP_CLIENT_BUFFER_SIZE;
  Buf = (char *) malloc(BufSize);
  if(Buf == NULL)
	  SU_END_THREAD(threadwork_ret_zero);
  if(FFSS_CB.CCB.OnBeginTCPThread != NULL)
    FFSS_CB.CCB.OnBeginTCPThread(Client);
  len = 0;
  while (1)
  {
    if(Client->User != 0)	/* Idle time out value */
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Client has defined a idle time out value of %d sec for this connection",(int)Client->User);
      FD_ZERO(&rfds);
      FD_SET(Client->sock,&rfds);
      tv.tv_sec = (int)Client->User;
      tv.tv_usec = 0;
      retval = select((int)Client->sock+1,&rfds,NULL,NULL,&tv);
      if(!retval)
      {
        if(FFSS_CB.CCB.OnIdleTimeout != NULL)
          FFSS_CB.CCB.OnIdleTimeout(Client);
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error on TCP port of the client (TIME OUT)");
        SU_FreeCS(Client);
        if(FFSS_CB.CCB.OnEndTCPThread != NULL)
          FFSS_CB.CCB.OnEndTCPThread(Client);
        free(Buf);
		SU_END_THREAD(threadwork_ret_zero);
      }
    }

    if(len >= BufSize)
    {
      FFSS_PrintSyslog(LOG_INFO,"WARNING : Client's buffer too short for this message (%d) (%s) ... DoS attack ?\n",len,inet_ntoa(Client->SAddr.sin_addr));
      if(FFSS_CB.CCB.OnError != NULL)
        FFSS_CB.CCB.OnError(Client,FFSS_ERROR_ATTACK,FFSS_ErrorTable[FFSS_ERROR_ATTACK],0,0);
      SU_FreeCS(Client);
      if(FFSS_CB.CCB.OnEndTCPThread != NULL)
        FFSS_CB.CCB.OnEndTCPThread(Client);
      free(Buf);
	  SU_END_THREAD(threadwork_ret_zero);
    }
    res = recv(Client->sock,Buf+len,(int)(BufSize-len),SU_MSG_NOSIGNAL);
    if(res == SOCKET_ERROR)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error on TCP port of the client (SOCKET_ERROR : %d)",errno);
      if(FFSS_CB.CCB.OnError != NULL)
        FFSS_CB.CCB.OnError(Client,FFSS_ERROR_SOCKET_ERROR,FFSS_ErrorTable[FFSS_ERROR_SOCKET_ERROR],0,0);
      SU_FreeCS(Client);
      if(FFSS_CB.CCB.OnEndTCPThread != NULL)
        FFSS_CB.CCB.OnEndTCPThread(Client);
      free(Buf);
	  SU_END_THREAD(threadwork_ret_zero);
    }
    else if(res == 0)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Disconnected : Remote host closed the connection");
      if(FFSS_CB.CCB.OnError != NULL)
        FFSS_CB.CCB.OnError(Client,FFSS_ERROR_REMOTE_CLOSED,FFSS_ErrorTable[FFSS_ERROR_REMOTE_CLOSED],0,0);
      SU_FreeCS(Client);
      if(FFSS_CB.CCB.OnEndTCPThread != NULL)
        FFSS_CB.CCB.OnEndTCPThread(Client);
      free(Buf);
	  SU_END_THREAD(threadwork_ret_zero);
    }
    len += res;
    SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Data found on TCP port from %s (%s) ... analysing",inet_ntoa(Client->SAddr.sin_addr),SU_NameOfPort(inet_ntoa(Client->SAddr.sin_addr)));
    analyse = true;
    while (analyse)
    {
      if(len < 5)
      {
        FFSS_PrintSyslog(LOG_WARNING,"Length of the message is less than 5 (%d) (%s) ... DoS attack ?\n",len,inet_ntoa(Client->SAddr.sin_addr));
        if(FFSS_CB.CCB.OnError != NULL)
          FFSS_CB.CCB.OnError(Client,FFSS_ERROR_ATTACK,FFSS_ErrorTable[FFSS_ERROR_ATTACK],0,0);
        SU_FreeCS(Client);
        if(FFSS_CB.CCB.OnEndTCPThread != NULL)
          FFSS_CB.CCB.OnEndTCPThread(Client);
        free(Buf);
		SU_END_THREAD(threadwork_ret_zero);
      }
      Size = *(FFSS_Field *)Buf;
      if(Size > len)
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Warning, Size of the message is greater than received data (%d - %d)... Message splitted ?",Size,len);
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
		  SU_END_THREAD(threadwork_ret_zero);
        }
        if(len > Size)
        {
          SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Warning, Size of the message is less than received data (%d - %d)... multiple messages ?",Size,len);
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
  FFSS_MainThread = SU_THREAD_SELF;
  context;
#endif
  signal(SIGTERM,FFSS_SignalHandler_Term);
#ifdef __unix__
  signal(SIGPIPE,FFSS_SignalHandler_BrokenPipe);
#endif
#if !defined(DEBUG) && defined(_WIN32)
  if(FFSS_LogFile == NULL)
  {
    if(getenv("FFSS_LOG_FILE") != NULL)
      FFSS_LogFile = SU_OpenLogFile("FFSS_Client.log");
  }
#endif /* !DEBUG && _WIN32 */
  FFSS_ShuttingDown = false;
#ifdef _WIN32
  if(!SU_SockInit(2,2))
    return false;
#endif /* _WIN32 */
  if(!FFSS_Filter_Init(FFSS_THREAD_CLIENT))
  {
    FFSS_PrintSyslog(LOG_ERR,"Error initializing FFSS Filter engine\n");
    return false;
  }
  if(!FFSS_QoS_Init(0))
  {
    FFSS_PrintSyslog(LOG_ERR,"Error initializing FFSS QoS engine\n");
    return false;
  }
  FC_SI_OUT_UDP = SU_CreateServer(0,SOCK_DGRAM,false);
  if(FC_SI_OUT_UDP == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating outgoing UDP socket (%d:%s)\n",errno,strerror(errno));
    return false;
  }
  len = (int)sizeof(struct sockaddr_in);
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
  if(!SU_CreateThread(&FC_THR_UDP,&FC_THRID_UDP,F_ThreadUDP,(void *)FFSS_THREAD_CLIENT,false))
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
  if(FC_SI_OUT_UDP != NULL)
    free(FC_SI_OUT_UDP);
  if(FFSS_MyIP != NULL)
    free(FFSS_MyIP);
  FFSS_PrintSyslog(LOG_INFO,"FFSS client shut down\n");
#ifdef _WIN32
  SU_CloseLogFile(FFSS_LogFile);
  SU_SockUninit();
#endif /* _WIN32 */
  return true;
}

#endif /* !FFSS_DRIVER */
