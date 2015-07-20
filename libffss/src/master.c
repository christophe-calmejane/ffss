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

SU_PServerInfo FM_SI_UDP=NULL,FM_SI_OUT_UDP=NULL,FM_SI_TCP=NULL;
SU_THREAD_HANDLE FM_THR_UDP,FM_THR_TCP;
SU_THREAD_ID FM_THRID_UDP,FM_THRID_TCP;
static SU_THREAD_RET_TYPE threadwork_ret_zero = 0;

void FM_AnalyseUDP(struct sockaddr_in Client, char Buf[], size_t Len)
{
	FFSS_Field Type;
  char *str,*str2,*str3;
	size_t pos;
  FFSS_Field val,val2,val3,val4,val5;
  FFSS_LongField lval;
  FFSS_Field state,type_ip;
  char IP[512];
  bool error;

  context;
  Type = *(FFSS_Field *)(Buf+sizeof(FFSS_Field));
  pos = sizeof(FFSS_Field)*2;
  switch(Type)
  {
    case FFSS_MESSAGE_STATE :
      context;
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a state message from server");
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      str2 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      str3 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (val2 == 0) || (str == NULL) || (str2 == NULL) || (str3 == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_STATE : One or many fields empty, or out of buffer (%s) (%ld,%ld,%p,%p,%p) ... DoS attack ?\n",inet_ntoa(Client.sin_addr),val,val2,str,str2,str3);
        break;
      }
      if((val2 > FFSS_PROTOCOL_VERSION) || (val2 < FFSS_PROTOCOL_VERSION_LEAST_COMPATIBLE))
      {
        FM_SendMessage_Error(inet_ntoa(Client.sin_addr),FFSS_ERROR_PROTOCOL_VERSION_ERROR,FFSS_ErrorTable[FFSS_ERROR_PROTOCOL_VERSION_ERROR]);
        break;
      }
      if(FFSS_CB.MCB.OnState != NULL)
        FFSS_CB.MCB.OnState(Client,val,str,str2,str3);
      break;
      case FFSS_MESSAGE_SERVER_LISTING :
        context;
        SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a server listing message from client");
        lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        str2 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if((str == NULL) || (str2 == NULL))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_SERVER_LISTING : One or many fields empty, or out of buffer (%s) (%p,%p) ... DoS attack ?\n",inet_ntoa(Client.sin_addr),str,str2);
          break;
        }
        if(FFSS_CB.MCB.OnServerListing != NULL)
          FFSS_CB.MCB.OnServerListing(Client,str,str2,val,lval);
        break;
    case FFSS_MESSAGE_CLIENT_SERVER_FAILED :
      context;
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a client/server failed message from client");
      type_ip = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      FFSS_UnpackIP(Buf,Buf+pos,Len,&pos,IP,type_ip);
      if((type_ip == 0) || (IP[0] == 0))
      {
        FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_CLIENT_SERVER_FAILED : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.MCB.OnClientServerFailed != NULL)
        FFSS_CB.MCB.OnClientServerFailed(IP);
      break;
    case FFSS_MESSAGE_PONG :
      context;
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a pong message from server");
      state = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if(state == 0)
      {
        FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_PONG : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(val >= FFSS_PROTOCOL_VERSION_LEAST_COMPATIBLE)
      {
        if(FFSS_CB.MCB.OnPong != NULL)
          FFSS_CB.MCB.OnPong(Client,state);
      }
      else
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Server protocol version mismatch");
      break;
    case FFSS_MESSAGE_DOMAINS_LISTING :
      context;
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a domains listing message from client");
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      if(FFSS_CB.MCB.OnDomainListing != NULL)
        FFSS_CB.MCB.OnDomainListing(Client,lval);
      break;
    case FFSS_MESSAGE_SEARCH :
      context;
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a friandise search message from client");
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      str2 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (str == NULL) || (str2 == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_SEARCH : One or many fields empty, or out of buffer (%s) (%ld,%p,%p) ... DoS attack ?\n",inet_ntoa(Client.sin_addr),val,str,str2);
        break;
      }
      if(FFSS_CB.MCB.OnSearch != NULL)
        FFSS_CB.MCB.OnSearch(Client,val,str,str2,val2,lval);
      break;
    case FFSS_MESSAGE_SEARCH_MASTER :
      context;
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a master search message from client or server");
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if(val == 0)
      {
        FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_SEARCH_MASTER : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.MCB.OnMasterSearch != NULL)
        FFSS_CB.MCB.OnMasterSearch(Client,val == FFSS_THREAD_SERVER,lval);
      break;
    case FFSS_MESSAGE_INDEX_ANSWER :
      context;
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val3 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val4 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val5 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received an index answer message (index size : %ld/%ld/%ld) Connect to %ld",val2,val3,val4,val5);
      error = false;
      if(FFSS_CB.MCB.OnIndexAnswer != NULL)
        FFSS_CB.MCB.OnIndexAnswer(Client,val,val2,val3,val4,val5);
      break;
    case FFSS_MESSAGE_INDEX_ANSWER_SAMBA :
      context;
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val3 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val4 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val5 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a samba index answer message (index size : %ld/%ld/%ld) Connect to %ld",val2,val3,val4,val5);
      error = false;
      if(FFSS_CB.MCB.OnIndexAnswerSamba != NULL)
        FFSS_CB.MCB.OnIndexAnswerSamba(Client,val,val2,val3,val4,val5);
      break;
    default :
      FFSS_PrintSyslog(LOG_WARNING,"Unknown UDP message type (%s) : %d ... DoS attack ?\n",inet_ntoa(Client.sin_addr),Type);
  }
}

bool FM_AnalyseTCP(SU_PClientSocket Master, char Buf[], size_t Len, bool *ident)
{
	FFSS_Field Type;
	size_t pos;
  char *str,*str2,*str3,*str4;
  FFSS_Field i,val,val2;
  FFSS_LongField lval;
  bool ret_val;
	size_t u_pos, u_Len;
  char *u_Buf;
  bool free_it;
  FFSS_Field state,type_ip,type_ip2;
  char IP[512],IP2[512];

  context;
  Type = *(FFSS_Field *)(Buf+sizeof(FFSS_Field));
  pos = sizeof(FFSS_Field)*2;
  ret_val = true;
  free_it = false;
  u_Buf = Buf;
  u_Len = Len;

  if(Type == FFSS_MESSAGE_MASTER_CONNECTION)
  {
    context;
    SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a connection message from master");
    val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
    if(val == 0)
    {
      FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_MASTER_CONNECTION : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Master->SAddr.sin_addr));
      ret_val = false;
    }
    else
    {
      if((val > FFSS_PROTOCOL_VERSION) || (val < FFSS_PROTOCOL_VERSION_LEAST_COMPATIBLE))
      {
        FM_SendMessage_ErrorMaster(Master->sock,FFSS_ERROR_PROTOCOL_VERSION_ERROR,FFSS_ErrorTable[FFSS_ERROR_PROTOCOL_VERSION_ERROR]);
        ret_val = false;
      }
      else
      {
        if(FFSS_CB.MCB.OnMasterConnected != NULL)
          FFSS_CB.MCB.OnMasterConnected(Master);
        *ident = true;
      }
    }
  }
  else
  {
    if(*ident != true)
      return ret_val;
    switch(Type)
    {
      case FFSS_MESSAGE_NEW_STATES :
        context;
        val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
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
              FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_NEW_STATES : Corrupted Z compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Master->SAddr.sin_addr));
              ret_val = false;
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
              FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_NEW_STATES : Corrupted BZ compressed buffer (%s) ... DoS attack ?\n",inet_ntoa(Master->SAddr.sin_addr));
              ret_val = false;
              break;
            }
            free_it = true;
            u_pos = 0;
            break;
#endif
          default :
            FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_NEW_STATES : Unknown compression type (%s) : %ld ... DoS attack ?\n",inet_ntoa(Master->SAddr.sin_addr),val2);
            ret_val = false;
            break;
        }
        if(ret_val == false)
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
            FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_NEW_STATES : One or many fields empty, or out of buffer (%s) (%ld,%ld,%s,%p,%p,%p,%p,%ld,%s) ... DoS attack ?\n",inet_ntoa(Master->SAddr.sin_addr),state,type_ip,IP,str,str2,str3,str4,type_ip2,IP2);
            break;
          }
          if(FFSS_CB.MCB.OnNewState != NULL)
            FFSS_CB.MCB.OnNewState(state,IP2,str,str2,str3,str4,IP);
        }
        break;
      case FFSS_MESSAGE_SERVER_LISTING :
        context;
        SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a server listing message from master");
        lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        str2 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if((str == NULL) || (str2 == NULL))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_SERVER_LISTING : One or many fields empty, or out of buffer (%s) (%p,%p) ... DoS attack ?\n",inet_ntoa(Master->SAddr.sin_addr),str,str2);
          break;
        }
        if(FFSS_CB.MCB.OnServerListingMaster != NULL)
          FFSS_CB.MCB.OnServerListingMaster(Master,str,str2,val,lval);
        break;
      case FFSS_MESSAGE_SEARCH_FW :
        context;
        SU_DBG_PrintDebug(FFSS_DBGMSG_PARSE_PROTO,"Received a forwarded friandise search message from master");
        lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        type_ip = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        FFSS_UnpackIP(Buf,Buf+pos,Len,&pos,IP,type_ip);
        if((type_ip == 0) || (IP[0] == 0))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_SEARCH_FW : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Master->SAddr.sin_addr));
          break;
        }
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if((val == 0) || (str == NULL))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_SEARCH_FW : One or many fields empty, or out of buffer (%s) (%ld,%p) ... DoS attack ?\n",inet_ntoa(Master->SAddr.sin_addr),val,str);
          break;
        }
        if(FFSS_CB.MCB.OnSearchForward != NULL)
          FFSS_CB.MCB.OnSearchForward(Master,IP,val,str,val2,lval);
        break;
      default :
        FFSS_PrintSyslog(LOG_WARNING,"Unknown TCP message type (%s) : %d ... DoS attack ?\n",inet_ntoa(Master->SAddr.sin_addr),Type);
        ret_val = false;
    }
  }
  if(free_it)
    free(u_Buf);
  return ret_val;
}

SU_THREAD_ROUTINE(FM_MasterThreadTCP,User)
{
  SU_PClientSocket Master = (SU_PClientSocket) User;
	size_t len;
  int res;
  FFSS_Field Size;
  bool analyse;
  fd_set rfds;
  int retval;
  char *Buf;
	size_t BufSize;
  bool Ident = false;

  context;
  SU_ThreadBlockSigs();
  BufSize = FFSS_TCP_MASTER_BUFFER_SIZE;
  Buf = (char *) malloc(BufSize);
  if(Buf == NULL)
  {
    FM_SendMessage_ErrorMaster(Master->sock,FFSS_ERROR_INTERNAL_ERROR,FFSS_ErrorTable[FFSS_ERROR_INTERNAL_ERROR]);
    SU_FreeCS(Master);
	SU_END_THREAD(threadwork_ret_zero);
  }

  len = 0;
  while(1)
  {
    FD_ZERO(&rfds);
    FD_SET(Master->sock,&rfds);
		retval = select((int)(Master->sock + 1), &rfds, NULL, NULL, NULL);
    if(retval <= 0) /* Some error occured */
    {
      if(FFSS_CB.MCB.OnMasterDisconnected != NULL)
        FFSS_CB.MCB.OnMasterDisconnected(Master);
      SU_FreeCS(Master);
      free(Buf);
	  SU_END_THREAD(threadwork_ret_zero);
    }

    context;
    if(len >= BufSize)
    {
      FFSS_PrintSyslog(LOG_INFO,"WARNING : Master's buffer too short for this message (%d) (%s) ... DoS attack ?\n",len,inet_ntoa(Master->SAddr.sin_addr));
      if(FFSS_CB.MCB.OnMasterDisconnected != NULL)
        FFSS_CB.MCB.OnMasterDisconnected(Master);
      SU_FreeCS(Master);
      free(Buf);
	  SU_END_THREAD(threadwork_ret_zero);
    }
		res = recv(Master->sock, Buf + len, (int)(BufSize - len), SU_MSG_NOSIGNAL);
    if(res == SOCKET_ERROR)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error on TCP port of the master (SOCKET_ERROR : %d)",errno);
      if(FFSS_CB.MCB.OnMasterDisconnected != NULL)
        FFSS_CB.MCB.OnMasterDisconnected(Master);
      SU_FreeCS(Master);
      free(Buf);
	  SU_END_THREAD(threadwork_ret_zero);
    }
    else if(res == 0)
    {
      if(FFSS_CB.MCB.OnMasterDisconnected != NULL)
        FFSS_CB.MCB.OnMasterDisconnected(Master);
      SU_FreeCS(Master);
      free(Buf);
	  SU_END_THREAD(threadwork_ret_zero);
    }
    len += res;
    SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Data found on TCP port from %s (%s) ... analysing",inet_ntoa(Master->SAddr.sin_addr),SU_NameOfPort(inet_ntoa(Master->SAddr.sin_addr)));
    analyse = true;
    while(analyse)
    {
      if(len < 5)
      {
        FFSS_PrintSyslog(LOG_WARNING,"Length of the message is less than 5 (%d) (%s) ... DoS attack ?\n",len,inet_ntoa(Master->SAddr.sin_addr));
        if(FFSS_CB.MCB.OnMasterDisconnected != NULL)
          FFSS_CB.MCB.OnMasterDisconnected(Master);
        SU_FreeCS(Master);
        free(Buf);
		SU_END_THREAD(threadwork_ret_zero);
      }
      context;
      Size = *(FFSS_Field *)Buf;
      if(Size > len)
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Warning, Size of the message is greater than received data (%d - %d)... Message splitted ?",Size,len);
        break;
        /* Keeps waiting for data */
      }
      else
      {
        if(!FM_AnalyseTCP(Master,Buf,Size,&Ident))
        {
          if(FFSS_CB.MCB.OnMasterDisconnected != NULL)
            FFSS_CB.MCB.OnMasterDisconnected(Master);
          SU_FreeCS(Master);
          free(Buf);
		  SU_END_THREAD(threadwork_ret_zero);
        }
        if(!Ident) /* First message was NOT a connection DoS Attack ? */
        {
          SU_FreeCS(Master);
          /* Must not call OnMasterDisconnected here, because no callback of the master was raised */
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

SU_THREAD_ROUTINE(FM_ThreadTCP,User)
{
  SU_PClientSocket Master;
  SU_THREAD_HANDLE MasterThr;
	SU_THREAD_ID MasterThrId;

  context;
  SU_ThreadBlockSigs();
  if(SU_ServerListen(FM_SI_TCP) == SOCKET_ERROR)
  {
    FFSS_PrintSyslog(LOG_ERR,"Couldn't listen on the TCP socket\n");
	SU_END_THREAD(threadwork_ret_zero);
  }
  SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"TCP thread launched...waiting for connections");
  while(1)
  {
    Master = SU_ServerAcceptConnection(FM_SI_TCP);
    context;
    if(FFSS_ShuttingDown)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"TCP Routine : FFSS Library is been shut down...");
      if(Master != NULL)
        SU_FreeCS(Master);
	  SU_END_THREAD(threadwork_ret_zero);
    }
    if(Master == NULL)
    {
      SU_SLEEP(1);
      continue;
    }
    /* Check for packet reject */
    switch(FFSS_Filter_GetActionOfChainFromIP(FFSS_FILTER_CHAINS_MASTER_TCP_CONNECTION_MASTER,INADDR_GET_IP(Master->SAddr.sin_addr)))
    {
      case FFSS_FILTER_ACTION_ACCEPT :
        break;
      case FFSS_FILTER_ACTION_REJECT :
        FFSS_PrintSyslog(LOG_WARNING,"Rejecting connection from %s\n",inet_ntoa(Master->SAddr.sin_addr));
        SU_FreeCS(Master);
        continue;
      default :
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in FM_ThreadTCP, unknown filter action");
		SU_END_THREAD(threadwork_ret_zero);
    }
    context;
    SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Master connected from %s (%s) ...",inet_ntoa(Master->SAddr.sin_addr),SU_NameOfPort(inet_ntoa(Master->SAddr.sin_addr)));
    if(!SU_CreateThread(&MasterThr,&MasterThrId,FM_MasterThreadTCP,(void *)Master,true))
    {
      FFSS_PrintSyslog(LOG_ERR,"Error creating TCP Master thread\n");
      SU_FreeCS(Master);
      continue;
    }
  }
}

/* FFSS Master : Init */
/* Initialisation of the FFSS Master - Must be called before any other FFSS function */
/* Returns true on success, false otherwise */
bool FM_Init(int MasterPort,const char *User,const char *Group,const char *Interface)
{
  bool error = false;
#ifdef FFSS_CONTEXT
  signal(SIGSEGV, FFSS_handle_SIGNAL);
  FFSS_MainThread = SU_THREAD_SELF;
  context;
#endif
#ifdef __unix__
  signal(SIGPIPE,FFSS_SignalHandler_BrokenPipe);
#endif
#if !defined(DEBUG) && defined(_WIN32)
  if(FFSS_LogFile == NULL)
    FFSS_LogFile = SU_OpenLogFile("FFSS_Master.log");
#endif /* !DEBUG && _WIN32 */
  FFSS_ShuttingDown = false;
  context;
#ifdef _WIN32
  if(!SU_SockInit(2,2))
    return false;
#endif /* _WIN32 */
  if(!FFSS_Filter_Init(FFSS_THREAD_MASTER))
  {
    FFSS_PrintSyslog(LOG_ERR,"Error initializing FFSS Filter engine\n");
    return false;
  }
  FM_SI_UDP = SU_CreateServer(MasterPort,SOCK_DGRAM,false);
  if(FM_SI_UDP == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating UDP socket on port %d (%d:%s)\n",MasterPort,errno,strerror(errno));
    return false;
  }
  context;
  FM_SI_OUT_UDP = SU_CreateServer(0,SOCK_DGRAM,false);
  if(FM_SI_OUT_UDP == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating outgoing UDP socket\n",errno,strerror(errno));
    SU_ServerDisconnect(FM_SI_UDP);
    free(FM_SI_UDP);
    return false;
  }
  FM_SI_TCP = SU_CreateServer(MasterPort,SOCK_STREAM,false);
  if(FM_SI_TCP == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating TCP socket on port %d (%d:%s)\n",MasterPort,errno,strerror(errno));
    SU_ServerDisconnect(FM_SI_UDP);
    SU_ServerDisconnect(FM_SI_OUT_UDP);
    free(FM_SI_UDP);
    free(FM_SI_OUT_UDP);
    return false;
  }
  if(SU_SetSocketOpt(FM_SI_OUT_UDP->sock,SO_BROADCAST,1) == -1)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error setting broadcast option to socket\n",errno,strerror(errno));
    SU_ServerDisconnect(FM_SI_UDP);
    SU_ServerDisconnect(FM_SI_OUT_UDP);
    SU_ServerDisconnect(FM_SI_TCP);
    free(FM_SI_UDP);
    free(FM_SI_OUT_UDP);
    free(FM_SI_TCP);
    return false;
  }
  context;
#ifdef __linux__
  if(Interface != NULL)
  {
    if(getuid() == 0)
    {
      if(setsockopt(FM_SI_UDP->sock,SOL_SOCKET,SO_BINDTODEVICE,Interface,strlen(Interface)+1) == SOCKET_ERROR)
      {
        error = true;
        FFSS_PrintSyslog(LOG_WARNING,"Warnig : Cannot bind only for %s (UDP socket) : %s\n",Interface,strerror(errno));
      }
      if(setsockopt(FM_SI_OUT_UDP->sock,SOL_SOCKET,SO_BINDTODEVICE,Interface,strlen(Interface)+1) == SOCKET_ERROR)
      {
        error = true;
        FFSS_PrintSyslog(LOG_WARNING,"Warnig : Cannot bind only for %s (UDP outgoing socket) : %s\n",Interface,strerror(errno));
      }
      if(setsockopt(FM_SI_TCP->sock,SOL_SOCKET,SO_BINDTODEVICE,Interface,strlen(Interface)+1) == SOCKET_ERROR)
      {
        error = true;
        FFSS_PrintSyslog(LOG_WARNING,"Warnig : Cannot bind only for %s (TCP socket) : %s\n",Interface,strerror(errno));
      }
      if(!error)
        FFSS_PrintSyslog(LOG_WARNING,"Entering limited bind mode to %s\n",Interface);
    }
    else
      FFSS_PrintSyslog(LOG_WARNING,"Warning : Master launched from a non-root user. Cannot bind to %s only\n",Interface);
  }
#endif /* __linux__ */
  context;
#ifdef __unix__
  if((User != NULL) || (Group != NULL))
  {
    if(getuid() == 0)
    {
      if(Group != NULL)
      {
        if(!SU_SetUserGroup(NULL,Group))
          FFSS_PrintSyslog(LOG_WARNING,"Warning : cannot setgid to group %s\n",Group);
      }
      if(User != NULL)
      {
        if(!SU_SetUserGroup(User,NULL))
          FFSS_PrintSyslog(LOG_WARNING,"Warning : cannot setuid to user %s\n",User);
      }
    }
    else
      FFSS_PrintSyslog(LOG_WARNING,"Warning : Master launched from a non-root user. Cannot use setuid/setgid\n");
  }
#endif /* __unix__ */
  context;
  if(!SU_CreateThread(&FM_THR_UDP,&FM_THRID_UDP,F_ThreadUDP,(void *)FFSS_THREAD_MASTER,false))
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating UDP thread\n");
    SU_ServerDisconnect(FM_SI_UDP);
    SU_ServerDisconnect(FM_SI_OUT_UDP);
    SU_ServerDisconnect(FM_SI_TCP);
    free(FM_SI_UDP);
    free(FM_SI_OUT_UDP);
    free(FM_SI_TCP);
    return false;
  }
  if(!SU_CreateThread(&FM_THR_TCP,&FM_THRID_TCP,FM_ThreadTCP,NULL,false))
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating TCP thread\n");
    SU_TermThread(FM_THR_UDP);
    SU_ServerDisconnect(FM_SI_UDP);
    SU_ServerDisconnect(FM_SI_OUT_UDP);
    SU_ServerDisconnect(FM_SI_TCP);
    free(FM_SI_UDP);
    free(FM_SI_OUT_UDP);
    free(FM_SI_TCP);
    return false;
  }
  FFSS_PrintSyslog(LOG_INFO,"FFSS master waiting on port %d (UDP and TCP)\n",MasterPort);
  return true;
}

/* FFSS Master : UnInit */
/* Uninitialisation of the FFSS Master - Must be called at the end of the main */
/* Returns true on success, false otherwise */
bool FM_UnInit(void)
{
  context;
  FFSS_ShuttingDown = true;
  SU_TermThread(FM_THR_UDP);
  SU_TermThread(FM_THR_TCP);
  SU_ServerDisconnect(FM_SI_UDP);
  SU_ServerDisconnect(FM_SI_OUT_UDP);
  SU_ServerDisconnect(FM_SI_TCP);
  if(FM_SI_UDP != NULL)
    free(FM_SI_UDP);
  if(FM_SI_OUT_UDP != NULL)
    free(FM_SI_OUT_UDP);
  if(FM_SI_TCP != NULL)
    free(FM_SI_TCP);
  if(FFSS_MyIP != NULL)
    free(FFSS_MyIP);
  FFSS_PrintSyslog(LOG_INFO,"FFSS master shut down\n");
#ifdef _WIN32
  SU_CloseLogFile(FFSS_LogFile);
  SU_SockUninit();
#endif /* _WIN32 */
  return true;
}

#endif /* !FFSS_DRIVER */
