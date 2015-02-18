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

void FS_AnalyseUDP(struct sockaddr_in Client,char Buf[],long int Len);
void FC_AnalyseUDP(struct sockaddr_in Client,char Buf[],long int Len);
void FM_AnalyseUDP(struct sockaddr_in Client,char Buf[],long int Len);

struct sockaddr_in FFSS_CurrentSIN;
time_t FFSS_When;
bool FFSS_ShuttingDown = false;
static SU_THREAD_RET_TYPE threadwork_ret_zero = 0;

SU_THREAD_ROUTINE(F_ThreadUDP,User)
{
  struct sockaddr_in Client;
  unsigned int len;
  int res = SOCKET_ERROR;
  FFSS_Field Size;
  bool analyse;
  int err;
  char whom[150];
  char *Buf;
  unsigned long int BufSize = 0;
  fd_set rfds;
  struct timeval tv;
  int retval = 0;
  FFSS_FILTER_ACTION filter_res;

  SU_ThreadBlockSigs();
  context;
  switch((int)User)
  {
    case FFSS_THREAD_SERVER :
      BufSize = FFSS_UDP_SERVER_BUFFER_SIZE;
      break;
    case FFSS_THREAD_CLIENT :
      BufSize = FFSS_UDP_CLIENT_BUFFER_SIZE;
      break;
    case FFSS_THREAD_MASTER :
      BufSize = FFSS_UDP_MASTER_BUFFER_SIZE;
      break;
    default :
      SU_DBG_PrintDebug(FFSS_DBGMSG_FATAL,"Error in UDP common thread, unknown thread type !");
	  SU_END_THREAD(threadwork_ret_zero);
  }
  Buf = (char *) malloc(BufSize);
  if(Buf == NULL)
	  SU_END_THREAD(threadwork_ret_zero);

  SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"UDP thread launched...waiting for data");
  err = 0;
  len = 0;
  FFSS_CurrentSIN.sin_port = 0;
  while(1)
  {
    context;
    if(len >= BufSize)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"WARNING : UDP buffer too short for this message !! Ignoring message...");
      len = 0;
    }
    if(len != 0)
    {
      FD_ZERO(&rfds);
      tv.tv_sec = FFSS_TIMEOUT_UDP_LOCK;
      tv.tv_usec = 0;
      switch((int)User)
      {
        case FFSS_THREAD_SERVER :
          FD_SET(FS_SI_UDP->sock,&rfds);
          retval = select(FS_SI_UDP->sock+1,&rfds,NULL,NULL,&tv);
          break;
        case FFSS_THREAD_CLIENT :
          FD_SET(FC_SI_OUT_UDP->sock,&rfds);
          retval = select(FC_SI_OUT_UDP->sock+1,&rfds,NULL,NULL,&tv);
          break;
        case FFSS_THREAD_MASTER :
          FD_SET(FM_SI_UDP->sock,&rfds);
          retval = select(FM_SI_UDP->sock+1,&rfds,NULL,NULL,&tv);
          break;
        default :
          SU_DBG_PrintDebug(FFSS_DBGMSG_FATAL,"Error in UDP common thread, unknown thread type !");
		  SU_END_THREAD(threadwork_ret_zero);
      }
      if(!retval) /* Time out */
      {
        len = 0;
        FFSS_CurrentSIN.sin_port = 0;
        SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Time out waiting end of command from %s...",inet_ntoa(FFSS_CurrentSIN.sin_addr));
        continue;
      }
    }
    context;
    switch((int)User)
    {
      case FFSS_THREAD_SERVER :
        res = SU_UDPReceiveFromSin(FS_SI_UDP,Buf+len,BufSize-len,&Client,true);
        break;
      case FFSS_THREAD_CLIENT :
        res = SU_UDPReceiveFromSin(FC_SI_OUT_UDP,Buf+len,BufSize-len,&Client,true);
        break;
      case FFSS_THREAD_MASTER :
        res = SU_UDPReceiveFromSin(FM_SI_UDP,Buf+len,BufSize-len,&Client,true);
        break;
      default :
        SU_DBG_PrintDebug(FFSS_DBGMSG_FATAL,"Error in UDP common thread, unknown thread type !");
		SU_END_THREAD(threadwork_ret_zero);
    }
    if(res == SOCKET_ERROR)
    {
      if(FFSS_ShuttingDown)
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"UDP Routine : FFSS Library is been shut down...");
		SU_END_THREAD(threadwork_ret_zero);
      }
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error on UDP port (SOCKET_ERROR : %d)",errno);
      if(FFSS_CB.CCB.OnUDPError != NULL)
        FFSS_CB.CCB.OnUDPError(errno);
      err++;
      if(err >= FFSS_UDP_MAX_ERRORS)
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_FATAL,"Too many consecutive errors on UDP port, exiting");
        if((int)User == FFSS_THREAD_CLIENT)
        {
          if(FFSS_CB.CCB.OnFatalError != NULL)
            FFSS_CB.CCB.OnFatalError();
        }
		SU_END_THREAD(threadwork_ret_zero);
      }
      continue;
    }
    else
      err = 0;
    context;
    SU_strcpy(whom,inet_ntoa(Client.sin_addr),sizeof(whom));
    SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Data found on UDP port from %s (%s) (%d bytes)... analysing",whom,SU_NameOfPort(whom),res);
    if(FFSS_CurrentSIN.sin_port != 0) /* If currently waiting more data from someone */
    {
      if(time(NULL) > (FFSS_When+FFSS_TIMEOUT_UDP_LOCK)) /* If time out expired */
      {
        memmove(Buf,Buf+len,res);
        len = 0;
        FFSS_CurrentSIN.sin_port = 0;
        SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Time out waiting data from %s... accepting data from %s",inet_ntoa(FFSS_CurrentSIN.sin_addr),whom);
      }
      else
      {
        if((memcmp(&FFSS_CurrentSIN.sin_addr,&Client.sin_addr,sizeof(Client.sin_addr)) != 0) || (FFSS_CurrentSIN.sin_port != Client.sin_port)) /* Not waiting data from you.. please resend later */
        {
          SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Sorry %s, I'm still waiting more data from %s... please resend later",whom,inet_ntoa(FFSS_CurrentSIN.sin_addr));
          if((int)User == FFSS_THREAD_MASTER)
          {
            FM_SendMessage_Error(whom,FFSS_ERROR_RESEND_LAST_UDP,FFSS_ErrorTable[FFSS_ERROR_RESEND_LAST_UDP]);
          }
          continue;
        }
        else
          FFSS_When = time(NULL); /* Update time */
      }
    }
    /* Check for packet reject */
    switch((int)User)
    {
      case FFSS_THREAD_SERVER :
        filter_res = FFSS_Filter_GetActionOfChainFromIP(FFSS_FILTER_CHAINS_SERVER_UDP_PACKET,INADDR_GET_IP(Client.sin_addr));
        break;
      case FFSS_THREAD_CLIENT :
        filter_res = FFSS_Filter_GetActionOfChainFromIP(FFSS_FILTER_CHAINS_CLIENT_UDP_PACKET,INADDR_GET_IP(Client.sin_addr));
        break;
      case FFSS_THREAD_MASTER :
        filter_res = FFSS_Filter_GetActionOfChainFromIP(FFSS_FILTER_CHAINS_MASTER_UDP_PACKET,INADDR_GET_IP(Client.sin_addr));
        break;
      default :
        SU_DBG_PrintDebug(FFSS_DBGMSG_FATAL,"Error in UDP common thread, unknown thread type !");
		SU_END_THREAD(threadwork_ret_zero);
    }
    switch(filter_res)
    {
      case FFSS_FILTER_ACTION_ACCEPT :
        break;
      case FFSS_FILTER_ACTION_REJECT :
        len = 0;
        FFSS_CurrentSIN.sin_port = 0;
        SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"UDP packet from %s has been rejected",whom);
        continue;
      default :
        SU_DBG_PrintDebug(FFSS_DBGMSG_FATAL,"Error in UDP common thread, unknown filter action");
		SU_END_THREAD(threadwork_ret_zero);
    }
    /* If size of message won't fit in the buffer */
    if((len == 0) && ((*(FFSS_Field *)Buf) > BufSize))
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Length of the message won't fit in the UDP buffer !!");
      if((int)User == FFSS_THREAD_MASTER)
      {
        FM_SendMessage_Error(whom,FFSS_ERROR_BUFFER_OVERFLOW,FFSS_ErrorTable[FFSS_ERROR_BUFFER_OVERFLOW]);
      }
      continue;
    }
    len += res;
    analyse = true;
    while(analyse)
    {
      if(len < 5)
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Length of the message is less than 5 (%d)... DoS attack ?",len);
        len = 0;
        break;
      }
      Size = *(FFSS_Field *)Buf;
      if(Size < 5)
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Size of the message is less than 5 (%d)... DoS attack ?",Size);
        len = 0;
        break;
      }
      if(Size > len)
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Warning, Size of the message is greater than received data (%d - %d)... Message splitted ?",Size,len);
        FFSS_When = time(NULL);
        memcpy(&FFSS_CurrentSIN,&Client,sizeof(FFSS_CurrentSIN)); /* Lock other hosts */
        break;
        /* Keeps waiting for data */
      }
      else
      {
        FFSS_CurrentSIN.sin_port = 0; /* Reset current receiving IP */
        /* Switch Client-Server-Master */
        context;
        switch((int)User)
        {
          case FFSS_THREAD_SERVER :
            FS_AnalyseUDP(Client,Buf,Size);
            break;
          case FFSS_THREAD_CLIENT :
            FC_AnalyseUDP(Client,Buf,Size);
            break;
          case FFSS_THREAD_MASTER :
            FM_AnalyseUDP(Client,Buf,Size);
            break;
          default :
            SU_DBG_PrintDebug(FFSS_DBGMSG_FATAL,"Error in UDP common thread, unknown thread type !");
			SU_END_THREAD(threadwork_ret_zero);
        }
        if(len > Size)
        {
          SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Warning, Size of the message is less than received data (%d - %d)... multiple messages ?",Size,len);
          FFSS_When = time(NULL);
          memcpy(&FFSS_CurrentSIN,&Client,sizeof(FFSS_CurrentSIN)); /* Lock other hosts */
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


void FFSS_SignalHandler_BrokenPipe(int sig)
{
  SU_DBG_PrintDebug(FFSS_DBGMSG_FATAL,"Received a BROKEN PIPE signal (thread %ld)",SU_THREAD_SELF);
  SU_END_THREAD(threadwork_ret_zero);
}
void FFSS_SignalHandler_Term(int sig)
{
  SU_DBG_PrintDebug(FFSS_DBGMSG_FATAL,"Received a TERM signal (thread %ld)",SU_THREAD_SELF);
  SU_END_THREAD(threadwork_ret_zero);
}

#endif /* !FFSS_DRIVER */
