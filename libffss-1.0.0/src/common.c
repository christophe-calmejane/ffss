#ifndef FFSS_DRIVER

#include "ffss.h"
#include "utils.h"
#include "common.h"

void FS_AnalyseUDP(struct sockaddr_in Client,char Buf[],long int Len);
void FC_AnalyseUDP(struct sockaddr_in Client,char Buf[],long int Len);
void FM_AnalyseUDP(struct sockaddr_in Client,char Buf[],long int Len);

char *FFSS_MyIP = NULL;
struct sockaddr_in FFSS_CurrentSIN;
time_t FFSS_When;
bool FFSS_ShuttingDown = false;

SU_THREAD_ROUTINE(F_ThreadUDP,User)
{
  struct sockaddr_in Client;
  int len,res=SOCKET_ERROR;
  FFSS_Field Size;
  bool analyse;
  int err;
  char whom[150];
  char *Buf;
  long int BufSize = 0;
  fd_set rfds;
  struct timeval tv;
  int retval = 0;

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
      FFSS_PrintDebug(1,"Error in UDP common thread, unknown thread type !\n");
      SU_END_THREAD(NULL);
  }
  Buf = (char *) malloc(BufSize);
  if(Buf == NULL)
    SU_END_THREAD(NULL);

  FFSS_PrintDebug(2,"UDP thread launched...waiting for data\n");
  err = 0;
  len = 0;
  FFSS_CurrentSIN.sin_port = 0;
  while(1)
  {
    context;
    if(len >= BufSize)
    {
      FFSS_PrintDebug(1,"WARNING : UDP buffer too short for this message !! Ignoring message...\n");
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
          FFSS_PrintDebug(1,"Error in UDP common thread, unknown thread type !\n");
          SU_END_THREAD(NULL);
      }
      if(!retval) /* Time out */
      {
        len = 0;
        FFSS_CurrentSIN.sin_port = 0;
        FFSS_PrintDebug(1,"Time out waiting end of command from %s...\n",inet_ntoa(FFSS_CurrentSIN.sin_addr));
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
        FFSS_PrintDebug(1,"Error in UDP common thread, unknown thread type !\n");
        SU_END_THREAD(NULL);
    }
    if(res == SOCKET_ERROR)
    {
      if(FFSS_ShuttingDown)
      {
        FFSS_PrintDebug(1,"UDP Routine : FFSS Library is been shut down...\n");
        SU_END_THREAD(NULL);
      }
      FFSS_PrintDebug(1,"Error on UDP port (SOCKET_ERROR : %d)\n",errno);
      if(FFSS_CB.CCB.OnUDPError != NULL)
        FFSS_CB.CCB.OnUDPError(errno);
      err++;
      if(err >= FFSS_UDP_MAX_ERRORS)
      {
        FFSS_PrintDebug(1,"Too many consecutive errors on UDP port, exiting\n");
        if((int)User == FFSS_THREAD_CLIENT)
        {
          if(FFSS_CB.CCB.OnFatalError != NULL)
            FFSS_CB.CCB.OnFatalError();
        }
        SU_END_THREAD(NULL);
      }
      continue;
    }
    else
      err = 0;
    context;
    SU_strcpy(whom,inet_ntoa(Client.sin_addr),sizeof(whom));
    FFSS_PrintDebug(5,"Data found on UDP port from %s (%s) (%d bytes)... analysing\n",whom,SU_NameOfPort(whom),res);
    if(FFSS_CurrentSIN.sin_port != 0) /* If currently waiting more data from someone */
    {
      if(time(NULL) > (FFSS_When+FFSS_TIMEOUT_UDP_LOCK)) /* If time out expired */
      {
        memmove(Buf,Buf+len,res);
        len = 0;
        FFSS_CurrentSIN.sin_port = 0;
        FFSS_PrintDebug(1,"Time out waiting data from %s... accepting data from %s\n",inet_ntoa(FFSS_CurrentSIN.sin_addr),whom);
      }
      else
      {
        if((memcmp(&FFSS_CurrentSIN.sin_addr,&Client.sin_addr,sizeof(Client.sin_addr)) != 0) || (FFSS_CurrentSIN.sin_port != Client.sin_port)) /* Not waiting data from you.. please resend later */
        {
          FFSS_PrintDebug(1,"Sorry %s, I'm still waiting more data from %s... please resend later\n",whom,inet_ntoa(FFSS_CurrentSIN.sin_addr));
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
    /* If size of message won't fit in the buffer */
    if((len == 0) && ((*(FFSS_Field *)Buf) > BufSize))
    {
      FFSS_PrintDebug(1,"Length of the message won't fit in the UDP buffer !!\n");
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
        FFSS_PrintDebug(1,"Length of the message is less than 5 (%d)... DoS attack ?\n",len);
        len = 0;
        break;
      }
      Size = *(FFSS_Field *)Buf;
      if(Size < 5)
      {
        FFSS_PrintDebug(1,"Size of the message is less than 5 (%d)... DoS attack ?\n",Size);
        len = 0;
        break;
      }
      if(Size > len)
      {
        FFSS_PrintDebug(5,"Warning, Size of the message is greater than received data (%d - %d)... Message splitted ?\n",Size,len);
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
            FFSS_PrintDebug(1,"Error in UDP common thread, unknown thread type !\n");
            SU_END_THREAD(NULL);
        }
        if(len > Size)
        {
          FFSS_PrintDebug(5,"Warning, Size of the message is less than received data (%d - %d)... multiple messages ?\n",Size,len);
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
  FFSS_PrintDebug(1,"Received a BROKEN PIPE signal (thread %ld)\n",SU_THREAD_SELF);
  SU_END_THREAD(NULL);
}
void FFSS_SignalHandler_Term(int sig)
{
  FFSS_PrintDebug(1,"Received a TERM signal (thread %ld)\n",SU_THREAD_SELF);
  SU_END_THREAD(NULL);
}

#endif /* !FFSS_DRIVER */
