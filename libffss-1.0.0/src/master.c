#ifndef FFSS_DRIVER

#include "ffss.h"
#include "utils.h"
#include "common.h"

SU_PServerInfo FM_SI_UDP=NULL,FM_SI_OUT_UDP=NULL;
SU_THREAD_HANDLE FM_THR_UDP;

void FM_AnalyseUDP(struct sockaddr_in Client,char Buf[],long int Len)
{
  int Type;
  char *str,*str2,*str3,*str4;
  long int pos;
  FFSS_Field val,val2,val3,val4,val5;
  FFSS_Field i,state,type_ip,type_ip2;
  char IP[512],IP2[512];
  bool error,free_it;
  char *u_Buf;
  long int u_pos,u_Len;

  context;
  Type = *(FFSS_Field *)(Buf+sizeof(FFSS_Field));
  pos = sizeof(FFSS_Field)*2;
  free_it = false;
  u_Buf = Buf;
  u_Len = Len;
  switch(Type)
  {
    case FFSS_MESSAGE_STATE :
      context;
      FFSS_PrintDebug(3,"Received a state message from server\n");
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      str2 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      str3 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (val2 == 0) || (str == NULL) || (str2 == NULL) || (str3 == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
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
        default :
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
        if(FFSS_CB.MCB.OnNewState != NULL)
          FFSS_CB.MCB.OnNewState(state,IP2,str,str2,str3,str4,IP);
      }
      break;
    case FFSS_MESSAGE_SERVER_LISTING:
      context;
      FFSS_PrintDebug(3,"Received a server listing message\n");
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      str2 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((str == NULL) || (str2 == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.MCB.OnServerListing != NULL)
        FFSS_CB.MCB.OnServerListing(Client,str,str2,val);
      break;
    case FFSS_MESSAGE_CLIENT_SERVER_FAILED:
      context;
      FFSS_PrintDebug(3,"Received a client/server failed message from client\n");
      type_ip = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      FFSS_UnpackIP(Buf,Buf+pos,Len,&pos,IP,type_ip);
      if((type_ip == 0) || (IP[0] == 0))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.MCB.OnClientServerFailed != NULL)
        FFSS_CB.MCB.OnClientServerFailed(IP);
      break;
    case FFSS_MESSAGE_PONG :
      context;
      FFSS_PrintDebug(3,"Received a pong message from server\n");
      state = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if(state == 0)
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.MCB.OnPong != NULL)
        FFSS_CB.MCB.OnPong(Client,state);
      break;
    case FFSS_MESSAGE_DOMAINS_LISTING :
      context;
      FFSS_PrintDebug(3,"Received a domains listing message from client\n");
      if(FFSS_CB.MCB.OnDomainListing != NULL)
        FFSS_CB.MCB.OnDomainListing(Client);
      break;
    case FFSS_MESSAGE_SEARCH :
      context;
      FFSS_PrintDebug(3,"Received a friandise search message from client\n");
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      str2 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (str == NULL) || (str2 == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.MCB.OnSearch != NULL)
        FFSS_CB.MCB.OnSearch(Client,val,str,str2,val2);
      break;
    case FFSS_MESSAGE_SEARCH_FW :
      context;
      FFSS_PrintDebug(3,"Received a forwarded friandise search message from master\n");
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      type_ip = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      FFSS_UnpackIP(Buf,Buf+pos,Len,&pos,IP,type_ip);
      if((type_ip == 0) || (IP[0] == 0))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (str == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.MCB.OnSearchForward != NULL)
        FFSS_CB.MCB.OnSearchForward(Client,IP,val,str,val2);
      break;
    case FFSS_MESSAGE_SEARCH_MASTER :
      context;
      FFSS_PrintDebug(3,"Received a master search message from client or server\n");
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if(val == 0)
      {
        FFSS_PrintSyslog(LOG_WARNING,"One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.MCB.OnMasterSearch != NULL)
        FFSS_CB.MCB.OnMasterSearch(Client,val == FFSS_THREAD_SERVER);
      break;
    case FFSS_MESSAGE_INDEX_ANSWER :
      context;
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val3 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val4 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val5 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      FFSS_PrintDebug(3,"Received an index answer message (index size : %ld/%ld/%ld) Connect to %ld\n",val2,val3,val4,val5);
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
      FFSS_PrintDebug(3,"Received a samba index answer message (index size : %ld/%ld/%ld) Connect to %ld\n",val2,val3,val4,val5);
      error = false;
      if(FFSS_CB.MCB.OnIndexAnswerSamba != NULL)
        FFSS_CB.MCB.OnIndexAnswerSamba(Client,val,val2,val3,val4,val5);
      break;
    default :
      FFSS_PrintSyslog(LOG_WARNING,"Unknown message type (%s) : %d ... DoS attack ?\n",inet_ntoa(Client.sin_addr),Type);
  }
  if(free_it)
    free(u_Buf);
}

/* FFSS Master : Init */
/* Initialisation of the FFSS Master - Must be called before any other FFSS function */
/* Returns true on success, false otherwise */
bool FM_Init(int MasterPort)
{
#ifdef FFSS_CONTEXT
  signal(SIGSEGV, FFSS_handle_SIGNAL);
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
  if(!SU_WSInit(2,2))
    return false;
#endif /* _WIN32 */
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
  if(SU_SetSocketOpt(FM_SI_OUT_UDP->sock,SO_BROADCAST,1) == -1)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error setting broadcast option to socket\n",errno,strerror(errno));
    SU_ServerDisconnect(FM_SI_UDP);
    SU_ServerDisconnect(FM_SI_OUT_UDP);
    free(FM_SI_UDP);
    free(FM_SI_OUT_UDP);
    return false;
  }
  context;
  if(!SU_CreateThread(&FM_THR_UDP,F_ThreadUDP,(void *)FFSS_THREAD_MASTER,false))
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating UDP thread\n");
    SU_ServerDisconnect(FM_SI_UDP);
    SU_ServerDisconnect(FM_SI_OUT_UDP);
    free(FM_SI_UDP);
    free(FM_SI_OUT_UDP);
    return false;
  }
  FFSS_PrintSyslog(LOG_INFO,"FFSS master waiting on port %d (UDP)\n",MasterPort);
  return true;
}

/* FFSS Master : UnInit */
/* Uninitialisation of the FFSS Master - Must be called at the end of the main */
/* Returns true on success, false otherwise */
bool FM_UnInit(void)
{
  FFSS_ShuttingDown = true;
  SU_TermThread(FM_THR_UDP);
  SU_ServerDisconnect(FM_SI_UDP);
  SU_ServerDisconnect(FM_SI_OUT_UDP);
  if(FM_SI_UDP != NULL)
    free(FM_SI_UDP);
  if(FM_SI_OUT_UDP != NULL)
    free(FM_SI_OUT_UDP);
  if(FFSS_MyIP != NULL)
    free(FFSS_MyIP);
  FFSS_PrintSyslog(LOG_INFO,"FFSS master shut down\n");
#ifdef _WIN32
  SU_CloseLogFile(FFSS_LogFile);
  SU_WSUninit();
#endif /* _WIN32 */
  return true;
}

#endif /* !FFSS_DRIVER */
