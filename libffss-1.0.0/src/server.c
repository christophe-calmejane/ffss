#ifndef FFSS_DRIVER

#include "ffss.h"
#include "utils.h"
#include "common.h"

bool FS_FTP;
SU_PServerInfo FS_SI_UDP=NULL,FS_SI_OUT_UDP=NULL,FS_SI_TCP=NULL,FS_SI_TCP_FTP=NULL;
SU_THREAD_HANDLE FS_THR_UDP,FS_THR_TCP,FS_THR_TCP_FTP;

char *FFSS_ErrorTable[]={"Nothing","Protocol version mismatch","Resource not available","Wrong login/password, or not specified","Too many connections","File or directory not found","Access denied","Not enough space","Cannot connect","Internal error","Too many active transfers","Directory not empty","File already exists","Idle time out","Quiet mode","Share is disabled","Ejected from share","Your message will overflow my receipt buffer","Requested transfer mode not supported","Please resend last UDP message","Bad search request","Too many answers","Socket Error","Possible DoS attack"};

typedef struct
{
  SU_PClientSocket Client;
  void *Info;
  FFSS_LongField User;
} FS_TInterThreadTmp, *FS_PInterThreadTmp;

FFSS_LongField FFSS_CurrentConnectionUserInfo;

void FS_AnalyseUDP(struct sockaddr_in Client,char Buf[],long int Len)
{
  int Type;
  char *str;
  long int pos;
  FFSS_Field val;
  FFSS_LongField lval;

  Type = *(FFSS_Field *)(Buf+sizeof(FFSS_Field));
  pos = sizeof(FFSS_Field)*2;
  switch(Type)
  {
    case FFSS_MESSAGE_PING :
      context;
      FFSS_PrintDebug(3,"Received a ping message\n");
      if(FFSS_CB.SCB.OnPing != NULL)
        FFSS_CB.SCB.OnPing(Client);
      break;
    case FFSS_MESSAGE_STATE_ANSWER :
      context;
      FFSS_PrintDebug(3,"Received a state answer from master\n");
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if(str == NULL)
      {
        FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_STATE_ANSWER : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.SCB.OnStateAnswer != NULL)
        FFSS_CB.SCB.OnStateAnswer(str);
      break;
    case FFSS_MESSAGE_SERVER_SEARCH :
      context;
      FFSS_PrintDebug(3,"Received a server search from client\n");
      if(FFSS_CB.SCB.OnServerSearch != NULL)
        FFSS_CB.SCB.OnServerSearch(Client);
      break;
    case FFSS_MESSAGE_SHARES_LISTING :
      context;
      FFSS_PrintDebug(3,"Received a shares listing request from client\n");
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      if(FFSS_CB.SCB.OnSharesListing != NULL)
        FFSS_CB.SCB.OnSharesListing(Client,lval);
      break;
    case FFSS_MESSAGE_INDEX_REQUEST :
      context;
      FFSS_PrintDebug(3,"Received a index request\n");
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if(val == 0)
      {
        FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_INDEX_REQUEST : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client.sin_addr));
        break;
      }
      if(FFSS_CB.SCB.OnIndexRequest != NULL)
        FFSS_CB.SCB.OnIndexRequest(Client,val);
      break;
    case FFSS_MESSAGE_ERROR :
      context;
      FFSS_PrintDebug(3,"Received an error message\n");
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (str == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_ERROR : One or many fields empty, or out of buffer (%s) (%ld,%p) ... DoS attack ?\n",inet_ntoa(Client.sin_addr),val,str);
        break;
      }
      if(FFSS_CB.SCB.OnError != NULL)
        FFSS_CB.SCB.OnError(val,str);
      break;
    case FFSS_MESSAGE_SEARCH_MASTER_ANSWER :
      context;
      lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (str == NULL))
      {
        FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_SEARCH_MASTER_ANSWER : One or many fields empty, or out of buffer (%s) (%ld,%p) ... DoS attack ?\n",inet_ntoa(Client.sin_addr),val,str);
        break;
      }
      if(FFSS_CB.SCB.OnMasterSearchAnswer != NULL)
        FFSS_CB.SCB.OnMasterSearchAnswer(Client,val,str,lval);
      break;
    default :
      FFSS_PrintSyslog(LOG_WARNING,"Unknown message type (%s) : %d ... DoS attack ?\n",inet_ntoa(Client.sin_addr),Type);
  }
}

bool FS_AnalyseTCP(SU_PClientSocket Client,char Buf[],long int Len,bool ident,void **Info)
{
  int Type;
  long int pos;
  char *str,*str2,*str3;
  FFSS_Field val,val2,val3;
  FFSS_LongField lval,lval2;
  bool ret_val;
  long int Length;

  Type = *(FFSS_Field *)(Buf+sizeof(FFSS_Field));
  pos = sizeof(FFSS_Field)*2;
  ret_val = true;

  if(Type == FFSS_MESSAGE_SHARE_CONNECTION)
  {
    if(ident) /* Already identified */
    {
      FFSS_PrintSyslog(LOG_WARNING,"From %s : Already identified... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr));
      return false;
    }
    FFSS_PrintDebug(3,"Received a share connection message from client\n");
    FFSS_CurrentConnectionUserInfo = lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
    val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
    val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
    str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
    str2 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
    str3 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
    if((str == NULL) || (str2 == NULL) || (str3 == NULL))
    {
      FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_SHARE_CONNECTION : One or many fields empty, or out of buffer (%s) (%p,%p,%p) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr),str,str2,str3);
      ret_val = false;
    }
    else
    {
      if((val > FFSS_PROTOCOL_VERSION) || (val < FFSS_PROTOCOL_VERSION_LEAST_COMPATIBLE))
      {
        FS_SendMessage_Error(Client->sock,FFSS_ERROR_PROTOCOL_VERSION_ERROR,FFSS_ErrorTable[FFSS_ERROR_PROTOCOL_VERSION_ERROR],FFSS_PROTOCOL_VERSION,lval);
        ret_val = false;
      }
      else
      {
        if(FFSS_CB.SCB.OnShareConnection != NULL)
        {
          *Info = FFSS_CB.SCB.OnShareConnection(Client,str,str2,str3,val2,lval);
          return (*Info) != NULL;
        }
        else
          return false;
      }
    }
  }
  else
  {
    if(ident == false)
    {
      FFSS_PrintSyslog(LOG_WARNING,"From %s : First message was NOT a ShareConnection message... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr));
      return false;
    }
    switch(Type)
    {
      case FFSS_MESSAGE_DIRECTORY_LISTING :
        context;
        FFSS_PrintDebug(3,"Received a directory listing message from client\n");
        lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if(str == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_DIRECTORY_LISTING : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr));
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnDirectoryListing != NULL)
          ret_val = FFSS_CB.SCB.OnDirectoryListing(Client,str,lval);
        break;
      case FFSS_MESSAGE_REC_DIR_LISTING :
        context;
        FFSS_PrintDebug(3,"Received a recursive directory listing message from client\n");
        lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if(str == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_REC_DIR_LISTING : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr));
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnRecursiveDirectoryListing != NULL)
          ret_val = FFSS_CB.SCB.OnRecursiveDirectoryListing(Client,str,lval);
        break;
      case FFSS_MESSAGE_DOWNLOAD :
        context;
        FFSS_PrintDebug(3,"Received a download message from client\n");
        lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        if((str == NULL) || (val2 == 0))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_DOWNLOAD : One or many fields empty, or out of buffer (%s) (%p,%ld) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr),str,val2);
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnDownload != NULL)
          ret_val = FFSS_CB.SCB.OnDownload(Client,str,lval,val2,lval2);
        break;
      case FFSS_MESSAGE_UPLOAD :
        context;
        FFSS_PrintDebug(3,"Received an upload message from client\n");
        lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        if((str == NULL) || (val2 == 0))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_UPLOAD : One or many fields empty, or out of buffer (%s) (%p,%ld) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr),str,val2);
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnUpload != NULL)
          ret_val = FFSS_CB.SCB.OnUpload(Client,str,lval,val2,lval2);
        break;
      case FFSS_MESSAGE_MOVE :
        context;
        FFSS_PrintDebug(3,"Received a move message from client\n");
        lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        str2 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if((str == NULL) || (str2 == NULL))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_MOVE : One or many fields empty, or out of buffer (%s) (%p,%p) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr),str,str2);
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnRename != NULL)
          ret_val = FFSS_CB.SCB.OnRename(Client,str,str2,lval2);
        break;
      case FFSS_MESSAGE_COPY :
        context;
        FFSS_PrintDebug(3,"Received a copy message from client\n");
        lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        str2 = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if((str == NULL) || (str2 == NULL))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_COPY : One or many fields empty, or out of buffer (%s) (%p,%p) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr),str,str2);
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnCopy != NULL)
          ret_val = FFSS_CB.SCB.OnCopy(Client,str,str2,lval2);
        break;
      case FFSS_MESSAGE_DELETE :
        context;
        FFSS_PrintDebug(3,"Received a delete message from client\n");
        lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if(str == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_DELETE : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr));
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnDelete != NULL)
          ret_val = FFSS_CB.SCB.OnDelete(Client,str,lval2);
        break;
      case FFSS_MESSAGE_MKDIR :
        context;
        FFSS_PrintDebug(3,"Received a mkdir message from client\n");
        lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if(str == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_MKDIR : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr));
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnMkDir != NULL)
          ret_val = FFSS_CB.SCB.OnMkDir(Client,str,lval2);
        break;
      case FFSS_MESSAGE_DISCONNECT :
        context;
        FFSS_PrintDebug(3,"Received a disconnect message from client\n");
        ret_val = false;
        if(FFSS_CB.SCB.OnDisconnect != NULL)
          ret_val = FFSS_CB.SCB.OnDisconnect(Client);
        break;
      case FFSS_MESSAGE_CANCEL_XFER :
        context;
        FFSS_PrintDebug(3,"Received a cancel xfer message from client\n");
        val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        if(FFSS_CB.SCB.OnCancelXFer != NULL)
          FFSS_CB.SCB.OnCancelXFer(Client,val);
        break;
      case FFSS_MESSAGE_STREAMING_OPEN :
        context;
        FFSS_PrintDebug(3,"Received a streaming OPEN message from client\n");
        lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
        if((val == 0) || (str == NULL))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_STREAMING_OPEN : One or many fields empty, or out of buffer (%s) (%ld,%p) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr),val,str);
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnStrmOpen != NULL)
          FFSS_CB.SCB.OnStrmOpen(Client,val,str,lval2);
        break;
      case FFSS_MESSAGE_STREAMING_CLOSE :
        context;
        FFSS_PrintDebug(3,"Received a streaming CLOSE message from client\n");
        val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        if(val == 0)
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_STREAMING_CLOSE : One or many fields empty, or out of buffer (%s) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr));
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnStrmClose != NULL)
          FFSS_CB.SCB.OnStrmClose(Client,val);
        break;
      case FFSS_MESSAGE_STREAMING_READ :
        context;
        FFSS_PrintDebug(3,"Received a streaming READ message from client\n");
        lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        val3 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        if((val == 0) || (val3 == 0))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_STREAMING_READ : One or many fields empty, or out of buffer (%s) (%ld,%ld) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr),val,val3);
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnStrmRead != NULL)
          FFSS_CB.SCB.OnStrmRead(Client,val,lval,val3,lval2);
        break;
      case FFSS_MESSAGE_STREAMING_WRITE :
        context;
        FFSS_PrintDebug(3,"Received a streaming WRITE message from client\n");
        lval2 = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        Length = Len-FFSS_MESSAGESIZE_STREAMING_WRITE*sizeof(FFSS_Field);
        if((val == 0) || (Length < 0))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_STREAMING_WRITE : One or many fields empty, or out of buffer (%s) (%ld,%ld) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr),val,Length);
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnStrmWrite != NULL)
          FFSS_CB.SCB.OnStrmWrite(Client,val,lval,Buf+pos,Length,lval2);
        break;
      case FFSS_MESSAGE_STREAMING_SEEK :
        context;
        FFSS_PrintDebug(3,"Received a streaming SEEK message from client\n");
        val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
        lval = FFSS_UnpackLongField(Buf,Buf+pos,Len,&pos);
        if((val == 0) || (val2 == 0))
        {
          FFSS_PrintSyslog(LOG_WARNING,"FFSS_MESSAGE_STREAMING_SEEK : One or many fields empty, or out of buffer (%s) (%ld,%ld) ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr),val,val2);
          ret_val = false;
          break;
        }
        if(FFSS_CB.SCB.OnStrmSeek != NULL)
          FFSS_CB.SCB.OnStrmSeek(Client,val,val2,lval);
        break;
      default :
        FFSS_PrintSyslog(LOG_WARNING,"Unknown message type (%s) : %d ... DoS attack ?\n",inet_ntoa(Client->SAddr.sin_addr),Type);
        ret_val = false;
    }
  }
  return ret_val;
}

bool FS_AnalyseTCP_FTP(SU_PClientSocket Client,char Buf[],long int Len)
{
  SU_PClientSocket DataPort;
  char *p,*cmd;
  char msg[4000];
  static char H[100],P[50];
  char *a,*b,*c,*d;
  char *x,*y;
#ifdef __unix__
  char *tmp;
#endif
  bool ret_val;

  p = strchr(Buf,' ');
  if(p != NULL)
  {
    p[0] = 0;
    p++;
  }
  cmd = Buf;

  if(strcasecmp(cmd,"HELP") == 0)
  {
    FFSS_PrintDebug(3,"Received a HELP message from FTP\n");
    snprintf(msg,sizeof(msg),"214-The following commands are recognized (* =>'s unimplemented)." CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    snprintf(msg,sizeof(msg),"214-USER    PASS*   ACCT*   CWD     XCWD*   CDUP    XCUP*   SMNT*" CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    snprintf(msg,sizeof(msg),"214-QUIT    REIN*   PORT    PASV*   TYPE    STRU*   MODE    RETR" CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    snprintf(msg,sizeof(msg),"214-STOR*   STOU*   APPE*   ALLO*   REST    RNFR    RNTO    ABOR*" CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    snprintf(msg,sizeof(msg),"214-DELE*   MDTM*   RMD*    XRMD*   MKD*    XMKD*   PWD     XPWD*" CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    snprintf(msg,sizeof(msg),"214-SIZE*   LIST    NLST*   SITE*   SYST    STAT*   HELP    NOOP" CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  }
  else if(strcasecmp(cmd,"QUIT") == 0)
  {
    FFSS_PrintDebug(3,"Received a QUIT message from FTP\n");
    snprintf(msg,sizeof(msg),"221 Goodbye." CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    return false;
  }
  else if(strcasecmp(cmd,"USER") == 0)
  {
    FFSS_PrintDebug(3,"Received a USER message from FTP\n");
    snprintf(msg,sizeof(msg),"230 Logged in." CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  }
  else if(strcasecmp(cmd,"NOOP") == 0)
  {
    FFSS_PrintDebug(3,"Received a NOOP message from FTP\n");
    snprintf(msg,sizeof(msg),"200 NOOP command successful." CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  }
  else if(strcasecmp(cmd,"SYST") == 0)
  {
    FFSS_PrintDebug(3,"Received a SYST message from FTP\n");
    snprintf(msg,sizeof(msg),"215 Unix Type: L8" CRLF);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  }
  else if(strcasecmp(cmd,"PORT") == 0)
  {
    FFSS_PrintDebug(3,"Received a PORT message from FTP\n");
    if(Len < 6)
    {
      snprintf(msg,sizeof(msg),"501 Missing Host/Port." CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    }
    else
    {
      a = strtok_r(cmd+5,",",&tmp);
      if(a != NULL)
      {
        b = strtok_r(NULL,",",&tmp);
        if(b != NULL)
        {
          c = strtok_r(NULL,",",&tmp);
          if(c != NULL)
          {
            d = strtok_r(NULL,",",&tmp);
            if(d != NULL)
            {
              x = strtok_r(NULL,",",&tmp);
              if(x != NULL)
              {
                y = strtok_r(NULL,",",&tmp);
                if(y != NULL)
                {
                  snprintf(H,sizeof(H),"%d.%d.%d.%d",atoi(a),atoi(b),atoi(c),atoi(d));
                  snprintf(P,sizeof(P),"%d",atoi(x)*256+atoi(y));
                  snprintf(msg,sizeof(msg),"200 PORT command successful." CRLF);
                  send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
                  return true;
                }
              }
            }
          }
        }
      }
      snprintf(msg,sizeof(msg),"421 Error in Host/Port." CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    }
  }
  else if(strcasecmp(cmd,"LIST") == 0)
  {
    FFSS_PrintDebug(3,"Received a LIST message from FTP\n");
    if(H[0] == 0)
    { /* Didn't received a PORT message */
      snprintf(msg,sizeof(msg),"503 Must receive a PORT command before." CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    }
    else
    {
      snprintf(msg,sizeof(msg),"150 Opening ASCII mode data connection for /bin/ls." CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      DataPort = SU_ClientConnect(H,P,SOCK_STREAM);
      if(DataPort == NULL)
      {
        snprintf(msg,sizeof(msg),"425 Can't open data connection to %s:%s : %d." CRLF,H,P,errno);
        send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      }
      else
      {
        ret_val = false;
        if(FFSS_CB.SCB.OnDirectoryListingFTP != NULL)
          ret_val = FFSS_CB.SCB.OnDirectoryListingFTP(Client,DataPort,p);
        SU_FreeCS(DataPort);
        DataPort = NULL;
        H[0] = 0;
        if(ret_val)
          snprintf(msg,sizeof(msg),"226 Transfer complete.\n");
        else
          snprintf(msg,sizeof(msg),"426 Unknown directory." CRLF);
        send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      }
    }
  }
  else if(strcasecmp(cmd,"RETR") == 0)
  {
    FFSS_PrintDebug(3,"Received a RETR message from FTP for %s\n",p);
    if(H[0] == 0)
    { /* Didn't received a PORT message */
      snprintf(msg,sizeof(msg),"503 Must receive a PORT command before." CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    }
    else
    {
      if((p == NULL) || (FFSS_CB.SCB.OnDownloadFTP == NULL))
      {
        snprintf(msg,sizeof(msg),"501 Missing RETR argument" CRLF);
        send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
      }
      else
        FFSS_CB.SCB.OnDownloadFTP(Client,p,0,H,P);
      H[0] = 0;
    }
  }
  else if(strcasecmp(cmd,"PWD") == 0)
  {
    FFSS_PrintDebug(3,"Received a PWD message from FTP\n");
    if(FFSS_CB.SCB.OnPWDFTP != NULL)
      FFSS_CB.SCB.OnPWDFTP(Client);
  }
  else if(strcasecmp(cmd,"TYPE") == 0)
  {
    FFSS_PrintDebug(3,"Received a TYPE message from FTP\n");
    if((p == NULL) || (FFSS_CB.SCB.OnTypeFTP == NULL))
    {
      snprintf(msg,sizeof(msg),"501 Missing TYPE argument" CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    }
    else
      FFSS_CB.SCB.OnTypeFTP(Client,p[0]);
  }
  else if(strcasecmp(cmd,"MODE") == 0)
  {
    FFSS_PrintDebug(3,"Received a MODE message from FTP\n");
    if((p == NULL) || (FFSS_CB.SCB.OnModeFTP == NULL))
    {
      snprintf(msg,sizeof(msg),"501 Missing MODE argument" CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    }
    else
      FFSS_CB.SCB.OnModeFTP(Client,p[0]);
  }
  else if(strcasecmp(cmd,"CWD") == 0)
  {
    FFSS_PrintDebug(3,"Received a CWD message from FTP\n");
    if((p == NULL) || (FFSS_CB.SCB.OnCWDFTP == NULL))
    {
      snprintf(msg,sizeof(msg),"501 Missing CWD argument" CRLF);
      send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
    }
    else
      FFSS_CB.SCB.OnCWDFTP(Client,p);
  }
  else if(strcasecmp(cmd,"CDUP") == 0)
  {
    FFSS_PrintDebug(3,"Received a CDUP message from FTP\n");
    FFSS_CB.SCB.OnCWDFTP(Client,"..");
  }
  else if(strcasecmp(cmd,"A") == 0)
  {
    FFSS_PrintDebug(3,"Received a QUIT message from FTP\n");
  }
  else /* Unknown command */
  {
    FFSS_PrintDebug(1,"Unknown FTP command : %s\n",cmd);
    snprintf(msg,sizeof(msg),"500 %s not understood" CRLF,cmd);
    send(Client->sock,msg,strlen(msg),SU_MSG_NOSIGNAL);
  }
  return true;
}

SU_THREAD_ROUTINE(FS_ClientThreadTCP,User)
{
  FS_PInterThreadTmp Tmp = (FS_PInterThreadTmp) User;
  SU_PClientSocket Client;
  void *Info;
  unsigned int len;
  int res;
  FFSS_Field Size;
  bool analyse;
  int do_not_do;
  fd_set rfds;
  struct timeval tv;
  int retval;
  char *Buf;
  unsigned long int BufSize;
  FFSS_LongField UserInfo;

  SU_ThreadBlockSigs();
  Client = Tmp->Client;
  Info = Tmp->Info;
  UserInfo = Tmp->User;
  free(Tmp);

  BufSize = FFSS_TCP_SERVER_BUFFER_SIZE;
  Buf = (char *) malloc(BufSize);
  if(Buf == NULL)
  {
    FS_SendMessage_Error(Client->sock,FFSS_ERROR_INTERNAL_ERROR,FFSS_ErrorTable[FFSS_ERROR_INTERNAL_ERROR],0,UserInfo);
    SU_FreeCS(Client);
    SU_END_THREAD(NULL);
  }

  if(FFSS_CB.SCB.OnBeginTCPThread != NULL)
    FFSS_CB.SCB.OnBeginTCPThread(Client,Info);
  len = 0;
  while(1)
  {
    do_not_do = 0;
    if(FFSS_CB.SCB.OnSelect != NULL)
      do_not_do = FFSS_CB.SCB.OnSelect();
    FD_ZERO(&rfds);
    FD_SET(Client->sock,&rfds);
    if(do_not_do == 2) /* Do not sleep */
      tv.tv_sec = 0;
    else
      tv.tv_sec = 4;
    tv.tv_usec = 0;
    retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
    if(!retval) /* Time out, checking if Server has now defined a time out */
    {
      if((Client->User == 0) || (do_not_do != 0))
        continue;
    }
    if(Client->User != 0) /* Idle time out value */
    {
      FFSS_PrintDebug(3,"Server has defined a idle time out value of %d sec for this connection\n",(int)Client->User);
      FD_ZERO(&rfds);
      FD_SET(Client->sock,&rfds);
      tv.tv_sec = (int)Client->User;
      tv.tv_usec = 0;
      retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
      if(!retval)
      {
        if(FFSS_CB.SCB.OnIdleTimeout != NULL)
          FFSS_CB.SCB.OnIdleTimeout(Client);
        FFSS_PrintDebug(1,"Error on TCP port of the server (TIME OUT)\n");
        if(FFSS_CB.SCB.OnEndTCPThread != NULL)
          FFSS_CB.SCB.OnEndTCPThread();
        SU_FreeCS(Client);
        free(Buf);
        SU_END_THREAD(NULL);
      }
    }

    if(len >= BufSize)
    {
      FFSS_PrintSyslog(LOG_INFO,"WARNING : Server's buffer too short for this message !!\n");
    }
    res = recv(Client->sock,Buf+len,BufSize-len,SU_MSG_NOSIGNAL);
    if(res == SOCKET_ERROR)
    {
      FFSS_PrintDebug(1,"Error on TCP port of the server (SOCKET_ERROR : %d)\n",errno);
      if(FFSS_CB.SCB.OnEndTCPThread != NULL)
        FFSS_CB.SCB.OnEndTCPThread();
      SU_FreeCS(Client);
      free(Buf);
      SU_END_THREAD(NULL);
    }
    else if(res == 0)
    {
      if(FFSS_CB.SCB.OnEndTCPThread != NULL)
        FFSS_CB.SCB.OnEndTCPThread();
      SU_FreeCS(Client);
      free(Buf);
      SU_END_THREAD(NULL);
    }
    len += res;
    FFSS_PrintDebug(6,"Data found on TCP port from %s (%s) ... analysing\n",inet_ntoa(Client->SAddr.sin_addr),SU_NameOfPort(inet_ntoa(Client->SAddr.sin_addr)));
    analyse = true;
    while(analyse)
    {
      if(len < 5)
      {
        FFSS_PrintSyslog(LOG_WARNING,"Length of the message is less than 5 (%d) (%s) ... DoS attack ?\n",len,inet_ntoa(Client->SAddr.sin_addr));
        if(FFSS_CB.SCB.OnEndTCPThread != NULL)
          FFSS_CB.SCB.OnEndTCPThread();
        SU_FreeCS(Client);
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
        if(!FS_AnalyseTCP(Client,Buf,Size,true,NULL))
        {
          if(FFSS_CB.SCB.OnEndTCPThread != NULL)
            FFSS_CB.SCB.OnEndTCPThread();
          SU_FreeCS(Client);
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

SU_THREAD_ROUTINE(FS_ClientThreadTCP_FTP,User)
{
  SU_PClientSocket Client = (SU_PClientSocket) User;
  char *p;
  unsigned int len;
  int res;
  FFSS_Field Size;
  bool analyse,ret_val;
  fd_set rfds;
  struct timeval tv;
  int retval;
  char *Buf;
  unsigned long int BufSize;

  SU_ThreadBlockSigs();
  BufSize = FFSS_TCP_SERVER_BUFFER_SIZE;
  Buf = (char *) malloc(BufSize);

  if(FFSS_CB.SCB.OnConnectionFTP != NULL)
  {
    ret_val = FFSS_CB.SCB.OnConnectionFTP(Client);
    if(!ret_val)
    {
      if(FFSS_CB.SCB.OnEndTCPThreadFTP != NULL)
        FFSS_CB.SCB.OnEndTCPThreadFTP();
      SU_FreeCS(Client);
      free(Buf);
      SU_END_THREAD(NULL);
    }
  }
  /* Send Server's (c) and logged in message */
  snprintf(Buf,BufSize,"220 %s" CRLF,FFSS_FTP_SERVER);
  send(Client->sock,Buf,strlen(Buf),SU_MSG_NOSIGNAL);
  len = 0;
  while(1)
  {
    FD_ZERO(&rfds);
    FD_SET(Client->sock,&rfds);
    tv.tv_sec = ((int)Client->User == 0)?FFSS_TIMEOUT_FTP:(int)Client->User;
    tv.tv_usec = 0;
    retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
    if(!retval)
    {
      if(FFSS_CB.SCB.OnIdleTimeoutFTP != NULL)
        FFSS_CB.SCB.OnIdleTimeoutFTP(Client);
      FFSS_PrintDebug(1,"Error on TCP FTP port of the server (TIME OUT)\n");
      if(FFSS_CB.SCB.OnEndTCPThreadFTP != NULL)
        FFSS_CB.SCB.OnEndTCPThreadFTP();
      SU_FreeCS(Client);
      free(Buf);
      SU_END_THREAD(NULL);
    }

    if(len >= BufSize)
    {
      FFSS_PrintSyslog(LOG_INFO,"WARNING : Server's FTP buffer too short for this message !!\n");
    }
    res = recv(Client->sock,Buf+len,BufSize-len,SU_MSG_NOSIGNAL);
    if(res == SOCKET_ERROR)
    {
      FFSS_PrintDebug(1,"Error on TCP FTP port of the server (SOCKET_ERROR : %d)\n",errno);
      if(FFSS_CB.SCB.OnEndTCPThreadFTP != NULL)
        FFSS_CB.SCB.OnEndTCPThreadFTP();
      SU_FreeCS(Client);
      free(Buf);
      SU_END_THREAD(NULL);
    }
    else if(res == 0)
    {
      if(FFSS_CB.SCB.OnEndTCPThreadFTP != NULL)
        FFSS_CB.SCB.OnEndTCPThreadFTP();
      SU_FreeCS(Client);
      free(Buf);
      SU_END_THREAD(NULL);
    }
    len += res;
    FFSS_PrintDebug(6,"Data found on TCP FTP port from %s (%s) ... analysing\n",inet_ntoa(Client->SAddr.sin_addr),SU_NameOfPort(inet_ntoa(Client->SAddr.sin_addr)));
    analyse = true;
    while(analyse)
    {
      Size = 0;
      p = NULL;
      while(Size < len)
      {
        if((Buf[Size] == 0x0D) || (Buf[Size] == 0x0A))
        {
          p = Buf+Size;
          break;
        }
        Size++;
      }
      if(p == NULL)
      {
        FFSS_PrintDebug(5,"Warning, Couldn't find CR/LF... Message splitted ?\n");
        break;
        /* Keeps waiting for data */
      }
      else
      {
        p[0] = 0;
        if(!FS_AnalyseTCP_FTP(Client,Buf,Size))
        {
          if(FFSS_CB.SCB.OnEndTCPThreadFTP != NULL)
            FFSS_CB.SCB.OnEndTCPThreadFTP();
          SU_FreeCS(Client);
          free(Buf);
          SU_END_THREAD(NULL);
        }
        Size++;
        while(Size < len)
        {
          if((Buf[Size] != 0x0D) && (Buf[Size] != 0x0A))
            break;
          Size++;
        }
        if(len > Size)
        {
          FFSS_PrintDebug(5,"Warning, Data found after CR/LF... multiple messages ?\n");
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

SU_THREAD_ROUTINE(FS_ThreadTCP,User)
{
  SU_PClientSocket Client;
  SU_THREAD_HANDLE ClientThr;
  char *IP;
  char Buf[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SHARE_CONNECTION + FFSS_MAX_SHARENAME_LENGTH+1 + FFSS_MAX_LOGIN_LENGTH+1 + FFSS_MAX_PASSWORD_LENGTH+1];
  FFSS_Field MsgSize,Pos;
  fd_set rfds;
  struct timeval tv;
  int retval,res;
  bool Error;
  FS_PInterThreadTmp TmpStruct;
  void *Info;

  SU_ThreadBlockSigs();
  if(SU_ServerListen(FS_SI_TCP) == SOCKET_ERROR)
  {
    FFSS_PrintSyslog(LOG_ERR,"Couldn't listen on the TCP socket\n");
    SU_END_THREAD(NULL);
  }
  FFSS_PrintDebug(1,"TCP thread launched...waiting for connections\n");
  while(1)
  {
    Client = SU_ServerAcceptConnection(FS_SI_TCP);
    if(FFSS_ShuttingDown)
    {
      FFSS_PrintDebug(1,"TCP Routine : FFSS Library is been shut down...\n");
      if(Client != NULL)
        SU_FreeCS(Client);
      SU_END_THREAD(NULL);
    }
    if(Client == NULL)
    {
      SU_SLEEP(1);
      continue;
    }
    IP = inet_ntoa(Client->SAddr.sin_addr);
    /* Check for packet reject */
    switch(FFSS_Filter_GetActionOfChainFromIP(FFSS_FILTER_CHAINS_SERVER_TCP_CONNECTION,INADDR_GET_IP(Client->SAddr.sin_addr)))
    {
      case FFSS_FILTER_ACTION_ACCEPT :
        break;
      case FFSS_FILTER_ACTION_REJECT :
        FFSS_PrintSyslog(LOG_WARNING,"Rejecting client %s\n",IP);
        SU_FreeCS(Client);
        continue;
      default :
        FFSS_PrintDebug(1,"Error in FS_ThreadTCP, unknown filter action\n");
        SU_END_THREAD(NULL);
    }

    FFSS_PrintDebug(5,"Client connected on TCP port of the server from %s (%s) ... checking connection\n",IP,SU_NameOfPort(IP));
    /* Getting Size of first message */
    FD_ZERO(&rfds);
    FD_SET(Client->sock,&rfds);
    tv.tv_sec = 5; /* 5 sec to send share connection message size */
    tv.tv_usec = 0;
    retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
    if(!retval)
    {
      FFSS_PrintSyslog(LOG_WARNING,"Client %s didn't send ShareConnection message size within 5 secondes... DoS attack ?\n",IP);
      SU_FreeCS(Client);
      continue;
    }
    res = recv(Client->sock,(char *)&MsgSize,sizeof(MsgSize),SU_MSG_NOSIGNAL);
    if(res != sizeof(MsgSize))
    {
      FFSS_PrintSyslog(LOG_WARNING,"Error getting client's (%s) ShareConnection message size : res = %d : %s\n",IP,res,(res >= 0)?"No error":strerror(errno));
      SU_FreeCS(Client);
      continue;
    }
    if((MsgSize > sizeof(Buf)) || (MsgSize <= (sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SHARE_CONNECTION)))
    {
      FFSS_PrintSyslog(LOG_WARNING,"From %s : Size of message is bigger than ShareConnection message max size : %ld - %d... DoS attack ?\n",IP,MsgSize,sizeof(Buf));
      SU_FreeCS(Client);
      continue;
    }

    /* Getting message - Must be a ShareConnection message */
    Pos = sizeof(MsgSize);
    Error = false;
    while(Pos != MsgSize)
    {
      FD_ZERO(&rfds);
      FD_SET(Client->sock,&rfds);
      tv.tv_sec = 5; /* 5 sec to send share connection message */
      tv.tv_usec = 0;
      retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
      if(!retval)
      {
        FFSS_PrintSyslog(LOG_WARNING,"Client %s didn't send ShareConnection message within 5 secondes... DoS attack ?\n",IP);
        SU_FreeCS(Client);
        Error = true;
        break;
      }
      /* While buffer is not filled with message */
      res = recv(Client->sock,Buf+Pos,MsgSize-Pos,SU_MSG_NOSIGNAL);
      if(res <= 0)
      {
        FFSS_PrintSyslog(LOG_WARNING,"Error getting client's (%s) ShareConnection message : res = %d : %s\n",IP,res,(res == 0)?"Socket closed":strerror(errno));
        SU_FreeCS(Client);
        Error = true;
        break;
      }
      Pos += res;
    }
    if(Error)
      continue;
    if(!FS_AnalyseTCP(Client,Buf,MsgSize,false,&Info)) /* Connection not accepted by server... */
    {
      FFSS_PrintDebug(5,"Connection from %s was refused by the server\n",IP);
      SU_FreeCS(Client);
      continue;
    }

    FFSS_PrintDebug(5,"Connection from %s (%s) accepted by server ... creating new thread\n",IP,SU_NameOfPort(IP));
    TmpStruct = (FS_PInterThreadTmp) malloc(sizeof(FS_TInterThreadTmp));
    TmpStruct->Client = Client;
    TmpStruct->Info = Info;
    TmpStruct->User = FFSS_CurrentConnectionUserInfo;
    if(!SU_CreateThread(&ClientThr,FS_ClientThreadTCP,(void *)TmpStruct,true))
    {
      FFSS_PrintSyslog(LOG_ERR,"Error creating TCP Client thread\n");
      SU_FreeCS(Client);
      continue;
    }
  }
}

SU_THREAD_ROUTINE(FS_ThreadTCP_FTP,User)
{
  SU_PClientSocket Client;
  SU_THREAD_HANDLE ClientThr;

  SU_ThreadBlockSigs();
  if(SU_ServerListen(FS_SI_TCP_FTP) == SOCKET_ERROR)
  {
    FFSS_PrintSyslog(LOG_ERR,"Couldn't listen on the TCP FTP socket\n");
    SU_END_THREAD(NULL);
  }
  FFSS_PrintDebug(1,"TCP FTP thread launched...waiting for connections\n");
  while(1)
  {
    Client = SU_ServerAcceptConnection(FS_SI_TCP_FTP);
    if(FFSS_ShuttingDown)
    {
      FFSS_PrintDebug(1,"TCP FTP Routine : FFSS Library is been shut down...\n");
      if(Client != NULL)
        SU_FreeCS(Client);
      SU_END_THREAD(NULL);
    }
    if(Client == NULL)
    {
      SU_SLEEP(1);
      continue;
    }
    /* Check for packet reject */
    switch(FFSS_Filter_GetActionOfChainFromIP(FFSS_FILTER_CHAINS_SERVER_TCP_FTP_CONNECTION,INADDR_GET_IP(Client->SAddr.sin_addr)))
    {
      case FFSS_FILTER_ACTION_ACCEPT :
        break;
      case FFSS_FILTER_ACTION_REJECT :
        FFSS_PrintSyslog(LOG_WARNING,"Rejecting ftp client %s\n",inet_ntoa(Client->SAddr.sin_addr));
        SU_FreeCS(Client);
        continue;
      default :
        FFSS_PrintDebug(1,"Error in FS_ThreadTCP_FTP, unknown filter action\n");
        SU_END_THREAD(NULL);
    }

    FFSS_PrintDebug(5,"Client connected on TCP FTP port of the server from %s (%s) ... creating new thread\n",inet_ntoa(Client->SAddr.sin_addr),SU_NameOfPort(inet_ntoa(Client->SAddr.sin_addr)));
    if(!SU_CreateThread(&ClientThr,FS_ClientThreadTCP_FTP,(void *)Client,true))
    {
      FFSS_PrintSyslog(LOG_ERR,"Error creating TCP FTP Client thread\n");
      SU_FreeCS(Client);
      continue;
    }
  }
}

/* FFSS Server : Init */
/* Initialisation of the FFSS Server - Must be called before any other FFSS function */
/* Returns true on success, false otherwise */
bool FS_Init(int ServerPort,bool FTP)
{
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
  {
    if(getenv("FFSS_LOG_FILE") != NULL)
      FFSS_LogFile = SU_OpenLogFile("FFSS_Server.log");
  }
#endif /* !DEBUG && _WIN32 */
  FFSS_ShuttingDown = false;
#ifdef _WIN32
  if(!SU_WSInit(2,2))
    return false;
#endif /* _WIN32 */
  if(!FFSS_Filter_Init(FFSS_THREAD_SERVER))
  {
    FFSS_PrintSyslog(LOG_ERR,"Error initializing FFSS Filter engine\n");
    return false;
  }
  FS_SI_UDP = SU_CreateServer(ServerPort,SOCK_DGRAM,false);
  if(FS_SI_UDP == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating UDP socket on port %d (%d:%s)\n",ServerPort,errno,strerror(errno));
    return false;
  }
  FS_SI_OUT_UDP = SU_CreateServer(0,SOCK_DGRAM,false);
  if(FS_SI_OUT_UDP == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating outgoing UDP socket (%d:%s)\n",errno,strerror(errno));
    SU_ServerDisconnect(FS_SI_UDP);
    free(FS_SI_UDP);
    return false;
  }
  FS_SI_TCP = SU_CreateServer(ServerPort,SOCK_STREAM,false);
  if(FS_SI_TCP == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating TCP socket on port %d (%d:%s)\n",ServerPort,errno,strerror(errno));
    SU_ServerDisconnect(FS_SI_UDP);
    SU_ServerDisconnect(FS_SI_OUT_UDP);
    free(FS_SI_UDP);
    free(FS_SI_OUT_UDP);
    return false;
  }
  if(!SU_CreateThread(&FS_THR_UDP,F_ThreadUDP,(void *)FFSS_THREAD_SERVER,false))
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating UDP thread\n");
    SU_ServerDisconnect(FS_SI_UDP);
    SU_ServerDisconnect(FS_SI_OUT_UDP);
    SU_ServerDisconnect(FS_SI_TCP);
    free(FS_SI_UDP);
    free(FS_SI_OUT_UDP);
    free(FS_SI_TCP);
    return false;
  }
  if(!SU_CreateThread(&FS_THR_TCP,FS_ThreadTCP,NULL,false))
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating TCP thread\n");
    SU_TermThread(FS_THR_UDP);
    SU_ServerDisconnect(FS_SI_UDP);
    SU_ServerDisconnect(FS_SI_OUT_UDP);
    SU_ServerDisconnect(FS_SI_TCP);
    free(FS_SI_UDP);
    free(FS_SI_OUT_UDP);
    free(FS_SI_TCP);
    return false;
  }

  FS_FTP = FTP;
  if(FTP)
  {
    FS_SI_TCP_FTP = SU_CreateServer(ServerPort+1,SOCK_STREAM,false);
    if(FS_SI_TCP_FTP == NULL)
    {
      FFSS_PrintSyslog(LOG_ERR,"Error creating TCP socket on port %d\n",ServerPort+1);
      SU_TermThread(FS_THR_UDP);
      SU_TermThread(FS_THR_TCP);
      SU_ServerDisconnect(FS_SI_UDP);
      SU_ServerDisconnect(FS_SI_OUT_UDP);
      SU_ServerDisconnect(FS_SI_TCP);
      free(FS_SI_UDP);
      free(FS_SI_OUT_UDP);
      free(FS_SI_TCP);
      return false;
    }
    if(!SU_CreateThread(&FS_THR_TCP_FTP,FS_ThreadTCP_FTP,NULL,false))
    {
      FFSS_PrintSyslog(LOG_ERR,"Error creating TCP thread\n");
      SU_TermThread(FS_THR_UDP);
      SU_TermThread(FS_THR_TCP);
      SU_ServerDisconnect(FS_SI_UDP);
      SU_ServerDisconnect(FS_SI_OUT_UDP);
      SU_ServerDisconnect(FS_SI_TCP);
      SU_ServerDisconnect(FS_SI_TCP_FTP);
      free(FS_SI_UDP);
      free(FS_SI_OUT_UDP);
      free(FS_SI_TCP);
      return false;
    }
    FFSS_PrintSyslog(LOG_INFO,"FFSS server waiting on port %d (UDP and TCP + FTP)\n",ServerPort);
  }
  else
    FFSS_PrintSyslog(LOG_INFO,"FFSS server waiting on port %d (UDP and TCP)\n",ServerPort);

  return true;
}

/* FFSS Server : UnInit */
/* Uninitialisation of the FFSS Server - Must be called at the end of the main */
/* Returns true on success, false otherwise */
bool FS_UnInit(void)
{
  FFSS_ShuttingDown = true;
  SU_TermThread(FS_THR_UDP);
  SU_TermThread(FS_THR_TCP);
  if(FS_FTP)
    SU_TermThread(FS_THR_TCP_FTP);
  SU_ServerDisconnect(FS_SI_UDP);
  SU_ServerDisconnect(FS_SI_OUT_UDP);
  SU_ServerDisconnect(FS_SI_TCP);
  if(FS_FTP)
  {
    SU_ServerDisconnect(FS_SI_TCP_FTP);
    if(FS_SI_TCP_FTP != NULL)
      free(FS_SI_TCP_FTP);
  }
  if(FS_SI_UDP != NULL)
    free(FS_SI_UDP);
  if(FS_SI_OUT_UDP != NULL)
    free(FS_SI_OUT_UDP);
  if(FS_SI_TCP != NULL)
    free(FS_SI_TCP);
  if(FFSS_MyIP != NULL)
    free(FFSS_MyIP);
  FFSS_PrintSyslog(LOG_INFO,"FFSS server shut down\n");
#ifdef _WIN32
  SU_CloseLogFile(FFSS_LogFile);
  SU_WSUninit();
#endif /* _WIN32 */
  return true;
}

#endif /* !FFSS_DRIVER */
