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
#include "transfer.h"
#include "common.h"
SU_THREAD_ROUTINE(FC_ClientThreadTCP,User);
#endif /* !FFSS_DRIVER */

/* ************************************ */
/*        GENERAL SENDING FUNCTIONS     */
/* ************************************ */
#ifdef FFSS_DRIVER
#include <ffss_tdi.h>
#define bool SU_BOOL
#define FFSS_SendBroadcast(a,b,c,d) FFSS_SendUDPBcast(b,c,d)
#undef FFSS_SERVER_PORT_S
#define FFSS_SERVER_PORT_S FFSS_SERVER_PORT
#undef FFSS_MASTER_PORT_S
#define FFSS_MASTER_PORT_S FFSS_MASTER_PORT
#define SU_UDPSendToAddr(a,b,c,d,e) FFSS_SendUDP(b,c,d,e)
#define SU_ClientDisconnect(x) x->disconnect(true);
#undef free
#define free(x) x->Delete()

int FFSS_SendUDPBcast(char *Text,size_t len,unsigned short port)
{
  TDI_STATUS resp;
  TDI_ADDRESS_IP addr = {htons(port),INADDR_BROADCAST};
  CTDI_CONNECTION_INFORMATION server(addr);

  resp = pUDP->sendto(server,Text,len,NULL);
  return (resp == TDI_PENDING || resp == TDI_SUCCESS);
}

int FFSS_SendUDP(char *Text,size_t len,char *Server,unsigned short port)
{
  TDI_STATUS resp;
  TDI_ADDRESS_IP addr = {htons(port),inet_addr(Server)};
  CTDI_CONNECTION_INFORMATION server(addr);

  KdPrint(("Sending UDP message to %s:%d\n",Server,port));
  resp = pUDP->sendto(server,Text,len,NULL);
  return (resp == TDI_PENDING || resp == TDI_SUCCESS);
}

bool FFSS_SendTcpPacketCS(FfssTCP *TCP,char *msg,size_t len,bool FreeMsg,bool Answer)
{
  TDI_STATUS resp;

  resp = TCP->send(msg,len,FreeMsg,Answer);
  return (resp == TDI_PENDING || resp == TDI_SUCCESS);
}

#else /* !FFSS_DRIVER */
bool FFSS_SendTcpPacket(SU_SOCKET Client, char *msg, size_t len, bool FreeMsg, bool QosCheck)
{
  int resp,retval;
  fd_set rfds;
  struct timeval tv;
  FFSS_PQosConn Qos;
  SU_TICKS et;
  SU_u32 tim;
	FFSS_LongField thrpt;
	FFSS_Field sleep_val, sleep_val2;

  context;
  FD_ZERO(&rfds);
  FD_SET(Client,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
	retval = select((int)(Client + 1), NULL, &rfds, NULL, &tv);
  if(!retval)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Sending message timed out !");
    if(FreeMsg)
      free(msg);
    return false;
  }
  if(QosCheck)
  {
    /* Check QoS */
    if(Client < FFSS_MAX_SOCKETS)
    {
      Qos = FFSS_QosConns[Client];
      if(Qos != NULL)
      {
        SU_GetTicks(&et);
        Qos->bytes += len;
        tim = SU_ElapsedTime(Qos->st,et,FFSS_CpuSpeed);
        if(tim >= FFSS_QOS_CHECK_DELAY)
        {
          thrpt = Qos->bytes / tim;
          sleep_val = FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,Qos->IP,thrpt-Qos->prev_thrpt,tim);
          sleep_val2 = FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,Qos->IP,thrpt-Qos->prev_thrpt,tim);
          Qos->prev_thrpt = thrpt;
          /* Reset counters */
          SU_GetTicks(&Qos->st);
          Qos->bytes = 0;
          /* Execute the sleep */
          if(sleep_val || sleep_val2)
          {
            if(sleep_val > sleep_val2)
              SU_USLEEP(sleep_val);
            else
              SU_USLEEP(sleep_val2);
          }
        }
      }
    }
  }

  /* TODO : Cryptage du message ici... sauf premier champ (taille) !! */
  /* Faut juste trouver comment faire le switch/case suivant le type actuel de cryptage */
  resp = send(Client,msg,(int)len,SU_MSG_NOSIGNAL);
  if(FreeMsg)
    free(msg);
  return (resp == len);
}

bool FFSS_SendTcpPacketCS(SU_PClientSocket Client, char *msg, size_t len, bool FreeMsg, bool Answer)
{
  return FFSS_SendTcpPacket(Client->sock,msg,len,FreeMsg,true);
}

/* ************************************ */
/*             SERVER MESSAGES          */
/* ************************************ */

/* FS_SendMessage_State Function                    */
/* Sends a STATE message to a master                */
/*  Master : The name of my master, or NULL if none */
/*  Name : The name of my server                    */
/*  OS : The os of my server                        */
/*  Comment : The comment of my server              */
/*  State : The new state of my server              */
bool FS_SendMessage_State(const char Master[],const char Name[],const char OS[],const char Comment[],int State)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STATE + FFSS_MAX_SERVERNAME_LENGTH+1 + FFSS_MAX_SERVEROS_LENGTH+1 + FFSS_MAX_SERVERCOMMENT_LENGTH+1];
  size_t len,pos;
  int resp;

  context;
  if(Master == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_STATE);
  pos = FFSS_PackField(msg,pos,State);
  pos = FFSS_PackField(msg,pos,FFSS_PROTOCOL_VERSION);

  len = strlen(Name)+1;
  if(len > FFSS_MAX_SERVERNAME_LENGTH)
    len = FFSS_MAX_SERVERNAME_LENGTH;
  pos = FFSS_PackString(msg,pos,Name,len);
  len = strlen(OS)+1;
  if(len > FFSS_MAX_SERVEROS_LENGTH)
    len = FFSS_MAX_SERVEROS_LENGTH;
  pos = FFSS_PackString(msg,pos,OS,len);
  len = strlen(Comment)+1;
  if(len > FFSS_MAX_SERVERCOMMENT_LENGTH)
    len = FFSS_MAX_SERVERCOMMENT_LENGTH;
  pos = FFSS_PackString(msg,pos,Comment,len);
  FFSS_PackField(msg,0,(FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending State message to %s",Master);
  resp = SU_UDPSendToAddr(FS_SI_OUT_UDP,msg,(int)pos,(char *)Master,FFSS_MASTER_PORT_S);
  return (resp != SOCKET_ERROR);
}

/* FS_SendMessage_ServerSearchAnswer Function       */
/* Sends a STATE message to a client                */
/*  Client : The sin of the client                  */
/*  Domain : The domain of my master                */
/*  Name : The name of my server                    */
/*  OS : The os of my server                        */
/*  Comment : The comment of my server              */
/*  State : The state of my server                  */
/*  IP : The IP address of my server                */
/*  MasterIP : The IP address of my server          */
bool FS_SendMessage_ServerSearchAnswer(struct sockaddr_in Client,const char Domain[],const char Name[],const char OS[],const char Comment[],int State,const char IP[],const char MasterIP[])
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_NEW_STATES + sizeof(FFSS_Field)*FFSS_MESSAGESIZE_NEW_STATES_2 + FFSS_IP_FIELD_SIZE*2 + FFSS_MAX_DOMAIN_LENGTH+1 + FFSS_MAX_SERVERNAME_LENGTH+1 + FFSS_MAX_SERVEROS_LENGTH+1 + FFSS_MAX_SERVERCOMMENT_LENGTH+1];
  size_t len,pos;
  int resp;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_NEW_STATES);
  pos = FFSS_PackField(msg,pos,FFSS_COMPRESSION_NONE);
  pos = FFSS_PackField(msg,pos,1);
  pos = FFSS_PackField(msg,pos,State);
  pos = FFSS_PackField(msg,pos,FFSS_IP_TYPE);
  FFSS_PackIP(msg+pos,MasterIP,FFSS_IP_TYPE);
  pos += FFSS_IP_FIELD_SIZE;

  len = strlen(Domain)+1;
  if(len > FFSS_MAX_DOMAIN_LENGTH)
    len = FFSS_MAX_DOMAIN_LENGTH;
  pos = FFSS_PackString(msg,pos,Domain,len);
  len = strlen(Name)+1;
  if(len > FFSS_MAX_SERVERNAME_LENGTH)
    len = FFSS_MAX_SERVERNAME_LENGTH;
  pos = FFSS_PackString(msg,pos,Name,len);
  len = strlen(OS)+1;
  if(len > FFSS_MAX_SERVEROS_LENGTH)
    len = FFSS_MAX_SERVEROS_LENGTH;
  pos = FFSS_PackString(msg,pos,OS,len);
  len = strlen(Comment)+1;
  if(len > FFSS_MAX_SERVERCOMMENT_LENGTH)
    len = FFSS_MAX_SERVERCOMMENT_LENGTH;
  pos = FFSS_PackString(msg,pos,Comment,len);
  pos = FFSS_PackField(msg,pos,FFSS_IP_TYPE);
  FFSS_PackIP(msg+pos,IP,FFSS_IP_TYPE);
  pos += FFSS_IP_FIELD_SIZE;
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Server search answer message to client");
  resp = SU_UDPSendToSin(FS_SI_OUT_UDP,msg,(int)pos,Client);
  return (resp != SOCKET_ERROR);
}

/* FS_SendMessage_ServerSharesAnswer Function                */
/* Sends a SHARES ANSWER message to a client                 */
/*  Client : The sin of the client                           */
/*  IP : The IP address of my server                         */
/*  ShareNames : A tab of the share names of my server       */
/*  ShareComments : A tab of the share comments of my server */
/*  NbShares : The number of shares of my server             */
/*  User : User pointer returned in message answer           */
bool FS_SendMessage_ServerSharesAnswer(struct sockaddr_in Client,const char IP[],const char **ShareNames,const char **ShareComments,int NbShares,FFSS_LongField User)
{
  char *msg;
  size_t size,len,pos;
  int resp;
  int i;

  context;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SHARES_LISTING_ANSWER + FFSS_IP_FIELD_SIZE + (FFSS_MAX_SHARENAME_LENGTH+1)*NbShares + (FFSS_MAX_SHARECOMMENT_LENGTH+1)*NbShares;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SHARES_LISTING_ANSWER);
  pos = FFSS_PackLongField(msg,pos,User);

  pos = FFSS_PackField(msg,pos,FFSS_IP_TYPE);
  FFSS_PackIP(msg+pos,IP,FFSS_IP_TYPE);
  pos += FFSS_IP_FIELD_SIZE;

  pos = FFSS_PackField(msg,pos,NbShares);
  for(i=0;i<NbShares;i++)
  {
    len = strlen(ShareNames[i])+1;
    if(len > FFSS_MAX_SHARENAME_LENGTH)
      len = FFSS_MAX_SHARENAME_LENGTH;
    pos = FFSS_PackString(msg,pos,ShareNames[i],len);
    len = strlen(ShareComments[i])+1;
    if(len > FFSS_MAX_SHARECOMMENT_LENGTH)
      len = FFSS_MAX_SHARECOMMENT_LENGTH;
    pos = FFSS_PackString(msg,pos,ShareComments[i],len);
  }
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Server shares answer message to client");
  resp = SU_UDPSendToSin(FS_SI_OUT_UDP,msg,(int)pos,Client);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FS_SendMessage_Pong Function     */
/* Sends a PONG message to a master */
/*  Master : The sin of my master   */
/*  State : The state of my server  */
bool FS_SendMessage_Pong(struct sockaddr_in Master,int State)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_PONG];
  size_t pos;
  int resp;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_PONG);
  pos = FFSS_PackField(msg,pos,State);
  pos = FFSS_PackField(msg,pos,FFSS_PROTOCOL_VERSION);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Pong message to master");
  resp = SU_UDPSendToAddr(FS_SI_OUT_UDP,msg,(int)pos,inet_ntoa(Master.sin_addr),FFSS_MASTER_PORT_S);
  return (resp != SOCKET_ERROR);
}

/* FS_SendMessage_Error Function                   */
/* Sends an ERROR message to a client              */
/*  Client : The socket of the client              */
/*  Code : The error code to send                  */
/*  Descr : The description of the error code      */
/*  Value : Extra value depending on error code    */
/*  User : User pointer returned in message answer */
bool FS_SendMessage_Error(SU_SOCKET Client,FFSS_Field Code,const char Descr[],FFSS_LongField Value,FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_ERROR + FFSS_MAX_ERRORMSG_LENGTH+1];
	size_t len, pos;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_ERROR);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,Code);
  pos = FFSS_PackLongField(msg,pos,Value);
  if(Descr != NULL)
  {
    len = strlen(Descr)+1;
    if(len > FFSS_MAX_ERRORMSG_LENGTH)
      len = FFSS_MAX_ERRORMSG_LENGTH;
    pos = FFSS_PackString(msg,pos,Descr,len);
  }
  else
    msg[pos++] = 0;

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Error message (%d:%s:%ld) to client",Code,Descr,Value);
  return FFSS_SendTcpPacket(Client,msg,(int)pos,false,false);
}

/* FS_SendMessage_DirectoryListingAnswer Function                     */
/* Sends a DIRECTORY LISTING ANSWER message to a client               */
/*  Client : The socket of the client                                 */
/*  Path : The path of the directory IN the share                     */
/*  Buffer : The buffer containing the nb of entries, and the entries */
/*  BufSize : The size of the buffer                                  */
/*  Compression : The type of compression to be applied to Buffer     */
/*  User : User pointer returned in message answer                    */
bool FS_SendMessage_DirectoryListingAnswer(SU_SOCKET Client, const char Path[], const char *Buffer, size_t BufSize, int Compression, FFSS_LongField User)
{
  char *msg;
	size_t len, size, pos;
	size_t CompSize;

  context;
  if((Buffer == NULL) || (BufSize == 0))
    return true;
	CompSize = (size_t)(BufSize*1.05 + 6000);
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DIRECTORY_LISTING_ANSWER + FFSS_MAX_PATH_LENGTH+1 + CompSize;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_DIRECTORY_LISTING_ANSWER);
  pos = FFSS_PackLongField(msg,pos,User);

  len = strlen(Path)+1;
  if(len > FFSS_MAX_PATH_LENGTH)
    len = FFSS_MAX_PATH_LENGTH;
  pos = FFSS_PackString(msg,pos,Path,len);

  pos = FFSS_PackField(msg,pos,Compression);
  switch(Compression)
  {
    case FFSS_COMPRESSION_NONE :
      memcpy(msg+pos,Buffer,BufSize);
      pos += BufSize;
      break;
#ifndef DISABLE_ZLIB
    case FFSS_COMPRESSION_ZLIB :
      if(!FFSS_CompresseZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in Z compression routine : Buffer too small ?");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
    case FFSS_COMPRESSION_BZLIB :
      if(!FFSS_CompresseBZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in BZ compression routine : Buffer too small ?");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif
    default :
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Unknown compression type : %d",Compression);
      free(msg);
      return false;
  }

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Directory listing answer message (\"%s\") to client",Path);
  return FFSS_SendTcpPacket(Client,msg,(int)pos,true,true);
}

/* FS_SendMessage_RecursiveDirectoryListingAnswer Function            */
/* Sends a RECURSIVE DIRECTORY LISTING ANSWER message to a client     */
/*  Client : The socket of the client                                 */
/*  Path : The path of the directory IN the share                     */
/*  Buffer : The buffer containing the nb of entries, and the entries */
/*  BufSize : The size of the buffer                                  */
/*  Compression : The type of compression to be applied to Buffer     */
/*  User : User pointer returned in message answer                    */
bool FS_SendMessage_RecursiveDirectoryListingAnswer(SU_SOCKET Client, const char Path[], const char *Buffer, size_t BufSize, int Compression, FFSS_LongField User)
{
  char *msg;
	size_t len, size, pos;
	size_t CompSize;

  context;
  if((Buffer == NULL) || (BufSize == 0))
    return true;
	CompSize = (size_t)(BufSize*1.05 + 6000);
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_REC_DIR_LISTING_ANSWER + FFSS_MAX_PATH_LENGTH+1 + CompSize;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_REC_DIR_LISTING_ANSWER);
  pos = FFSS_PackLongField(msg,pos,User);

  len = strlen(Path)+1;
  if(len > FFSS_MAX_PATH_LENGTH)
    len = FFSS_MAX_PATH_LENGTH;
  pos = FFSS_PackString(msg,pos,Path,len);

  pos = FFSS_PackField(msg,pos,Compression);
  switch(Compression)
  {
    case FFSS_COMPRESSION_NONE :
      memcpy(msg+pos,Buffer,BufSize);
      pos += BufSize;
      break;
#ifndef DISABLE_ZLIB
    case FFSS_COMPRESSION_ZLIB :
      if(!FFSS_CompresseZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in Z compression routine : Buffer too small ?");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
    case FFSS_COMPRESSION_BZLIB :
      if(!FFSS_CompresseBZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in BZ compression routine : Buffer too small ?");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif
    default :
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Unknown compression type : %d",Compression);
      free(msg);
      return false;
  }

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Recursive Directory listing answer message (\"%s\") to client",Path);
  return FFSS_SendTcpPacket(Client,msg,(int)pos,true,true);
}

/* FS_SendMessage_InitXFer Function                        */
/* Sends an INIT XFER message to a client                  */
/*  Client : The socket of the client                      */
/*  Tag : The xfer tag that will be used when sending data */
/*  FileName : The name of the requested file              */
bool FS_SendMessage_InitXFer(SU_SOCKET Client,FFSS_Field Tag,const char FileName[])
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_INIT_XFER + FFSS_MAX_FILEPATH_LENGTH+1];
	size_t len, pos;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_INIT_XFER);
  pos = FFSS_PackField(msg,pos,Tag);
  if(FileName != NULL)
  {
    len = strlen(FileName)+1;
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    pos = FFSS_PackString(msg,pos,FileName,len);
  }
  else
    msg[pos++] = 0;

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Init XFer message (%d:%s) to client",Tag,FileName);
  return FFSS_SendTcpPacket(Client,msg,(int)pos,false,true);
}

/* FS_SendMessage_MasterSearch Function            */
/* Sends a MASTER SEARCH message to broadcast      */
/*  User : User pointer returned in message answer */
bool FS_SendMessage_MasterSearch(FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_MASTER];
	size_t pos;
  int resp;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SEARCH_MASTER);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,FFSS_THREAD_SERVER);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Master Search message to broadcast");
  resp = FFSS_SendBroadcast(FS_SI_OUT_UDP,msg,(int)pos,FFSS_MASTER_PORT_S);
  return (resp != SOCKET_ERROR);
}

/* FS_SendMessage_IndexAnswer Function                             */
/* Sends an INDEX ANSWER message to someone                        */
/*  Host : The name of the host to send to                         */
/*  Port : The port to use                                         */
/*  Buffers : Chained list of share buffers                        */
/*  Sizes : Chained list of sizes of share buffers                 */
/*  Compression : The type of compression to be applied to Buffers */
bool FS_SendMessage_IndexAnswer(const char Host[],const char Port[],SU_PList Buffers,SU_PList Sizes,int Compression)
{
  SU_PList Ptr,Ptr2;
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_INDEX_ANSWER];
	size_t pos, total, total_ft, total_node;
  int resp;
	size_t partial, current, slen;
  char *data;
  SU_SOCKET sock=0,client;
  struct sockaddr_in SAddr;
  fd_set rfds;
  struct timeval tv;
  int retval,len;
  FFSS_Field NbBufs;

  context;
  if(Host == NULL)
    return true;
  sock = socket(AF_INET,SOCK_STREAM,getprotobyname("tcp")->p_proto);
  if(sock == SOCKET_ERROR)
    return false;
  SAddr.sin_family = AF_INET;
  SAddr.sin_port = 0;
  SAddr.sin_addr.s_addr = 0;
  if(bind(sock,(struct sockaddr *)&(SAddr), sizeof(SAddr)) == SOCKET_ERROR)
  {
    SU_CLOSE_SOCKET(sock);
    return SOCKET_ERROR;
  }
  if(listen(sock,1) == SOCKET_ERROR)
  {
    SU_CLOSE_SOCKET(sock);
    return false;
  }
  len = sizeof(struct sockaddr_in);
  if( getsockname(sock,(struct sockaddr *)&(SAddr),&len) == -1 )
  {
    SU_CLOSE_SOCKET(sock);
    return false;
  }

  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_INDEX_ANSWER);
  pos = FFSS_PackField(msg,pos,Compression);

  Ptr = Buffers;
  Ptr2 = Sizes;
  total = sizeof(FFSS_Field);
  total_ft = 0;
  total_node = 0;
  NbBufs = 0;
  while(Ptr != NULL)
  {
    if(NbBufs%2)
			total_node += (size_t)Ptr2->Data;
    else
			total_ft += (size_t)Ptr2->Data;
    total += sizeof(FFSS_Field);
    switch(Compression)
    {
      case FFSS_COMPRESSION_NONE :
        break;
#ifndef DISABLE_ZLIB
      case FFSS_COMPRESSION_ZLIB :
				partial = ((size_t) Ptr2->Data)*1.05+12;
        data = (char *)Ptr->Data;
        Ptr->Data = malloc(partial);
				if(!FFSS_CompresseZlib(data,(size_t)Ptr2->Data,(char *)Ptr->Data,&partial))
        {
          SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in Z compression routine : Buffer too small ?");
          SU_CLOSE_SOCKET(sock);
          free(data);
          return false;
        }
        free(data);
        Ptr2->Data = (void *) partial;
        break;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
      case FFSS_COMPRESSION_BZLIB :
				partial = ((size_t) Ptr2->Data)*1.05+1600;
        data = (char *)Ptr->Data;
        Ptr->Data = malloc(partial);
				if(!FFSS_CompresseBZlib(data,(size_t)Ptr2->Data,(char *)Ptr->Data,&partial))
        {
          SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in BZ compression routine : Buffer too small ?");
          SU_CLOSE_SOCKET(sock);
          free(data);
          return false;
        }
        free(data);
        Ptr2->Data = (void *) partial;
        break;
#endif
      default :
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Unknown compression type : %d",Compression);
        return false;
    }
		total += (size_t)Ptr2->Data;
    Ptr = Ptr->Next;
    Ptr2 = Ptr2->Next;
    NbBufs++;
  }
  NbBufs /= 2;

  context;
	pos = FFSS_PackField(msg, pos, (FFSS_Field)total);
	pos = FFSS_PackField(msg, pos, (FFSS_Field)total_ft);
	pos = FFSS_PackField(msg, pos, (FFSS_Field)total_node);
  pos = FFSS_PackField(msg,pos,ntohs(SAddr.sin_port));
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending index answer message to %s:%s",Host,Port);
  resp = SU_UDPSendToAddr(FS_SI_OUT_UDP,msg,(int)pos,(char *)Host,(char *)Port);
  if(resp == SOCKET_ERROR)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Failed to connect master");
    SU_CLOSE_SOCKET(sock);
    return false;
  }

  FD_ZERO(&rfds);
  FD_SET(sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_INDEX_ACCEPT;
  tv.tv_usec = 0;
	retval = select((int)(sock + 1), &rfds, NULL, NULL, &tv);
  if(!retval)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Timed out waiting for master to connect to port %d",ntohs(SAddr.sin_port));
    SU_CLOSE_SOCKET(sock);
    return false;
  }

  client = accept(sock,(struct sockaddr *)&SAddr,&len);
  SU_CLOSE_SOCKET(sock);
  if(client == SOCKET_ERROR)
    return false;

  /* Send nb buffers */
  context;
  FD_ZERO(&rfds);
  FD_SET(client,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_INDEX_XFER;
  tv.tv_usec = 0;
	retval = select((int)(client + 1), NULL, &rfds, NULL, &tv);
  if(!retval)
  {
    SU_CLOSE_SOCKET(sock);
    return false;
  }
  resp = send(client,(char *)&NbBufs,sizeof(NbBufs),SU_MSG_NOSIGNAL);
  if(resp == SOCKET_ERROR)
  {
    SU_CLOSE_SOCKET(sock);
    return false;
  }

  Ptr = Buffers;
  Ptr2 = Sizes;
  while(Ptr != NULL)
  {
    data = (char *)Ptr->Data;
    partial = (FFSS_Field)Ptr2->Data;
    current = 0;
    /* Send size */
    FD_ZERO(&rfds);
    FD_SET(client,&rfds);
    tv.tv_sec = FFSS_TIMEOUT_INDEX_XFER;
    tv.tv_usec = 0;
		retval = select((int)(client + 1), NULL, &rfds, NULL, &tv);
    if(!retval)
    {
      SU_CLOSE_SOCKET(sock);
      return false;
    }
    resp = send(client,(char *)&partial,sizeof(FFSS_Field),SU_MSG_NOSIGNAL);
    if(resp == SOCKET_ERROR)
    {
      SU_CLOSE_SOCKET(sock);
      return false;
    }
    while(current < partial)
    {
      if((current+FFSS_TRANSFER_BUFFER_SIZE) <= partial)
        slen = FFSS_TRANSFER_BUFFER_SIZE;
      else
        slen = partial - current;
      /* Send buffer */
      FD_ZERO(&rfds);
      FD_SET(client,&rfds);
      tv.tv_sec = FFSS_TIMEOUT_INDEX_XFER;
      tv.tv_usec = 0;
			retval = select((int)(client + 1), NULL, &rfds, NULL, &tv);
      if(!retval)
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Timed out waiting for master to retrieve index data");
        SU_CLOSE_SOCKET(sock);
        return false;
      }
      resp = send(client,data+current,(int)slen,SU_MSG_NOSIGNAL);
      if(resp == SOCKET_ERROR)
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error sending index data to master (%ld %ld) : %s",current,slen,strerror(errno));
        SU_CLOSE_SOCKET(sock);
        return false;
      }
      current += slen;
    }
    Ptr = Ptr->Next;
    Ptr2 = Ptr2->Next;
  }
  SU_CLOSE_SOCKET(sock);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Index successfully sent to %s:%s",Host,Port);
  return true;
}

/* FS_SendMessage_StrmOpenAnswer Function             */
/* Sends an STREAMING OPEN answer message to a client */
/*  Client : The socket of the client                 */
/*  Path : The path of the file requested             */
/*  Code : The error code to send                     */
/*  Handle : The handle of the file if successfull    */
/*  FileSize : Size of the file                       */
/*  User : User pointer returned in message answer    */
bool FS_SendMessage_StrmOpenAnswer(SU_SOCKET Client,const char Path[],FFSS_Field Code,FFSS_Field Handle,FFSS_LongField FileSize,FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_OPEN_ANSWER + FFSS_MAX_FILEPATH_LENGTH+1];
	size_t len, pos;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_STREAMING_OPEN_ANSWER);
  pos = FFSS_PackLongField(msg,pos,User);

  if(Path != NULL)
  {
    len = strlen(Path)+1;
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    pos = FFSS_PackString(msg,pos,Path,len);
  }
  else
    msg[pos++] = 0;

  pos = FFSS_PackField(msg,pos,Code);
  pos = FFSS_PackField(msg,pos,Handle);
  pos = FFSS_PackLongField(msg,pos,FileSize);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Streaming OPEN answer message to client");
  return FFSS_SendTcpPacket(Client,msg,(int)pos,false,true);
}

/* FS_SendMessage_StrmReadAnswer Function             */
/* Sends an STREAMING READ answer message to a client */
/*  Client : The socket of the client                 */
/*  Handle : The handle of the file                   */
/*  Buf : The buffer of datas                         */
/*  BlocLen : The length of the datas                 */
/*  Code : The error code to send                     */
/*  User : User pointer returned in message answer    */
bool FS_SendMessage_StrmReadAnswer(SU_SOCKET Client, FFSS_Field Handle, char *Buf, size_t BlocLen, FFSS_Field Code, FFSS_LongField User)
{
  char *msg;
	size_t size, pos;

  context;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_READ_ANSWER + BlocLen;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_STREAMING_READ_ANSWER);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,Handle);
  pos = FFSS_PackField(msg,pos,Code);
  memcpy(msg+pos,Buf,BlocLen);
  pos += BlocLen;

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Streaming READ answer message to client");
  return FFSS_SendTcpPacket(Client,msg,(int)pos,true,true);
}

/* FS_SendMessage_StrmWriteAnswer Function             */
/* Sends an STREAMING WRITE answer message to a client */
/*  Client : The socket of the client                  */
/*  Handle : The handle of the file                    */
/*  Code : The error code to send                      */
/*  User : User pointer returned in message answer     */
bool FS_SendMessage_StrmWriteAnswer(SU_SOCKET Client,FFSS_Field Handle,FFSS_Field Code,FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_WRITE_ANSWER];
	size_t pos;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_STREAMING_WRITE_ANSWER);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,Handle);
  pos = FFSS_PackField(msg,pos,Code);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Streaming WRITE answer message to client");
  return FFSS_SendTcpPacket(Client,msg,(int)pos,false,true);
}

/* FS_SendMessage_ShortMessage Function           */
/* Sends a SHORT MESSAGE message back to a client */
/*  Server : The sin of the client to respond to  */
/*  Message : Message to be sent to the server    */
bool FS_SendMessage_ShortMessage(struct sockaddr_in Client,const char Message[])
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SHORT_MESSAGE+FFSS_SHORT_MESSAGE_MAX+1];
	size_t pos, len;
  int resp;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SHORT_MESSAGE);
  if(Message == NULL)
    return true;
  len = strlen(Message)+1;
  if(len > FFSS_SHORT_MESSAGE_MAX)
    len = FFSS_SHORT_MESSAGE_MAX;
  pos = FFSS_PackString(msg,pos,Message,len);

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Short Message to client");
  resp = SU_UDPSendToSin(FS_SI_OUT_UDP,msg,(int)pos,Client);
  return (resp != SOCKET_ERROR);
}


#endif /* FFSS_DRIVER */


/* ************************************ */
/*             CLIENT MESSAGES          */
/* ************************************ */

/* FC_SendMessage_ServerSearch Function       */
/* Sends a SERVER SEARCH message to broadcast */
bool FC_SendMessage_ServerSearch(void)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_SEARCH];
	size_t pos;
  int resp;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SERVER_SEARCH);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Servers Search message to broadcast");
  resp = FFSS_SendBroadcast(FC_SI_OUT_UDP,msg,(int)pos,FFSS_SERVER_PORT_S);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_SharesListing Function                       */
/* Sends a SHARES LISTING message to a server                  */
/*  Server : The name of the server we want the shares listing */
/*  User : User pointer returned in message answer             */
bool FC_SendMessage_SharesListing(const char Server[],FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SHARES_LISTING];
	size_t pos;
  int resp;

  context;
  if(Server == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SHARES_LISTING);
  pos = FFSS_PackLongField(msg,pos,User);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Shares Listing message to %s",Server);
  resp = SU_UDPSendToAddr(FC_SI_OUT_UDP,msg,(int)pos,(char *)Server,FFSS_SERVER_PORT_S);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_ServerList Function                      */
/* Sends a SERVER LIST message to a master                 */
/*  Master : The name of my master, or NULL if none        */
/*  OS : The desired OS, or NULL if requesting all         */
/*  Domain : The desired domain, or NULL if requesting all */
/*  User : User pointer returned in message answer         */
bool FC_SendMessage_ServerList(const char Master[],const char OS[],const char Domain[],FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING + FFSS_MAX_SERVEROS_LENGTH+1 + FFSS_MAX_DOMAIN_LENGTH+1];
	size_t len, pos;
  int resp;
  FFSS_BitField Comps;

  context;
  if(Master == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SERVER_LISTING);
  pos = FFSS_PackLongField(msg,pos,User);

  Comps = FFSS_COMPRESSION_NONE;
#ifndef DISABLE_ZLIB
  Comps |= FFSS_COMPRESSION_ZLIB;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
  Comps |= FFSS_COMPRESSION_BZLIB;
#endif
	pos = FFSS_PackField(msg, pos, (FFSS_Field)Comps);

  if(OS != NULL)
  {
    len = strlen(OS)+1;
    if(len > FFSS_MAX_SERVEROS_LENGTH)
      len = FFSS_MAX_SERVEROS_LENGTH;
    pos = FFSS_PackString(msg,pos,OS,len);
  }
  else
    msg[pos++] = 0;
  if(Domain != NULL)
  {
    len = strlen(Domain)+1;
    if(len > FFSS_MAX_DOMAIN_LENGTH)
      len = FFSS_MAX_DOMAIN_LENGTH;
    pos = FFSS_PackString(msg,pos,Domain,len);
  }
  else
    msg[pos++] = 0;
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Server listing message to %s",Master);
  resp = SU_UDPSendToAddr(FC_SI_OUT_UDP,msg,(int)pos,(char *)Master,FFSS_MASTER_PORT_S);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_ShareConnect Function                  */
/* Sends a SHARE CONNECTION message to a server          */
/*  Server : The name of Server we wish to connect to    */
/*  ShareName : The Share Name we wish to connect to     */
/*  Login : The Login we may use (or NULL if none)       */
/*  Password : The Password we may use (or NULL if none) */
/*  User : User pointer returned in message answer       */
SU_PClientSocket FC_SendMessage_ShareConnect(const char Server[],const char ShareName[],const char Login[],const char Password[],FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SHARE_CONNECTION + FFSS_MAX_SHARENAME_LENGTH+1 + FFSS_MAX_LOGIN_LENGTH+1 + FFSS_MAX_PASSWORD_LENGTH+1];
	size_t len, pos;
  bool resp;
  SU_PClientSocket CS;
  FFSS_BitField Comps;
#ifndef FFSS_DRIVER
  SU_THREAD_HANDLE Thread;
	SU_THREAD_ID ThreadId;

  context;
  if(Server == NULL)
    return NULL;
  CS = SU_ClientConnect((char *)Server,FFSS_SERVER_PORT,SOCK_STREAM);
  if(CS == NULL)
    return NULL;

#else /* FFSS_DRIVER */
  TDI_ADDRESS_IP addr = {htons(FFSS_SERVER_PORT),inet_addr(Server)};
  CTDI_CONNECTION_INFORMATION server(addr);

  CS = new FfssTCP(Server,ShareName);
  if(CS == NULL)
    return NULL;
  if(!CS->IsCreated())
  {
    CS->Delete();
    return NULL;
  }
  CS->SetEvents(TRUE);
  CS->connect(server);
  if(!CS->IsConnected())
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"FC_SendMessage_ShareConnect : Cannot connect to %s",Server);
    CS->Delete();
    return NULL;
  }
#endif /* !FFSS_DRIVER */

  SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Connected to %s",Server);
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SHARE_CONNECTION);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,FFSS_PROTOCOL_VERSION);

  Comps = FFSS_COMPRESSION_NONE;
#ifndef DISABLE_ZLIB
  Comps |= FFSS_COMPRESSION_ZLIB;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
  Comps |= FFSS_COMPRESSION_BZLIB;
#endif
	pos = FFSS_PackField(msg, pos, (FFSS_Field)Comps);

  len = strlen(ShareName)+1;
  if(len > FFSS_MAX_SHARENAME_LENGTH)
    len = FFSS_MAX_SHARENAME_LENGTH;
  pos = FFSS_PackString(msg,pos,ShareName,len);
  if(Login != NULL)
  {
    len = strlen(Login)+1;
    if(len > FFSS_MAX_LOGIN_LENGTH)
      len = FFSS_MAX_LOGIN_LENGTH;
    pos = FFSS_PackString(msg,pos,Login,len);
  }
  else
    msg[pos++] = 0;
  if(Password != NULL)
  {
    len = strlen(Password)+1;
    if(len > FFSS_MAX_PASSWORD_LENGTH)
      len = FFSS_MAX_PASSWORD_LENGTH;
    pos = FFSS_PackString(msg,pos,Password,len);
  }
  else
    msg[pos++] = 0;
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Share connection message to %s",Server);
  resp = FFSS_SendTcpPacketCS(CS,msg,pos,false,true);
  if(!resp)
  {
    SU_ClientDisconnect(CS);
    free(CS);
    return NULL;
  }
#ifndef FFSS_DRIVER
  SU_CreateThread(&Thread,&ThreadId,FC_ClientThreadTCP,(void *)CS,true);
#endif /* !FFSS_DRIVER */
  return CS;
}

/* FC_SendMessage_DirectoryListing Function             */
/* Sends a DIRECTORY LISTING message to a server        */
/*  Server : The Server's structure we are connected to */
/*  Path : The path we request a listing                */
/*  User : User pointer returned in message answer      */
bool FC_SendMessage_DirectoryListing(SU_PClientSocket Server,const char Path[],FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DIRECTORY_LISTING + FFSS_MAX_PATH_LENGTH+1];
	size_t pos;
	size_t len;

  context;
  if(Server == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_DIRECTORY_LISTING);
  pos = FFSS_PackLongField(msg,pos,User);
  len = strlen(Path)+1;
  if(len > FFSS_MAX_PATH_LENGTH)
    len = FFSS_MAX_PATH_LENGTH;
  pos = FFSS_PackString(msg,pos,Path,len);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Directory Listing message to server for %s",Path);
  return FFSS_SendTcpPacketCS(Server,msg,pos,false,true);
}

/* FC_SendMessage_RecursiveDirectoryListing Function       */
/* Sends a RECURSIVE DIRECTORY LISTING message to a server */
/*  Server : The Server's structure we are connected to    */
/*  Path : The path we request a listing                   */
/*  User : User pointer returned in message answer         */
bool FC_SendMessage_RecursiveDirectoryListing(SU_PClientSocket Server,const char Path[],FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_REC_DIR_LISTING + FFSS_MAX_PATH_LENGTH+1];
	size_t pos;
	size_t len;

  context;
  if(Server == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_REC_DIR_LISTING);
  pos = FFSS_PackLongField(msg,pos,User);
  len = strlen(Path)+1;
  if(len > FFSS_MAX_PATH_LENGTH)
    len = FFSS_MAX_PATH_LENGTH;
  pos = FFSS_PackString(msg,pos,Path,len);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Recursive Directory Listing message to server for %s",Path);
  return FFSS_SendTcpPacketCS(Server,msg,pos,false,true);
}
#ifndef FFSS_DRIVER
/* FC_SendMessage_Download Function                                 */
/* Sends a DOWNLOAD message to a server                             */
/*  Server : The Server's structure we are connected to             */
/*  Path : The path of requested file (in the share)                */
/*  StartingPos : The pos we want to download the file starting at  */
/*  EndingPos : The pos we want the download to stop (0=full file)  */
/*  UseConnSock : Use a separate socket/thread, or use the existing */
/*  User : User pointer returned in message answer                  */
SU_SOCKET FC_SendMessage_Download(SU_PClientSocket Server,const char Path[],FFSS_LongField StartingPos,FFSS_LongField EndingPos,bool UseConnSock,FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DOWNLOAD + FFSS_MAX_FILEPATH_LENGTH+1];
	size_t pos;
	bool resp;
	int len; // Force usage of int here
  SU_SOCKET sock=0;
  struct sockaddr_in SAddr;

  context;
  if(Server == NULL)
    return SOCKET_ERROR;
  if(!UseConnSock)
  {
    sock = socket(AF_INET,SOCK_STREAM,getprotobyname("tcp")->p_proto);
    if(sock == SOCKET_ERROR)
      return SOCKET_ERROR;
    SAddr.sin_family = AF_INET;
    SAddr.sin_port = 0;
    SAddr.sin_addr.s_addr = 0;
    if( bind(sock,(struct sockaddr *)&(SAddr), sizeof(SAddr)) == SOCKET_ERROR )
    {
      SU_CLOSE_SOCKET(sock);
      return SOCKET_ERROR;
    }
    if(listen(sock,1) == SOCKET_ERROR)
    {
      SU_CLOSE_SOCKET(sock);
      return SOCKET_ERROR;
    }
    len = sizeof(struct sockaddr_in);
    if( getsockname(sock,(struct sockaddr *)&(SAddr),&len) == -1 )
    {
      SU_CLOSE_SOCKET(sock);
      return SOCKET_ERROR;
    }
  }
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_DOWNLOAD);
  pos = FFSS_PackLongField(msg,pos,User);
	len = (int)(strlen(Path) + 1);
  if(len > FFSS_MAX_FILEPATH_LENGTH)
    len = FFSS_MAX_FILEPATH_LENGTH;
  pos = FFSS_PackString(msg,pos,Path,len);
  pos = FFSS_PackLongField(msg,pos,StartingPos);
  pos = FFSS_PackLongField(msg,pos,EndingPos);
  if(UseConnSock)
    pos = FFSS_PackField(msg,pos,-1);
  else
    pos = FFSS_PackField(msg,pos,ntohs(SAddr.sin_port));
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Download message to server for %s starting at %lld (ending at %lld)",Path,StartingPos,EndingPos);
  resp = FFSS_SendTcpPacketCS(Server,msg,pos,false,true);
  if(!resp)
  {
    if(!UseConnSock)
      SU_CLOSE_SOCKET(sock);
    return SOCKET_ERROR;
  }
  if(UseConnSock)
    return 0;
  else
    return sock;
}

/* FC_SendMessage_Disconnect Function                   */
/* Sends an DISCONNECT message to a server              */
/*  Server : The Server's structure we are connected to */
void FC_SendMessage_Disconnect(SU_PClientSocket Server)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DISCONNECT];
	size_t pos;

  context;
  if(Server == NULL)
    return;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_DISCONNECT);

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Disconnect message to server");
  FFSS_SendTcpPacketCS(Server,msg,pos,false,false);
}

/* FC_SendMessage_CancelXFer Function                   */
/* Sends an CANCEL XFER message to a server             */
/*  Server : The Server's structure we are connected to */
/*  XFerTag : The tag of the xfer we want to cancel     */
void FC_SendMessage_CancelXFer(SU_PClientSocket Server,FFSS_Field XFerTag)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_CANCEL_XFER];
	size_t pos;

  context;
  if(Server == NULL)
    return;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_CANCEL_XFER);
  pos = FFSS_PackField(msg,pos,XFerTag);

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending cancel xfer message to server");
  FFSS_SendTcpPacketCS(Server,msg,pos,false,false);
  SU_FreeCS(Server);
}
#endif /* !FFSS_DRIVER */
/* FC_SendMessage_DomainListing Function           */
/* Sends a DOMAIN LIST message to a master         */
/*  Master : The name of my master                 */
/*  User : User pointer returned in message answer */
bool FC_SendMessage_DomainListing(const char Master[],FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DOMAINS_LISTING];
	size_t pos;
  int resp;

  context;
  if(Master == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_DOMAINS_LISTING);
  pos = FFSS_PackLongField(msg,pos,User);

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Domains listing message to %s",Master);
  resp = SU_UDPSendToAddr(FC_SI_OUT_UDP,msg,(int)pos,(char *)Master,FFSS_MASTER_PORT_S);
  return (resp != SOCKET_ERROR);
}
#ifndef FFSS_DRIVER
/* FC_SendMessage_Search Function                          */
/* Sends a SEARCH message to a master                      */
/*  Master : The name of my master                         */
/*  Domain : The desired domain, or NULL if requesting all */
/*  Keys   : A String of keywords                          */
/*  User : User pointer returned in message answer         */
bool FC_SendMessage_Search(const char Master[],const char Domain[],const char Key[],FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH + FFSS_MAX_DOMAIN_LENGTH+1 + FFSS_MAX_KEYWORDS_LENGTH+1];
	size_t len, pos;
  int resp;
  FFSS_BitField Comps;

  context;
  if(Master == NULL)
    return true;

  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SEARCH);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,ntohs(FC_SI_OUT_UDP->SAddr.sin_port));

  Comps = FFSS_COMPRESSION_NONE;
#ifndef DISABLE_ZLIB
  Comps |= FFSS_COMPRESSION_ZLIB;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
  Comps |= FFSS_COMPRESSION_BZLIB;
#endif
	pos = FFSS_PackField(msg, pos, (FFSS_Field)Comps);

  if(Domain != NULL)
  {
    len = strlen(Domain)+1;
    if(len > FFSS_MAX_DOMAIN_LENGTH)
      len = FFSS_MAX_DOMAIN_LENGTH;
    pos = FFSS_PackString(msg,pos,Domain,len);
  }
  else
    msg[pos++] = 0;
  len = strlen(Key)+1;
  if(len > FFSS_MAX_KEYWORDS_LENGTH)
    len = FFSS_MAX_KEYWORDS_LENGTH;
  pos = FFSS_PackString(msg,pos,Key,len);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Search message to %s - Reply to port %d",Master,ntohs(FC_SI_OUT_UDP->SAddr.sin_port));
  resp = SU_UDPSendToAddr(FC_SI_OUT_UDP,msg,(int)pos,(char *)Master,FFSS_MASTER_PORT_S);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_MasterSearch Function            */
/* Sends a MASTER SEARCH message to broadcast      */
/*  User : User pointer returned in message answer */
bool FC_SendMessage_MasterSearch(FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_MASTER];
	size_t pos;
  int resp;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SEARCH_MASTER);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,FFSS_THREAD_CLIENT);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Master Search message to broadcast");
  resp = FFSS_SendBroadcast(FC_SI_OUT_UDP,msg,pos,FFSS_MASTER_PORT_S);
  return (resp != SOCKET_ERROR);
}
#endif /* !FFSS_DRIVER */

/* FC_SendMessage_StrmOpen Function                     */
/* Sends an STREAMING OPEN message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Path : The path of the requested file               */
/*  Flags : The opening mode flags                      */
/*  User : User pointer returned in message answer      */
bool FC_SendMessage_StrmOpen(SU_PClientSocket Server,const char Path[],int Flags,FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_OPEN + FFSS_MAX_FILEPATH_LENGTH+1];
	size_t len, pos;

  context;
  if(Server == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_STREAMING_OPEN);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,Flags);

  if(Path != NULL)
  {
    len = strlen(Path)+1;
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    pos = FFSS_PackString(msg,pos,Path,len);
  }
  else
    msg[pos++] = 0;

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Streaming OPEN message to client");
  return FFSS_SendTcpPacketCS(Server,msg,pos,false,true);
}

/* FC_SendMessage_StrmClose Function                     */
/* Sends an STREAMING CLOSE message to a server          */
/*  Server : The Server's structure we are connected to  */
/*  Handle : The handle of the file to close             */
bool FC_SendMessage_StrmClose(SU_PClientSocket Server,FFSS_Field Handle)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_CLOSE];
	size_t pos;

  context;
  if(Server == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_STREAMING_CLOSE);
  pos = FFSS_PackField(msg,pos,Handle);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Streaming CLOSE message to client");
  return FFSS_SendTcpPacketCS(Server,msg,pos,false,false);
}

/* FC_SendMessage_StrmRead Function                     */
/* Sends an STREAMING READ message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  StartPos : The start position of the requested bloc */
/*  Length : Indicative length requested                */
/*  User : User pointer returned in message answer      */
bool FC_SendMessage_StrmRead(SU_PClientSocket Server, FFSS_Field Handle, FFSS_LongField StartPos, size_t Length, FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_READ];
	size_t pos;

  context;
  if(Server == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_STREAMING_READ);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,Handle);
  pos = FFSS_PackLongField(msg,pos,StartPos);
	pos = FFSS_PackField(msg, pos, (FFSS_Field)Length);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Streaming READ message to client");
  return FFSS_SendTcpPacketCS(Server,msg,pos,false,true);
}

/* FC_SendMessage_StrmWrite Function                    */
/* Sends an STREAMING WRITE message to a server         */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  StartPos : The start position of the requested bloc */
/*  Buf : The buffer of datas                           */
/*  BlocLen : The length of the datas                   */
/*  User : User pointer returned in message answer      */
bool FC_SendMessage_StrmWrite(SU_PClientSocket Server, FFSS_Field Handle, FFSS_LongField StartPos, char *Buf, size_t BlocLen, FFSS_LongField User)
{
  char *msg;
	size_t size, pos;

  context;
  if(Server == NULL)
    return true;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_WRITE + BlocLen;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_STREAMING_WRITE);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,Handle);
  pos = FFSS_PackLongField(msg,pos,StartPos);

  memcpy(msg+pos,Buf,BlocLen);
  pos += BlocLen;

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Streaming WRITE message to client");
  return FFSS_SendTcpPacketCS(Server,msg,pos,true,true);
}

/* FC_SendMessage_StrmSeek Function                     */
/* Sends an STREAMING SEEK message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  Flags : The flags for the seek operation            */
/*  StartPos : The position of the seek                 */
bool FC_SendMessage_StrmSeek(SU_PClientSocket Server,FFSS_Field Handle,int Flags,FFSS_LongField StartPos)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_SEEK];
	size_t pos;

  context;
  if(Server == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_STREAMING_SEEK);
  pos = FFSS_PackField(msg,pos,Handle);
  pos = FFSS_PackField(msg,pos,Flags);
  pos = FFSS_PackLongField(msg,pos,StartPos);

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Streaming SEEK message to client");
  return FFSS_SendTcpPacketCS(Server,msg,pos,false,false);
}

/* FC_SendMessage_ShortMessage Function                        */
/* Sends a SHORT MESSAGE message to a server                   */
/*  Server : The name of the server we want the shares listing */
/*  Message : Message to be sent to the server                 */
bool FC_SendMessage_ShortMessage(const char Server[],const char Message[])
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SHORT_MESSAGE+FFSS_SHORT_MESSAGE_MAX+1];
	size_t pos, len;
  int resp;

  context;
  if(Server == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SHORT_MESSAGE);
  if(Message == NULL)
    return true;
  len = strlen(Message)+1;
  if(len > FFSS_SHORT_MESSAGE_MAX)
    len = FFSS_SHORT_MESSAGE_MAX;
  pos = FFSS_PackString(msg,pos,Message,len);

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Short Message to %s",Server);
  resp = SU_UDPSendToAddr(FC_SI_OUT_UDP,msg,(int)pos,(char *)Server,FFSS_SERVER_PORT_S);
  return (resp != SOCKET_ERROR);
}


#ifndef FFSS_DRIVER
/* ************************************ */
/*             MASTER MESSAGES          */
/* ************************************ */

SU_THREAD_ROUTINE(FM_MasterThreadTCP,User);

/* FM_SendMessage_Connect Function  */
/* Connects to a foreign master     */
/*  Master : The name of the Master */
SU_PClientSocket FM_SendMessage_Connect(const char Master[])
{
  SU_PClientSocket CS;
  SU_THREAD_HANDLE MasterThr;
	SU_THREAD_ID MasterThrId;

  context;
  if(Master == NULL)
    return NULL;
  CS = SU_ClientConnect((char *)Master,FFSS_MASTER_PORT,SOCK_STREAM);
  if(CS == NULL)
    return NULL;
  if(!SU_CreateThread(&MasterThr,&MasterThrId,FM_MasterThreadTCP,(void *)CS,true))
  {
    FFSS_PrintSyslog(LOG_ERR,"Error creating TCP Master thread\n");
    SU_FreeCS(CS);
    return NULL;
  }
  return CS;
}

/* FM_SendMessage_MasterConnection Function       */
/* Sends a MASTER CONNECTION message to a master  */
/* Must be the first message sent upon connection */
/*  Master : The socket of the Master             */
bool FM_SendMessage_MasterConnection(SU_SOCKET Master)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_MASTER_CONNECTION];
	size_t pos;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_MASTER_CONNECTION);
  pos = FFSS_PackField(msg,pos,FFSS_PROTOCOL_VERSION);

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Master connection message to master");
  return FFSS_SendTcpPacket(Master,msg,pos,false,false);
}

/* FM_SendMessage_Ping Function    */
/* Sends a PING message to servers */
bool FM_SendMessage_Ping()
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_PING];
	size_t pos;
  int resp;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_PING);
  pos = FFSS_PackField(msg,pos,FFSS_PROTOCOL_VERSION);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Ping message to servers");
  resp = FFSS_SendBroadcast(FM_SI_OUT_UDP,msg,pos,FFSS_SERVER_PORT_S);
  return (resp != SOCKET_ERROR);
}

/* FM_SendMessage_NewStatesMaster Function                          */
/* Sends a NEW STATES message to a master                           */
/*  Master : The socket of the master                               */
/*  Buffer : The buffer containing the nb of states, and the states */
/*  BufSize : The size of the buffer                                */
/*  Compression : The type of compression to be applied to Buffer   */
bool FM_SendMessage_NewStatesMaster(SU_SOCKET Master, const char *Buffer, size_t BufSize, int Compression)
{
  char *msg;
	size_t size, pos;
	size_t CompSize;

  context;
  if((Buffer == NULL) || (BufSize == 0))
    return true;
	CompSize = (size_t)(BufSize*1.05 + 6000);
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_NEW_STATES + CompSize;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_NEW_STATES);
  pos = FFSS_PackField(msg,pos,Compression);
  switch(Compression)
  {
    case FFSS_COMPRESSION_NONE :
      memcpy(msg+pos,Buffer,BufSize);
      pos += BufSize;
      break;
#ifndef DISABLE_ZLIB
    case FFSS_COMPRESSION_ZLIB :
      if(!FFSS_CompresseZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in Z compression routine : Buffer too small ?");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
    case FFSS_COMPRESSION_BZLIB :
      if(!FFSS_CompresseBZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in BZ compression routine : Buffer too small ?");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif
    default :
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Unknown compression type : %d",Compression);
      free(msg);
      return false;
  }

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending New States message to master");
  return FFSS_SendTcpPacket(Master,msg,pos,true,false);
}

/* FM_SendMessage_ServerListing Function                            */
/* Sends a NEW STATES message to client                             */
/*  Client : The sin of the client                                  */
/*  Buffer : The buffer containing the nb of domains, and the hosts */
/*  BufSize : The size of the buffer                                */
/*  Compression : The type of compression to be applied to Buffer   */
/*  User : User pointer returned in message answer                  */
bool FM_SendMessage_ServerListing(struct sockaddr_in Client, const char *Buffer, size_t BufSize, int Compression, FFSS_LongField User)
{
  char *msg;
	size_t size, pos;
  int resp;
  size_t CompSize;

  context;
  if((Buffer == NULL) || (BufSize == 0))
    return true;
	CompSize = (size_t)(BufSize*1.05 + 6000);
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER + CompSize;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SERVER_LISTING_ANSWER);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,Compression);
  switch(Compression)
  {
    case FFSS_COMPRESSION_NONE :
      memcpy(msg+pos,Buffer,BufSize);
      pos += BufSize;
      break;
#ifndef DISABLE_ZLIB
    case FFSS_COMPRESSION_ZLIB :
      if(!FFSS_CompresseZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in Z compression routine : Buffer too small ?");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
    case FFSS_COMPRESSION_BZLIB :
      if(!FFSS_CompresseBZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in BZ compression routine : Buffer too small ?");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif
    default :
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Unknown compression type : %d",Compression);
      free(msg);
      return false;
  }

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Server Listing message to client");
  resp = SU_UDPSendToSin(FM_SI_OUT_UDP,msg,(int)pos,Client);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FM_SendMessage_Error Function              */
/* Sends an ERROR message to a server         */
/*  Server : The name of the server           */
/*  Code : The error code to send             */
/*  Descr : The description of the error code */
bool FM_SendMessage_Error(const char Server[],FFSS_Field Code,const char Descr[])
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_ERROR + FFSS_MAX_ERRORMSG_LENGTH+1];
	size_t len, pos;
  int resp;

  context;
  if(Server == NULL)
    return true;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_ERROR);
  pos = FFSS_PackField(msg,pos,Code);
  if(Descr != NULL)
  {
    len = strlen(Descr)+1;
    if(len > FFSS_MAX_ERRORMSG_LENGTH)
      len = FFSS_MAX_ERRORMSG_LENGTH;
    pos = FFSS_PackString(msg,pos,Descr,len);
  }
  else
    msg[pos++] = 0;

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Error message (%d:%s) to server %s",Code,Descr,Server);
  resp = SU_UDPSendToAddr(FM_SI_OUT_UDP,msg,(int)pos,(char *)Server,FFSS_SERVER_PORT_S);
  return (resp == pos);
}

/* FM_SendMessage_ErrorClient Function        */
/* Sends an ERROR message to a client         */
/*  Client : The sin of the client            */
/*  Code : The error code to send             */
/*  Descr : The description of the error code */
bool FM_SendMessage_ErrorClient(struct sockaddr_in Client,FFSS_Field Code,const char Descr[])
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_ERROR + FFSS_MAX_ERRORMSG_LENGTH+1];
	size_t len, pos;
  int resp;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_ERROR);
  pos = FFSS_PackField(msg,pos,Code);
  if(Descr != NULL)
  {
    len = strlen(Descr)+1;
    if(len > FFSS_MAX_ERRORMSG_LENGTH)
      len = FFSS_MAX_ERRORMSG_LENGTH;
    pos = FFSS_PackString(msg,pos,Descr,len);
  }
  else
    msg[pos++] = 0;

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Error message (%d:%s) to client",Code,Descr);
  resp = SU_UDPSendToSin(FM_SI_OUT_UDP,msg,(int)pos,Client);
  return (resp == pos);
}

/* FM_SendMessage_ErrorMaster Function        */
/* Sends an ERROR message to a master         */
/*  Master : The socket of the Master         */
/*  Code : The error code to send             */
/*  Descr : The description of the error code */
bool FM_SendMessage_ErrorMaster(SU_SOCKET Master,FFSS_Field Code,const char Descr[])
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_ERROR + FFSS_MAX_ERRORMSG_LENGTH+1];
	size_t len, pos;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_ERROR);
  pos = FFSS_PackField(msg,pos,Code);
  if(Descr != NULL)
  {
    len = strlen(Descr)+1;
    if(len > FFSS_MAX_ERRORMSG_LENGTH)
      len = FFSS_MAX_ERRORMSG_LENGTH;
    pos = FFSS_PackString(msg,pos,Descr,len);
  }
  else
    msg[pos++] = 0;

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Error message (%d:%s) to master",Code,Descr);
  return FFSS_SendTcpPacket(Master,msg,pos,false,false);
}

/* FM_SendMessage_ServerList Function              */
/* Sends a SERVER LIST message to a foreign master */
/*  Master : The socket of the Master              */
/*  User : User pointer returned in message answer */
bool FM_SendMessage_ServerList(SU_SOCKET Master,FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING + FFSS_MAX_SERVEROS_LENGTH+1 + FFSS_MAX_DOMAIN_LENGTH+1];
	size_t pos;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SERVER_LISTING);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,FFSS_COMPRESSION_BZLIB);

  msg[pos++] = 0; /* OS is NULL */
  msg[pos++] = 0; /* Domain is NULL */

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Server listing message to master");
  return FFSS_SendTcpPacket(Master,msg,pos,false,false);
}

/* FM_SendMessage_DomainListingAnswer Function     */
/* Sends a DOMAIN ANSWER message to client         */
/*  Client : The sin of the client                 */
/*  NbDomains : Nomber of domains                  */
/*  Domains : Array of strings (name of domains)   */
/*  User : User pointer returned in message answer */
bool FM_SendMessage_DomainListingAnswer(struct sockaddr_in Client,int NbDomains,char *Domains[],FFSS_LongField User)
{
  char *msg;
	size_t len, size, pos;
  int resp,i;

  context;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DOMAINS_LISTING_ANSWER + NbDomains*(FFSS_MAX_DOMAIN_LENGTH+1);
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_DOMAINS_LISTING_ANSWER);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,NbDomains);

  for(i=0;i<NbDomains;i++)
  {
    len = strlen(Domains[i])+1;
    if(len > FFSS_MAX_DOMAIN_LENGTH)
      len = FFSS_MAX_DOMAIN_LENGTH;
    pos = FFSS_PackString(msg,pos,Domains[i],len);
  }
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Domains Listing message to client");
  resp = SU_UDPSendToSin(FM_SI_OUT_UDP,msg,(int)pos,Client);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FM_SendMessage_MasterSearchAnswer Function               */
/* Sends a MASTER SEARCH ANSWER message to client or server */
/*  Client : The sin of the client or the server            */
/*  Server : True if from server                            */
/*  Domain : The name of my domain                          */
/*  User : User pointer returned in message answer          */
bool FM_SendMessage_MasterSearchAnswer(struct sockaddr_in Client,bool Server,const char Domain[],FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_MASTER_ANSWER + FFSS_MAX_DOMAIN_LENGTH+1];
	size_t len, pos;
  int resp;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SEARCH_MASTER_ANSWER);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,FFSS_PROTOCOL_VERSION);
  len = strlen(Domain)+1;
  if(len > FFSS_MAX_DOMAIN_LENGTH)
    len = FFSS_MAX_DOMAIN_LENGTH;
  pos = FFSS_PackString(msg,pos,Domain,len);

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  if(Server)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Master search answer message to server");
    resp = SU_UDPSendToAddr(FM_SI_OUT_UDP,msg,(int)pos,inet_ntoa(Client.sin_addr),FFSS_SERVER_PORT_S);
  }
  else
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Master search answer message to client");
    resp = SU_UDPSendToSin(FM_SI_OUT_UDP,msg,(int)pos,Client);
  }
  return (resp != SOCKET_ERROR);
}

/* FM_SendMessage_SearchAnswer Function                                          */
/* Sends a SEARCH ANSWER message to client                                       */
/*  Client : The sin of the client                                               */
/*  Buffer : The buffer containing the query, the nb of answers, and the answers */
/*  BufSize : The size of the buffer                                             */
/*  Compression : The type of compression to be applied to Buffer                */
/*  User : User pointer returned in message answer                               */
bool FM_SendMessage_SearchAnswer(struct sockaddr_in Client, const char *Buffer, size_t BufSize, int Compression, FFSS_LongField User)
{
  char *msg;
	size_t size, pos;
  int resp;
  size_t CompSize;

  context;
  if((Buffer == NULL) || (BufSize == 0))
    return true;
	CompSize = (size_t)(BufSize*1.05 + 6000);
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_ANSWER + CompSize;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SEARCH_ANSWER);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,Compression);

  switch(Compression)
  {
    case FFSS_COMPRESSION_NONE :
      memcpy(msg+pos,Buffer,BufSize);
      pos += BufSize;
      break;
#ifndef DISABLE_ZLIB
    case FFSS_COMPRESSION_ZLIB :
      if(!FFSS_CompresseZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in Z compression routine : Buffer too small ?");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif /* !DISABLE_ZLIB */
#ifdef HAVE_BZLIB
    case FFSS_COMPRESSION_BZLIB :
      if(!FFSS_CompresseBZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error in BZ compression routine : Buffer too small ?");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif
    default :
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Unknown compression type : %d",Compression);
      free(msg);
      return false;
  }

	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Search Answer message to client (%s:%d)",inet_ntoa(Client.sin_addr),ntohs(Client.sin_port));
  resp = SU_UDPSendToSin(FM_SI_OUT_UDP,msg,(int)pos,Client);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FM_SendMessage_SearchForward Function                 */
/* Sends a SEARCH message to a foreign master            */
/*  Master : The socket of the Master                    */
/*  Client : The sin of the client                       */
/*  Compressions  : Compressions supported by the client */
/*  Keys   : A String of keywords                        */
/*  User : User pointer returned in message answer       */
bool FM_SendMessage_SearchForward(SU_SOCKET Master, struct sockaddr_in Client, FFSS_BitField Compressions, const char Key[], FFSS_LongField User)
{
  char msg[sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_FW + FFSS_IP_FIELD_SIZE + FFSS_MAX_KEYWORDS_LENGTH+1];
	size_t len, pos;

  context;
  pos = sizeof(FFSS_Field);
  pos = FFSS_PackField(msg,pos,FFSS_MESSAGE_SEARCH_FW);
  pos = FFSS_PackLongField(msg,pos,User);
  pos = FFSS_PackField(msg,pos,ntohs(Client.sin_port));
  pos = FFSS_PackField(msg,pos,(FFSS_Field)Compressions);
  pos = FFSS_PackField(msg,pos,FFSS_IP_TYPE);
  FFSS_PackIP(msg+pos,inet_ntoa(Client.sin_addr),FFSS_IP_TYPE);
  pos += FFSS_IP_FIELD_SIZE;
  len = strlen(Key)+1;
  if(len > FFSS_MAX_KEYWORDS_LENGTH)
    len = FFSS_MAX_KEYWORDS_LENGTH;
  pos = FFSS_PackString(msg,pos,Key,len);
	FFSS_PackField(msg, 0, (FFSS_Field)pos);
  SU_DBG_PrintDebug(FFSS_DBGMSG_OUT_MSG,"Sending Search Forward message to master - Reply to %s:%d",inet_ntoa(Client.sin_addr),ntohs(Client.sin_port));
  return FFSS_SendTcpPacket(Master,msg,pos,false,false);
}

#endif /* !FFSS_DRIVER */
