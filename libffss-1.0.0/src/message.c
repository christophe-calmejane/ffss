#include "ffss.h"
#include "utils.h"

#ifdef IS_BIG_ENDIAN
#error FIX ME : All fields should be packed using a pack_field function
#endif /* IS_BIG_ENDIAN */

SU_THREAD_ROUTINE(FC_ClientThreadTCP,User);

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
  char *msg;
  long int len,size,pos;
  int resp;

  if(Master == NULL)
    return true;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STATE + FFSS_MAX_SERVERNAME_LENGTH+1 + FFSS_MAX_SERVEROS_LENGTH+1 + FFSS_MAX_SERVERCOMMENT_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_STATE;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = State;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_PROTOCOL_VERSION;
  pos += sizeof(FFSS_Field);

  len = strlen(Name)+1;
  if(len > FFSS_MAX_SERVERNAME_LENGTH)
    len = FFSS_MAX_SERVERNAME_LENGTH;
  SU_strcpy(msg+pos,Name,len);
  pos += len;
  len = strlen(OS)+1;
  if(len > FFSS_MAX_SERVEROS_LENGTH)
    len = FFSS_MAX_SERVEROS_LENGTH;
  SU_strcpy(msg+pos,OS,len);
  pos += len;
  len = strlen(Comment)+1;
  if(len > FFSS_MAX_SERVERCOMMENT_LENGTH)
    len = FFSS_MAX_SERVERCOMMENT_LENGTH;
  SU_strcpy(msg+pos,Comment,len);
  pos += len;
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending State message to %s\n",Master);
  resp = SU_UDPSendToAddr(FS_SI_OUT_UDP,msg,pos,(char *)Master,FFSS_MASTER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FS_SendMessage_ServerSearchAnswer Function       */
/* Sends a STATE message to a client                */
/*  Domain : The domain of my master                */
/*  Name : The name of my server                    */
/*  OS : The os of my server                        */
/*  Comment : The comment of my server              */
/*  State : The state of my server                  */
/*  IP : The IP address of my server                */
/*  MasterIP : The IP address of my server          */
bool FS_SendMessage_ServerSearchAnswer(struct sockaddr_in Client,const char Domain[],const char Name[],const char OS[],const char Comment[],int State,const char IP[],const char MasterIP[])
{
  char *msg;
  long int len,size,pos;
  int resp;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_NEW_STATES + sizeof(FFSS_Field)*FFSS_MESSAGESIZE_NEW_STATES_2 + FFSS_IP_FIELD_SIZE*2 + FFSS_MAX_DOMAIN_LENGTH+1 + FFSS_MAX_SERVERNAME_LENGTH+1 + FFSS_MAX_SERVEROS_LENGTH+1 + FFSS_MAX_SERVERCOMMENT_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_NEW_STATES;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_COMPRESSION_NONE;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = 1;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = State;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_IP_TYPE;
  pos += sizeof(FFSS_Field);
  FFSS_PackIP(msg+pos,MasterIP,FFSS_IP_TYPE);
  pos += FFSS_IP_FIELD_SIZE;

  len = strlen(Domain)+1;
  if(len > FFSS_MAX_DOMAIN_LENGTH)
    len = FFSS_MAX_DOMAIN_LENGTH;
  SU_strcpy(msg+pos,Domain,len);
  pos += len;
  len = strlen(Name)+1;
  if(len > FFSS_MAX_SERVERNAME_LENGTH)
    len = FFSS_MAX_SERVERNAME_LENGTH;
  SU_strcpy(msg+pos,Name,len);
  pos += len;
  len = strlen(OS)+1;
  if(len > FFSS_MAX_SERVEROS_LENGTH)
    len = FFSS_MAX_SERVEROS_LENGTH;
  SU_strcpy(msg+pos,OS,len);
  pos += len;
  len = strlen(Comment)+1;
  if(len > FFSS_MAX_SERVERCOMMENT_LENGTH)
    len = FFSS_MAX_SERVERCOMMENT_LENGTH;
  SU_strcpy(msg+pos,Comment,len);
  pos += len;
  *(FFSS_Field *)(msg+pos) = FFSS_IP_TYPE;
  pos += sizeof(FFSS_Field);
  FFSS_PackIP(msg+pos,IP,FFSS_IP_TYPE);
  pos += FFSS_IP_FIELD_SIZE;
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Server search answer message to client\n");
  resp = SU_UDPSendToSin(FS_SI_OUT_UDP,msg,pos,Client);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FS_SendMessage_ServerSharesAnswer Function                */
/* Sends a SHARES ANSWER message to a client                 */
/*  IP : The IP address of my server                         */
/*  ShareNames : A tab of the share names of my server       */
/*  ShareComments : A tab of the share comments of my server */
/*  NbShares : The number of shares of my server             */
bool FS_SendMessage_ServerSharesAnswer(struct sockaddr_in Client,const char IP[],const char **ShareNames,const char **ShareComments,int NbShares)
{
  char *msg;
  long int len,size,pos;
  int resp;
  int i;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SHARES_LISTING_ANSWER + FFSS_IP_FIELD_SIZE + (FFSS_MAX_SHARENAME_LENGTH+1)*NbShares + (FFSS_MAX_SHARECOMMENT_LENGTH+1)*NbShares;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SHARES_LISTING_ANSWER;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_IP_TYPE;
  pos += sizeof(FFSS_Field);
  FFSS_PackIP(msg+pos,IP,FFSS_IP_TYPE);
  pos += FFSS_IP_FIELD_SIZE;

  *(FFSS_Field *)(msg+pos) = NbShares;
  pos += sizeof(FFSS_Field);
  for(i=0;i<NbShares;i++)
  {
    len = strlen(ShareNames[i])+1;
    if(len > FFSS_MAX_SHARENAME_LENGTH)
      len = FFSS_MAX_SHARENAME_LENGTH;
    SU_strcpy(msg+pos,ShareNames[i],len);
    pos += len;
    len = strlen(ShareComments[i])+1;
    if(len > FFSS_MAX_SHARECOMMENT_LENGTH)
      len = FFSS_MAX_SHARECOMMENT_LENGTH;
    SU_strcpy(msg+pos,ShareComments[i],len);
    pos += len;
  }
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Server shares answer message to client\n");
  resp = SU_UDPSendToSin(FS_SI_OUT_UDP,msg,pos,Client);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FS_SendMessage_Pong Function     */
/* Sends a PONG message to a master */
/*  Master : The sin of my master   */
/*  State : The state of my server  */
bool FS_SendMessage_Pong(struct sockaddr_in Master,int State)
{
  char *msg;
  long int size,pos;
  int resp;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_PONG;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_PONG;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = State;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Pong message to master\n");
  resp = SU_UDPSendToAddr(FS_SI_OUT_UDP,msg,pos,inet_ntoa(Master.sin_addr),FFSS_MASTER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FS_SendMessage_Error Function              */
/* Sends an ERROR message to a client         */
/*  Client : The socket of the client         */
/*  Code : The error code to send             */
/*  Descr : The description of the error code */
bool FS_SendMessage_Error(int Client,FFSS_Field Code,const char Descr[])
{
  char *msg;
  long int len,size,pos;
  int resp;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_ERROR + FFSS_MAX_ERRORMSG_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_ERROR;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = Code;
  pos += sizeof(FFSS_Field);
  if(Descr != NULL)
  {
    len = strlen(Descr)+1;
    if(len > FFSS_MAX_ERRORMSG_LENGTH)
      len = FFSS_MAX_ERRORMSG_LENGTH;
    SU_strcpy(msg+pos,Descr,len);
    pos += len;
  }
  else
    msg[pos++] = 0;

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Error message (%d:%s) to client\n",Code,Descr);
  FD_ZERO(&rfds);
  FD_SET(Client,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Client+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Error message timed out !\n");
    return false;
  }
  resp = send(Client,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp == pos);
}

/* FS_SendMessage_DirectoryListingAnswer Function                     */
/* Sends a DIRECTORY LISTING ANSWER message to a client               */
/*  Client : The socket of the client                                 */
/*  Path : The path of the directory IN the share                     */
/*  Buffer : The buffer containing the nb of entries, and the entries */
/*  BufSize : The size of the buffer                                  */
/*  Compression : The type of compression to be applied to Buffer     */
bool FS_SendMessage_DirectoryListingAnswer(int Client,const char Path[],const char *Buffer,long int BufSize,int Compression)
{
  char *msg;
  long int len,size,pos;
  int resp;
  fd_set rfds;
  struct timeval tv;
  int retval;
  long int CompSize;

  CompSize = BufSize*1.05+6000;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DIRECTORY_LISTING_ANSWER + FFSS_MAX_PATH_LENGTH+1 + CompSize;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_DIRECTORY_LISTING_ANSWER;
  pos += sizeof(FFSS_Field);

  len = strlen(Path)+1;
  if(len > FFSS_MAX_PATH_LENGTH)
    len = FFSS_MAX_PATH_LENGTH;
  SU_strcpy(msg+pos,Path,len);
  pos += len;

  *(FFSS_Field *)(msg+pos) = Compression;
  pos += sizeof(FFSS_Field);
  switch(Compression)
  {
    case FFSS_COMPRESSION_NONE :
      memcpy(msg+pos,Buffer,BufSize);
      pos += BufSize;
      break;
    case FFSS_COMPRESSION_ZLIB :
      if(!FFSS_CompresseZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        FFSS_PrintDebug(1,"Error in Z compression routine : Buffer too small ?\n");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#ifdef HAVE_BZLIB
    case FFSS_COMPRESSION_BZLIB :
      if(!FFSS_CompresseBZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        FFSS_PrintDebug(1,"Error in BZ compression routine : Buffer too small ?\n");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif
    default :
      FFSS_PrintDebug(1,"Unknown compression type : %d\n",Compression);
      free(msg);
      return false;
  }

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Directory listing answer message (\"%s\") to client\n",Path);
  FD_ZERO(&rfds);
  FD_SET(Client,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Client+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Directory listing message timed out !\n");
    return false;
  }
  resp = send(Client,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp == pos);
}

/* FS_SendMessage_InitXFer Function                        */
/* Sends an INIT XFER message to a client                  */
/*  Client : The socket of the client                      */
/*  Tag : The xfer tag that will be used when sending data */
/*  FileName : The name of the requested file              */
bool FS_SendMessage_InitXFer(int Client,FFSS_Field Tag,const char FileName[])
{
  char *msg;
  long int len,size,pos;
  int resp;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_INIT_XFER + FFSS_MAX_FILEPATH_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_INIT_XFER;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = Tag;
  pos += sizeof(FFSS_Field);
  if(FileName != NULL)
  {
    len = strlen(FileName)+1;
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    SU_strcpy(msg+pos,FileName,len);
    pos += len;
  }
  else
    msg[pos++] = 0;

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Init XFer message (%d:%s) to client\n",Tag,FileName);
  FD_ZERO(&rfds);
  FD_SET(Client,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Client+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Error message timed out !\n");
    return false;
  }
  resp = send(Client,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp == pos);
}

/* FS_SendMessage_MasterSearch Function      */
/* Sends a MASTER SEARCH message to broadcast */
bool FS_SendMessage_MasterSearch()
{
  char *msg;
  long int size,pos;
  int resp;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_MASTER;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SEARCH_MASTER;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_THREAD_SERVER;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Master Search message to broadcast\n");
  resp = FFSS_SendBroadcast(FS_SI_OUT_UDP,msg,pos,FFSS_MASTER_PORT_S);
  free(msg);
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
  char *msg;
  long int size,pos,total,total_ft,total_node;
  int resp;
  long int partial,current,slen;
  char *data;
  int sock=0,len,client;
  struct sockaddr_in SAddr;
  fd_set rfds;
  struct timeval tv;
  int retval;
  FFSS_Field NbBufs;

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

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_INDEX_ANSWER;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_INDEX_ANSWER;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Compression;
  pos += sizeof(FFSS_Field);

  Ptr = Buffers;
  Ptr2 = Sizes;
  total = sizeof(FFSS_Field);
  total_ft = 0;
  total_node = 0;
  NbBufs = 0;
  while(Ptr != NULL)
  {
    if(NbBufs%2)
      total_node += (long int) Ptr2->Data;
    else
      total_ft += (long int) Ptr2->Data;
    total += sizeof(FFSS_Field);
    switch(Compression)
    {
      case FFSS_COMPRESSION_NONE :
        break;
      case FFSS_COMPRESSION_ZLIB :
        partial = ((long int) Ptr2->Data)*1.05+12;
        data = (char *)Ptr->Data;
        Ptr->Data = malloc(partial);
        if(!FFSS_CompresseZlib(data,(long int)Ptr2->Data,(char *)Ptr->Data,&partial))
        {
          FFSS_PrintDebug(1,"Error in Z compression routine : Buffer too small ?\n");
          SU_CLOSE_SOCKET(sock);
          free(data);
          free(msg);
          return false;
        }
        free(data);
        Ptr2->Data = (void *) partial;
        break;
#ifdef HAVE_BZLIB
      case FFSS_COMPRESSION_BZLIB :
        partial = ((long int) Ptr2->Data)*1.05+1600;
        data = (char *)Ptr->Data;
        Ptr->Data = malloc(partial);
        if(!FFSS_CompresseBZlib(data,(long int)Ptr2->Data,(char *)Ptr->Data,&partial))
        {
          FFSS_PrintDebug(1,"Error in BZ compression routine : Buffer too small ?\n");
          SU_CLOSE_SOCKET(sock);
          free(data);
          free(msg);
          return false;
        }
        free(data);
        Ptr2->Data = (void *) partial;
        break;
#endif
      default :
        FFSS_PrintDebug(1,"Unknown compression type : %d\n",Compression);
        free(msg);
        return false;
    }
    total += (long int) Ptr2->Data;
    Ptr = Ptr->Next;
    Ptr2 = Ptr2->Next;
    NbBufs++;
  }
  NbBufs /= 2;

  *(FFSS_Field *)(msg+pos) = total;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = total_ft;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = total_node;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = ntohs(SAddr.sin_port);
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending index answer message to %s:%s\n",Host,Port);
  resp = SU_UDPSendToAddr(FS_SI_OUT_UDP,msg,pos,(char *)Host,(char *)Port);
  free(msg);
  if(resp == SOCKET_ERROR)
  {
    FFSS_PrintDebug(3,"Failed to connect master\n");
    SU_CLOSE_SOCKET(sock);
    return false;
  }

  FD_ZERO(&rfds);
  FD_SET(sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_INDEX_ACCEPT;
  tv.tv_usec = 0;
  retval = select(sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
  {
    FFSS_PrintDebug(3,"Timed out waiting for master to connect to port %d\n",ntohs(SAddr.sin_port));
    SU_CLOSE_SOCKET(sock);
    return false;
  }

  client = accept(sock,(struct sockaddr *)&SAddr,&len);
  SU_CLOSE_SOCKET(sock);
  if(client == SOCKET_ERROR)
    return false;

  /* Send nb buffers */
  FD_ZERO(&rfds);
  FD_SET(client,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_INDEX_XFER;
  tv.tv_usec = 0;
  retval = select(client+1,NULL,&rfds,NULL,&tv);
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
    retval = select(client+1,NULL,&rfds,NULL,&tv);
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
      retval = select(client+1,NULL,&rfds,NULL,&tv);
      if(!retval)
      {
        FFSS_PrintDebug(3,"Timed out waiting for master to retrieve index data\n");
        SU_CLOSE_SOCKET(sock);
        return false;
      }
      resp = send(client,data+current,slen,SU_MSG_NOSIGNAL);
      if(resp == SOCKET_ERROR)
      {
        FFSS_PrintDebug(3,"Error sending index data to master (%ld %ld) : %s\n",current,slen,strerror(errno));
        SU_CLOSE_SOCKET(sock);
        return false;
      }
      current += slen;
    }
    Ptr = Ptr->Next;
    Ptr2 = Ptr2->Next;
  }
  SU_CLOSE_SOCKET(sock);
  FFSS_PrintDebug(3,"Index successfully sent to %s:%s\n",Host,Port);
  return true;
}

/* FS_SendMessage_StrmOpenAnswer Function             */
/* Sends an STREAMING OPEN answer message to a client */
/*  Client : The socket of the client                 */
/*  Path : The path of the file requested             */
/*  Code : The error code to send                     */
/*  Handle : The handle of the file if successfull    */
/*  FileSize : Size of the file                       */
bool FS_SendMessage_StrmOpenAnswer(int Client,const char Path[],FFSS_Field Code,long int Handle,FFSS_LongField FileSize)
{
  char *msg;
  long int len,size,pos;
  int resp;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_OPEN_ANSWER + FFSS_MAX_FILEPATH_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_STREAMING_OPEN_ANSWER;
  pos += sizeof(FFSS_Field);

  if(Path != NULL)
  {
    len = strlen(Path)+1;
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    SU_strcpy(msg+pos,Path,len);
    pos += len;
  }
  else
    msg[pos++] = 0;

  *(FFSS_Field *)(msg+pos) = Code;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = Handle;
  pos += sizeof(FFSS_Field);
  *(FFSS_LongField *)(msg+pos) = FileSize;
  pos += sizeof(FFSS_LongField);

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Streaming OPEN answer message to client\n");
  FD_ZERO(&rfds);
  FD_SET(Client,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Client+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Streaming OPEN answer message timed out !\n");
    return false;
  }
  resp = send(Client,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp == pos);
}

/* FS_SendMessage_StrmReadAnswer Function             */
/* Sends an STREAMING READ answer message to a client */
/*  Client : The socket of the client                 */
/*  Handle : The handle of the file                   */
/*  Buf : The buffer of datas                         */
/*  BlocLen : The length of the datas                 */
bool FS_SendMessage_StrmReadAnswer(int Client,long int Handle,char *Buf,long int BlocLen)
{
  char *msg;
  long int size,pos;
  int resp;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_READ_ANSWER + BlocLen;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_STREAMING_READ_ANSWER;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Handle;
  pos += sizeof(FFSS_Field);
  memcpy(msg+pos,Buf,BlocLen);
  pos += BlocLen;

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Streaming READ answer message to client\n");
  FD_ZERO(&rfds);
  FD_SET(Client,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Client+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Streaming READ answer message timed out !\n");
    return false;
  }
  resp = send(Client,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp == pos);
}

/* FS_SendMessage_StrmWriteAnswer Function             */
/* Sends an STREAMING WRITE answer message to a client */
/*  Client : The socket of the client                  */
/*  Handle : The handle of the file                    */
/*  Code : The error code to send                      */
bool FS_SendMessage_StrmWriteAnswer(int Client,long int Handle,FFSS_Field Code)
{
  char *msg;
  long int size,pos;
  int resp;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_WRITE_ANSWER;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_STREAMING_WRITE_ANSWER;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Handle;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Code;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Streaming WRITE answer message to client\n");
  FD_ZERO(&rfds);
  FD_SET(Client,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Client+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Streaming WRITE answer message timed out !\n");
    return false;
  }
  resp = send(Client,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp == pos);
}



/* ************************************ */
/*             CLIENT MESSAGES          */
/* ************************************ */

/* FC_SendMessage_ServerSearch Function       */
/* Sends a SERVER SEARCH message to broadcast */
bool FC_SendMessage_ServerSearch(void)
{
  char *msg;
  long int size,pos;
  int resp;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_SEARCH;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SERVER_SEARCH;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Servers Search message to broadcast\n");
  resp = FFSS_SendBroadcast(FC_SI_OUT_UDP,msg,pos,FFSS_SERVER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_SharesListing Function                       */
/* Sends a SHARES LISTING message to a server                  */
/*  Server : The name of the server we want the shares listing */
bool FC_SendMessage_SharesListing(const char Server[])
{
  char *msg;
  long int size,pos;
  int resp;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SHARES_LISTING;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SHARES_LISTING;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Shares Listing message to %s\n",Server);
  resp = SU_UDPSendToAddr(FC_SI_OUT_UDP,msg,pos,(char *)Server,FFSS_SERVER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_ServerList Function                      */
/* Sends a SERVER LIST message to a master                 */
/*  Master : The name of my master, or NULL if none        */
/*  OS : The desired OS, or NULL if requesting all         */
/*  Domain : The desired domain, or NULL if requesting all */
bool FC_SendMessage_ServerList(const char Master[],const char OS[],const char Domain[])
{
  char *msg;
  long int len,size,pos;
  int resp;
  long int Comps;

  if(Master == NULL)
    return true;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING + FFSS_MAX_SERVEROS_LENGTH+1 + FFSS_MAX_DOMAIN_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SERVER_LISTING;
  pos += sizeof(FFSS_Field);

  Comps = FFSS_COMPRESSION_NONE | FFSS_COMPRESSION_ZLIB;
#ifdef HAVE_BZLIB
  Comps |= FFSS_COMPRESSION_BZLIB;
#endif
  *(FFSS_Field *)(msg+pos) = Comps;
  pos += sizeof(FFSS_Field);

  if(OS != NULL)
  {
    len = strlen(OS)+1;
    if(len > FFSS_MAX_SERVEROS_LENGTH)
      len = FFSS_MAX_SERVEROS_LENGTH;
    SU_strcpy(msg+pos,OS,len);
    pos += len;
  }
  else
    msg[pos++] = 0;
  if(Domain != NULL)
  {
    len = strlen(Domain)+1;
    if(len > FFSS_MAX_DOMAIN_LENGTH)
      len = FFSS_MAX_DOMAIN_LENGTH;
    SU_strcpy(msg+pos,Domain,len);
    pos += len;
  }
  else
    msg[pos++] = 0;
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Server listing message to %s\n",Master);
  resp = SU_UDPSendToAddr(FC_SI_OUT_UDP,msg,pos,(char *)Master,FFSS_MASTER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_ShareConnect Function                  */
/* Sends a SHARE CONNECTION message to a server          */
/*  Server : The name of Server we wish to connect to    */
/*  ShareName : The Share Name we wish to connect to     */
/*  Login : The Login we may use (or NULL if none)       */
/*  Password : The Password we may use (or NULL if none) */
SU_PClientSocket FC_SendMessage_ShareConnect(const char Server[],const char ShareName[],const char Login[],const char Password[])
{
  char *msg;
  long int len,size,pos;
  int resp;
  SU_PClientSocket CS;
  fd_set rfds;
  struct timeval tv;
  int retval;
  long int Comps;
#ifndef DRIVER
  SU_THREAD_HANDLE Thread;
#endif /* !DRIVER */

  CS = SU_ClientConnect((char *)Server,FFSS_SERVER_PORT_S,SOCK_STREAM);
  if(CS == NULL)
    return NULL;

  FFSS_PrintDebug(2,"Connected to %s\n",Server);
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SHARE_CONNECTION + FFSS_MAX_SHARENAME_LENGTH+1 + FFSS_MAX_LOGIN_LENGTH+1 + FFSS_MAX_PASSWORD_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SHARE_CONNECTION;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_PROTOCOL_VERSION;
  pos += sizeof(FFSS_Field);

  Comps = FFSS_COMPRESSION_NONE | FFSS_COMPRESSION_ZLIB;
#ifdef HAVE_BZLIB
  Comps |= FFSS_COMPRESSION_BZLIB;
#endif
  *(FFSS_Field *)(msg+pos) = Comps;
  pos += sizeof(FFSS_Field);

  len = strlen(ShareName)+1;
  if(len > FFSS_MAX_SHARENAME_LENGTH)
    len = FFSS_MAX_SHARENAME_LENGTH;
  SU_strcpy(msg+pos,ShareName,len);
  pos += len;
  if(Login != NULL)
  {
    len = strlen(Login)+1;
    if(len > FFSS_MAX_LOGIN_LENGTH)
      len = FFSS_MAX_LOGIN_LENGTH;
    SU_strcpy(msg+pos,Login,len);
    pos += len;
  }
  else
    msg[pos++] = 0;
  if(Password != NULL)
  {
    len = strlen(Password)+1;
    if(len > FFSS_MAX_PASSWORD_LENGTH)
      len = FFSS_MAX_PASSWORD_LENGTH;
    SU_strcpy(msg+pos,Password,len);
    pos += len;
  }
  else
    msg[pos++] = 0;
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Share connection message to %s\n",Server);
  FD_ZERO(&rfds);
  FD_SET(CS->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(CS->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    SU_CLOSE_SOCKET(CS->sock);
    free(CS);
    FFSS_PrintDebug(3,"Sending Share connection message timed out !\n");
    return NULL;
  }
  resp = send(CS->sock,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  if(resp != pos)
  {
    SU_CLOSE_SOCKET(CS->sock);
    free(CS);
    return NULL;
  }
#ifndef DRIVER
  SU_CreateThread(&Thread,FC_ClientThreadTCP,(void *)CS,true);
#endif /* !DRIVER */
  return CS;
}

/* FC_SendMessage_DirectoryListing Function             */
/* Sends a DIRECTORY LISTING message to a server        */
/*  Server : The Server's structure we are connected to */
/*  Path : The path we request a listing                */
bool FC_SendMessage_DirectoryListing(SU_PClientSocket Server,const char Path[])
{
  char *msg;
  long int size,pos;
  int resp,len;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DIRECTORY_LISTING + FFSS_MAX_PATH_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_DIRECTORY_LISTING;
  pos += sizeof(FFSS_Field);
  len = strlen(Path)+1;
  if(len > FFSS_MAX_PATH_LENGTH)
    len = FFSS_MAX_PATH_LENGTH;
  SU_strcpy(msg+pos,Path,len);
  pos += len;
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Directory Listing message to server for %s\n",Path);
  FD_ZERO(&rfds);
  FD_SET(Server->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Server->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Directory listing message timed out !\n");
    return false;
  }
  resp = send(Server->sock,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_Download Function                                 */
/* Sends a DOWNLOAD message to a server                             */
/*  Server : The Server's structure we are connected to             */
/*  Path : The path of requested file (in the share)                */
/*  StartingPos : The pos we want to download the file starting at  */
/*  UseConnSock : Use a separate socket/thread, or use the existing */
int FC_SendMessage_Download(SU_PClientSocket Server,const char Path[],FFSS_LongField StartingPos,bool UseConnSock)
{
  char *msg;
  long int size,pos;
  int resp,len;
  int sock=0;
  struct sockaddr_in SAddr;
  fd_set rfds;
  struct timeval tv;
  int retval;

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
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DOWNLOAD + FFSS_MAX_FILEPATH_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_DOWNLOAD;
  pos += sizeof(FFSS_Field);
  len = strlen(Path)+1;
  if(len > FFSS_MAX_FILEPATH_LENGTH)
    len = FFSS_MAX_FILEPATH_LENGTH;
  SU_strcpy(msg+pos,Path,len);
  pos += len;
  *(FFSS_LongField *)(msg+pos) = StartingPos;
  pos += sizeof(FFSS_LongField);
  if(UseConnSock)
    *(FFSS_Field *)(msg+pos) = -1;
  else
    *(FFSS_Field *)(msg+pos) = ntohs(SAddr.sin_port);
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Download message to server for %s starting at %ld\n",Path,StartingPos);
  FD_ZERO(&rfds);
  FD_SET(Server->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Server->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    if(!UseConnSock)
      SU_CLOSE_SOCKET(sock);
    FFSS_PrintDebug(3,"Sending Download message timed out !\n");
    return SOCKET_ERROR;
  }
  resp = send(Server->sock,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  if(resp == SOCKET_ERROR)
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
  char *msg;
  long int size,pos;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DISCONNECT;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_DISCONNECT;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Disconnect message to server\n");
  FD_ZERO(&rfds);
  FD_SET(Server->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Server->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
    FFSS_PrintDebug(3,"Sending Disconnect message timed out !\n");
  else
    send(Server->sock,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return;
}

/* FC_SendMessage_CancelXFer Function                   */
/* Sends an CANCEL XFER message to a server             */
/*  Server : The Server's structure we are connected to */
/*  XFerTag : The tag of the xfer we want to cancel     */
void FC_SendMessage_CancelXFer(SU_PClientSocket Server,FFSS_Field XFerTag)
{
  char *msg;
  long int size,pos;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_CANCEL_XFER;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_CANCEL_XFER;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = XFerTag;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending cancel xfer message to server\n");
  FD_ZERO(&rfds);
  FD_SET(Server->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Server->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
    FFSS_PrintDebug(3,"Sending cancel xfer message timed out !\n");
  else
    send(Server->sock,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  SU_FreeCS(Server);
  return;
}

/* FC_SendMessage_DomainListing Function   */
/* Sends a DOMAIN LIST message to a master */
/*  Master : The name of my master         */
bool FC_SendMessage_DomainListing(const char Master[])
{
  char *msg;
  long int size,pos;
  int resp;

  if(Master == NULL)
    return true;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DOMAINS_LISTING;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_DOMAINS_LISTING;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Domains listing message to %s\n",Master);
  resp = SU_UDPSendToAddr(FC_SI_OUT_UDP,msg,pos,(char *)Master,FFSS_MASTER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_Search Function                          */
/* Sends a SEARCH message to a master                      */
/*  Master : The name of my master                         */
/*  Domain : The desired domain, or NULL if requesting all */
/*  Keys   : A String of keywords                          */
bool FC_SendMessage_Search(const char Master[],const char Domain[],const char Key[])
{
  char *msg;
  long int len,size,pos;
  int resp;
  long int Comps;

  if(Master == NULL)
    return true;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH + FFSS_MAX_DOMAIN_LENGTH+1 + FFSS_MAX_KEYWORDS_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SEARCH;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = ntohs(FC_SI_OUT_UDP->SAddr.sin_port);
  pos += sizeof(FFSS_Field);

  Comps = FFSS_COMPRESSION_NONE | FFSS_COMPRESSION_ZLIB;
#ifdef HAVE_BZLIB
  Comps |= FFSS_COMPRESSION_BZLIB;
#endif
  *(FFSS_Field *)(msg+pos) = Comps;
  pos += sizeof(FFSS_Field);

  if(Domain != NULL)
  {
    len = strlen(Domain)+1;
    if(len > FFSS_MAX_DOMAIN_LENGTH)
      len = FFSS_MAX_DOMAIN_LENGTH;
    SU_strcpy(msg+pos,Domain,len);
    pos += len;
  }
  else
    msg[pos++] = 0;
  len = strlen(Key)+1;
  if(len > FFSS_MAX_KEYWORDS_LENGTH)
    len = FFSS_MAX_KEYWORDS_LENGTH;
  SU_strcpy(msg+pos,Key,len);
  pos += len;
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Search message to %s - Reply to port %d\n",Master,ntohs(FC_SI_OUT_UDP->SAddr.sin_port));
  resp = SU_UDPSendToAddr(FC_SI_OUT_UDP,msg,pos,(char *)Master,FFSS_MASTER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_MasterSearch Function       */
/* Sends a MASTER SEARCH message to broadcast */
bool FC_SendMessage_MasterSearch()
{
  char *msg;
  long int size,pos;
  int resp;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_MASTER;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SEARCH_MASTER;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_THREAD_CLIENT;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Master Search message to broadcast\n");
  resp = FFSS_SendBroadcast(FC_SI_OUT_UDP,msg,pos,FFSS_MASTER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_StrmOpen Function                     */
/* Sends an STREAMING OPEN message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Path : The path of the requested file               */
/*  Flags : The opening mode flags                      */
bool FC_SendMessage_StrmOpen(SU_PClientSocket Server,const char Path[],int Flags)
{
  char *msg;
  long int len,size,pos;
  int resp;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_OPEN + FFSS_MAX_FILEPATH_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_STREAMING_OPEN;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Flags;
  pos += sizeof(FFSS_Field);

  if(Path != NULL)
  {
    len = strlen(Path)+1;
    if(len > FFSS_MAX_FILEPATH_LENGTH)
      len = FFSS_MAX_FILEPATH_LENGTH;
    SU_strcpy(msg+pos,Path,len);
    pos += len;
  }
  else
    msg[pos++] = 0;

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Streaming OPEN message to client\n");
  FD_ZERO(&rfds);
  FD_SET(Server->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Server->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Streaming OPEN message timed out !\n");
    return false;
  }
  resp = send(Server->sock,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_StrmClose Function                     */
/* Sends an STREAMING CLOSE message to a server          */
/*  Server : The Server's structure we are connected to  */
/*  Handle : The handle of the file to close             */
bool FC_SendMessage_StrmClose(SU_PClientSocket Server,long int Handle)
{
  char *msg;
  long int size,pos;
  int resp;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_CLOSE;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_STREAMING_CLOSE;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Handle;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Streaming CLOSE message to client\n");
  FD_ZERO(&rfds);
  FD_SET(Server->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Server->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Streaming CLOSE message timed out !\n");
    return false;
  }
  resp = send(Server->sock,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_StrmRead Function                     */
/* Sends an STREAMING READ message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  StartPos : The start position of the requested bloc */
/*  Length : Indicative length requested                */
bool FC_SendMessage_StrmRead(SU_PClientSocket Server,long int Handle,FFSS_LongField StartPos,long int Length)
{
  char *msg;
  long int size,pos;
  int resp;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_READ;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_STREAMING_READ;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Handle;
  pos += sizeof(FFSS_Field);

  *(FFSS_LongField *)(msg+pos) = StartPos;
  pos += sizeof(FFSS_LongField);

  *(FFSS_Field *)(msg+pos) = Length;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Streaming READ message to client\n");
  FD_ZERO(&rfds);
  FD_SET(Server->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Server->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Streaming READ message timed out !\n");
    return false;
  }
  resp = send(Server->sock,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_StrmWrite Function                    */
/* Sends an STREAMING WRITE message to a server         */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  StartPos : The start position of the requested bloc */
/*  Buf : The buffer of datas                           */
/*  BlocLen : The length of the datas                   */
bool FC_SendMessage_StrmWrite(SU_PClientSocket Server,long int Handle,FFSS_LongField StartPos,char *Buf,long int BlocLen)
{
  char *msg;
  long int size,pos;
  int resp;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_WRITE + BlocLen;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_STREAMING_WRITE;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Handle;
  pos += sizeof(FFSS_Field);

  *(FFSS_LongField *)(msg+pos) = StartPos;
  pos += sizeof(FFSS_LongField);

  memcpy(msg+pos,Buf,BlocLen);
  pos += BlocLen;

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Streaming WRITE message to client\n");
  FD_ZERO(&rfds);
  FD_SET(Server->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Server->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Streaming WRITE message timed out !\n");
    return false;
  }
  resp = send(Server->sock,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FC_SendMessage_StrmSeek Function                     */
/* Sends an STREAMING SEEK message to a server          */
/*  Server : The Server's structure we are connected to */
/*  Handle : The handle of the file to close            */
/*  Flags : The flags for the seek operation            */
/*  StartPos : The position of the seek                 */
bool FC_SendMessage_StrmSeek(SU_PClientSocket Server,long int Handle,int Flags,FFSS_LongField StartPos)
{
  char *msg;
  long int size,pos;
  int resp;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_STREAMING_SEEK;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_STREAMING_SEEK;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Handle;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Flags;
  pos += sizeof(FFSS_Field);

  *(FFSS_LongField *)(msg+pos) = StartPos;
  pos += sizeof(FFSS_LongField);

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Streaming SEEK message to client\n");
  FD_ZERO(&rfds);
  FD_SET(Server->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TCP_MESSAGE;
  tv.tv_usec = 0;
  retval = select(Server->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    free(msg);
    FFSS_PrintDebug(3,"Sending Streaming SEEK message timed out !\n");
    return false;
  }
  resp = send(Server->sock,msg,pos,SU_MSG_NOSIGNAL);
  free(msg);
  return (resp != SOCKET_ERROR);
}


/* ************************************ */
/*             MASTER MESSAGES          */
/* ************************************ */

/* FM_SendMessage_Ping Function    */
/* Sends a PING message to servers */
bool FM_SendMessage_Ping()
{
  char *msg;
  long int size,pos;
  int resp;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_PING;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_PING;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Ping message to servers\n");
  resp = FFSS_SendBroadcast(FM_SI_OUT_UDP,msg,pos,FFSS_SERVER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FM_SendMessage_NewStatesMaster Function                          */
/* Sends a NEW STATES message to a master                           */
/*  Master : The name of the master we want to send states          */
/*  Buffer : The buffer containing the nb of states, and the states */
/*  BufSize : The size of the buffer                                */
/*  Compression : The type of compression to be applied to Buffer   */
bool FM_SendMessage_NewStatesMaster(const char Master[],const char *Buffer,long int BufSize,int Compression)
{
  char *msg;
  long int size,pos;
  int resp;
  long int CompSize;

  CompSize = BufSize*1.05+6000;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_NEW_STATES + CompSize;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_NEW_STATES;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Compression;
  pos += sizeof(FFSS_Field);
  switch(Compression)
  {
    case FFSS_COMPRESSION_NONE :
      memcpy(msg+pos,Buffer,BufSize);
      pos += BufSize;
      break;
    case FFSS_COMPRESSION_ZLIB :
      if(!FFSS_CompresseZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        FFSS_PrintDebug(1,"Error in Z compression routine : Buffer too small ?\n");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#ifdef HAVE_BZLIB
    case FFSS_COMPRESSION_BZLIB :
      if(!FFSS_CompresseBZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        FFSS_PrintDebug(1,"Error in BZ compression routine : Buffer too small ?\n");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif
    default :
      FFSS_PrintDebug(1,"Unknown compression type : %d\n",Compression);
      free(msg);
      return false;
  }

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending New States message to master\n");
  resp = SU_UDPSendToAddr(FM_SI_OUT_UDP,msg,pos,(char *)Master,FFSS_MASTER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FM_SendMessage_ServerListing Function                            */
/* Sends a NEW STATES message to client                             */
/*  Client : The sin of the client                                  */
/*  Buffer : The buffer containing the nb of domains, and the hosts */
/*  BufSize : The size of the buffer                                */
/*  Compression : The type of compression to be applied to Buffer   */
bool FM_SendMessage_ServerListing(struct sockaddr_in Client,const char *Buffer,long int BufSize,int Compression)
{
  char *msg;
  long int size,pos;
  int resp;
  long int CompSize;

  CompSize = BufSize*1.05+6000;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING_ANSWER + CompSize;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SERVER_LISTING_ANSWER;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Compression;
  pos += sizeof(FFSS_Field);
  switch(Compression)
  {
    case FFSS_COMPRESSION_NONE :
      memcpy(msg+pos,Buffer,BufSize);
      pos += BufSize;
      break;
    case FFSS_COMPRESSION_ZLIB :
      if(!FFSS_CompresseZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        FFSS_PrintDebug(1,"Error in Z compression routine : Buffer too small ?\n");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#ifdef HAVE_BZLIB
    case FFSS_COMPRESSION_BZLIB :
      if(!FFSS_CompresseBZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        FFSS_PrintDebug(1,"Error in BZ compression routine : Buffer too small ?\n");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif
    default :
      FFSS_PrintDebug(1,"Unknown compression type : %d\n",Compression);
      free(msg);
      return false;
  }

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Server Listing message to client\n");
  resp = SU_UDPSendToSin(FM_SI_OUT_UDP,msg,pos,Client);
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
  char *msg;
  long int len,size,pos;
  int resp;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_ERROR + FFSS_MAX_ERRORMSG_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_ERROR;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = Code;
  pos += sizeof(FFSS_Field);
  if(Descr != NULL)
  {
    len = strlen(Descr)+1;
    if(len > FFSS_MAX_ERRORMSG_LENGTH)
      len = FFSS_MAX_ERRORMSG_LENGTH;
    SU_strcpy(msg+pos,Descr,len);
    pos += len;
  }
  else
    msg[pos++] = 0;

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Error message (%d:%s) to server %s\n",Code,Descr,Server);
  resp = SU_UDPSendToAddr(FM_SI_OUT_UDP,msg,pos,(char *)Server,FFSS_SERVER_PORT_S);
  free(msg);
  return (resp == pos);
}

/* FM_SendMessage_ErrorClient Function        */
/* Sends an ERROR message to a client         */
/*  Client : The sin of the client            */
/*  Code : The error code to send             */
/*  Descr : The description of the error code */
bool FM_SendMessage_ErrorClient(struct sockaddr_in Client,FFSS_Field Code,const char Descr[])
{
  char *msg;
  long int len,size,pos;
  int resp;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_ERROR + FFSS_MAX_ERRORMSG_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_ERROR;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = Code;
  pos += sizeof(FFSS_Field);
  if(Descr != NULL)
  {
    len = strlen(Descr)+1;
    if(len > FFSS_MAX_ERRORMSG_LENGTH)
      len = FFSS_MAX_ERRORMSG_LENGTH;
    SU_strcpy(msg+pos,Descr,len);
    pos += len;
  }
  else
    msg[pos++] = 0;

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Error message (%d:%s) to client\n",Code,Descr);
  resp = SU_UDPSendToSin(FM_SI_OUT_UDP,msg,pos,Client);
  free(msg);
  return (resp == pos);
}

/* FM_SendMessage_ServerList Function              */
/* Sends a SERVER LIST message to a foreign master */
/*  Master : The name of the foreign master        */
bool FM_SendMessage_ServerList(const char Master[])
{
  char *msg;
  long int size,pos;
  int resp;

  if(Master == NULL)
    return true;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SERVER_LISTING + FFSS_MAX_SERVEROS_LENGTH+1 + FFSS_MAX_DOMAIN_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SERVER_LISTING;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_COMPRESSION_BZLIB;
  pos += sizeof(FFSS_Field);

  msg[pos++] = 0; /* OS is NULL */
  msg[pos++] = 0; /* Domain is NULL */

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Server listing message to %s\n",Master);
  resp = SU_UDPSendToAddr(FM_SI_OUT_UDP,msg,pos,(char *)Master,FFSS_MASTER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FM_SendMessage_DomainListingAnswer Function */
/* Sends a DOMAIN ANSWER message to client     */
/*  Client : The sin of the client             */
/*  Domains : A SU_PList of FM_PDomain         */
bool FM_SendMessage_DomainListingAnswer(struct sockaddr_in Client,SU_PList Domains)
{
  char *msg;
  long int len,size,pos;
  int resp,nb;
  SU_PList Ptr;

  nb = SU_ListCount(Domains);
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DOMAINS_LISTING_ANSWER + nb*(FFSS_MAX_DOMAIN_LENGTH+1);
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_DOMAINS_LISTING_ANSWER;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = nb;
  pos += sizeof(FFSS_Field);

  Ptr = Domains;
  while(Ptr != NULL)
  {
    len = strlen(((FM_PDomain)Ptr->Data)->Name)+1;
    if(len > FFSS_MAX_DOMAIN_LENGTH)
      len = FFSS_MAX_DOMAIN_LENGTH;
    SU_strcpy(msg+pos,((FM_PDomain)Ptr->Data)->Name,len);
    pos += len;
    Ptr = Ptr->Next;
  }
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Domains Listing message to client\n");
  resp = SU_UDPSendToSin(FM_SI_OUT_UDP,msg,pos,Client);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FM_SendMessage_MasterSearchAnswer Function               */
/* Sends a MASTER SEARCH ANSWER message to client or server */
/*  Client : The sin of the client or the server            */
/*  Server : True if from server                            */
/*  Domain : The name of my domain                          */
bool FM_SendMessage_MasterSearchAnswer(struct sockaddr_in Client,bool Server,const char Domain[])
{
  char *msg;
  long int len,size,pos;
  int resp;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_MASTER_ANSWER + FFSS_MAX_DOMAIN_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SEARCH_MASTER_ANSWER;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_PROTOCOL_VERSION;
  pos += sizeof(FFSS_Field);
  len = strlen(Domain)+1;
  if(len > FFSS_MAX_DOMAIN_LENGTH)
    len = FFSS_MAX_DOMAIN_LENGTH;
  SU_strcpy(msg+pos,Domain,len);
  pos += len;

  *(FFSS_Field *)(msg) = pos;
  if(Server)
  {
    FFSS_PrintDebug(3,"Sending Master search answer message to server\n");
    resp = SU_UDPSendToAddr(FM_SI_OUT_UDP,msg,pos,inet_ntoa(Client.sin_addr),FFSS_SERVER_PORT_S);
  }
  else
  {
    FFSS_PrintDebug(3,"Sending Master search answer message to client\n");
    resp = SU_UDPSendToSin(FM_SI_OUT_UDP,msg,pos,Client);
  }
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FM_SendMessage_SearchAnswer Function                                          */
/* Sends a SEARCH ANSWER message to client                                       */
/*  Client : The sin of the client                                               */
/*  Buffer : The buffer containing the query, the nb of answers, and the answers */
/*  BufSize : The size of the buffer                                             */
/*  Compression : The type of compression to be applied to Buffer                */
bool FM_SendMessage_SearchAnswer(struct sockaddr_in Client,const char *Buffer,long int BufSize,int Compression)
{
  char *msg;
  long int size,pos;
  int resp;
  long int CompSize;

  CompSize = BufSize*1.05+6000;
  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_ANSWER + CompSize;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SEARCH_ANSWER;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Compression;
  pos += sizeof(FFSS_Field);
  switch(Compression)
  {
    case FFSS_COMPRESSION_NONE :
      memcpy(msg+pos,Buffer,BufSize);
      pos += BufSize;
      break;
    case FFSS_COMPRESSION_ZLIB :
      if(!FFSS_CompresseZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        FFSS_PrintDebug(1,"Error in Z compression routine : Buffer too small ?\n");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#ifdef HAVE_BZLIB
    case FFSS_COMPRESSION_BZLIB :
      if(!FFSS_CompresseBZlib((char *)Buffer,BufSize,msg+pos,&CompSize))
      {
        FFSS_PrintDebug(1,"Error in BZ compression routine : Buffer too small ?\n");
        free(msg);
        return false;
      }
      pos += CompSize;
      break;
#endif
    default :
      FFSS_PrintDebug(1,"Unknown compression type : %d\n",Compression);
      free(msg);
      return false;
  }

  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Search Answer message to client (%s:%d)\n",inet_ntoa(Client.sin_addr),ntohs(Client.sin_port));
  resp = SU_UDPSendToSin(FM_SI_OUT_UDP,msg,pos,Client);
  free(msg);
  return (resp != SOCKET_ERROR);
}

/* FM_SendMessage_SearchForward Function                 */
/* Sends a SEARCH message to a foreign master            */
/*  Master : The name of the master                      */
/*  Client : The sin of the client                       */
/*  Compression   : Compressions supported by the client */
/*  Keys   : A String of keywords                        */
bool FM_SendMessage_SearchForward(const char Master[],struct sockaddr_in Client,int Compression,const char Key[])
{
  char *msg;
  long int len,size,pos;
  int resp;

  if(Master == NULL)
    return true;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_SEARCH_FW + FFSS_IP_FIELD_SIZE + FFSS_MAX_KEYWORDS_LENGTH+1;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_SEARCH_FW;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = ntohs(Client.sin_port);
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = Compression;
  pos += sizeof(FFSS_Field);

  *(FFSS_Field *)(msg+pos) = FFSS_IP_TYPE;
  pos += sizeof(FFSS_Field);
  FFSS_PackIP(msg+pos,inet_ntoa(Client.sin_addr),FFSS_IP_TYPE);
  pos += FFSS_IP_FIELD_SIZE;

  len = strlen(Key)+1;
  if(len > FFSS_MAX_KEYWORDS_LENGTH)
    len = FFSS_MAX_KEYWORDS_LENGTH;
  SU_strcpy(msg+pos,Key,len);
  pos += len;
  *(FFSS_Field *)(msg) = pos;
  FFSS_PrintDebug(3,"Sending Search Forward message to %s - Reply to %s:%d\n",Master,inet_ntoa(Client.sin_addr),ntohs(Client.sin_port));
  resp = SU_UDPSendToAddr(FM_SI_OUT_UDP,msg,pos,(char *)Master,FFSS_MASTER_PORT_S);
  free(msg);
  return (resp != SOCKET_ERROR);
}
