#ifdef FFSS_DRIVER

/* Functions of this driver are NOT reentrant */

#include "ffss.h"
#include "utils.h"
#include "transfer.h"

SU_PList FC_Handles = NULL; /* FC_PHandle */
long int FC_CurrentHandle = 1;

FC_PHandle FC_CreateHandle(void)
{
  FC_PHandle Hdl;

  Hdl = (FC_PHandle) malloc(sizeof(FC_THandle));
  if(Hdl == NULL)
    return NULL;
  memset(Hdl,0,sizeof(FC_THandle));
  Hdl->Handle = FC_CurrentHandle++;
  Hdl->BufSize = FFSS_TCP_BUFFER_SIZE;
  Hdl->Buf = (char *) malloc(Hdl->BufSize);
  if(Hdl->Buf == NULL)
  {
    free(Hdl);
    return NULL;
  }
  FC_Handles = SU_AddElementHead(FC_Handles,Hdl);
  return Hdl;
}

void FC_FreeHandle(FC_PHandle Hdl)
{
  if(Hdl->Buf != NULL)
    free(Hdl->Buf);
  free(Hdl);
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
        FFSS_PrintDebug(1,"One or many fields empty, or out of buffer... DoS attack ?\n");
        ret_val = false;
        break;
      }
      if(FFSS_CB.CCB.OnError != NULL)
        ret_val = FFSS_CB.CCB.OnError(Server,val,str);
      break;
    case FFSS_MESSAGE_DIRECTORY_LISTING_ANSWER :
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if(str == NULL)
      {
        FFSS_PrintDebug(1,"One or many fields empty, or out of buffer... DoS attack ?\n");
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
            FFSS_PrintDebug(1,"Corrupted Z compressed buffer ... DoS attack ?\n");
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
            FFSS_PrintDebug(1,"Corrupted BZ compressed buffer ... DoS attack ?\n");
            ret_val = false;
            break;
          }
          free_it = true;
          u_pos = 0;
          break;
#endif
        default:
          FFSS_PrintDebug(1,"Unknown compression type : %ld ... DoS attack ?\n",val2);
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
          FFSS_PrintDebug(1,"One or many fields empty, or out of buffer... DoS attack ?\n");
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
      if(FFSS_CB.CCB.OnDirectoryListingAnswer != NULL)
        ret_val = FFSS_CB.CCB.OnDirectoryListingAnswer(Server,str,val,Ptr);
      SU_FreeListElem(Ptr);
      break;
    case FFSS_MESSAGE_INIT_XFER :
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      str = FFSS_UnpackString(Buf,Buf+pos,Len,&pos);
      if(str == NULL)
      {
        FFSS_PrintDebug(1,"One or many fields empty, or out of buffer... DoS attack ?\n");
        ret_val = false;
        break;
      }
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
        FFSS_PrintDebug(1,"One or many fields empty, or out of buffer... DoS attack ?\n");
        ret_val = false;
        break;
      }
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
        FFSS_PrintDebug(1,"One or many fields empty, or out of buffer... DoS attack ?\n");
        ret_val = false;
        break;
      }
      if(FFSS_CB.CCB.OnStrmOpenAnswer != NULL)
        FFSS_CB.CCB.OnStrmOpenAnswer(Server,str,val,val2,val3);
      break;
    case FFSS_MESSAGE_STREAMING_READ_ANSWER :
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      Length = Len-FFSS_MESSAGESIZE_STREAMING_READ_ANSWER*sizeof(FFSS_Field);
      if((val == 0) || (Length < 0))
      {
        FFSS_PrintDebug(1,"One or many fields empty, or out of buffer... DoS attack ?\n");
        ret_val = false;
        break;
      }
      if(FFSS_CB.CCB.OnStrmReadAnswer != NULL)
        FFSS_CB.CCB.OnStrmReadAnswer(Server,val,Buf+pos,Length);
      break;
    case FFSS_MESSAGE_STREAMING_WRITE_ANSWER :
      val = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      val2 = FFSS_UnpackField(Buf,Buf+pos,Len,&pos);
      if((val == 0) || (val2 == 0))
      {
        FFSS_PrintDebug(1,"One or many fields empty, or out of buffer... DoS attack ?\n");
        ret_val = false;
        break;
      }
      if(FFSS_CB.CCB.OnStrmWriteAnswer != NULL)
        FFSS_CB.CCB.OnStrmWriteAnswer(Server,val,val2);
      break;
    default:
      FFSS_PrintDebug(1,"Unknown Type of message (%d)... DoS attack ?\n",Type);
      ret_val = false;
  }
  if(free_it)
    free(u_Buf);
  return ret_val;
}

bool FC_WaitDataTCP(SU_PClientSocket Client,FC_PHandle Hdl)
{
  int len,res;
  FFSS_Field Size;
  bool analyse;
  fd_set rfds;
  struct timeval tv;
  int retval;

  len = 0;
  while(1)
  {
    if(Client->User != 0)	/* Idle time out value */
    {
      FFSS_PrintDebug(5,"Client has defined a idle time out value of %d sec for this connection\n",(int)Client->User);
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
#ifdef __unix__
        close(Client->sock);
#else
        closesocket(Client->sock);
#endif
        free(Client);
        if(FFSS_CB.CCB.OnEndTCPThread != NULL)
          FFSS_CB.CCB.OnEndTCPThread(Client);
        return false;
      }
    }

    if(len >= Hdl->BufSize)
    {
      FFSS_PrintDebug(1,"WARNING : Client's buffer too short for this message !!\n");
    }
    res = recv(Client->sock,Hdl->Buf+len,Hdl->BufSize-len,SU_MSG_NOSIGNAL);
    if(res == SOCKET_ERROR)
    {
      FFSS_PrintDebug(1,"Error on TCP port of the client (SOCKET_ERROR : %d)\n",errno);
#ifdef __unix__
      close(Client->sock);
#else
      closesocket(Client->sock);
#endif
      free(Client);
      if(FFSS_CB.CCB.OnEndTCPThread != NULL)
        FFSS_CB.CCB.OnEndTCPThread(Client);
      return false;
    }
    else if(res == 0)
    {
#ifdef __unix__
      close(Client->sock);
#else
      closesocket(Client->sock);
#endif
      free(Client);
      if(FFSS_CB.CCB.OnEndTCPThread != NULL)
        FFSS_CB.CCB.OnEndTCPThread(Client);
      return false;
    }
    len += res;
    FFSS_PrintDebug(5,"Data found on TCP port from %s (%s) ... analysing\n",inet_ntoa(Client->SAddr.sin_addr),SU_NameOfPort(inet_ntoa(Client->SAddr.sin_addr)));
    analyse = true;
    while (analyse)
    {
      if(len < 5)
      {
	FFSS_PrintDebug(1,"Length of the message is less than 5 (%d)... DoS attack ?\n",len);
#ifdef __unix__
        close(Client->sock);
#else
        closesocket(Client->sock);
#endif
        free(Client);
        if(FFSS_CB.CCB.OnEndTCPThread != NULL)
          FFSS_CB.CCB.OnEndTCPThread(Client);
        return false;
      }
      Size = *(FFSS_Field *)Hdl->Buf;
      if(Size > len)
      {
        FFSS_PrintDebug(5,"Warning, Size of the message is greater than received data (%d - %d)... Message splitted ?\n",Size,len);
        break;
        /* Keeps waiting for data */
      }
      else
      {
        if(!FC_AnalyseTCP(Client,Hdl->Buf,Size))
        {
#ifdef __unix__
          close(Client->sock);
#else
          closesocket(Client->sock);
#endif
          free(Client);
          if(FFSS_CB.CCB.OnEndTCPThread != NULL)
            FFSS_CB.CCB.OnEndTCPThread(Client);
          return false;
	}
	if(len > Size)
	{
	  FFSS_PrintDebug(5,"Warning, Size of the message is less than received data (%d - %d)... multiple messages ?\n",Size,len);
	  memmove(Hdl->Buf,Hdl->Buf+Size,len-Size);
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

/* FFSS Client : UnInit */
/* Initialisation of the FFSS Client - Must be called before any other FFSS function */
/* Returns true on success, false otherwise */
bool FC_Init(void)
{
  FC_Handles = NULL;
  FFSS_PrintDebug(1,"FFSS driver running\n");
  return true;
}

/* FFSS Client : UnInit */
/* Uninitialisation of the FFSS Client - Must be called at the end of the main */
/* Returns true on success, false otherwise */
bool FC_UnInit(void)
{
  SU_PList Ptr;

  Ptr = FC_Handles;
  while(Ptr != NULL)
  {
    FC_FreeHandle((FC_PHandle)Ptr->Data);
  }
  SU_FreeList(FC_Handles);
  FFSS_PrintDebug(1,"FFSS driver shut down\n");
  return true;
}

#endif
