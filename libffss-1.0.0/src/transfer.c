#include "ffss.h"
#include "utils.h"
#include "common.h"

char *FFSS_TransferErrorTable[]={"","Transfer buffer allocation failed","Timed out","Send error","EOF from remote host","Error reading file","Error accepting connection","Error opening file","Recv error","Local write error. Disk full ?","Received file bigger than specified","Checksum check failed","Transfer canceled"};
long int FFSS_TransferBufferSize = FFSS_TRANSFER_BUFFER_SIZE;
long int FFSS_TransferReadBufferSize = FFSS_TRANSFER_READ_BUFFER_SIZE;

void FFSS_FreeTransfer(FFSS_PTransfer T)
{
  if(T->FileName != NULL)
    free(T->FileName);
  if(T->LocalPath != NULL)
    free(T->LocalPath);
  if(T->fp != NULL)
    fclose(T->fp);
  if(!T->XI.UseConnSock)
    SU_CLOSE_SOCKET(T->sock);
  free(T);
}

SU_THREAD_ROUTINE(FFSS_UploadFileFunc,Info)
{
  FFSS_PTransfer FT = (FFSS_PTransfer) Info;
  long int total=0,rpos=0,rlen;
  FFSS_Field fsize,Checksum;
  fd_set rfds;
  struct timeval tv;
  int retval,res,len;
  char *RBuf;
  time_t t1,t2;

  SU_ThreadBlockSigs();
  fseek(FT->fp,0,SEEK_END);
  fsize = ftell(FT->fp);
  rewind(FT->fp);
  FT->FileSize = fsize;
  Checksum = FFSS_ComputeChecksum(0,NULL,0);
  if(FT->StartingPos != 0)
  {
    fseek(FT->fp,FT->StartingPos,SEEK_SET);
    fsize -= FT->StartingPos;
    FT->XFerPos = FT->StartingPos;
  }

  if(FFSS_TransferReadBufferSize == 0)
    FFSS_TransferReadBufferSize = FFSS_TRANSFER_READ_BUFFER_SIZE;
  RBuf = (char *) malloc(FFSS_TransferReadBufferSize);
  if(RBuf == NULL)
  {
    FFSS_PrintDebug(1,"Cannot allocate buffer in Upload function\n");
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_MALLOC,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_MALLOC],false);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_MALLOC,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_MALLOC],false);
    }
    FFSS_FreeTransfer(FT);
    SU_END_THREAD(NULL);
  }
  t1 = time(NULL);
  /* Send File Size (or remaining bytes to xfer if resume) */
  FD_ZERO(&rfds);
  FD_SET(FT->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TRANSFER;
  tv.tv_usec = 0;
  retval = select(FT->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    FFSS_PrintDebug(1,"Transfer timed out\n");
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],false);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],false);
    }
    FFSS_FreeTransfer(FT);
    free(RBuf);
    SU_END_THREAD(NULL);
  }
  res = send(FT->sock,(char *)&fsize,sizeof(fsize),SU_MSG_NOSIGNAL);
  if(res == SOCKET_ERROR)
  {
    FFSS_PrintDebug(1,"Error while uploading file (size) : %d %s\n",errno,strerror(errno));
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_SEND,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_SEND],false);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_SEND,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_SEND],false);
    }
    FFSS_FreeTransfer(FT);
    free(RBuf);
    SU_END_THREAD(NULL);
  }
  else if(res == 0)
  {
    FFSS_PrintDebug(1,"EOF from remote host while uploading file\n");
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_EOF,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_EOF],false);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_EOF,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_EOF],false);
    }
    FFSS_FreeTransfer(FT);
    free(RBuf);
    SU_END_THREAD(NULL);
  }

  while(total < fsize)
  {
    if(FT->Cancel)
    {
      if(FT->ThreadType == FFSS_THREAD_SERVER)
      {
        if(FFSS_CB.SCB.OnTransferFailed != NULL)
          FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_CANCELED,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_CANCELED],false);
      }
      else
      {
        if(FFSS_CB.CCB.OnTransferFailed != NULL)
          FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_CANCELED,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_CANCELED],false);
      }
      FFSS_FreeTransfer(FT);
      SU_END_THREAD(NULL);
    }
    if((total+FFSS_TransferReadBufferSize) <= fsize)
      rlen = FFSS_TransferReadBufferSize;
    else
      rlen = fsize - total;
    if(fread(RBuf,1,rlen,FT->fp) != rlen)
    {
      FFSS_PrintDebug(1,"Error reading file while uploading : %d\n",errno);
      if(FT->ThreadType == FFSS_THREAD_SERVER)
      {
        if(FFSS_CB.SCB.OnTransferFailed != NULL)
          FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_READ_FILE,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_READ_FILE],false);
      }
      else
      {
        if(FFSS_CB.CCB.OnTransferFailed != NULL)
          FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_READ_FILE,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_READ_FILE],false);
      }
      FFSS_FreeTransfer(FT);
      free(RBuf);
      SU_END_THREAD(NULL);
    }
    Checksum = FFSS_ComputeChecksum(Checksum,RBuf,rlen);
    rpos = 0;
    while(rpos < rlen)
    {
      if((rpos+FFSS_TransferBufferSize) <= rlen)
        len = FFSS_TransferBufferSize;
      else
        len = rlen - rpos;
      res = 0;
      while(res != len)
      {
        FD_ZERO(&rfds);
        FD_SET(FT->sock,&rfds);
        tv.tv_sec = FFSS_TIMEOUT_TRANSFER;
        tv.tv_usec = 0;
        retval = select(FT->sock+1,NULL,&rfds,NULL,&tv);
        if(!retval)
        {
          FFSS_PrintDebug(1,"Transfer timed out while uploading file\n");
          if(FT->ThreadType == FFSS_THREAD_SERVER)
          {
            if(FFSS_CB.SCB.OnTransferFailed != NULL)
              FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],false);
          }
          else
          {
            if(FFSS_CB.CCB.OnTransferFailed != NULL)
              FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],false);
          }
          FFSS_FreeTransfer(FT);
          free(RBuf);
          SU_END_THREAD(NULL);
        }
        res += send(FT->sock,RBuf+rpos+res,len-res,SU_MSG_NOSIGNAL);
        if(res <= 0)
        {
          FFSS_PrintDebug(1,"Error while uploading file (buf) : %d %s\n",errno,strerror(errno));
          if(FT->ThreadType == FFSS_THREAD_SERVER)
          {
            if(FFSS_CB.SCB.OnTransferFailed != NULL)
              FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_SEND,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_SEND],false);
          }
          else
          {
            if(FFSS_CB.CCB.OnTransferFailed != NULL)
              FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_SEND,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_SEND],false);
          }
          FFSS_FreeTransfer(FT);
          free(RBuf);
          SU_END_THREAD(NULL);
        }
      }
      FT->XFerPos += len;
      if(FT->ThreadType == FFSS_THREAD_SERVER)
      {
        if(FFSS_CB.SCB.OnTransferActive != NULL)
          FFSS_CB.SCB.OnTransferActive(FT,len,false);
      }
      else
      {
        if(FFSS_CB.CCB.OnTransferActive != NULL)
          FFSS_CB.CCB.OnTransferActive(FT,len,false);
      }
      rpos += len;
    }
    total += rlen;
  }

  /* Send Checksum */
  FD_ZERO(&rfds);
  FD_SET(FT->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TRANSFER;
  tv.tv_usec = 0;
  retval = select(FT->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    FFSS_PrintDebug(1,"Transfer timed out\n");
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],false);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],false);
    }
    FFSS_FreeTransfer(FT);
    free(RBuf);
    SU_END_THREAD(NULL);
  }
  res = send(FT->sock,(char *)&Checksum,sizeof(Checksum),SU_MSG_NOSIGNAL);
  if(res == SOCKET_ERROR)
  {
    FFSS_PrintDebug(1,"Error while uploading file (chksum) : %d %s\n",errno,strerror(errno));
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_SEND,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_SEND],false);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_SEND,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_SEND],false);
    }
    FFSS_FreeTransfer(FT);
    free(RBuf);
    SU_END_THREAD(NULL);
  }
  else if(res == 0)
  {
    FFSS_PrintDebug(1,"EOF from client while uploading file\n");
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_EOF,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_EOF],false);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_EOF,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_EOF],false);
    }
    FFSS_FreeTransfer(FT);
    free(RBuf);
    SU_END_THREAD(NULL);
  }

  if(FT->ThreadType == FFSS_THREAD_SERVER)
  {
    if(FFSS_CB.SCB.OnTransferSuccess != NULL)
      FFSS_CB.SCB.OnTransferSuccess(FT,false);
  }
  else
  {
    if(FFSS_CB.CCB.OnTransferSuccess != NULL)
      FFSS_CB.CCB.OnTransferSuccess(FT,false);
  }
  t2 = time(NULL);
  if(t1 == t2)
    t2 = t1+1;
  FFSS_PrintDebug(1,"Successfully uploaded the file %s in %d sec (%.2f ko/s) (%ld)\n",FT->LocalPath,((int)(t2-t1)),fsize/1024.0/(t2-t1),SU_THREAD_SELF);
  FFSS_FreeTransfer(FT);
  free(RBuf);
  SU_END_THREAD(NULL);
}

SU_THREAD_ROUTINE(FFSS_DownloadFileFunc,Info)
{
  FFSS_PTransfer FT = (FFSS_PTransfer) Info;
  struct sockaddr sad;
  int len;
  int client;
  char Buf[FFSS_TRANSFER_BUFFER_SIZE*2];
  int res;
  long int total=0;
  fd_set rfds;
  struct timeval tv;
  int retval;
  FILE *fp;
  FFSS_Field Size;
  FFSS_Field ChkSum,Checksum;
  bool error = false;
  time_t t1,t2;

  SU_ThreadBlockSigs();
  context;
  Checksum = FFSS_ComputeChecksum(0,NULL,0);

  FD_ZERO(&rfds);
  FD_SET(FT->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_ACCEPT;
  tv.tv_usec = 0;
  retval = select(FT->sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
  {
    FFSS_PrintDebug(1,"Error accepting connection (timed out)\n");
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],true);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],true);
    }
    FFSS_FreeTransfer(FT);
    SU_END_THREAD(NULL);
  }
  len = sizeof(sad);
  client = accept(FT->sock,&sad,&len);
  SU_CLOSE_SOCKET(FT->sock);
  FT->sock = client;
  FFSS_PrintDebug(1,"Connection accepted from %s\n",inet_ntoa(((struct sockaddr_in *)&sad)->sin_addr));
  if(FT->sock == SOCKET_ERROR)
  {
    FFSS_PrintDebug(1,"Error accepting connections\n");
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_ACCEPT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_ACCEPT],true);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_ACCEPT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_ACCEPT],true);
    }
    FFSS_FreeTransfer(FT);
    SU_END_THREAD(NULL);
  }
  t1 = time(NULL);
  if(FT->LocalPath != NULL)
  {
    if(FT->StartingPos != 0)
      fp = fopen(FT->LocalPath,"ab");
    else
      fp = fopen(FT->LocalPath,"wb");
  }
  else
    fp = stdout;
  FT->XFerPos = 0; /* Do not set this to real pos, because if resume is used, FT->FileSize will be remaning bytes only */
  if(fp == NULL)
  {
    FFSS_PrintDebug(1,"Can't open local file for writting (%s)\n",FT->LocalPath);
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_OPENING,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_OPENING],true);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_OPENING,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_OPENING],true);
    }
    FFSS_FreeTransfer(FT);
    SU_END_THREAD(NULL);
  }
  context;
  FD_ZERO(&rfds);
  FD_SET(FT->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TRANSFER;
  tv.tv_usec = 0;
  retval = select(FT->sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
  {
    FFSS_PrintDebug(1,"Transfer timed out while downloading file\n");
    /* Remove or not file from disk (ask user) */
    if(FT->LocalPath != NULL)
      fclose(fp);
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],true);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],true);
    }
    error = true;
  }
  if(!error)
  {
    res = recv(FT->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
    if(res == SOCKET_ERROR)
    {
      FFSS_PrintDebug(1,"Error while downloading file (size) : %d %s\n",errno,strerror(errno));
      /* Remove or not file from disk (ask user) */
      if(FT->LocalPath != NULL)
        fclose(fp);
      if(FT->ThreadType == FFSS_THREAD_SERVER)
      {
        if(FFSS_CB.SCB.OnTransferFailed != NULL)
          FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_RECV,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_RECV],true);
      }
      else
      {
        if(FFSS_CB.CCB.OnTransferFailed != NULL)
          FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_RECV,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_RECV],true);
      }
      error = true;
    }
    else if(res == 0)
    {
      FFSS_PrintDebug(1,"EOF from server while downloading file\n");
      /* Remove or not file from disk (ask user) */
      if(FT->LocalPath != NULL)
        fclose(fp);
      if(FT->ThreadType == FFSS_THREAD_SERVER)
      {
        if(FFSS_CB.SCB.OnTransferFailed != NULL)
          FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_EOF,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_EOF],true);
      }
      else
      {
        if(FFSS_CB.CCB.OnTransferFailed != NULL)
          FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_EOF,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_EOF],true);
      }
      error = true;
    }
    FT->FileSize = Size;
  }

  while(!error)
  {
    context;
    if(FT->Cancel)
    {
      if(FT->ThreadType == FFSS_THREAD_SERVER)
      {
        if(FFSS_CB.SCB.OnTransferFailed != NULL)
          FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_CANCELED,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_CANCELED],true);
      }
      else
      {
        if(FFSS_CB.CCB.OnTransferFailed != NULL)
          FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_CANCELED,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_CANCELED],true);
      }
      FFSS_FreeTransfer(FT);
      SU_END_THREAD(NULL);
    }
    FD_ZERO(&rfds);
    FD_SET(FT->sock,&rfds);
    tv.tv_sec = FFSS_TIMEOUT_TRANSFER;
    tv.tv_usec = 0;
    retval = select(FT->sock+1,&rfds,NULL,NULL,&tv);
    if(!retval)
    {
      FFSS_PrintDebug(1,"Transfer timed out while downloading file\n");
      /* Remove or not file from disk (ask user) */
      if(FT->LocalPath != NULL)
        fclose(fp);
      if(FT->ThreadType == FFSS_THREAD_SERVER)
      {
        if(FFSS_CB.SCB.OnTransferFailed != NULL)
          FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],true);
      }
      else
      {
        if(FFSS_CB.CCB.OnTransferFailed != NULL)
          FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],true);
      }
      error = true;
    }
    if(!error)
    {
      context;
      len = sizeof(Buf);
      if(len > (Size-total))
        len = Size-total;
      if(len == 0) /* End of file, getting checksum */
        res = recv(FT->sock,&ChkSum,sizeof(Checksum),SU_MSG_NOSIGNAL);
      else
        res = recv(FT->sock,Buf,len,SU_MSG_NOSIGNAL);
      if(res == SOCKET_ERROR)
      {
        FFSS_PrintDebug(1,"Error while downloading file (buf) : %d %s\n",errno,strerror(errno));
        /* Remove or not file from disk (ask user) */
        if(FT->LocalPath != NULL)
          fclose(fp);
        if(FT->ThreadType == FFSS_THREAD_SERVER)
        {
          if(FFSS_CB.SCB.OnTransferFailed != NULL)
            FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_RECV,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_RECV],true);
        }
        else
        {
          if(FFSS_CB.CCB.OnTransferFailed != NULL)
            FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_RECV,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_RECV],true);
        }
        error = true;
      }
      else if(res == 0)
      {
        FFSS_PrintDebug(1,"EOF from server while downloading file\n");
        /* Remove or not file from disk (ask user) */
        if(FT->LocalPath != NULL)
          fclose(fp);
        if(FT->ThreadType == FFSS_THREAD_SERVER)
        {
          if(FFSS_CB.SCB.OnTransferFailed != NULL)
            FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_EOF,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_EOF],true);
        }
        else
        {
          if(FFSS_CB.CCB.OnTransferFailed != NULL)
            FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_EOF,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_EOF],true);
        }
        error = true;
      }
      else
      {
        context;
        if(FT->ThreadType == FFSS_THREAD_SERVER)
        {
          if(FFSS_CB.SCB.OnTransferActive != NULL)
            FFSS_CB.SCB.OnTransferActive(FT,res,true);
        }
        else
        {
          if(FFSS_CB.CCB.OnTransferActive != NULL)
            FFSS_CB.CCB.OnTransferActive(FT,res,true);
        }
      }
      if(!error)
      {
        context;
        if(len != 0)
        {
          total += res;
          if(fwrite(Buf,1,res,fp) != res)
          {
            FFSS_PrintDebug(1,"Disk full ?\n");
            /* Remove or not file from disk (ask user) */
            if(FT->LocalPath != NULL)
              fclose(fp);
            if(FT->ThreadType == FFSS_THREAD_SERVER)
            {
              if(FFSS_CB.SCB.OnTransferFailed != NULL)
                FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_WRITE_FILE,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_WRITE_FILE],true);
            }
            else
            {
              if(FFSS_CB.CCB.OnTransferFailed != NULL)
                FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_WRITE_FILE,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_WRITE_FILE],true);
            }
            error = true;
          }
          Checksum = FFSS_ComputeChecksum(Checksum,Buf,res);
          FT->XFerPos += res;
        }
        if(!error)
        {
          if(len == 0)
          {
            context;
#ifdef DISABLE_CHECKSUM
            if(true)
#else
            if((ChkSum == Checksum) || (ChkSum == 1)) /* A ChkSum of 1 means NO checksum on the other side */
#endif
            {
              FFSS_PrintDebug(1,"File successfully downloaded\n");
              if(FT->LocalPath != NULL)
                fclose(fp);
              if(FT->ThreadType == FFSS_THREAD_SERVER)
              {
                if(FFSS_CB.SCB.OnTransferSuccess != NULL)
                  FFSS_CB.SCB.OnTransferSuccess(FT,true);
              }
              else
              {
                if(FFSS_CB.CCB.OnTransferSuccess != NULL)
                  FFSS_CB.CCB.OnTransferSuccess(FT,true);
              }
            }
            else
            {
              FFSS_PrintDebug(1,"Checksum check failed\n");
              /* Remove or not file from disk (ask user) */
              if(FT->LocalPath != NULL)
                fclose(fp);
              if(FT->ThreadType == FFSS_THREAD_SERVER)
              {
                if(FFSS_CB.SCB.OnTransferFailed != NULL)
                  FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_CHECKSUM,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_CHECKSUM],true);
              }
              else
              {
                if(FFSS_CB.CCB.OnTransferFailed != NULL)
                  FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_CHECKSUM,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_CHECKSUM],true);
              }
              error = true;
            }
            break;
          }
          else if(total > Size)
          {
            context;
            FFSS_PrintDebug(1,"Received data bigger than file size (%ld - %ld)\n",total,Size);
            /* Remove or not file from disk (ask user) */
            if(FT->LocalPath != NULL)
              fclose(fp);
            if(FT->ThreadType == FFSS_THREAD_SERVER)
            {
              if(FFSS_CB.SCB.OnTransferFailed != NULL)
                FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_FILE_BIGGER,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_FILE_BIGGER],true);
            }
            else
            {
              if(FFSS_CB.CCB.OnTransferFailed != NULL)
                FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_FILE_BIGGER,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_FILE_BIGGER],true);
            }
            error = true;
          }
        }
      }
    }
  }
  FFSS_FreeTransfer(FT);
  if(!error)
  {
    t2 = time(NULL);
    if(t1 == t2)
      t2 = t1+1;
    FFSS_PrintDebug(1,"Successfully downloaded the file in %d sec (%.2f ko/s)\n",((int)(t2-t1)),Size/1024.0/(t2-t1));
  }
  SU_END_THREAD(NULL);
}


bool FFSS_UploadFile(SU_PClientSocket Client,const char FilePath[],long int StartingPos,int Port,void *User,bool UseConnSock,FFSS_PTransfer *FT_out)
{
  FILE *fp;
  SU_THREAD_HANDLE Thread;
  struct sockaddr_in SAddr;
  int sock=0;
  FFSS_PTransfer FT;

  context;
  *FT_out = NULL;
  fp = fopen(FilePath,"rb");
  if(fp == NULL)
  {
    FFSS_PrintDebug(1,"Couldn't open file for upload : %s (%d)\n",FilePath,errno);
    return FS_SendMessage_Error(Client->sock,FFSS_ERROR_FILE_NOT_FOUND,FFSS_ErrorTable[FFSS_ERROR_FILE_NOT_FOUND]);
  }
  if(!UseConnSock)
  {
    sock = socket(AF_INET,SOCK_STREAM,getprotobyname("tcp")->p_proto);
    if(sock == SOCKET_ERROR)
    {
      fclose(fp);
      return FS_SendMessage_Error(Client->sock,FFSS_ERROR_INTERNAL_ERROR,FFSS_ErrorTable[FFSS_ERROR_INTERNAL_ERROR]);
    }
    SAddr.sin_family = AF_INET;
    SAddr.sin_port = htons(Port);
    SAddr.sin_addr.s_addr = Client->SAddr.sin_addr.s_addr;
    if(connect(sock,(struct sockaddr *)(&SAddr),sizeof(SAddr)) == SOCKET_ERROR)
    {
      SU_CLOSE_SOCKET(sock);
      fclose(fp);
      return FS_SendMessage_Error(Client->sock,FFSS_ERROR_CANNOT_CONNECT,FFSS_ErrorTable[FFSS_ERROR_CANNOT_CONNECT]);
    }
  }
  FT = (FFSS_PTransfer) malloc(sizeof(FFSS_TTransfer));
  memset(FT,0,sizeof(FFSS_TTransfer));
  FT->fp = fp;
  FT->StartingPos = StartingPos;
  FT->XI.UseConnSock = UseConnSock;
  if(UseConnSock)
    FT->sock = Client->sock;
  else
    FT->sock = sock;
  FT->LocalPath = strdup(FilePath);
  FT->Client = Client;
  FT->User = User;
  FT->ThreadType = FFSS_THREAD_SERVER;
  if(!UseConnSock)
  {
    if(!SU_CreateThread(&Thread,FFSS_UploadFileFunc,(void *)FT,true))
    {
      FFSS_FreeTransfer(FT);
      return FS_SendMessage_Error(Client->sock,FFSS_ERROR_TOO_MANY_TRANSFERS,FFSS_ErrorTable[FFSS_ERROR_TOO_MANY_TRANSFERS]);
    }
  }
  if(FT_out != NULL)
    *FT_out = FT;
  return true;
}

bool FFSS_DownloadFile(SU_PClientSocket Server,const char RemotePath[],const char LocalPath[],long int StartingPos,void *User,bool UseConnSock,FFSS_PTransfer *FT_out)
{
  int sock;
  SU_THREAD_HANDLE Thread;
  FFSS_PTransfer FT;

  context;
  sock = FC_SendMessage_Download(Server,RemotePath,StartingPos,UseConnSock);
  if(sock == SOCKET_ERROR)
  {
    FFSS_PrintDebug(1,"Error sending DOWNLOAD request : %d\n",errno);
    return false;
  }
  FT = (FFSS_PTransfer) malloc(sizeof(FFSS_TTransfer));
  memset(FT,0,sizeof(FFSS_TTransfer));
  FT->FileName = strdup(RemotePath);
  if(LocalPath != NULL)
    FT->LocalPath = strdup(LocalPath);
  FT->StartingPos = StartingPos;
  FT->XI.UseConnSock = UseConnSock;
  if(UseConnSock)
    FT->sock = Server->sock;
  else
    FT->sock = sock;
  FT->Client = Server;
  FT->User = User;
  FT->ThreadType = FFSS_THREAD_CLIENT;
  if(!UseConnSock)
  {
    if(!SU_CreateThread(&Thread,FFSS_DownloadFileFunc,(void *)FT,true))
    {
      FFSS_FreeTransfer(FT);
      return false;
    }
  }
  if(FT_out != NULL)
    *FT_out = FT;
  return true;
}

bool FFSS_SendData(FFSS_PTransfer FT,FFSS_Field Tag,char *Buf,int len)
{
  char *msg;
  int size,pos;
  int res;
  fd_set rfds;
  struct timeval tv;
  int retval;

  size = sizeof(FFSS_Field)*FFSS_MESSAGESIZE_DATA + len;
  msg = (char *) malloc(size);
  pos = sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = FFSS_MESSAGE_DATA;
  pos += sizeof(FFSS_Field);
  *(FFSS_Field *)(msg+pos) = Tag;
  pos += sizeof(FFSS_Field);
  memcpy(msg+pos,Buf,len);
  pos += len;

  *(FFSS_Field *)(msg) = pos;

  FD_ZERO(&rfds);
  FD_SET(FT->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TRANSFER;
  tv.tv_usec = 0;
  retval = select(FT->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    FFSS_PrintDebug(1,"Transfer timed out\n");
    if(FFSS_CB.SCB.OnTransferFailed != NULL)
      FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_TIMEOUT,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_TIMEOUT],false);
    FFSS_FreeTransfer(FT);
    free(msg);
    return false;
  }
  res = 0;
  while(res != pos)
  {
    res += send(FT->sock,msg+res,pos-res,SU_MSG_NOSIGNAL);
    if(res == SOCKET_ERROR)
    {
      FFSS_PrintDebug(1,"Error while uploading file (data) : %d %s\n",errno,strerror(errno));
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_SEND,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_SEND],false);
      FFSS_FreeTransfer(FT);
      free(msg);
      return false;
    }
    else if(res == 0)
    {
      FFSS_PrintDebug(1,"EOF from remote host while uploading file\n");
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_EOF,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_EOF],false);
      FFSS_FreeTransfer(FT);
      free(msg);
      return false;
    }
  }
  free(msg);
  return true;
}

void FFSS_OnDataDownload(FFSS_PTransfer FT,const char Buf[],int Len)
{
  FFSS_Field Checksum=0;

  if(FT->XI.fsize == 0) /* Extract FileSize */
  {
    FT->XI.fsize = *(FFSS_Field *)(Buf);
    Len -= sizeof(FFSS_Field);
  }
  FT->XI.total += Len;
  if(FT->XI.total > FT->XI.fsize) /* Extract Checksum */
  {
    Len -= sizeof(FFSS_Field);
    Checksum = *(FFSS_Field *)(Buf+Len);
  }
  if(Len != 0)
  {
    if(FT->fp != NULL)
      fwrite(Buf,1,Len,FT->fp);
    FT->XI.Checksum = FFSS_ComputeChecksum(FT->XI.Checksum,Buf,Len);
  }
  if(FT->XI.total == (FT->XI.fsize+sizeof(FFSS_Field))) /* Compares checksum */
  {
#ifdef DISABLE_CHECKSUM
    if(true)
#else
    if((Checksum == FT->XI.Checksum) || (Checksum == 1)) /* A Checksum of 1 means NO checksum on the other side */
#endif
    {
      if(FT->ThreadType == FFSS_THREAD_SERVER)
      {
        if(FFSS_CB.SCB.OnTransferSuccess != NULL)
          FFSS_CB.SCB.OnTransferSuccess(FT,FT->XI.Download);
      }
      else
      {
        if(FFSS_CB.CCB.OnTransferSuccess != NULL)
          FFSS_CB.CCB.OnTransferSuccess(FT,FT->XI.Download);
      }
    }
    else
    {
      if(FT->ThreadType == FFSS_THREAD_SERVER)
      {
        if(FFSS_CB.SCB.OnTransferFailed != NULL)
          FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_CHECKSUM,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_CHECKSUM],FT->XI.Download);
      }
      else
      {
        if(FFSS_CB.CCB.OnTransferFailed != NULL)
          FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_CHECKSUM,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_CHECKSUM],FT->XI.Download);
      }
    }
    FFSS_FreeTransfer(FT);
  }
}

void FFSS_InitXFerDownload(FFSS_PTransfer FT,FFSS_Field XFerTag)
{
  FT->XI.Download = true;
  FT->XI.XFerTag = XFerTag;
  FT->XI.total = 0;
  FT->XI.fsize = 0;
  FT->XI.Checksum = FFSS_ComputeChecksum(0,NULL,0);
  if(FT->StartingPos != 0)
    FT->fp = fopen(FT->LocalPath,"ab");
  else
    FT->fp = fopen(FT->LocalPath,"wb");
  if(FT->fp == NULL)
  {
    FFSS_PrintDebug(1,"Can't open local file for writting (%s)\n",FT->LocalPath);
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_OPENING,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_OPENING],FT->XI.Download);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferFailed != NULL)
        FFSS_CB.CCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_OPENING,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_OPENING],FT->XI.Download);
    }
    FFSS_FreeTransfer(FT);
    FC_SendMessage_CancelXFer(FT->Client,XFerTag);
  }
}
