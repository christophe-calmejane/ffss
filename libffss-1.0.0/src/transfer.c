#include "ffss.h"
#include "utils.h"
#include "common.h"
#include "transfer.h"

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
  unsigned long int rpos=0,rlen;
  FFSS_LongField fsize,total=0;
  FFSS_Field Checksum;
  fd_set rfds;
  struct timeval tv;
  int retval,res,len;
  char *RBuf;
  time_t t1,t2;
  SU_TICKS st,et;
  unsigned long int tim;
  unsigned long int bytes;
  unsigned long int prev_thrpt,sleep_val,sleep_val2;
  unsigned long IP = INADDR_GET_IP(FT->Client->SAddr.sin_addr);

  SU_ThreadBlockSigs();
  fseek(FT->fp,0,SEEK_END);
  fsize = ftell(FT->fp);
  rewind(FT->fp);
  if(FT->EndingPos != 0)
  {
    if(FT->EndingPos < fsize)
      fsize = FT->EndingPos + 1;
    else
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Requested EndingSize is less than actual file size");
  }
  FT->FileSize = fsize;
  Checksum = FFSS_ComputeChecksum(0,NULL,0);
  if(FT->StartingPos != 0)
  {
    fseek(FT->fp,FT->StartingPos,SEEK_SET);
    fsize -= FT->StartingPos;
  }
  FT->XFerPos = 0; /* Do not set this to real pos, because if resume is used, FT->FileSize will be remaning bytes only */

  if(FFSS_TransferReadBufferSize == 0)
    FFSS_TransferReadBufferSize = FFSS_TRANSFER_READ_BUFFER_SIZE;
  RBuf = (char *) malloc(FFSS_TransferReadBufferSize);
  if(RBuf == NULL)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Cannot allocate buffer in Upload function");
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
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Transfer timed out");
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
#ifdef WORDS_BIGENDIAN
#error FIX ME ??
#endif /* WORDS_BIGENDIAN */
  if(res == SOCKET_ERROR)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error while uploading file (size) : %d %s",errno,strerror(errno));
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
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"EOF from remote host while uploading file");
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

  SU_GetTicks(&st);
  bytes = 0;
  prev_thrpt = 0;
  while(total < fsize) /* Main loop */
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
      /* Reset throughput */
      FT->Throughput = 0;
      FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
      FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
      FFSS_FreeTransfer(FT);
      SU_END_THREAD(NULL);
    }
    if((total+FFSS_TransferReadBufferSize) <= fsize)
      rlen = FFSS_TransferReadBufferSize;
    else
      rlen = (long int)(fsize - total);
    if(fread(RBuf,1,rlen,FT->fp) != rlen)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error reading file while uploading : %d",errno);
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
      /* Reset throughput */
      FT->Throughput = 0;
      FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
      FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
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
      while(res != len) /* Inner loop - Send read buffer */
      {
        FD_ZERO(&rfds);
        FD_SET(FT->sock,&rfds);
        tv.tv_sec = FFSS_TIMEOUT_TRANSFER;
        tv.tv_usec = 0;
        retval = select(FT->sock+1,NULL,&rfds,NULL,&tv);
        if(!retval)
        {
          SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Transfer timed out while uploading file");
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
          /* Reset throughput */
          FT->Throughput = 0;
          FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
          FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
          FFSS_FreeTransfer(FT);
          free(RBuf);
          SU_END_THREAD(NULL);
        }
        res += send(FT->sock,RBuf+rpos+res,len-res,SU_MSG_NOSIGNAL);
        if(res <= 0)
        {
          SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error while uploading file (buf) : %d %s",errno,strerror(errno));
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
          /* Reset throughput */
          FT->Throughput = 0;
          FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
          FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
          FFSS_FreeTransfer(FT);
          free(RBuf);
          SU_END_THREAD(NULL);
        }
      } /* while(res != len) */
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

      /* QoS Check */
      SU_GetTicks(&et);
      bytes += len;
      tim = SU_ElapsedTime(st,et,FFSS_CpuSpeed);
      if(tim >= FFSS_QOS_CHECK_DELAY)
      {
        FT->Throughput = bytes / tim;
        sleep_val = FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,IP,FT->Throughput-prev_thrpt,tim);
        sleep_val2 = FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,tim);
        prev_thrpt = FT->Throughput;
        /* Reset counters */
        SU_GetTicks(&st);
        bytes = 0;
        /* Execute the sleep */
        if(sleep_val || sleep_val2)
        {
          if(sleep_val > sleep_val2)
            SU_USLEEP(sleep_val);
          else
            SU_USLEEP(sleep_val2);
        }
      }
    } /* while(rpos < rlen) */
    total += rlen;
  } /* while(total < fsize) */

  /* Update one last time the throughput (for small files) */
  SU_GetTicks(&et);
  tim = SU_ElapsedTime(st,et,FFSS_CpuSpeed);
  if(tim == 0) /* avoid divide error */
    tim = 1;
  FT->Throughput = bytes / tim;
  FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,IP,FT->Throughput-prev_thrpt,tim);
  FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,tim);
  prev_thrpt = FT->Throughput;

  /* Send Checksum */
  FD_ZERO(&rfds);
  FD_SET(FT->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TRANSFER;
  tv.tv_usec = 0;
  retval = select(FT->sock+1,NULL,&rfds,NULL,&tv);
  if(!retval)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Transfer timed out");
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
    /* Reset throughput */
    FT->Throughput = 0;
    FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
    FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
    FFSS_FreeTransfer(FT);
    free(RBuf);
    SU_END_THREAD(NULL);
  }
#ifdef WORDS_BIGENDIAN
#error FIX ME ??
#endif /* WORDS_BIGENDIAN */
  res = send(FT->sock,(char *)&Checksum,sizeof(Checksum),SU_MSG_NOSIGNAL);
  if(res == SOCKET_ERROR)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error while uploading file (chksum) : %d %s",errno,strerror(errno));
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
    /* Reset throughput */
    FT->Throughput = 0;
    FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
    FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
    FFSS_FreeTransfer(FT);
    free(RBuf);
    SU_END_THREAD(NULL);
  }
  else if(res == 0)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"EOF from client while uploading file");
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
    /* Reset throughput */
    FT->Throughput = 0;
    FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
    FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
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
  SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Successfully uploaded the file %s in %d sec (%.2f ko/s) (%ld)",FT->LocalPath,((int)(t2-t1)),fsize/1024.0/(t2-t1),SU_THREAD_SELF);

  /* Reset throughput */
  FT->Throughput = 0;
  FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
  FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);

  FFSS_FreeTransfer(FT);
  free(RBuf);
  SU_END_THREAD(NULL);
}

SU_THREAD_ROUTINE(FFSS_DownloadFileFunc,Info)
{
  FFSS_PTransfer FT = (FFSS_PTransfer) Info;
  struct sockaddr sad;
  unsigned long int len;
  int res;
  SU_SOCKET client;
  char Buf[FFSS_TRANSFER_BUFFER_SIZE*2];
  fd_set rfds;
  struct timeval tv;
  int retval;
  FILE *fp;
  FFSS_LongField Size,total=0;
  FFSS_Field ChkSum,Checksum;
  bool error = false;
  time_t t1,t2;
  SU_TICKS st,et;
  unsigned long int tim;
  unsigned long int bytes;
  unsigned long int prev_thrpt,sleep_val,sleep_val2;
  unsigned long IP = INADDR_GET_IP(FT->Client->SAddr.sin_addr);

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
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error accepting connection (timed out)");
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
  retval = sizeof(sad);
  client = accept(FT->sock,&sad,&retval);
  SU_CLOSE_SOCKET(FT->sock);
  FT->sock = client;
  SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Connection accepted from %s",inet_ntoa(((struct sockaddr_in *)&sad)->sin_addr));
  if(FT->sock == SOCKET_ERROR)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error accepting connections");
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

    if(fp == NULL)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Can't open local file for writting (%s)",FT->LocalPath);
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
  }
  else
    fp = NULL;
  FT->XFerPos = 0; /* Do not set this to real pos, because if resume is used, FT->FileSize will be remaning bytes only */

  context;
  FD_ZERO(&rfds);
  FD_SET(FT->sock,&rfds);
  tv.tv_sec = FFSS_TIMEOUT_TRANSFER;
  tv.tv_usec = 0;
  retval = select(FT->sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Transfer timed out while downloading file");
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
#ifdef WORDS_BIGENDIAN
#error FIX ME ??
#endif /* WORDS_BIGENDIAN */
    if(res == SOCKET_ERROR)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error while downloading file (size) : %d %s",errno,strerror(errno));
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
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"EOF from server while downloading file");
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

  SU_GetTicks(&st);
  bytes = 0;
  prev_thrpt = 0;
  while(!error) /* Main loop */
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
      /* Reset throughput */
      FT->Throughput = 0;
      FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_DOWNLOAD,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
      FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
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
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Transfer timed out while downloading file");
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
      if(len > (unsigned long int)(Size-total)) /* WARNING HERE !!! This may bug, depending on the cast policy */
        len = (unsigned long int)(Size-total);
      if(len == 0) /* End of file, getting checksum */
        res = recv(FT->sock,(char *)&ChkSum,sizeof(Checksum),SU_MSG_NOSIGNAL);
      else
        res = recv(FT->sock,Buf,len,SU_MSG_NOSIGNAL);
      if(res == SOCKET_ERROR)
      {
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error while downloading file (buf) : %d %s",errno,strerror(errno));
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
        SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"EOF from server while downloading file");
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
          if(fp == NULL)
          {
            if(FT->ThreadType == FFSS_THREAD_SERVER)
            {
              if(FFSS_CB.SCB.OnTransferFileWrite != NULL)
              {
                if(!FFSS_CB.SCB.OnTransferFileWrite(FT,Buf,res,FT->XFerPos))
                  error = true;
              }
              else
                error = true;
            }
            else
            {
              if(FFSS_CB.CCB.OnTransferFileWrite != NULL)
              {
                if(!FFSS_CB.CCB.OnTransferFileWrite(FT,Buf,res,FT->XFerPos))
                  error = true;
              }
              else
                error = true;
            }
            if(error)
            {
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
            }
          }
          else
          {
            if(fwrite(Buf,1,res,fp) != (unsigned int)res)
            {
              SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Disk full ?");
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
          }
          Checksum = FFSS_ComputeChecksum(Checksum,Buf,res);
          FT->XFerPos += res;
          total += res;

          /* QoS Check */
          SU_GetTicks(&et);
          bytes += len;
          tim = SU_ElapsedTime(st,et,FFSS_CpuSpeed);
          if(tim >= FFSS_QOS_CHECK_DELAY)
          {
            FT->Throughput = bytes / tim;
            sleep_val = FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_DOWNLOAD,IP,FT->Throughput-prev_thrpt,tim);
            sleep_val2 = FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,tim);
            prev_thrpt = FT->Throughput;
            /* Reset counters */
            SU_GetTicks(&st);
            bytes = 0;
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
        if(!error)
        {
          if(len == 0)
          {
            context;
#ifdef DISABLE_CHECKSUM
            if(true)
#else /* !DISABLE_CHECKSUM */
            if((ChkSum == Checksum) || (ChkSum == 1)) /* A ChkSum of 1 means NO checksum on the other side */
#endif /* DISABLE_CHECKSUM */
            {
              SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"File successfully downloaded");
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
              SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Checksum check failed");
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
            SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Received data bigger than file size (%ld - %ld)",total,Size);
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
        } /* if(!error) */
      } /* if(!error) */
    } /* if(!error) */
  } /* while(!error) */

  /* Reset throughput */
  FT->Throughput = 0;
  FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_DOWNLOAD,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);
  FFSS_QoS_UpdateRate(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,IP,FT->Throughput-prev_thrpt,FFSS_QOS_CHECK_DELAY);

  FFSS_FreeTransfer(FT);
  if(!error)
  {
    t2 = time(NULL);
    if(t1 == t2)
      t2 = t1+1;
    SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"Successfully downloaded the file in %d sec (%.2f ko/s)",((int)(t2-t1)),Size/1024.0/(t2-t1));
  }
  SU_END_THREAD(NULL);
}


bool FFSS_UploadFile(SU_PClientSocket Client,const char FilePath[],FFSS_LongField StartingPos,FFSS_LongField EndingPos,int Port,void *User,bool UseConnSock,FFSS_LongField UserInfo,FFSS_PTransfer *FT_out)
{
  FILE *fp = NULL;
  SU_THREAD_HANDLE Thread;
  struct sockaddr_in SAddr;
  SU_SOCKET sock=0;
  FFSS_PTransfer FT;

  /* ATTENTION ICI : Utilisation de messages specifiques Server->Client pour une fonction generique !! */
  context;
  *FT_out = NULL;
  fp = fopen(FilePath,"rb");
  if(fp == NULL)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Couldn't open file for upload : %s (%d)",FilePath,errno);
    return FS_SendMessage_Error(Client->sock,FFSS_ERROR_FILE_NOT_FOUND,FFSS_ErrorTable[FFSS_ERROR_FILE_NOT_FOUND],Port,UserInfo);
  }
  if(!UseConnSock)
  {
    sock = socket(AF_INET,SOCK_STREAM,getprotobyname("tcp")->p_proto);
    if(sock == SOCKET_ERROR)
    {
      fclose(fp);
      return FS_SendMessage_Error(Client->sock,FFSS_ERROR_INTERNAL_ERROR,FFSS_ErrorTable[FFSS_ERROR_INTERNAL_ERROR],0,UserInfo);
    }
    SAddr.sin_family = AF_INET;
    SAddr.sin_port = htons(Port);
    SAddr.sin_addr.s_addr = Client->SAddr.sin_addr.s_addr;
    if(connect(sock,(struct sockaddr *)(&SAddr),sizeof(SAddr)) == SOCKET_ERROR)
    {
      SU_CLOSE_SOCKET(sock);
      fclose(fp);
      return FS_SendMessage_Error(Client->sock,FFSS_ERROR_CANNOT_CONNECT,FFSS_ErrorTable[FFSS_ERROR_CANNOT_CONNECT],Port,UserInfo);
    }
  }
  FT = (FFSS_PTransfer) malloc(sizeof(FFSS_TTransfer));
  memset(FT,0,sizeof(FFSS_TTransfer));
  FT->fp = fp;
  FT->StartingPos = StartingPos;
  FT->EndingPos = EndingPos;
  FT->UserInfo = UserInfo;
  FT->XI.UseConnSock = UseConnSock;
  if(UseConnSock)
    FT->sock = Client->sock;
  else
    FT->sock = sock;
  FT->Port = Port; /* CHECK THIS !!! MAY NOT BE THIS PORT (INSTEAD LOCAL PORT, NOT REMOTE) */
  FT->LocalPath = strdup(FilePath);
  FT->Client = Client;
  FT->User = User;
  FT->ThreadType = FFSS_THREAD_SERVER;
  if(!UseConnSock)
  {
    if(!SU_CreateThread(&Thread,FFSS_UploadFileFunc,(void *)FT,true))
    {
      FFSS_FreeTransfer(FT);
      return FS_SendMessage_Error(Client->sock,FFSS_ERROR_TOO_MANY_TRANSFERS,FFSS_ErrorTable[FFSS_ERROR_TOO_MANY_TRANSFERS],Port,UserInfo);
    }
  }
  if(FT_out != NULL)
    *FT_out = FT;
  return true;
}

bool FFSS_DownloadFile(SU_PClientSocket Server,const char RemotePath[],const char LocalPath[],FFSS_LongField StartingPos,FFSS_LongField EndingPos,void *User,bool UseConnSock,FFSS_LongField UserInfo,FFSS_PTransfer *FT_out)
{
  SU_SOCKET sock;
  struct sockaddr_in saddr;
  int i;
  SU_THREAD_HANDLE Thread;
  FFSS_PTransfer FT;

  context;
  if(EndingPos != 0)
  {
    if(EndingPos <= StartingPos)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"FFSS_DownloadFile : Not trying to download file, as StartingPos >= EndinfPos");
      return false;
    }
    if(UseConnSock)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"FFSS_DownloadFile : Not trying to download file, as UseConnSock is not yet supported for EndingPos != 0");
      return false;
    }
    if(LocalPath != NULL)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"FFSS_DownloadFile : Not trying to download file, as LocalPath != NULL is not yet supported for EndingPos != 0");
      return false;
    }
  }
  sock = FC_SendMessage_Download(Server,RemotePath,StartingPos,EndingPos,UseConnSock,UserInfo);
  if(sock == SOCKET_ERROR)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error sending DOWNLOAD request : %d (%s)",errno,strerror(errno));
    return false;
  }
  i = sizeof(saddr);
  getsockname(sock,(struct sockaddr *)&saddr,&i);
  FT = (FFSS_PTransfer) malloc(sizeof(FFSS_TTransfer));
  memset(FT,0,sizeof(FFSS_TTransfer));
  FT->FileName = strdup(RemotePath);
  if(LocalPath != NULL)
    FT->LocalPath = strdup(LocalPath);
  FT->StartingPos = StartingPos;
  FT->EndingPos = EndingPos;
  FT->UserInfo = UserInfo;
  FT->XI.UseConnSock = UseConnSock;
  if(UseConnSock)
    FT->sock = Server->sock;
  else
    FT->sock = sock;
  FT->Port = ntohs(saddr.sin_port);
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
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Transfer timed out");
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
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error while uploading file (data) : %d %s",errno,strerror(errno));
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_SEND,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_SEND],false);
      FFSS_FreeTransfer(FT);
      free(msg);
      return false;
    }
    else if(res == 0)
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"EOF from remote host while uploading file");
      if(FFSS_CB.SCB.OnTransferFailed != NULL)
        FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_EOF,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_EOF],false);
      FFSS_FreeTransfer(FT);
      free(msg);
      return false;
    }
  }
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
  free(msg);
  return true;
}

void FFSS_OnDataDownload(FFSS_PTransfer FT,const char Buf[],int Len)
{
  FFSS_Field Checksum=0;

  if(FT->XI.fsize == 0) /* Extract FileSize */
  {
    FT->XI.fsize = *(FFSS_LongField *)(Buf);
    Len -= sizeof(FFSS_LongField);
    SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"FFSS_OnDataDownload : Receiving file size : %lld",FT->XI.fsize);
  }
  FT->XI.total += Len;
  if(FT->XI.total > FT->XI.fsize) /* Extract Checksum */
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"FFSS_OnDataDownload : All data received, after %lld bytes (fsize=%lld)...extracting checksum",FT->XI.total,FT->XI.fsize);
    Len -= sizeof(FFSS_Field);
    Checksum = *(FFSS_Field *)(Buf+Len);
  }
  if(Len != 0)
  {
    if(FT->fp != NULL)
      fwrite(Buf,1,Len,FT->fp);
    FT->XI.Checksum = FFSS_ComputeChecksum(FT->XI.Checksum,Buf,Len);
    if(FT->ThreadType == FFSS_THREAD_SERVER)
    {
      if(FFSS_CB.SCB.OnTransferActive != NULL)
        FFSS_CB.SCB.OnTransferActive(FT,Len,false);
    }
    else
    {
      if(FFSS_CB.CCB.OnTransferActive != NULL)
        FFSS_CB.CCB.OnTransferActive(FT,Len,false);
    }
  }
  if(FT->XI.total == (FT->XI.fsize+sizeof(FFSS_Field))) /* Compares checksum */
  {
#ifdef DISABLE_CHECKSUM
    if(true)
#else /* !DISABLE_CHECKSUM */
    if((Checksum == FT->XI.Checksum) || (Checksum == 1)) /* A Checksum of 1 means NO checksum on the other side */
#endif /* DISABLE_CHECKSUM */
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
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Can't open local file for writting (%s)",FT->LocalPath);
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
