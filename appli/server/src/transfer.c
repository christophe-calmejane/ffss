#include "server.h"

FFSS_Field FS_CurrentXFerTag = 0;

bool FS_InitXFerUpload(SU_PClientSocket Client,FFSS_PTransfer FT,const char Path[],bool Download)
{
  FT->XI.XFerTag = FS_CurrentXFerTag++;
  if(FS_CurrentXFerTag == 0)
    FS_CurrentXFerTag++;
  fseek(FT->fp,0,SEEK_END);
  FT->XI.fsize = ftell(FT->fp);
  rewind(FT->fp);
  FT->XI.Download = Download;
  FT->XI.Checksum = FFSS_ComputeChecksum(0,NULL,0);

  if(!FS_SendMessage_InitXFer(Client->sock,FT->XI.XFerTag,Path))
  {
    return false;
  }
  return FFSS_SendData(FT,FT->XI.XFerTag,(char *)&FT->XI.fsize,sizeof(FT->XI.fsize)); /* Send filesize */
}

bool FS_TransferBloc(FFSS_PTransfer FT,FS_PConn Conn) /* False on END OF TRANSFER */
{
  unsigned long int rpos=0,rlen;
  int len;
  bool last;

  if(FT->Cancel)
  {
    if(FFSS_CB.SCB.OnTransferFailed != NULL)
      FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_CANCELED,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_CANCELED],false);
    FFSS_FreeTransfer(FT);
    return false;
  }

  if((FT->XI.total+FFSS_TRANSFER_READ_BUFFER_SIZE) <= FT->XI.fsize)
  {
    rlen = FFSS_TRANSFER_READ_BUFFER_SIZE;
    last = false;
  }
  else
  {
    rlen = (unsigned long int)(FT->XI.fsize - FT->XI.total);
    last = true;
  }
  if(fread(Conn->TransferBuffer,1,rlen,FT->fp) != rlen)
  {
    SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error reading file while uploading : %d",errno);
    if(FFSS_CB.SCB.OnTransferFailed != NULL)
      FFSS_CB.SCB.OnTransferFailed(FT,FFSS_ERROR_TRANSFER_READ_FILE,FFSS_TransferErrorTable[FFSS_ERROR_TRANSFER_READ_FILE],false);
    FFSS_FreeTransfer(FT);
    return false;
  }
  FT->XI.total += rlen;
  FT->XI.Checksum = FFSS_ComputeChecksum(FT->XI.Checksum,Conn->TransferBuffer,rlen);
  rpos = 0;
  while(rpos < rlen)
  {
    if((rpos+FFSS_TRANSFER_BUFFER_SIZE) <= rlen)
      len = FFSS_TRANSFER_BUFFER_SIZE;
    else
      len = rlen - rpos;
    if(!FFSS_SendData(FT,FT->XI.XFerTag,Conn->TransferBuffer+rpos,len))
      return false;
    if(FFSS_CB.SCB.OnTransferActive != NULL)
      FFSS_CB.SCB.OnTransferActive(FT,len,false);
    rpos += len;
  }

  if(last)
  {
    /* Send Checksum */
    SU_DBG_PrintDebug(FS_DBGMSG_XFER,"FS_TransferBloc : Last packet sent after %lld bytes (fsize=%lld)...sending checksum",FT->XI.total,FT->XI.fsize);
    if(!FFSS_SendData(FT,FT->XI.XFerTag,(char *)&FT->XI.Checksum,sizeof(FT->XI.Checksum)))
      return false;
    if(FFSS_CB.SCB.OnTransferSuccess != NULL)
      FFSS_CB.SCB.OnTransferSuccess(FT,false);
    FFSS_FreeTransfer(FT);
    return false;
  }
  return true;
}

