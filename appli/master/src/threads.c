#include "index.h"

extern volatile bool FM_ShuttingDown;

SU_THREAD_ROUTINE(FM_ThreadPing,User)
{
  SU_PList Ptr,Ptr2;
  time_t now;
  int SaveIndexCount;

  SU_ThreadBlockSigs();
  FFSS_PrintDebug(2,"PING thread running...\n");
  SaveIndexCount = 0;
  while(1)
  {
    SU_SLEEP(FFSS_PING_INTERVAL);
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);
    SaveIndexCount++;
    if(SaveIndexCount > FM_INDEX_DUMP_INTERVAL_PING)
    {
#ifdef DEBUG
      FFSS_PrintDebug(4,"%d%% of unused space in hash table\n",FMI_GetUnusedHashPos());
      FFSS_PrintDebug(4,"%d strings in index\n",FMI_GetIndexCount());
#endif /* DEBUG */
      FMI_GarbageCollector();
      FMI_SaveIndex(FM_MYINDEX_FILE);
      SaveIndexCount = 0;
    }
    /* PING SEQUENCE */
    FFSS_PrintDebug(4,"THREADS : PING : Sending PING sequence\n");
    now = time(NULL);
    SU_SEM_WAIT(FM_MySem2);
    Ptr = FM_MyDomain.Hosts;
    while(Ptr != NULL)
    {
      if(((FM_PHost)Ptr->Data)->State != FFSS_STATE_OFF)
      {
        if((((FM_PHost)Ptr->Data)->LastPong + FFSS_PING_TIMEOUT) < now)
        {
          /* Server timed out */
          FFSS_PrintDebug(5,"THREADS : PING : Server %s timed out, changing state\n",((FM_PHost)Ptr->Data)->Name);
          ((FM_PHost)Ptr->Data)->State = FFSS_STATE_OFF;
          ((FM_PHost)Ptr->Data)->OffSince = time(NULL);
          FM_AddStateToMyQueue(&FM_MyDomain,(FM_PHost)Ptr->Data);
        }
      }
      Ptr = Ptr->Next;
    }
    /* Send PING message - Even if server timed out */
    FM_SendMessage_Ping();
    SU_SEM_POST(FM_MySem2);

    /* REMOVE SEQUENCE */
    now = time(NULL);
    SU_SEM_WAIT(FM_MySem2);
    Ptr = FM_MyDomain.Hosts;
    Ptr2 = NULL;
    while(Ptr != NULL)
    {
      if((((FM_PHost)Ptr->Data)->State == FFSS_STATE_OFF) && ((((FM_PHost)Ptr->Data)->OffSince + FFSS_KEEP_HOST_DELAY) < now))
      {
        FFSS_PrintDebug(3,"THREADS : PING : Removing host %s from my domain\n",((FM_PHost)Ptr->Data)->IP);
        FFSS_PrintDebug(3,"He is off since %ld - and we are now %ld\n",((FM_PHost)Ptr->Data)->OffSince,now);
        /* Remove this host from my domain */
        FM_FreeHost((FM_PHost)Ptr->Data);
        Ptr = SU_DelElementHead(Ptr);
        if(Ptr2 == NULL)
          FM_MyDomain.Hosts = Ptr;
        else
          Ptr2->Next = Ptr;
      }
      else
      {
        Ptr2 = Ptr;
        Ptr = Ptr->Next;
      }
    }
    SU_SEM_POST(FM_MySem2);
    FM_SaveHosts(FM_MyDomain.Hosts,FM_MYHOSTS_FILE);

  }
  SU_END_THREAD(NULL);
}


SU_THREAD_ROUTINE(FM_ThreadQueue,User)
{
  char *buf;
  long int len;
  SU_PList Ptr;

  SU_ThreadBlockSigs();
  FFSS_PrintDebug(2,"QUEUE thread running...\n");
  while(1)
  {
    SU_SLEEP(FFSS_STATE_BROADCAST_INTERVAL);
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);
    /* Building states of servers of my domain */
    /* Acquire semaphore */
    SU_SEM_WAIT(FM_MySem);
    buf = FM_BuildStatesBuffer(FM_MyQueue,&len);
    FM_MyQueue = NULL;
    /* Release semaphore */
    SU_SEM_POST(FM_MySem);
    if(buf != NULL)
    {
      FFSS_PrintDebug(5,"THREADS : QUEUE : Sending States queue to co-masters\n");
      /* Send states to co-masters */
      Ptr = FM_Domains;
      while(Ptr != NULL)
      {
        if(Ptr->Data != (&FM_MyDomain))
        {
          if(((FM_PDomain)Ptr->Data)->CS != NULL)
            FM_SendMessage_NewStatesMaster(((FM_PDomain)Ptr->Data)->CS->sock,buf,len,FFSS_COMPRESSION_BZLIB);
        }
        Ptr = Ptr->Next;
      }
      free(buf);
    }
  }
  SU_END_THREAD(NULL);
}

void FM_FreeSearch(FM_PSearch Sch)
{
  free(Sch->Domain);
  free(Sch->KeyWords);
  free(Sch);
}

SU_THREAD_ROUTINE(FM_ThreadSearch,User)
{
  char *buf;
  long int len;
  long int comp;
#ifdef DEBUG
  struct timeval t1,t2;
  struct timezone tz={0,0};
#endif /* DEBUG */
  FM_PSearch Sch;
  char tmp[1024];

  SU_ThreadBlockSigs();
  FFSS_PrintDebug(2,"SEARCH thread running...\n");
  while(1)
  {
    SU_SLEEP(1);
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);

    if(FM_SearchQueue == NULL)
      continue;

    Sch = (FM_PSearch)FM_SearchQueue->Data;
    SU_SEM_WAIT(FM_MySem5);
    if(FM_SearchLogFile != NULL)
    {
      snprintf(tmp,sizeof(tmp),"Search from %s for domain %s : %s",inet_ntoa(Sch->Client.sin_addr),Sch->Domain,Sch->KeyWords);
      SU_WriteToLogFile(FM_SearchLogFile,tmp);
    }
#ifdef DEBUG
    timerclear(&t1);
    timerclear(&t2);
    gettimeofday(&t1,&tz);
#endif /* DEBUG */
    buf = FM_Search(Sch,&len);
#ifdef DEBUG
    gettimeofday(&t2,&tz);
#endif /* DEBUG */
    SU_SEM_POST(FM_MySem5);
#ifdef DEBUG
    snprintf(tmp,sizeof(tmp),"Search time for %s : %.2f milli secondes",Sch->KeyWords,((t2.tv_sec*1000000+t2.tv_usec)-(t1.tv_sec*1000000+t1.tv_usec))/1000.);
    FFSS_PrintDebug(4,"%s\n",tmp);
    if(FM_SearchLogFile != NULL)
      SU_WriteToLogFile(FM_SearchLogFile,tmp);
#endif /* DEBUG */
    if(buf != NULL)
    {
      if(len >= FM_COMPRESSION_TRIGGER_BZLIB)
      {
        if(Sch->Compressions & FFSS_COMPRESSION_BZLIB)
          comp = FFSS_COMPRESSION_BZLIB;
        else if(Sch->Compressions & FFSS_COMPRESSION_ZLIB)
          comp = FFSS_COMPRESSION_ZLIB;
        else
          comp = FFSS_COMPRESSION_NONE;
      }
      else if(len >= FM_COMPRESSION_TRIGGER_ZLIB)
      {
        if(Sch->Compressions & FFSS_COMPRESSION_ZLIB)
          comp = FFSS_COMPRESSION_ZLIB;
        else
          comp = FFSS_COMPRESSION_NONE;
      }
      else
        comp = FFSS_COMPRESSION_NONE;
      FM_SendMessage_SearchAnswer(Sch->Client,buf,len,comp);
      free(buf);
    }
    FM_FreeSearch(Sch);

    SU_SEM_WAIT(FM_MySem4);
    FM_SearchQueue = SU_DelElementHead(FM_SearchQueue);
    SU_SEM_POST(FM_MySem4);
  }
  SU_END_THREAD(NULL);
}
