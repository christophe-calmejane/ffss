#include "index.h"

extern volatile bool FM_ShuttingDown;

SU_THREAD_ROUTINE(FM_ThreadPing,User)
{
  SU_PList Ptr,Ptr2,Ptr3;
  time_t now;
  int SaveIndexCount;
  char *buf;
  long int len;

  context;
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
      context;
      FFSS_PrintDebug(4,"%d%% of unused space in hash table\n",FMI_GetUnusedHashPos());
      FFSS_PrintDebug(4,"%d strings in index\n",FMI_GetIndexCount());
      FMI_GarbageCollector();
      context;
      FMI_SaveIndex(FM_MYINDEX_FILE);
      SaveIndexCount = 0;
    }
    /* PING SEQUENCE */
    context;
    FFSS_PrintDebug(4,"THREADS : PING : Sending PING sequence\n");
    now = time(NULL);
    SU_SEM_WAIT(FM_MySem2);
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);
    Ptr = FM_MyDomain.Hosts;
    while(Ptr != NULL)
    {
      if(((FM_PHost)Ptr->Data)->State != FFSS_STATE_OFF)
      {
        if((((FM_PHost)Ptr->Data)->LastPong + FFSS_PING_TIMEOUT) < now)
        {
          /* Server timed out */
          context;
          FFSS_PrintDebug(5,"THREADS : PING : Server %s timed out, changing state\n",((FM_PHost)Ptr->Data)->Name);
          ((FM_PHost)Ptr->Data)->State = FFSS_STATE_OFF;
          ((FM_PHost)Ptr->Data)->OffSince = time(NULL);
          FM_AddStateToMyQueue(&FM_MyDomain,(FM_PHost)Ptr->Data);
        }
      }
      Ptr = Ptr->Next;
    }
    /* Send PING message - Even if server timed out */
    context;
//#ifndef DEBUG
    FM_SendMessage_Ping();
//#endif /* !DEBUG */
    SU_SEM_POST(FM_MySem2);

    /* REMOVE SEQUENCE */
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);
    FFSS_PrintDebug(4,"THREADS : PING : Sending REMOVE sequence\n");
    now = time(NULL);
    SU_SEM_WAIT(FM_MySem2);
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);
    Ptr = FM_MyDomain.Hosts;
    Ptr2 = NULL;
    while(Ptr != NULL)
    {
      if((((FM_PHost)Ptr->Data)->State == FFSS_STATE_OFF) && ((((FM_PHost)Ptr->Data)->OffSince + FFSS_KEEP_HOST_DELAY) < now))
      {
        context;
        FFSS_PrintDebug(3,"THREADS : PING : Removing host %s from my domain\n",((FM_PHost)Ptr->Data)->IP);
        FFSS_PrintDebug(3,"He is off since %ld - and we are now %ld\n",((FM_PHost)Ptr->Data)->OffSince,now);
        /* Checking if host is not in queue */
        SU_SEM_WAIT(FM_MySem);
        Ptr3 = FM_MyQueue;
        while(Ptr3 != NULL)
        {
          if(((FM_PQueue)Ptr3->Data)->Host == Ptr->Data)
          {
            ((FM_PQueue)Ptr3->Data)->Removed = true;
            FFSS_PrintDebug(3,"He is in my state queue... marking as removed\n");
          }
          Ptr3 = Ptr3->Next;
        }
        SU_SEM_POST(FM_MySem);
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
    context;
    FM_SaveHosts(FM_MyDomain.Hosts,FM_MYHOSTS_FILE);
    SU_SEM_POST(FM_MySem2);

    /* UPDATE CO-MASTERS SEQUENCE */
    SU_SLEEP(5); /* Wait for some servers to answer the ping message */
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);
    FFSS_PrintDebug(4,"THREADS : PING : Sending UPDATE sequence\n");
    /* Building states of servers of my domain */
    context;
    /* Acquire semaphore */
    SU_SEM_WAIT(FM_MySem);
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);
    buf = FM_BuildStatesBuffer(FM_MyQueue,&len);
    FM_MyQueue = NULL;
    /* Release semaphore */
    SU_SEM_POST(FM_MySem);
    if(buf != NULL)
    {
      FFSS_PrintDebug(5,"THREADS : PING : Sending States queue to co-masters\n");
      /* Send states to co-masters */
      Ptr = FM_Domains;
      while(Ptr != NULL)
      {
        if(!FM_IsMyDomain(Ptr->Data))
        {
          context;
          if(((FM_PDomain)Ptr->Data)->CS != NULL)
          {
            if(!FM_SendMessage_NewStatesMaster(((FM_PDomain)Ptr->Data)->CS->sock,buf,len,FFSS_COMPRESSION_BZLIB))
              FFSS_PrintSyslog(LOG_WARNING,"Error sending New States message %s (%d:%s)\n",((FM_PDomain)Ptr->Data)->Master,errno,strerror(errno));
          }
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
  context;
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

  context;
  SU_ThreadBlockSigs();
  FFSS_PrintDebug(2,"SEARCH thread running...\n");
  while(1)
  {
    SU_SLEEP(1);
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);

    if(FM_SearchQueue == NULL)
      continue;

    context;
    Sch = (FM_PSearch)FM_SearchQueue->Data;
    SU_SEM_WAIT(FM_MySem5);
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);
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
    context;
    buf = FM_Search(Sch,&len);
#ifdef DEBUG
    gettimeofday(&t2,&tz);
#endif /* DEBUG */
    SU_SEM_POST(FM_MySem5);
    context;
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
      context;
      FM_SendMessage_SearchAnswer(Sch->Client,buf,len,comp);
      free(buf);
    }
    FM_FreeSearch(Sch);

    SU_SEM_WAIT(FM_MySem4);
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);
    FM_SearchQueue = SU_DelElementHead(FM_SearchQueue);
    SU_SEM_POST(FM_MySem4);
  }
  SU_END_THREAD(NULL);
}
