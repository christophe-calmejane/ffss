#include "index.h"

extern volatile bool FM_ShuttingDown;

void FM_CheckMasterConnections(void)
{
  SU_PList Ptr;
  FM_PDomain Domain;

  /* Connecting to foreign masters */
  context;
  SU_DBG_PrintDebug(FM_DBGMSG_GLOBAL,"FM_CheckMasterConnections : Checking if all masters are still connected");
  Ptr = FM_Domains;
  while(Ptr != NULL)
  {
    Domain = (FM_PDomain) Ptr->Data;
    if((Domain->CS == NULL) && (Domain != &FM_MyDomain))
    {
      SU_DBG_PrintDebug(FM_DBGMSG_GLOBAL,"FM_CheckMasterConnections : Master of domain %s not connected... trying to reconnect...",Domain->Name);
      SU_SEM_WAIT(FM_TmpSem); /* Lock to protect a free of the CS struct before end of init */
      Domain->CS = FM_SendMessage_Connect(Domain->Master);
      if(Domain->CS != NULL)
      {
        SU_DBG_PrintDebug(FM_DBGMSG_GLOBAL,"FM_CheckMasterConnections : Successfully connected... requesting servers list");
        FM_SendMessage_MasterConnection(Domain->CS->sock);
        FM_SendMessage_ServerList(Domain->CS->sock,0);
      }
      SU_SEM_POST(FM_TmpSem);
    }
    Ptr = Ptr->Next;
  }
}

SU_THREAD_ROUTINE(FM_ThreadPing,User)
{
  SU_PList Ptr,Ptr2,Ptr3;
  time_t now;
  int SaveIndexCount;
  char *buf;
  long int len;

  context;
  SU_ThreadBlockSigs();
  SU_DBG_PrintDebug(FM_DBGMSG_GLOBAL,"PING thread running...");
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
      SU_DBG_PrintDebug(FM_DBGMSG_INDEX,"%d%% of unused space in hash table",FMI_GetUnusedHashPos());
      SU_DBG_PrintDebug(FM_DBGMSG_INDEX,"%d strings in index",FMI_GetIndexCount());
      FMI_GarbageCollector();
      context;
      FMI_SaveIndex(FM_MYINDEX_FILE);
      SaveIndexCount = 0;
    }
    /* Check master connections */
    FM_CheckMasterConnections();
    /* PING SEQUENCE */
    context;
    SU_DBG_PrintDebug(FM_DBGMSG_GLOBAL,"THREADS : PING : Sending PING sequence");
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
          SU_DBG_PrintDebug(FM_DBGMSG_STATES,"THREADS : PING : Server %s timed out, changing state",((FM_PHost)Ptr->Data)->Name);
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
    SU_DBG_PrintDebug(FM_DBGMSG_GLOBAL,"THREADS : PING : Sending REMOVE sequence");
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
        SU_DBG_PrintDebug(FM_DBGMSG_STATES,"THREADS : PING : Removing host %s from my domain",((FM_PHost)Ptr->Data)->IP);
        SU_DBG_PrintDebug(FM_DBGMSG_STATES,"He is off since %ld - and we are now %ld",((FM_PHost)Ptr->Data)->OffSince,now);
        /* Checking if host is not in queue */
        SU_SEM_WAIT(FM_MySem);
        Ptr3 = FM_MyQueue;
        while(Ptr3 != NULL)
        {
          if(((FM_PQueue)Ptr3->Data)->Host == Ptr->Data)
          {
            ((FM_PQueue)Ptr3->Data)->Removed = true;
            SU_DBG_PrintDebug(FM_DBGMSG_STATES,"He is in my state queue... marking as removed");
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
    SU_DBG_PrintDebug(FM_DBGMSG_GLOBAL,"THREADS : PING : Sending UPDATE sequence");
    /* Building states of servers of my domain */
    context;
    /* Acquire semaphore */
    SU_SEM_WAIT(FM_MySem);
    if(FM_ShuttingDown)
      SU_END_THREAD(NULL);
#if 0
    buf = FM_BuildStatesBuffer(FM_MyQueue,&len);
    FM_MyQueue = NULL;
#else
    {
    SU_PList Queue;
    FM_PQueue Que;

    Queue = NULL;
    Ptr = FM_MyDomain.Hosts;
    while(Ptr != NULL)
    {
      Que = (FM_PQueue) malloc(sizeof(FM_TQueue));
      memset(Que,0,sizeof(FM_TQueue));
      Que->Domain = &FM_MyDomain;
      Que->Host = (FM_PHost)Ptr->Data;
      Queue = SU_AddElementHead(Queue,Que);
      Ptr = Ptr->Next;
    }
    buf = FM_BuildStatesBuffer(Queue,&len);
    }
#endif
    /* Release semaphore */
    SU_SEM_POST(FM_MySem);
    if(buf != NULL)
    {
      SU_DBG_PrintDebug(FM_DBGMSG_GLOBAL,"THREADS : PING : Sending States queue to co-masters");
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
#ifdef STATS
  struct timeval t1,t2;
  struct timezone tz={0,0};
#endif /* STATS */
  FM_PSearch Sch;
  char tmp[1024];

  context;
  SU_ThreadBlockSigs();
  SU_DBG_PrintDebug(FM_DBGMSG_GLOBAL,"SEARCH thread running...");
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
#ifdef STATS
    timerclear(&t1);
    timerclear(&t2);
    gettimeofday(&t1,&tz);
#endif /* STATS */
    context;
    buf = FM_Search(Sch,&len);
#ifdef STATS
    gettimeofday(&t2,&tz);
#endif /* STATS */
    SU_SEM_POST(FM_MySem5);
    context;
#ifdef STATS
    snprintf(tmp,sizeof(tmp),"Search time for %s : %.2f milli secondes",Sch->KeyWords,((t2.tv_sec*1000000+t2.tv_usec)-(t1.tv_sec*1000000+t1.tv_usec))/1000.);
    SU_DBG_PrintDebug(FM_DBGMSG_SEARCH,"%s",tmp);
    if(FM_SearchLogFile != NULL)
      SU_WriteToLogFile(FM_SearchLogFile,tmp);
#endif /* STATS */
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
      FM_SendMessage_SearchAnswer(Sch->Client,buf,len,comp,Sch->User);
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
