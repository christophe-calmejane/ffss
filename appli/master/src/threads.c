#include "index.h"

#ifdef _WIN32
void FM_ThreadPing(void *User)
#else /* _WIN32 */
void *FM_ThreadPing(void *User)
#endif /* _WIN32 */
{
  SU_PList Ptr,Ptr2;
  time_t now;
  int SaveIndexCount;

  FFSS_PrintDebug(2,"PING thread running...\n");
  SaveIndexCount = 0;
  while(1)
  {
#ifdef _WIN32
    Sleep(FFSS_PING_INTERVAL*1000);
#else /* _WIN32 */
    sleep(FFSS_PING_INTERVAL);
#endif /* _WIN32 */
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
#ifdef __unix__
    sem_wait(&FM_MySem2);
#else /* __unix__ */
    WaitForSingleObject(FM_MySem2,INFINITE);
#endif /* __unix__ */
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
#ifdef __unix__
    sem_post(&FM_MySem2);
#else /* __unix__ */
    ReleaseSemaphore(FM_MySem2,1,NULL);
#endif /* __unix__ */

    /* REMOVE SEQUENCE */
    now = time(NULL);
#ifdef __unix__
    sem_wait(&FM_MySem2);
#else /* __unix__ */
    WaitForSingleObject(FM_MySem2,INFINITE);
#endif /* __unix__ */
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
#ifdef __unix__
    sem_post(&FM_MySem2);
#else /* __unix__ */
    ReleaseSemaphore(FM_MySem2,1,NULL);
#endif /* __unix__ */
    FM_SaveHosts(FM_MyDomain.Hosts,FM_MYHOSTS_FILE);

  }
#ifdef __unix__
  return 0;
#endif /* __unix__ */
}


#ifdef _WIN32
void FM_ThreadQueue(void *User)
#else /* _WIN32 */
void *FM_ThreadQueue(void *User)
#endif /* _WIN32 */
{
  char *buf;
  long int len;
  SU_PList Ptr;

  FFSS_PrintDebug(2,"QUEUE thread running...\n");
  while(1)
  {
#ifdef _WIN32
    Sleep(FFSS_STATE_BROADCAST_INTERVAL*1000);
#else /* _WIN32 */
    sleep(FFSS_STATE_BROADCAST_INTERVAL);
#endif /* _WIN32 */
    /* Building states of servers of my domain */
    /* Acquire semaphore */
#ifdef __unix__
    sem_wait(&FM_MySem);
#else /* __unix__ */
    WaitForSingleObject(FM_MySem,INFINITE);
#endif /* __unix__ */
    buf = FM_BuildStatesBuffer(FM_MyQueue,&len);
    FM_MyQueue = NULL;
    /* Release semaphore */
#ifdef __unix__
    sem_post(&FM_MySem);
#else /* __unix__ */
    ReleaseSemaphore(FM_MySem,1,NULL);
#endif /* __unix__ */
    if(buf != NULL)
    {
      FFSS_PrintDebug(5,"THREADS : QUEUE : Sending States queue to co-masters\n");
      /* Send states to co-masters */
      Ptr = FM_Domains;
      while(Ptr != NULL)
      {
        if(Ptr->Data != (&FM_MyDomain))
          FM_SendMessage_NewStatesMaster(((FM_PDomain)Ptr->Data)->Master,buf,len,FFSS_COMPRESSION_BZLIB);
        Ptr = Ptr->Next;
      }
      free(buf);
    }
  }
#ifdef __unix__
  return 0;
#endif /* __unix__ */
}

void FM_FreeSearch(FM_PSearch Sch)
{
  free(Sch->Domain);
  free(Sch->KeyWords);
  free(Sch);
}

#ifdef _WIN32
void FM_ThreadSearch(void *User)
#else /* _WIN32 */
void *FM_ThreadSearch(void *User)
#endif /* _WIN32 */
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

  FFSS_PrintDebug(2,"SEARCH thread running...\n");
  while(1)
  {
#ifdef _WIN32
    Sleep(1000);
#else /* _WIN32 */
    sleep(1);
#endif /* _WIN32 */

    if(FM_SearchQueue == NULL)
      continue;

    Sch = (FM_PSearch)FM_SearchQueue->Data;
#ifdef __unix__
    sem_wait(&FM_MySem5);
#else /* __unix__ */
    WaitForSingleObject(FM_MySem5,INFINITE);
#endif /* __unix__ */
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
#ifdef __unix__
    sem_post(&FM_MySem5);
#else /* __unix__ */
    ReleaseSemaphore(FM_MySem5,1,NULL);
#endif /* __unix__ */
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

#ifdef __unix__
    sem_wait(&FM_MySem4);
#else /* __unix__ */
    WaitForSingleObject(FM_MySem4,INFINITE);
#endif /* __unix__ */
    FM_SearchQueue = SU_DelElementHead(FM_SearchQueue);
#ifdef __unix__
    sem_post(&FM_MySem4);
#else /* __unix__ */
    ReleaseSemaphore(FM_MySem4,1,NULL);
#endif /* __unix__ */
  }

#ifdef __unix__
  return 0;
#endif /* __unix__ */
}
