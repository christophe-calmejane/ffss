
#define INTER_BOURRIN
#ifdef INTER_BOURRIN
#define _GNU_SOURCE
char *Cur_KW[50];
int Cur_Nb;
#endif /* INTER_BOURRIN */

#include "master.h"
#include "index.h"
#include <ctype.h>

void FM_ToUpper(char S[])
{
  int i,l;

  l = strlen(S);
  for(i=0;i<l;i++)
    S[i] = SU_toupper(S[i]);
}

/*
 * FM_BuildPath
 *   Builds an answer path from a Node, using the Host structure
 */
char *FM_BuildPath(FM_PFTControler Host, /* <-- Host struct              */
                   FM_PFTNode Node,      /* <-- Node to build path from  */
                   char Answer[],        /* --> Buffer to fill with path */
                   long int AnswerSize)  /* <-- Size of buffer           */
{
  int Indexes[1024];
  int NbIdx,i;

  Answer[0] = (char) Host->State;
  if(Host->Samba)
    Answer[0] |= (char) FFSS_SEARCH_IS_SAMBA;
  if(&Host->FTNodes[Node->Last] == Node)
    Answer[0] |= (char) FFSS_SEARCH_IS_FILE;
  Answer[1] = 0;

  NbIdx = 0;
  while(Node->Father != -1)
  {
    Indexes[NbIdx++] = Node->Pos;
    Node = &Host->FTNodes[Node->Father];
  }

  SU_strcat(Answer,Host->Name,AnswerSize);
  SU_strcat(Answer,"/",AnswerSize);
  SU_strcat(Answer,Host->FileTree+Node->Pos,AnswerSize);
  for(i=(NbIdx-1);i>=0;i--)
  {
    SU_strcat(Answer,"/",AnswerSize);
    SU_strcat(Answer,Host->FileTree+Indexes[i],AnswerSize);
  }
#ifdef INTER_BOURRIN
  if(Cur_Nb != 1)
  for(i=0;i<Cur_Nb;i++)
  {
    if(SU_nocasestrstr(Answer,Cur_KW[i]) == NULL)
    {
      Answer[0] = 0;
      return Answer;
    }
  }
#endif /* INTER_BOURRIN */
  return Answer;
}

char *FM_IntersectAnswers_rec(FM_PSTNode Node,    /* <-- Current Node              */
                              unsigned char Tags, /* <-- Tags of requested keyword */
                              char *Answers,      /* <-- Answer buffer             */
                              long int *BufSize,  /* <-> Buffer size               */
                              long int *BufPos,   /* <-> Buffer pos                */
                              long int *Count)    /* <-> Nb results                */
{
  char *Ans,*str;
  int i,j;
  FM_PSTLeaf Leaf;
  FM_PFTControler Host;
  long int buf_size,buf_pos,count,len;
  char StrBuf[1024];

  Ans = Answers;
  buf_size = *BufSize;
  buf_pos = *BufPos;
  count = *Count;
  //FFSS_PrintDebug(6,"Looking in node : %d sub nodes\n",Node->NbNodes);
  for(i=0;i<Node->NbNodes;i++)
    Ans = FM_IntersectAnswers_rec(&Node->STNodes[i],Tags,Ans,&buf_size,&buf_pos,&count);
  //FFSS_PrintDebug(6,"Looking in node : %d leafs\n",Node->NbLeafs);
  for(i=0;i<Node->NbLeafs;i++)
  {
    Leaf = &Node->STLeafs[i];
    Host = FM_Controler.Hosts[Leaf->NumHost];
    //FFSS_PrintDebug(6,"Looking in leaf : %d files\n",Leaf->NbFiles);
    for(j=0;j<Leaf->NbFiles;j++)
    {
      if((Host->FTNodes[Leaf->NumFiles[j]].Tags & Tags) == Tags)
      {
        //FFSS_PrintDebug(6,"Found file '%s'\n",Host->FileTree+Host->FTNodes[Leaf->NumFiles[j]].Pos);

        /* Flush Path */
        str = FM_BuildPath(Host,&Host->FTNodes[Leaf->NumFiles[j]],StrBuf,sizeof(StrBuf)); /* result string */
        len = strlen(str)+1;
#ifdef INTER_BOURRIN
        if(str[0] != 0)
        {
          bool do_it=true;
          if(Cur_Nb != 1)
            do_it = memmem(Ans,buf_pos,str,len-1) == NULL;
          if(do_it)
          {
#endif /* INTER_BOURRIN */
        while((buf_pos+len) >= buf_size)
        {
          buf_size += 50*FFSS_MAX_FILEPATH_LENGTH;
          Ans = (char *) realloc(Ans,buf_size);
        }
        memcpy(Ans+buf_pos,str,len);
        buf_pos += len;
        count++;
#ifdef INTER_BOURRIN
          }
        }
#endif /* INTER_BOURRIN */
      }
    }
  }
  *BufSize = buf_size;
  *BufPos = buf_pos;
  *Count = count;
  return Ans;
}

char *FM_IntersectAnswers(SU_PList STAnswers, /* <-- List of Nodes             */
                          unsigned char Tags, /* <-- Tags of requested keyword */
                          char *str,          /* <-- Answer buffer             */
                          long int *BufSize,  /* <-> Buffer size               */
                          long int *BufPos,   /* <-> Buffer pos                */
                          long int *Count)    /* <-> Nb results                */
{
  SU_PList Ptr;
  char *Ans;
  FM_PSTNode Node;

  Ptr = STAnswers;
  Ans = str;
  while(Ptr != NULL)
  {
    Node = (FM_PSTNode) Ptr->Data;
    Ans = FM_IntersectAnswers_rec(Node,Tags,Ans,BufSize,BufPos,Count);
    Ptr = Ptr->Next;
  }
  return Ans;
}

/* Returns a buffer to be sent then freed, or NULL if request failed */
char *FM_Search(FM_PSearch Sch,     /* <-- Search struct         */
                long int *size_out) /* --> Size of answer string */
{
  char KeyWords[1024];
  int i,key_pos,nb_sch;
  char *answer;
  long int pos,len,curr,dom,dom_pos,buf_size,pos_nb;
  SU_PList Ptr;
  FM_PDomain Dom;
  FM_PSTNode Node;
  SU_PList STAnswers;
  unsigned char Tags;
  bool done;
#ifdef DEBUG
  struct timeval t1,t2,t3;
  struct timezone tz={0,0};
  char tmp[1024];
#endif /* DEBUG */

  pos = 0;
  dom = 0;
  buf_size = FFSS_MAX_KEYWORDS_LENGTH*2+1;
  answer = (char *) malloc(buf_size);
  len = strlen(Sch->KeyWords)+1;
  if(len > FFSS_MAX_KEYWORDS_LENGTH)
    len = FFSS_MAX_KEYWORDS_LENGTH;
  SU_strcpy(answer+pos,Sch->KeyWords,len);
  pos += len;
  dom_pos = pos;
  pos += sizeof(FFSS_Field);
  SU_strcpy(KeyWords,Sch->KeyWords,sizeof(KeyWords));
  FM_ToUpper((char *)KeyWords);

  context;
  if(!Sch->Master)
  {
    /* First pass to forward search */
    Ptr = FM_Domains;
    while(Ptr != NULL)
    {
      Dom = (FM_PDomain) Ptr->Data;
      if(!SU_nocasestrwcmp(Dom->Name,Sch->Domain)) /* If domain not matching searched domains */
      {
        Ptr = Ptr->Next;
        continue;
      }
      if(!FM_IsMyDomain(Dom)) /* Not for my domain, forward request */
      {
        /* Forwarding request to foreign master */
        FFSS_PrintDebug(4,"Forwarding search \"%s\" to %s\n",KeyWords,Dom->Master);
        if(Dom->CS != NULL)
        {
          if(!FM_SendMessage_SearchForward(Dom->CS->sock,Sch->Client,Sch->Compressions,KeyWords))
            FFSS_PrintSyslog(LOG_WARNING,"Error forwarding search to %s (%d:%s)\n",Dom->Master,errno,strerror(errno));
        }
        Ptr = Ptr->Next;
        continue;
      }
      Ptr = Ptr->Next;
    }
  }

  context;
  /* Second pass to perform local search */
  done = false;
  Ptr = FM_Domains;
  while(Ptr != NULL)
  {
    Dom = (FM_PDomain) Ptr->Data;
    if(!SU_nocasestrwcmp(Dom->Name,Sch->Domain)) /* If domain not matching searched domains */
    {
      Ptr = Ptr->Next;
      continue;
    }
    if(!FM_IsMyDomain(Dom)) /* Not for my domain, forward request */
    {
      Ptr = Ptr->Next;
      continue;
    }

#ifdef DEBUG
    timerclear(&t1);
    timerclear(&t2);
    timerclear(&t3);
    gettimeofday(&t1,&tz);
#endif /* DEBUG */
    STAnswers = NULL;
    Tags = FFSS_FILE_TAGS_NOTHING;
    done = false;
    key_pos = 0;
    nb_sch = 0;
#ifdef INTER_BOURRIN
    Cur_Nb = 0;
#endif /* INTER_BOURRIN */
    while(!done)
    {
      i = 0;
      while(isalnum(KeyWords[key_pos+i]))
        i++;
      if(KeyWords[key_pos+i] == 0)
        done = true;
      KeyWords[key_pos+i] = 0;
      if(strlen(KeyWords+key_pos) < 4)
        Tags |= FFSS_GetWordTags(KeyWords+key_pos);
      else
      {
        context;
        nb_sch++;
#ifdef INTER_BOURRIN
        Cur_KW[Cur_Nb++] = KeyWords+key_pos;
#endif /* INTER_BOURRIN */
        Node = FMI_SearchKey(KeyWords+key_pos);
        if(Node != NULL)
          STAnswers = SU_AddElementHead(STAnswers,Node);
      }
      key_pos += i + 1;
    }

    if((STAnswers == NULL) && (nb_sch == 0)) /* No valid words */
    {
      FM_SendMessage_ErrorClient(Sch->Client,FFSS_ERROR_BAD_SEARCH_REQUEST,FFSS_ErrorTable[FFSS_ERROR_BAD_SEARCH_REQUEST]);
      free(answer);
      return NULL;
    }
    /* Flush Domain Name */
    len = strlen(Dom->Name)+1;
    while((pos+len) >= buf_size)
    {
      //FFSS_PrintDebug(5,"Having to realloc buffer of %ld to fit %ld\n",buf_size,pos+len);
      buf_size += FFSS_MAX_DOMAIN_LENGTH*2+1;
      answer = (char *) realloc(answer,buf_size);
    }
    if(len > FFSS_MAX_DOMAIN_LENGTH)
      len = FFSS_MAX_DOMAIN_LENGTH;
    SU_strcpy(answer+pos,Dom->Name,len);
    pos += len;
    dom++;
    pos_nb = pos;
    pos += sizeof(FFSS_Field); /* Skip Nb Results pos */
    curr = 0;

#ifdef DEBUG
    gettimeofday(&t2,&tz);
#endif /* DEBUG */
    context;
    answer = FM_IntersectAnswers(STAnswers,Tags,answer,&buf_size,&pos,&curr);
    if(curr > FM_TOO_MANY_ANSWERS_TRIGGER)
    {
      FM_SendMessage_ErrorClient(Sch->Client,FFSS_ERROR_TOO_MANY_ANSWERS,FFSS_ErrorTable[FFSS_ERROR_TOO_MANY_ANSWERS]);
      free(answer);
      return NULL;
    }
#ifdef DEBUG
    gettimeofday(&t3,&tz);
    snprintf(tmp,sizeof(tmp),"Search time : ST=%.3f msec INTER=%.3f msec (%ld answers)",((t2.tv_sec*1000000+t2.tv_usec)-(t1.tv_sec*1000000+t1.tv_usec))/1000.,((t3.tv_sec*1000000+t3.tv_usec)-(t2.tv_sec*1000000+t2.tv_usec))/1000.,curr);
    FFSS_PrintDebug(4,"%s\n",tmp);
    if(FM_SearchLogFile != NULL)
      SU_WriteToLogFile(FM_SearchLogFile,tmp);
#endif /* DEBUG */
    SU_FreeList(STAnswers);

    *(FFSS_Field *)(answer+pos_nb) = curr; /* Nb results */

    Ptr = Ptr->Next;
  }

  if(!done)
  {
    free(answer);
    return NULL;
  }
  *(FFSS_Field *)(answer+dom_pos) = dom;
  *size_out = pos;

  return answer;
}
