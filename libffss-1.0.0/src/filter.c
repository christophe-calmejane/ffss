#include "ffss.h"

typedef struct
{
  unsigned long IP;          /* IP base of the rule */
  unsigned long Mask;        /* Mask of the rule */
  char *IP_str;              /* String version of the IP */
  char *Mask_str;            /* String version of the mask */
  FFSS_FILTER_ACTION Action; /* Action to perform if rule matches */
  char *Name;                /* Optional name of the rule */
} FFSS_TRule, *FFSS_PRule;

typedef struct
{
  SU_PList Rules;             /* List of rules */ /* FFSS_PRule */
  char *Name;                 /* Name of the chain */
  FFSS_FILTER_ACTION Default; /* Default action if no rule matched */
} FFSS_TChain, *FFSS_PChain;

#define FFSS_FILTER_CHAINS_COUNT 3
FFSS_PChain FFSS_Chains[FFSS_FILTER_CHAINS_COUNT] = {NULL,};
SU_SEM_HANDLE FFSS_SemFilter;   /* Semaphore to protect the use of FFSS_Chains */
bool FFSS_Filter_Created = false;
FFSS_Filter_TApi FFSS_Filter_Api={0};

#define FFSS_FILTER_CHECK_CHAIN(x,y) if((x) >= FFSS_FILTER_CHAINS_COUNT) return (y);
#define FFSS_FILTER_CHECK_CHAIN_2(x,y) if(FFSS_Chains[x] == NULL) { SU_SEM_POST(FFSS_SemFilter);return (y); }
#define FFSS_FILTER_CHECK_INIT(x) if(!FFSS_Filter_Created) return (x);

/* FFSS Filter Internal Functions */
void FFSS_Filter_FreeRule(FFSS_PRule Rule)
{
  if(Rule == NULL)
    return;
  if(Rule->IP_str != NULL)
    free(Rule->IP_str);
  if(Rule->Mask_str != NULL)
    free(Rule->Mask_str);
  if(Rule->Name != NULL)
    free(Rule->Name);
  free(Rule);
}

FFSS_PRule FFSS_Filter_CreateRule(const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[])
{
  FFSS_PRule Rule;
  unsigned long ip;
  unsigned long mask;

  ip = inet_addr(IP);
  mask = inet_addr(Mask);
  if(ip == INADDR_NONE)
    return false;
  if(mask == INADDR_NONE)
  {
    if(strcmp(Mask,"255.255.255.255") != 0)
      return false;
  }
  Rule = (FFSS_PRule) malloc(sizeof(FFSS_TRule));
  memset(Rule,0,sizeof(FFSS_TRule));
  Rule->IP = ip;
  Rule->Mask = mask;
  Rule->IP_str = SU_strdup(IP);
  Rule->Mask_str = SU_strdup(Mask);
  Rule->Action = Action;
  Rule->Name = SU_strdup(Name);
  return Rule;
}

bool FFSS_Filter_CreateChain(FFSS_FILTER_CHAIN Chain,const char Name[],FFSS_FILTER_ACTION Default)
{
  FFSS_PChain Ch;

  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  if(FFSS_Chains[Chain] != NULL) /* Chain already exists */
  {
    SU_SEM_POST(FFSS_SemFilter);
    return false;
  }
  Ch = (FFSS_PChain) malloc(sizeof(FFSS_TChain));
  memset(Ch,0,sizeof(FFSS_TChain));
  FFSS_Chains[Chain] = Ch;
  Ch->Name = strdup(Name);
  Ch->Default = Default;
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_Init(FFSS_THREAD_TYPE ThreadType)
{
  if(FFSS_Filter_Created)
    return true;
  FFSS_Filter_Created = true;
  /* Create semaphore */
  if(!SU_CreateSem(&FFSS_SemFilter,1,1,"FFSSFilterSem"))
    return false;
  switch(ThreadType)
  {
    case FFSS_THREAD_SERVER :
      FFSS_Filter_CreateChain(FFSS_FILTER_CHAINS_SERVER_UDP_PACKET,"UDP Packet",FFSS_FILTER_ACTION_ACCEPT);
      FFSS_Filter_CreateChain(FFSS_FILTER_CHAINS_SERVER_TCP_CONNECTION,"TCP Connection",FFSS_FILTER_ACTION_ACCEPT);
      FFSS_Filter_CreateChain(FFSS_FILTER_CHAINS_SERVER_TCP_FTP_CONNECTION,"TCP FTP Connection",FFSS_FILTER_ACTION_ACCEPT);
      break;
    case FFSS_THREAD_CLIENT :
      FFSS_Filter_CreateChain(FFSS_FILTER_CHAINS_CLIENT_UDP_PACKET,"UDP Packet",FFSS_FILTER_ACTION_ACCEPT);
      break;
    case FFSS_THREAD_MASTER :
      FFSS_Filter_CreateChain(FFSS_FILTER_CHAINS_MASTER_UDP_PACKET,"UDP Packet",FFSS_FILTER_ACTION_ACCEPT);
      FFSS_Filter_CreateChain(FFSS_FILTER_CHAINS_MASTER_TCP_CONNECTION_MASTER,"TCP Connection Master",FFSS_FILTER_ACTION_ACCEPT);
      break;
    default :
      return false;
  }
  /* Init FFSS_Filter_Api struct */
  FFSS_Filter_Api.AddRuleToChain_Head = FFSS_Filter_AddRuleToChain_Head;
  FFSS_Filter_Api.AddRuleToChain_Tail = FFSS_Filter_AddRuleToChain_Tail;
  FFSS_Filter_Api.AddRuleToChain_Pos = FFSS_Filter_AddRuleToChain_Pos;
  FFSS_Filter_Api.SetDefaultActionOfChain = FFSS_Filter_SetDefaultActionOfChain;
  FFSS_Filter_Api.GetDefaultActionOfChain = FFSS_Filter_GetDefaultActionOfChain;
  FFSS_Filter_Api.DelRuleFromChain_Pos = FFSS_Filter_DelRuleFromChain_Pos;
  FFSS_Filter_Api.DelRuleFromChain_Name = FFSS_Filter_DelRuleFromChain_Name;
  FFSS_Filter_Api.ClearChain = FFSS_Filter_ClearChain;
  FFSS_Filter_Api.GetRuleOfChain_Pos = FFSS_Filter_GetRuleOfChain_Pos;
  FFSS_Filter_Api.GetRuleOfChain_Name = FFSS_Filter_GetRuleOfChain_Name;
  FFSS_Filter_Api.EnumChains = FFSS_Filter_EnumChains;
  FFSS_Filter_Api.EnumRulesOfChain = FFSS_Filter_EnumRulesOfChain;
  FFSS_Filter_Api.Initialized = true;
  return true;
}


/* FFSS Filter API */
bool FFSS_Filter_AddRuleToChain_Head(FFSS_FILTER_CHAIN Chain,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[])
{
  FFSS_PRule Rule;

  FFSS_FILTER_CHECK_INIT(false);
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  Rule = FFSS_Filter_CreateRule(IP,Mask,Action,Name);
  if(Rule == NULL)
    return false;
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,false);
  FFSS_Chains[Chain]->Rules = SU_AddElementHead(FFSS_Chains[Chain]->Rules,Rule);
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_AddRuleToChain_Tail(FFSS_FILTER_CHAIN Chain,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[])
{
  FFSS_PRule Rule;

  FFSS_FILTER_CHECK_INIT(false);
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  Rule = FFSS_Filter_CreateRule(IP,Mask,Action,Name);
  if(Rule == NULL)
    return false;
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,false);
  FFSS_Chains[Chain]->Rules = SU_AddElementTail(FFSS_Chains[Chain]->Rules,Rule);
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_AddRuleToChain_Pos(FFSS_FILTER_CHAIN Chain,unsigned int Pos,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[])
{
  FFSS_PRule Rule;

  FFSS_FILTER_CHECK_INIT(false);
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  Rule = FFSS_Filter_CreateRule(IP,Mask,Action,Name);
  if(Rule == NULL)
    return false;
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,false);
  FFSS_Chains[Chain]->Rules = SU_AddElementPos(FFSS_Chains[Chain]->Rules,Pos,Rule);
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_SetDefaultActionOfChain(FFSS_FILTER_CHAIN Chain,FFSS_FILTER_ACTION Action)
{
  FFSS_FILTER_CHECK_INIT(false);
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,false);
  FFSS_Chains[Chain]->Default = Action;
  return true;
}

bool FFSS_Filter_GetDefaultActionOfChain(FFSS_FILTER_CHAIN Chain,FFSS_FILTER_ACTION *Action)
{
  FFSS_FILTER_CHECK_INIT(false);
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,false);
  if(Action != NULL)
    *Action = FFSS_Chains[Chain]->Default;
  return true;
}

bool FFSS_Filter_DelRuleFromChain_Pos(FFSS_FILTER_CHAIN Chain,unsigned int Pos)
{
  FFSS_FILTER_CHECK_INIT(false);
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,false);
  if(Pos >= SU_ListCount(FFSS_Chains[Chain]->Rules))
  {
    SU_SEM_POST(FFSS_SemFilter);
    return false;
  }
  FFSS_Filter_FreeRule(SU_GetElementPos(FFSS_Chains[Chain]->Rules,Pos));
  FFSS_Chains[Chain]->Rules = SU_DelElementPos(FFSS_Chains[Chain]->Rules,Pos);
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_DelRuleFromChain_Name(FFSS_FILTER_CHAIN Chain,const char Name[])
{
  SU_PList Ptr;
  FFSS_PRule Rule;

  if(Name == NULL)
    return false;
  FFSS_FILTER_CHECK_INIT(false);
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,false);
  Ptr = FFSS_Chains[Chain]->Rules;
  while(Ptr != NULL)
  {
    Rule = (FFSS_PRule) Ptr->Data;
    if(Rule->Name != NULL)
    {
      if(strcmp(Rule->Name,Name) == 0) /* Found it */
      {
        FFSS_Filter_FreeRule(Rule);
        SU_SEM_POST(FFSS_SemFilter);
        return true;
      }
    }
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FFSS_SemFilter);
  return false;
}

bool FFSS_Filter_ClearChain(FFSS_FILTER_CHAIN Chain)
{
  SU_PList Ptr;

  FFSS_FILTER_CHECK_INIT(false);
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,false);
  Ptr = FFSS_Chains[Chain]->Rules;
  while(Ptr != NULL)
  {
    FFSS_Filter_FreeRule(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(FFSS_Chains[Chain]->Rules);
  FFSS_Chains[Chain]->Rules = NULL;
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_GetRuleOfChain_Pos(FFSS_FILTER_CHAIN Chain,unsigned int Pos,char **IP,char **Mask,FFSS_FILTER_ACTION *Action,char **Name)
{
  FFSS_PRule Rule;

  FFSS_FILTER_CHECK_INIT(false);
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,false);
  if(Pos >= SU_ListCount(FFSS_Chains[Chain]->Rules))
  {
    SU_SEM_POST(FFSS_SemFilter);
    return false;
  }
  Rule = (FFSS_PRule) SU_GetElementPos(FFSS_Chains[Chain]->Rules,Pos);
  if(Rule == NULL)
  {
    SU_SEM_POST(FFSS_SemFilter);
    return false;
  }
  if(IP != NULL)
    *IP = SU_strdup(Rule->IP_str);
  if(Mask != NULL)
    *Mask = SU_strdup(Rule->Mask_str);
  if(Action != NULL)
    *Action = Rule->Action;
  if(Name != NULL)
    *Name = SU_strdup(Rule->Name);
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_GetRuleOfChain_Name(FFSS_FILTER_CHAIN Chain,const char Name[],char **IP,char **Mask,FFSS_FILTER_ACTION *Action)
{
  SU_PList Ptr;
  FFSS_PRule Rule;

  if(Name == NULL)
    return false;
  FFSS_FILTER_CHECK_INIT(false);
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,false);
  Ptr = FFSS_Chains[Chain]->Rules;
  while(Ptr != NULL)
  {
    Rule = (FFSS_PRule) Ptr->Data;
    if(Rule->Name != NULL)
    {
      if(strcmp(Rule->Name,Name) == 0) /* Found it */
      {
        if(IP != NULL)
          *IP = SU_strdup(Rule->IP_str);
        if(Mask != NULL)
          *Mask = SU_strdup(Rule->Mask_str);
        if(Action != NULL)
          *Action = Rule->Action;
        SU_SEM_POST(FFSS_SemFilter);
        return true;
      }
    }
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FFSS_SemFilter);
  return false;
}

bool FFSS_Filter_EnumChains(FFSS_FILTER_CHAINS_ENUM_CB EnumCB)
{
  int i;

  if(EnumCB == NULL)
    return false;
  FFSS_FILTER_CHECK_INIT(false);
  SU_SEM_WAIT(FFSS_SemFilter);
  for(i=0;i<FFSS_FILTER_CHAINS_COUNT;i++)
  {
    if(FFSS_Chains[i] != NULL)
      EnumCB(i,FFSS_Chains[i]->Name,FFSS_Chains[i]->Default);
  }
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_EnumRulesOfChain(FFSS_FILTER_CHAIN Chain,FFSS_FILTER_RULES_ENUM_CB EnumCB)
{
  SU_PList Ptr;
  FFSS_PRule Rule;

  if(EnumCB == NULL)
    return false;
  FFSS_FILTER_CHECK_INIT(false);
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,false);
  Ptr = FFSS_Chains[Chain]->Rules;
  while(Ptr != NULL)
  {
    Rule = (FFSS_PRule) Ptr->Data;
    EnumCB(Rule->IP_str,Rule->Mask_str,Rule->Action,Rule->Name);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

FFSS_FILTER_ACTION FFSS_Filter_GetActionOfChainFromIP(FFSS_FILTER_CHAIN Chain,unsigned long IP)
{
  SU_PList Ptr;
  FFSS_PRule Rule;
  FFSS_FILTER_ACTION res;

  if(Chain >= FFSS_FILTER_CHAINS_COUNT)
    return FFSS_FILTER_ACTION_REJECT;

  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_FILTER_CHECK_CHAIN_2(Chain,FFSS_FILTER_ACTION_REJECT);
  res = FFSS_Chains[Chain]->Default; /* If nothing matches... return default */
  Ptr = FFSS_Chains[Chain]->Rules;
  while(Ptr != NULL)
  {
    Rule = (FFSS_PRule) Ptr->Data;
    if(((IP & Rule->Mask) & Rule->IP) == Rule->IP) /* Match */
    {
      res = Rule->Action;
      break;
    }
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FFSS_SemFilter);

  return res; 
}
