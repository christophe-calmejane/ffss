#include "ffss.h"

#define FFSS_FILTER_CHAINS_COUNT 3
SU_PList FFSS_Chains[FFSS_FILTER_CHAINS_COUNT] = {NULL,}; /* FFSS_PRule */
SU_SEM_HANDLE FFSS_SemFilter;   /* Semaphore to protect the use of FFSS_Chains */
bool FFSS_Filter_Created = false;

#define FFSS_FILTER_CHECK_CHAIN(x,y) if((x) >= FFSS_FILTER_CHAINS_COUNT) return (y);
#define FFSS_FILTER_CHECK_INIT if(!FFSS_Filter_Created) { if(!FFSS_Filter_Init()) return false; }

typedef struct
{
  unsigned long IP;          /* IP base of the rule */
  unsigned long Mask;        /* Mask of the rule */
  char *IP_str;              /* String version of the IP */
  char *Mask_str;            /* String version of the mask */
  FFSS_FILTER_ACTION Action; /* Action to perform if rule matches */
  char *Name;                /* Optional name of the rule */
} FFSS_TRule, *FFSS_PRule;

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

bool FFSS_Filter_Init(void)
{
  if(FFSS_Filter_Created)
    return true;
  FFSS_Filter_Created = true;
  /* Create semaphore */
  if(!SU_CreateSem(&FFSS_SemFilter,1,1,"FFSSFilterSem"))
    return false;
  return true;
}


/* FFSS Filter API */
bool FFSS_Filter_AddRuleToChain_Head(FFSS_FILTER_CHAIN Chain,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[])
{
  FFSS_PRule Rule;

  FFSS_FILTER_CHECK_INIT;
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  Rule = FFSS_Filter_CreateRule(IP,Mask,Action,Name);
  if(Rule == NULL)
    return false;
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_Chains[Chain] = SU_AddElementHead(FFSS_Chains[Chain],Rule);
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_AddRuleToChain_Tail(FFSS_FILTER_CHAIN Chain,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[])
{
  FFSS_PRule Rule;

  FFSS_FILTER_CHECK_INIT;
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  Rule = FFSS_Filter_CreateRule(IP,Mask,Action,Name);
  if(Rule == NULL)
    return false;
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_Chains[Chain] = SU_AddElementTail(FFSS_Chains[Chain],Rule);
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_AddRuleToChain_Pos(FFSS_FILTER_CHAIN Chain,unsigned int Pos,const char IP[],const char Mask[],FFSS_FILTER_ACTION Action,const char Name[])
{
  FFSS_PRule Rule;

  FFSS_FILTER_CHECK_INIT;
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  Rule = FFSS_Filter_CreateRule(IP,Mask,Action,Name);
  if(Rule == NULL)
    return false;
  SU_SEM_WAIT(FFSS_SemFilter);
  FFSS_Chains[Chain] = SU_AddElementPos(FFSS_Chains[Chain],Pos,Rule);
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_AddDefaultRuleToChain(FFSS_FILTER_CHAIN Chain,FFSS_FILTER_ACTION Action)
{
  FFSS_FILTER_CHECK_INIT;
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  return FFSS_Filter_AddRuleToChain_Tail(Chain,"0.0.0.0","255.255.255.255",Action,"Default Rule Of Chain");
}

bool FFSS_Filter_DelRuleFromChain_Pos(FFSS_FILTER_CHAIN Chain,unsigned int Pos)
{
  FFSS_FILTER_CHECK_INIT;
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  if(Pos >= SU_ListCount(FFSS_Chains[Chain]))
  {
    SU_SEM_POST(FFSS_SemFilter);
    return false;
  }
  FFSS_Filter_FreeRule(SU_GetElementPos(FFSS_Chains[Chain],Pos));
  FFSS_Chains[Chain] = SU_DelElementPos(FFSS_Chains[Chain],Pos);
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_DelRuleFromChain_Name(FFSS_FILTER_CHAIN Chain,const char Name[])
{
  SU_PList Ptr;
  FFSS_PRule Rule;

  if(Name == NULL)
    return false;
  FFSS_FILTER_CHECK_INIT;
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  Ptr = FFSS_Chains[Chain];
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

  FFSS_FILTER_CHECK_INIT;
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  Ptr = FFSS_Chains[Chain];
  while(Ptr != NULL)
  {
    FFSS_Filter_FreeRule(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(FFSS_Chains[Chain]);
  FFSS_Chains[Chain] = NULL;
  SU_SEM_POST(FFSS_SemFilter);
  return true;
}

bool FFSS_Filter_GetRuleOfChain_Pos(FFSS_FILTER_CHAIN Chain,unsigned int Pos,char **IP,char **Mask,FFSS_FILTER_ACTION *Action,char **Name)
{
  FFSS_PRule Rule;

  FFSS_FILTER_CHECK_INIT;
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  if(Pos >= SU_ListCount(FFSS_Chains[Chain]))
  {
    SU_SEM_POST(FFSS_SemFilter);
    return false;
  }
  Rule = (FFSS_PRule) SU_GetElementPos(FFSS_Chains[Chain],Pos);
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
  FFSS_FILTER_CHECK_INIT;
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  Ptr = FFSS_Chains[Chain];
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

bool FFSS_Filter_EnumRulesOfChain(FFSS_FILTER_CHAIN Chain,FFSS_RULES_ENUM_CB EnumCB)
{
  SU_PList Ptr;
  FFSS_PRule Rule;

  if(EnumCB == NULL)
    return false;
  FFSS_FILTER_CHECK_INIT;
  FFSS_FILTER_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemFilter);
  Ptr = FFSS_Chains[Chain];
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

  if(Chain >= FFSS_FILTER_CHAINS_COUNT)
    return FFSS_FILTER_ACTION_REJECT;

  SU_SEM_WAIT(FFSS_SemFilter);
  Ptr = FFSS_Chains[Chain];
  while(Ptr != NULL)
  {
    Rule = (FFSS_PRule) Ptr->Data;
    if(((IP & Rule->Mask) & Rule->IP) == Rule->IP) /* Match */
    {
      SU_SEM_POST(FFSS_SemFilter);
      return Rule->Action;
    }
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FFSS_SemFilter);

  return FFSS_FILTER_ACTION_ACCEPT; /* Nothing matched... no default rule... accept */
}
