/*
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
#include <ffss/ffss.h>
#include "common.h"

typedef struct
{
  FFSS_Field IP;             /* IP base of the rule */
	FFSS_Field Mask;           /* Mask of the rule */
  char *IP_str;                 /* String version of the IP */
  char *Mask_str;               /* String version of the mask */
  FFSS_QOS_CRITERIA Criteria;   /* Criteria to use if rule matches */
  FFSS_QOS_VALUE Value;         /* Criteria's value */
  char *Name;                   /* Optional name of the rule */
  FFSS_LongField Throughput; /* Current throughput (bytes/msec) */
} FFSS_TQosRule, *FFSS_PQosRule;

typedef struct
{
  SU_PList Rules;             /* List of rules */ /* FFSS_PQosRule */
  char *Name;                 /* Name of the chain */
} FFSS_TQosChain, *FFSS_PQosChain;


#define FFSS_QOS_CHAINS_COUNT 3
FFSS_PQosChain FFSS_QosChains[FFSS_QOS_CHAINS_COUNT] = {NULL,};
SU_SEM_HANDLE FFSS_SemQos;   /* Semaphore to protect the use of FFSS_QosChains */
bool FFSS_Qos_Created = false;
FFSS_QoS_TApi FFSS_QoS_Api={0,};
SU_CPUSPEED FFSS_CpuSpeed = 0;
FFSS_PQosConn FFSS_QosConns[FFSS_MAX_SOCKETS] = {NULL,};

#define FFSS_QOS_CHECK_CHAIN(x,y) if((x) >= FFSS_QOS_CHAINS_COUNT) return (y);
#define FFSS_QOS_CHECK_CHAIN_2(x,y) if(FFSS_QosChains[x] == NULL) { SU_SEM_POST(FFSS_SemQos);return (y); }
#define FFSS_QOS_CHECK_INIT(x) if(!FFSS_Qos_Created) return (x);

/* FFSS QoS Internal Functions */
void FFSS_QoS_FreeRule(FFSS_PQosRule Rule)
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

FFSS_PQosRule FFSS_QoS_CreateRule(const char IP[],const char Mask[],FFSS_QOS_CRITERIA Criteria,FFSS_QOS_VALUE Value,const char Name[])
{
  FFSS_PQosRule Rule;
	FFSS_Field ip;
	FFSS_Field mask;

  ip = inet_addr(IP);
  mask = inet_addr(Mask);
  if(ip == INADDR_NONE)
    return NULL;
  if(mask == INADDR_NONE)
  {
    if(strcmp(Mask,"255.255.255.255") != 0)
      return NULL;
  }
  switch(Criteria)
  {
    case FFSS_QOS_CRITERIA_BANDWIDTH_PER_CENT :
      if(Value > 100)
        return NULL;
      break;
  }
  Rule = (FFSS_PQosRule) malloc(sizeof(FFSS_TQosRule));
  memset(Rule,0,sizeof(FFSS_TQosRule));
  Rule->IP = ip;
  Rule->Mask = mask;
  Rule->IP_str = SU_strdup(IP);
  Rule->Mask_str = SU_strdup(Mask);
  Rule->Criteria = Criteria;
  Rule->Value = Value;
  Rule->Name = SU_strdup(Name);
  return Rule;
}

bool FFSS_QoS_CreateChain(FFSS_QOS_CHAIN Chain,const char Name[])
{
  FFSS_PQosChain Ch;

  FFSS_QOS_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemQos);
  if(FFSS_QosChains[Chain] != NULL) /* Chain already exists */
  {
    SU_SEM_POST(FFSS_SemQos);
    return false;
  }
  Ch = (FFSS_PQosChain) malloc(sizeof(FFSS_TQosChain));
  memset(Ch,0,sizeof(FFSS_TQosChain));
  FFSS_QosChains[Chain] = Ch;
  Ch->Name = strdup(Name);
  SU_SEM_POST(FFSS_SemQos);
  return true;
}

static SU_THREAD_RET_TYPE threadwork_ret_zero = 0;

SU_THREAD_ROUTINE(FFSS_CPUSpeedCompute, Info)
{
 FFSS_CpuSpeed = SU_GetCPUSpeed();
 SU_END_THREAD(threadwork_ret_zero);
 SU_THREAD_RETURN(0);
}

bool FFSS_QoS_Init(FFSS_LongField BandWidth)
{
  SU_THREAD_HANDLE Thread;
	SU_THREAD_ID ThreadId;

  if(FFSS_Qos_Created)
    return true;
  FFSS_Qos_Created = true;
  /* Create semaphore */
  if(!SU_CreateSem(&FFSS_SemQos,1,1,"FFSSQosSem"))
    return false;

  if(!SU_CreateThread(&Thread,&ThreadId,FFSS_CPUSpeedCompute,NULL,true))
    return false;

  FFSS_QoS_CreateChain(FFSS_QOS_CHAINS_TRAFFIC_UPLOAD,"Upload Traffic");
  FFSS_QoS_CreateChain(FFSS_QOS_CHAINS_TRAFFIC_DOWNLOAD,"Download Traffic");
  FFSS_QoS_CreateChain(FFSS_QOS_CHAINS_TRAFFIC_GLOBAL,"Global Traffic");

  /* Init FFSS_QoS_Api struct */
  FFSS_QoS_Api.AddRuleToChain_Head = FFSS_QoS_AddRuleToChain_Head;
  FFSS_QoS_Api.AddRuleToChain_Tail = FFSS_QoS_AddRuleToChain_Tail;
  FFSS_QoS_Api.AddRuleToChain_Pos = FFSS_QoS_AddRuleToChain_Pos;
  FFSS_QoS_Api.DelRuleFromChain_Pos = FFSS_QoS_DelRuleFromChain_Pos;
  FFSS_QoS_Api.DelRuleFromChain_Name = FFSS_QoS_DelRuleFromChain_Name;
  FFSS_QoS_Api.ClearChain = FFSS_QoS_ClearChain;
  FFSS_QoS_Api.GetRuleOfChain_Pos = FFSS_QoS_GetRuleOfChain_Pos;
  FFSS_QoS_Api.GetRuleOfChain_Name = FFSS_QoS_GetRuleOfChain_Name;
  FFSS_QoS_Api.EnumChains = FFSS_QoS_EnumChains;
  FFSS_QoS_Api.EnumRulesOfChain = FFSS_QoS_EnumRulesOfChain;
  FFSS_QoS_Api.Initialized = true;
  FFSS_QoS_Api.BandWidth = BandWidth;
  return true;
}

/* FFSS QoS API */
bool FFSS_QoS_AddRuleToChain_Head(FFSS_QOS_CHAIN Chain,const char IP[],const char Mask[],FFSS_QOS_CRITERIA Criteria,FFSS_QOS_VALUE Value,const char Name[])
{
  FFSS_PQosRule Rule;

  FFSS_QOS_CHECK_INIT(false);
  FFSS_QOS_CHECK_CHAIN(Chain,false);
  Rule = FFSS_QoS_CreateRule(IP,Mask,Criteria,Value,Name);
  if(Rule == NULL)
    return false;
  SU_SEM_WAIT(FFSS_SemQos);
  FFSS_QOS_CHECK_CHAIN_2(Chain,false);
  FFSS_QosChains[Chain]->Rules = SU_AddElementHead(FFSS_QosChains[Chain]->Rules,Rule);
  SU_SEM_POST(FFSS_SemQos);
  return true;
}

bool FFSS_QoS_AddRuleToChain_Tail(FFSS_QOS_CHAIN Chain,const char IP[],const char Mask[],FFSS_QOS_CRITERIA Criteria,FFSS_QOS_VALUE Value,const char Name[])
{
  FFSS_PQosRule Rule;

  FFSS_QOS_CHECK_INIT(false);
  FFSS_QOS_CHECK_CHAIN(Chain,false);
  Rule = FFSS_QoS_CreateRule(IP,Mask,Criteria,Value,Name);
  if(Rule == NULL)
    return false;
  SU_SEM_WAIT(FFSS_SemQos);
  FFSS_QOS_CHECK_CHAIN_2(Chain,false);
  FFSS_QosChains[Chain]->Rules = SU_AddElementTail(FFSS_QosChains[Chain]->Rules,Rule);
  SU_SEM_POST(FFSS_SemQos);
  return true;
}

bool FFSS_QoS_AddRuleToChain_Pos(FFSS_QOS_CHAIN Chain,unsigned int Pos,const char IP[],const char Mask[],FFSS_QOS_CRITERIA Criteria,FFSS_QOS_VALUE Value,const char Name[])
{
  FFSS_PQosRule Rule;

  FFSS_QOS_CHECK_INIT(false);
  FFSS_QOS_CHECK_CHAIN(Chain,false);
  Rule = FFSS_QoS_CreateRule(IP,Mask,Criteria,Value,Name);
  if(Rule == NULL)
    return false;
  SU_SEM_WAIT(FFSS_SemQos);
  FFSS_QOS_CHECK_CHAIN_2(Chain,false);
  FFSS_QosChains[Chain]->Rules = SU_AddElementPos(FFSS_QosChains[Chain]->Rules,Pos,Rule);
  SU_SEM_POST(FFSS_SemQos);
  return true;
}

bool FFSS_QoS_DelRuleFromChain_Pos(FFSS_QOS_CHAIN Chain,unsigned int Pos)
{
  FFSS_QOS_CHECK_INIT(false);
  FFSS_QOS_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemQos);
  FFSS_QOS_CHECK_CHAIN_2(Chain,false);
  if(Pos >= SU_ListCount(FFSS_QosChains[Chain]->Rules))
  {
    SU_SEM_POST(FFSS_SemQos);
    return false;
  }
  FFSS_QoS_FreeRule((FFSS_TQosRule *)SU_GetElementPos(FFSS_QosChains[Chain]->Rules,Pos));
  FFSS_QosChains[Chain]->Rules = SU_DelElementPos(FFSS_QosChains[Chain]->Rules,Pos);
  SU_SEM_POST(FFSS_SemQos);
  return true;
}

bool FFSS_QoS_DelRuleFromChain_Name(FFSS_QOS_CHAIN Chain,const char Name[])
{
  SU_PList Ptr;
  FFSS_PQosRule Rule;

  if(Name == NULL)
    return false;
  FFSS_QOS_CHECK_INIT(false);
  FFSS_QOS_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemQos);
  FFSS_QOS_CHECK_CHAIN_2(Chain,false);
  Ptr = FFSS_QosChains[Chain]->Rules;
  while(Ptr != NULL)
  {
    Rule = (FFSS_PQosRule) Ptr->Data;
    if(Rule->Name != NULL)
    {
      if(strcmp(Rule->Name,Name) == 0) /* Found it */
      {
        FFSS_QoS_FreeRule(Rule);
        SU_SEM_POST(FFSS_SemQos);
        return true;
      }
    }
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FFSS_SemQos);
  return false;
}

bool FFSS_QoS_ClearChain(FFSS_QOS_CHAIN Chain)
{
  SU_PList Ptr;

  FFSS_QOS_CHECK_INIT(false);
  FFSS_QOS_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemQos);
  FFSS_QOS_CHECK_CHAIN_2(Chain,false);
  Ptr = FFSS_QosChains[Chain]->Rules;
  while(Ptr != NULL)
  {
    FFSS_QoS_FreeRule((FFSS_TQosRule *)Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(FFSS_QosChains[Chain]->Rules);
  FFSS_QosChains[Chain]->Rules = NULL;
  SU_SEM_POST(FFSS_SemQos);
  return true;
}

bool FFSS_QoS_GetRuleOfChain_Pos(FFSS_QOS_CHAIN Chain,unsigned int Pos,char **IP,char **Mask,FFSS_QOS_CRITERIA *Criteria,FFSS_QOS_VALUE *Value,char **Name)
{
  FFSS_PQosRule Rule;

  FFSS_QOS_CHECK_INIT(false);
  FFSS_QOS_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemQos);
  FFSS_QOS_CHECK_CHAIN_2(Chain,false);
  if(Pos >= SU_ListCount(FFSS_QosChains[Chain]->Rules))
  {
    SU_SEM_POST(FFSS_SemQos);
    return false;
  }
  Rule = (FFSS_PQosRule) SU_GetElementPos(FFSS_QosChains[Chain]->Rules,Pos);
  if(Rule == NULL)
  {
    SU_SEM_POST(FFSS_SemQos);
    return false;
  }
  if(IP != NULL)
    *IP = SU_strdup(Rule->IP_str);
  if(Mask != NULL)
    *Mask = SU_strdup(Rule->Mask_str);
  if(Criteria != NULL)
    *Criteria = Rule->Criteria;
  if(Value != NULL)
    *Value = Rule->Value;
  if(Name != NULL)
    *Name = SU_strdup(Rule->Name);
  SU_SEM_POST(FFSS_SemQos);
  return true;
}

bool FFSS_QoS_GetRuleOfChain_Name(FFSS_QOS_CHAIN Chain,const char Name[],char **IP,char **Mask,FFSS_QOS_CRITERIA *Criteria,FFSS_QOS_VALUE *Value)
{
  SU_PList Ptr;
  FFSS_PQosRule Rule;

  if(Name == NULL)
    return false;
  FFSS_QOS_CHECK_INIT(false);
  FFSS_QOS_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemQos);
  FFSS_QOS_CHECK_CHAIN_2(Chain,false);
  Ptr = FFSS_QosChains[Chain]->Rules;
  while(Ptr != NULL)
  {
    Rule = (FFSS_PQosRule) Ptr->Data;
    if(Rule->Name != NULL)
    {
      if(strcmp(Rule->Name,Name) == 0) /* Found it */
      {
        if(IP != NULL)
          *IP = SU_strdup(Rule->IP_str);
        if(Mask != NULL)
          *Mask = SU_strdup(Rule->Mask_str);
        if(Criteria != NULL)
          *Criteria = Rule->Criteria;
        if(Value  != NULL)
          *Value = Rule->Value;
        SU_SEM_POST(FFSS_SemQos);
        return true;
      }
    }
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FFSS_SemQos);
  return false;
}

bool FFSS_QoS_EnumChains(FFSS_QOS_CHAINS_ENUM_CB EnumCB)
{
  int i;

  if(EnumCB == NULL)
    return false;
  FFSS_QOS_CHECK_INIT(false);
  SU_SEM_WAIT(FFSS_SemQos);
  for(i=0;i<FFSS_QOS_CHAINS_COUNT;i++)
  {
    if(FFSS_QosChains[i] != NULL)
      EnumCB(i,FFSS_QosChains[i]->Name);
  }
  SU_SEM_POST(FFSS_SemQos);
  return true;
}

bool FFSS_QoS_EnumRulesOfChain(FFSS_QOS_CHAIN Chain,FFSS_QOS_RULES_ENUM_CB EnumCB)
{
  SU_PList Ptr;
  FFSS_PQosRule Rule;

  if(EnumCB == NULL)
    return false;
  FFSS_QOS_CHECK_INIT(false);
  FFSS_QOS_CHECK_CHAIN(Chain,false);
  SU_SEM_WAIT(FFSS_SemQos);
  FFSS_QOS_CHECK_CHAIN_2(Chain,false);
  Ptr = FFSS_QosChains[Chain]->Rules;
  while(Ptr != NULL)
  {
    Rule = (FFSS_PQosRule) Ptr->Data;
    EnumCB(Rule->IP_str,Rule->Mask_str,Rule->Criteria,Rule->Value,Rule->Name);
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FFSS_SemQos);
  return true;
}

FFSS_Field FFSS_QoS_UpdateRate(FFSS_QOS_CHAIN Chain, FFSS_Field IP, FFSS_SLongField ThroughputDelta, FFSS_LongField TimeDelta)
{
  SU_PList Ptr;
  FFSS_PQosRule Rule;
	FFSS_LongField value = 1;
	FFSS_Field ret = 0;

  if(Chain >= FFSS_QOS_CHAINS_COUNT)
    return 0;

  SU_SEM_WAIT(FFSS_SemQos);
  FFSS_QOS_CHECK_CHAIN_2(Chain,0);
  Ptr = FFSS_QosChains[Chain]->Rules;
  while(Ptr != NULL)
  {
    Rule = (FFSS_PQosRule) Ptr->Data;
    if(((IP & Rule->Mask) & Rule->IP) == Rule->IP) /* Match */
    {
      Rule->Throughput += ThroughputDelta; /* Update throughput */
      switch(Rule->Criteria)
      {
        case FFSS_QOS_CRITERIA_BYTES_PER_MSEC :
          value = Rule->Value;
          break;
        case FFSS_QOS_CRITERIA_BANDWIDTH_PER_CENT :
					value = FFSS_QoS_Api.BandWidth / 100 * Rule->Value;
          break;
      }
      if(Rule->Throughput > value)
      {
				ret = (FFSS_Field)(MIN(32000,(Rule->Throughput - value) / (float)value * TimeDelta)); /* Nb msec to wait */
        SU_DBG_PrintDebug(FFSS_DBGMSG_GLOBAL,"QoS Engine : Quota excedeed... sleeping for %d msec",ret);
      }
      break;
    }
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FFSS_SemQos);

  return ret;
}

