#include "master.h"

bool FM_ReadString(FILE *fp,char Buf[],int len)
{
  char c;
  int pos,res;
  bool writ;

  pos = 0;
  writ = true;
  res = fread(&c,1,1,fp);
  while(res == 1)
  {
    if(c == 0)
    {
      Buf[pos] = 0;
      return true;
    }
    if(pos >= (len-1))
      writ = false;
    if(writ)
    {
      Buf[pos] = c;
      pos++;
    }
    res = fread(&c,1,1,fp);
  }
  Buf[pos] = 0;
  return false;
}

SU_PList FM_LoadHosts(const char FileName[])
{
  FILE *fp;
  SU_PList Hosts = NULL;
  FM_PHost Hst;
  time_t OffSince;
  char Dom[FFSS_MAX_DOMAIN_LENGTH+1];
  char Name[FFSS_MAX_SERVERNAME_LENGTH+1];
  char OS[FFSS_MAX_SERVEROS_LENGTH+1];
  char Comment[FFSS_MAX_SERVERCOMMENT_LENGTH+1];
  char IP[200];
  char c;

#ifdef DEBUG
  printf("MASTER : Loading my local servers\n");
#endif /* DEBUG */
  fp = fopen(FileName,"rb");
  if(fp == NULL)
    return NULL;

  context;
  FM_ReadString(fp,Dom,sizeof(Dom));
  if(strcasecmp(FM_MyDomain.Name,Dom) != 0)
    return NULL;
  while(FM_ReadString(fp,Name,sizeof(Name)))
  {
    FM_ReadString(fp,OS,sizeof(OS));
    FM_ReadString(fp,Comment,sizeof(Comment));
    FM_ReadString(fp,IP,sizeof(IP));
    fscanf(fp,"%ld",&OffSince);
    fread(&c,1,1,fp); /* Zaps ending \0 */
    context;
    Hst = (FM_PHost) malloc(sizeof(FM_THost));
    memset(Hst,0,sizeof(FM_THost));
    Hst->Name = strdup(Name);
    Hst->OS = strdup(OS);
    Hst->Comment = strdup(Comment);
    Hst->IP = strdup(IP);
    Hst->OffSince = OffSince;
    Hst->LastPong = time(NULL);
    Hst->State = FFSS_STATE_OFF;

    Hosts = SU_AddElementHead(Hosts,Hst);
#ifdef DEBUG
    printf("\tLoading %s : %s (%s-%s)\n",Name,Comment,OS,IP);
#endif /* DEBUG */
  }
  return Hosts;
}

bool FM_SaveHosts(SU_PList Hosts,const char FileName[])
{
  FILE *fp;
  SU_PList Ptr;
  FM_PHost Hst;

  FFSS_PrintDebug(1,"Saving my local servers\n");
  fp = fopen(FileName,"wb");
  if(fp == NULL)
    return false;

  fprintf(fp,"%s%c",FM_MyDomain.Name,0);
  Ptr = Hosts;
  while(Ptr != NULL)
  {
    Hst = (FM_PHost) Ptr->Data;
    fprintf(fp,"%s%c%s%c%s%c%s%c%ld%c",Hst->Name,0,Hst->OS,0,Hst->Comment,0,Hst->IP,0,(Hst->OffSince == 0)?time(NULL):Hst->OffSince,0);
    Ptr = Ptr->Next;
  }
  fclose(fp);
  return true;
}
