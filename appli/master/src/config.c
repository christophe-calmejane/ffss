#include "master.h"

bool FM_LoadConfigFile(const char FileName[],bool UserGroup)
{
  static FILE *fp;
  char Name[256],Value[256],*Nam,*Ip,*V,*User=NULL,*Group=NULL;
  char *Int = NULL;
  FM_PDomain Domain;

  context;
  if(UserGroup)
  {
    fp = fopen(FileName,"rt");
    if(fp == NULL)
      return 0;
  }

  while(SU_ParseConfig(fp,Name,sizeof(Name),Value,sizeof(Value)))
  {
    if(UserGroup)
    {
      if(strcasecmp(Name,"User") == 0)
        User = strdup(Value);
      if(strcasecmp(Name,"Group") == 0)
        Group = strdup(Value);
    }
    else
    {
      if(strcasecmp(Name,"MyIP") == 0)
      {
        context;
        if(Value[0] == 0)
        {
          FFSS_PrintSyslog(LOG_WARNING,"Config Loader : Missing parameter for MyIP Line\n");
          continue;
        }
        FM_MyDomain.Master = strdup(Value);
      }
      else if(strcasecmp(Name,"Master") == 0)
      {
        context;
        Nam = strtok(Value,",");
        V = strtok(NULL,",");
        if((Nam == NULL) || (V == NULL))
        {
          FFSS_PrintSyslog(LOG_WARNING,"Config Loader : Missing parameter for Master Line : %s %s,%s\n",Name,Nam,V);
          continue;
        }
        Ip = SU_AdrsOfPort(V);
        if(Ip == NULL)
        {
          FFSS_PrintSyslog(LOG_WARNING,"Config Loader : Cannot find ip for %s\n",V);
          continue;
        }
        Domain = (FM_PDomain) malloc(sizeof(FM_TDomain));
        memset(Domain,0,sizeof(FM_TDomain));
        Domain->Name = strdup(Nam);
        Domain->Master = strdup(Ip);
#ifdef DEBUG
        printf("Config Loader Info : Adding master %s for domain %s\n",Ip,Nam);
#endif /* DEBUG */
        FM_Domains = SU_AddElementHead(FM_Domains,Domain);
        context;
        FM_SendMessage_ServerList(Ip);
      }
      else if(strcasecmp(Name,"Domain") == 0)
        FM_MyDomain.Name = strdup(Value);
      else if(strcasecmp(Name,"Interface") == 0)
        Int = strdup(Value);
    }
  }

  if(UserGroup)
    rewind(fp);
  else
    fclose(fp);

  if(UserGroup == false)
  {
    context;
    if(FM_MyDomain.Name == NULL)
    {
      FFSS_PrintSyslog(LOG_ERR,"Config Loader : No domain specified for local domain\n");
      return false;
    }
    if(FM_MyDomain.Master == NULL)
    {
      context;
      if(Int == NULL)
      {
        FFSS_PrintSyslog(LOG_WARNING,"Config Loader : No network interface defined... using eth0\n");
        Int = strdup("eth0");
      }
      if(!FFSS_GetMyIP(FM_SI_UDP,Int))
      {
        FFSS_PrintSyslog(LOG_ERR,"Config Loader : Cannot get my ip for interface %s\n",Int);
        return false;
      }
      FM_MyDomain.Master = FFSS_MyIP;
    }
    else
      FFSS_PrintSyslog(LOG_INFO,"Forcing IP %s\n",FM_MyDomain.Master);
    if(Int != NULL)
      free(Int);

    FM_Domains = SU_AddElementHead(FM_Domains,&FM_MyDomain);
    context;
    FM_MyDomain.Hosts = FM_LoadHosts(FM_MYHOSTS_FILE);
  }
  else
  {
    if(Group == NULL)
      Group = strdup("nogroup");
    if(User == NULL)
      User = strdup("ffss");
    FM_User = User;
    FM_Group = Group;
  }

  return true;
}
