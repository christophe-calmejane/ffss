/* This is the conf conn plugin for ffss server */
/* (c) Christophe Calmejane - 2001'02           */
/*     aka Ze KiLleR / SkyTech                  */
/*                                              */
/* http://zekiller.skytech.org                  */
/* mailto : zekiller@skytech.org                */

#define CONFCONN_NAME      "Conf Conn Plugin"
#define CONFCONN_VERSION   "0.1"
#define CONFCONN_COPYRIGHT "(c) Ze KiLleR - 2001'02"
#define CONFCONN_PLUGIN_REG_KEY FSP_BASE_REG_KEY CONFCONN_NAME "\\"

/* The only file we need to include is server.h */
#include "../../src/plugin.h"
#undef malloc
#undef strdup

typedef struct
{
  char *IP;
  char *Login;
  char *Pwd;
} CC_TConf, *CC_PConf;

/* We have to declare a FS_PPlugin structure for our callbacks */
FS_PPlugin Pl;
SU_PList CC_Confs = NULL; /* CC_PConf */
void CC_AddConf(const char IP[],const char Login[],const char Pwd[]);

/* ******************** */
/* OS dependant section */
/* ******************** */
#ifdef _WIN32
void CC_LoadConfig()
{
  char IP[100];
  char Buf[100];
  HKEY key;
  LONG ret = ERROR_SUCCESS;
  DWORD len,len2;
  int idx = 0;
  char *p;

  key = SU_RB_OpenKeys(CONFCONN_PLUGIN_REG_KEY,KEY_READ);
  if(key == NULL)
    return;
  while(ret != ERROR_NO_MORE_ITEMS)
  {
    len = sizeof(IP);
    len2 = sizeof(Buf);
    ret = RegEnumValue(key,idx,IP,&len,NULL,NULL,Buf,&len2);
    if(ret == ERROR_SUCCESS)
    {
      p = strchr(Buf,'|');
      if(p != NULL)
      {
        p[0] = 0;
        CC_AddConf(IP,Buf,p+1);
      }
    }
    idx++;
  }
}
void CC_SaveConfig()
{
  SU_PList Ptr;
  char Key[512];
  char Buf[150];
  CC_PConf Conf;

  /* First remove previous values */
  SU_RB_DelKey(CONFCONN_PLUGIN_REG_KEY);

  /* Then add all values */
  Ptr = CC_Confs;
  while(Ptr != NULL)
  {
    Conf = (CC_PConf) Ptr->Data;
    snprintf(Key,sizeof(Key),"%s%s",CONFCONN_PLUGIN_REG_KEY,Conf->IP);
    snprintf(Buf,sizeof(Buf),"%s|%s",Conf->Login,Conf->Pwd);
    SU_RB_SetStrValue(Key,Buf);
    Ptr = Ptr->Next;
  }
}
#else /* !_WIN32 */
void CC_LoadConfig()
{
}
void CC_SaveConfig()
{
}
#endif /* _WIN32 */

/* ********************** */
/* OS independant section */
/* ********************** */
void CC_AddConf(const char IP[],const char Login[],const char Pwd[])
{
  CC_PConf Conf;

  Conf = (CC_PConf) malloc(sizeof(CC_TConf));
  memset(Conf,0,sizeof(CC_TConf));
  Conf->IP = strdup(IP);
  Conf->Login = strdup(Login);
  Conf->Pwd = strdup(Pwd);
  CC_Confs = SU_AddElementHead(CC_Confs,Conf);
}

void CC_FreeConf(CC_PConf Conf)
{
  if(Conf->IP != NULL)
    free(Conf->IP);
  if(Conf->Login != NULL)
    free(Conf->Login);
  if(Conf->Pwd != NULL)
    free(Conf->Pwd);
  free(Conf);
}

/* We declare a callback for the OnCheckConfConn callback */
bool OnCheckConfConn(SU_PClientSocket Client)
{
  char *ip = inet_ntoa(Client->SAddr.sin_addr);
  fd_set rfds;
  struct timeval tv;
  int retval,res;
  FFSS_Field Length;
  char Login[100],Pwd[100];
  SU_PList Ptr;
  CC_PConf Conf;

  if(strcmp(ip,"127.0.0.1") == 0) /* Localhost.... accept it */
    return true;

  /* Remote connection... search ip in CC_Confs, then check login/pwd */
  Ptr = CC_Confs;
  while(Ptr != NULL)
  {
    Conf = (CC_PConf) Ptr->Data;
    if(strcmp(ip,Conf->IP) == 0)
      break;
    Ptr = Ptr->Next;
  }
  if(Ptr == NULL) /* Not found */
    return false;
  /* Get login length */
  FD_ZERO(&rfds);
  FD_SET(Client->sock,&rfds);
  tv.tv_sec = 2; /* 2 secondes */
  tv.tv_usec = 0;
  retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
    return false;
  res = recv(Client->sock,(char *)&Length,sizeof(Length),SU_MSG_NOSIGNAL);
  if(res != sizeof(Length))
    return false;
  if((Length == 0) || (Length >= sizeof(Login)))
    return false;
  /* Get Login */
  FD_ZERO(&rfds);
  FD_SET(Client->sock,&rfds);
  tv.tv_sec = 2; /* 2 secondes */
  tv.tv_usec = 0;
  retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
    return false;
  res = recv(Client->sock,Login,Length,SU_MSG_NOSIGNAL);
  if(res != Length)
    return false;
  Login[Length] = 0;
  /* Get pwd length */
  FD_ZERO(&rfds);
  FD_SET(Client->sock,&rfds);
  tv.tv_sec = 2; /* 2 secondes */
  tv.tv_usec = 0;
  retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
    return false;
  res = recv(Client->sock,(char *)&Length,sizeof(Length),SU_MSG_NOSIGNAL);
  if(res != sizeof(Length))
    return false;
  if((Length == 0) || (Length >= sizeof(Pwd)))
    return false;
  /* Get Pwd */
  FD_ZERO(&rfds);
  FD_SET(Client->sock,&rfds);
  tv.tv_sec = 2; /* 2 secondes */
  tv.tv_usec = 0;
  retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
    return false;
  res = recv(Client->sock,Pwd,Length,SU_MSG_NOSIGNAL);
  if(res != Length)
    return false;
  Pwd[Length] = 0;

  /* Now check login/pwd */
  if((strcmp(Login,Conf->Login) != 0) || (strcmp(Pwd,Conf->Pwd) != 0))
    return false;
  /* Ok, identified... accept the connection */
  return true;
}


/* This is the Init fonction (Name it CAREFULLY) called on each LoadPlugin call */
FS_PLUGIN_EXPORT FS_PPlugin Plugin_Init(void)
{
  /* Setting all callbacks to NULL */
  Pl = (FS_PPlugin) malloc(sizeof(FS_TPlugin));
  if(Pl == NULL)
    return NULL;
  memset(Pl,0,sizeof(FS_TPlugin));

  /* Setting plugin infos */
  Pl->Name = CONFCONN_NAME;
  Pl->Author = CONFCONN_COPYRIGHT;
  Pl->Version = CONFCONN_VERSION;

  /* Setting our callbacks */
  Pl->OnCheckConfConn = OnCheckConfConn;

  CC_LoadConfig();
  /* And finaly returning the FS_PPlugin structure to the server.
   * If something goes wrong during this init function, free everything you have allocated and return NULL.
   * UnInit function will not be called in this case.
  */
  return Pl;
}

/* This is the UnInit fonction (Name it CAREFULLY) called on each UnLoadPlugin call */
FS_PLUGIN_EXPORT void Plugin_UnInit(void)
{
  SU_PList Ptr;

  CC_SaveConfig();
  Ptr = CC_Confs;
  while(Ptr != NULL)
  {
    CC_FreeConf((CC_PConf)Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(CC_Confs);
  CC_Confs = NULL;
}

