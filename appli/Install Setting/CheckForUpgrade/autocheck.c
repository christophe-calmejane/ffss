#include <ffss.h>
#include <wininet.h>
#undef malloc

#define FFSS_CHECK_URL_BASE "http://ffss.fr.st/"
#define FFSS_CHECK_URL_VERSION FFSS_CHECK_URL_BASE "CurrentVersion"
#define FFSS_CHECK_RB_BASE FFSS_LM_REGISTRY_PATH "AutoCheck_"
#define FFSS_CHECK_RB_CONN_TYPE  FFSS_CHECK_RB_BASE "Type"
#define FFSS_CHECK_RB_PROXY_HOST FFSS_CHECK_RB_BASE "Proxy_Host"
#define FFSS_CHECK_RB_PROXY_PORT FFSS_CHECK_RB_BASE "Proxy_Port"
#define FFSS_CHECK_RB_PROXY_USER FFSS_CHECK_RB_BASE "Proxy_User"
#define FFSS_CHECK_RB_PROXY_PWD  FFSS_CHECK_RB_BASE "Proxy_Pwd"

void ProcOnOkCheckUpdate(SU_PAnswer Ans,void *User)
{
  char *pos;
  char buf[4096];
  char Version[100];
  char Data[1024];

  if(Ans->Data == NULL)
    return;
  if(Ans->Data_Length >= sizeof(Data))
    Ans->Data_Length = sizeof(Data);
  SU_strcpy(Data,Ans->Data,Ans->Data_Length+1);
  pos = SU_strchrl(Data,"\n\r",NULL);
  if(pos != NULL)
  {
    pos[0] = 0;
    pos++;
  }
  SU_RB_GetStrValue(FFSS_LM_REGISTRY_PATH "CurrentVersion",Version,sizeof(Version),FFSS_VERSION);
  if(strncmp(Data,Version,sizeof(Version)) > 0)
  {
    if((pos == NULL) || (pos[0] == 0))
      _snprintf(buf,sizeof(buf),"A new version of FFSS (%s) is available for download - Please upgrade :\nftp-samba-ffss://orion.fleming.u-psud.fr\nftp://tabarka.rub.u-psud.fr",Data);
    else
      _snprintf(buf,sizeof(buf),"A new version of FFSS (%s) is available for download - Please upgrade :\n%s",Data,pos);
    MessageBox(NULL,buf,"FFSS UPDATE INFO",MB_OK);
  }
}

void CheckProxy()
{
  char Host[1024];
  int Port;
  char User[1024];
  char Pwd[1024];
  int ConnType;
  char Buffer[1024];
  DWORD dwBufferLength=sizeof(Buffer);
  LPINTERNET_PROXY_INFO lpInternetProxy;

  ConnType = SU_RB_GetIntValue(FFSS_CHECK_RB_CONN_TYPE,0);
  switch(ConnType)
  {
  case 0 : /* No connection */
    exit(0);
  case 1 : /* Direct connection */
    break;
  case 2 : /* Use proxy */
    SU_RB_GetStrValue(FFSS_CHECK_RB_PROXY_HOST,Host,sizeof(Host),"");
    Port = SU_RB_GetIntValue(FFSS_CHECK_RB_PROXY_PORT,0);
    SU_RB_GetStrValue(FFSS_CHECK_RB_PROXY_USER,User,sizeof(User),"");
    SU_RB_GetStrValue(FFSS_CHECK_RB_PROXY_PWD,Pwd,sizeof(Pwd),"");
    SU_SetProxy(Host,Port,User,Pwd);
    break;
  case 3 : /* Use IE settings */
    /* Get global proxy configuration */
    if(InternetQueryOption(NULL,INTERNET_OPTION_PROXY,Buffer,&dwBufferLength) == TRUE)
    {
      char *host;

      lpInternetProxy=(LPINTERNET_PROXY_INFO)Buffer;
      /* Not in direct connection  */
      if(lpInternetProxy->dwAccessType != INTERNET_OPEN_TYPE_DIRECT)
      {
        host = strtok((char*)lpInternetProxy->lpszProxy,":");
        Port = atol(strtok(NULL,""));

        dwBufferLength = sizeof(User);
        User[0] = 0;
        InternetQueryOption(NULL,INTERNET_OPTION_PROXY_USERNAME,User,&dwBufferLength);
        dwBufferLength = sizeof(Pwd);
        Pwd[0] = 0;
        InternetQueryOption(NULL,INTERNET_OPTION_PROXY_PASSWORD,Pwd,&dwBufferLength);
        SU_SetProxy(host,Port,User,Pwd);
      }
    }
    break;
  }
}

void ProcOnOkRedirect(SU_PAnswer Ans,void *User)
{
  SU_PList Exec;
  SU_PHTTPActions Act;
  char *p,*q;

  p = strstr(Ans->Data,"3;URL=");
  if(p == NULL)
    return;
  p += 6;
  q = strchr(p,'"');
  if(q == NULL)
    return;
  q[0] = 0;

  Exec = NULL;
  Act = (SU_PHTTPActions) malloc(sizeof(SU_THTTPActions));
  memset(Act,0,sizeof(SU_THTTPActions));
  Act->Command = ACT_GET;
  strcpy(Act->URL,p);
  Act->CB.OnOk = ProcOnOkCheckUpdate;
  Exec = SU_AddElementHead(Exec,Act);
  if(SU_ExecuteActions(Exec) != 0)
  {
    MessageBox(NULL,"Cannot connect to " FFSS_CHECK_URL_BASE ". Check HTTP settings (re-run 'Configure FFSS')","FFSS UPDATE INFO",MB_OK);
    exit(2);
  }
  SU_FreeAction(Act);
  SU_FreeList(Exec);
}

int APIENTRY WinMain(HINSTANCE hInstance,HINSTANCE hPrevInstance,LPSTR lpCmdLine,int nCmdShow)
{
  SU_PList Exec = NULL;
  SU_PHTTPActions Act;
  float fl; // This float is needed to force VC to link the math lib.. VC bug ;(

  fl = 2.2;
  if((int)Exec == 666)
    printf("%f\n",fl);

  if(!SU_WSInit(2,2))
  {
    MessageBox(NULL,"Cannot init winsock2 dll","FFSS UPDATE INFO",MB_OK);
    return 1;
  }
  CheckProxy();
  Exec = NULL;
  Act = (SU_PHTTPActions) malloc(sizeof(SU_THTTPActions));
  memset(Act,0,sizeof(SU_THTTPActions));
  Act->Command = ACT_GET;
  strcpy(Act->URL,FFSS_CHECK_URL_VERSION);
  Act->CB.OnOk = ProcOnOkRedirect;
  Exec = SU_AddElementHead(Exec,Act);
  if(SU_ExecuteActions(Exec) != 0)
  {
    MessageBox(NULL,"Cannot connect to " FFSS_CHECK_URL_BASE ". Check HTTP settings (re-run 'Configure FFSS')","FFSS UPDATE INFO",MB_OK);
    return 2;
  }
  SU_FreeAction(Act);
  SU_FreeList(Exec);
  return 0;
}
