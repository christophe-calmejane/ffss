#include <ffss.h>
#include <wininet.h>
#undef malloc
#include "AutoPatch\\resource.h"

/* CurrentPatches Format :
  <FFSS version patch applies to> <ID of file to patch> <Version of patched file> <File Name>
*/

#define FFSS_CHECK_FILES_AUTO_CHECK    1
#define FFSS_CHECK_FILES_UPDATER       2

#define FFSS_CHECK_FILES_WINFFSS       3

#define FFSS_CHECK_FILES_SERVER        4
#define FFSS_CHECK_FILES_MANAGER       5
#define FFSS_CHECK_FILES_SHAREMAN      6
#define FFSS_CHECK_FILES_FFSSDKILLER   7

#define FFSS_CHECK_FILES_TRAYCONN      8
#define FFSS_CHECK_FILES_LOG           9
#define FFSS_CHECK_FILES_CONFCONN      10
#define FFSS_CHECK_FILES_IPFILTER      11

#define FFSS_CHECK_FILES_COUNT         12


#define FFSS_CHECK_URL_BASE "http://ffss.fr.st/"
#define FFSS_CHECK_URL_PATCHES FFSS_CHECK_URL_BASE "CurrentPatches"
#define FFSS_CHECK_URL_GET_PATCHES FFSS_CHECK_URL_BASE "patches/"
#define FFSS_CHECK_RB_BASE "HKEY_CURRENT_USER\\Software\\FFSS\\AutoCheck_"
#define FFSS_CHECK_RB_CONN_TYPE  FFSS_CHECK_RB_BASE "Type"
#define FFSS_CHECK_RB_PROXY_HOST FFSS_CHECK_RB_BASE "Proxy_Host"
#define FFSS_CHECK_RB_PROXY_PORT FFSS_CHECK_RB_BASE "Proxy_Port"
#define FFSS_CHECK_RB_PROXY_USER FFSS_CHECK_RB_BASE "Proxy_User"
#define FFSS_CHECK_RB_PROXY_PWD  FFSS_CHECK_RB_BASE "Proxy_Pwd"

typedef struct
{
  char *FilePath;
  bool KillServer;
} FCU_TFiles;

FCU_TFiles FCU_Files[FFSS_CHECK_FILES_COUNT] = {{"",false},
                                                {"AutoCheck.exe",false},
                                                {"ffssupdater.exe",false},

                                                {"Client\\winffss.exe",false},

                                                {"Server\\ffssd.exe",true},
                                                {"Server\\FFSS_Share.exe",false},
                                                {"Server\\ShareMan.exe",false},
                                                {"Server\\ffssdkiller.exe",false},

                                                {"Server\\Plugins\\TrayConn.dll",true},
                                                {"Server\\Plugins\\Log.dll",true},
                                                {"Server\\Plugins\\ConfConn.dll",true},
                                                {"Server\\Plugins\\ipfilter.dll",true}

                                                };

int CurrentIdx = 0; /* Idx of file to patch */
char *CurrentFileVersion = NULL; /* New file version to set in registry */
HINSTANCE AP_hInstance;
bool DoPatch = true;
bool ChangeLog = false;

void ProcOnOkRedirect(SU_PAnswer Ans,void *User);

#define FFSS_REGISTRY_PATH_PROCESSID FFSS_REGISTRY_PATH "Server\\ProcessId"
#define SLEEP_TIME 100
#define MAX_WAIT 30

LRESULT CALLBACK wndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  switch (message) {
    case WM_COMMAND:
      switch(LOWORD(wParam))
      {
        case IDOK:
          DoPatch = true;
          EndDialog(hwnd,0);
          PostQuitMessage(0);
          return TRUE;
        case IDCANCEL:
          DoPatch = false;
          EndDialog(hwnd,0);
          PostQuitMessage(0);
          return TRUE;
      }
      break;
    case WM_DESTROY:
      PostQuitMessage(0);
      return TRUE;
  }
  return FALSE;
}

void ProcOnOkGotChangeLog(SU_PAnswer Ans,void *User)
{
  MSG msg;
  HWND hwnd,dlg;
  char buf[4096];
  int len;
  char InstallDir[512];
  char FileToPatch[1024];
  RECT rect1,rect2;

  SU_RB_GetStrValue(FFSS_REGISTRY_PATH "InstallDirectory",InstallDir,sizeof(InstallDir),"");
  if(InstallDir[0] == 0)
    return;
  snprintf(FileToPatch,sizeof(FileToPatch),"%s%s",InstallDir,FCU_Files[CurrentIdx].FilePath);

  hwnd = CreateDialog(AP_hInstance,MAKEINTRESOURCE(IDD_DIALOG1),GetDesktopWindow(),wndProc);
  if(hwnd == NULL)
    return;
  GetWindowRect(GetDesktopWindow(),&rect1);
  GetWindowRect(hwnd,&rect2);
  SetWindowPos(hwnd,HWND_TOPMOST,(rect1.right/2)-((rect2.right-rect2.left)/2),(rect1.bottom/2)-((rect2.bottom-rect2.top)/2),0,0,SWP_NOSIZE);

  dlg = GetDlgItem(hwnd,(int)MAKEINTRESOURCE(IDC_LABEL));
  snprintf(buf,sizeof(buf),"ChangeLog for file : %s",FileToPatch);
  SetWindowText(dlg,buf);
  dlg = GetDlgItem(hwnd,(int)MAKEINTRESOURCE(IDC_EDIT1));
  len = Ans->Data_Length + 1;
  if(len >= sizeof(buf))
    len = sizeof(buf);
  SU_strcpy(buf,Ans->Data,len);
  SetWindowText(dlg,buf);
  ShowWindow(hwnd,SW_SHOW);
  while(GetMessage(&msg,hwnd,0,0))
  {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
  ChangeLog = true;
}

void KillServer()
{
  DWORD ProcessId;
  HANDLE Process;
  DWORD res;
  int nb = 0;

  ProcessId = SU_RB_GetIntValue(FFSS_REGISTRY_PATH_PROCESSID,0);
  if(ProcessId == 0)
    return;
  Process = OpenProcess(PROCESS_TERMINATE | PROCESS_QUERY_INFORMATION,false,ProcessId);
  if(Process == NULL)
    return;
  if(TerminateProcess(Process,0) == 0)
    return;
  do
  {
    if(GetExitCodeProcess(Process,&res) == 0)
      return;
    if(res != STILL_ACTIVE)
      break;
    Sleep(100);
    nb++;
  } while(nb < MAX_WAIT);
}

void ProcOnOkGotPatch(SU_PAnswer Ans,void *User)
{
  char buf[1024];
  FILE *fp;
  char InstallDir[512];
  char FileToPatch[1024];

  SU_RB_GetStrValue(FFSS_REGISTRY_PATH "InstallDirectory",InstallDir,sizeof(InstallDir),"");
  if(InstallDir[0] == 0)
    return;
  snprintf(FileToPatch,sizeof(FileToPatch),"%s%s",InstallDir,FCU_Files[CurrentIdx].FilePath);

  fp = fopen(FileToPatch,"wb");
  while(fp == NULL) /* Cannot open for writing... file in use ? */
  {
    if(FCU_Files[CurrentIdx].KillServer) /* Server related file... kill server */
    {
      snprintf(buf,sizeof(buf),"FFSS Server must be killed in order to update file '%s'. Do you want me to (try to) kill it ?",FileToPatch);
      if(MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_YESNO) != IDYES)
      {
        snprintf(buf,sizeof(buf),"Aborting file patching for '%s'",FileToPatch);
        MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_OK);
        return;
      }
      KillServer();
    }
    else /* Display message box */
    {
      snprintf(buf,sizeof(buf),"Can't open file '%s' for writing. File may be in use. Close it, then clic on RETRY",FileToPatch);
      if(MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_RETRYCANCEL) == IDCANCEL)
      {
        snprintf(buf,sizeof(buf),"Aborting file patching for '%s'",FileToPatch);
        MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_OK);
        return;
      }
    }
    fp = fopen(FileToPatch,"wb");
  }
  fwrite(Ans->Data,Ans->Data_Length,1,fp);
  fclose(fp);
  /* Update File version in registry */
  snprintf(buf,sizeof(buf),"%sPatches\\%s",FFSS_REGISTRY_PATH,FCU_Files[CurrentIdx].FilePath);
  SU_RB_SetStrValue(buf,CurrentFileVersion);
  snprintf(buf,sizeof(buf),"Successfully patched file '%s' to version %s",FileToPatch,CurrentFileVersion);
  MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_OK);
}

bool ReadLine(const char Buf[],int *ofs,int buf_len,char S[],int len)
{
  int i;
  char c;

  i = 0;
  S[0] = 0;
  c = Buf[*ofs];
  if((c == 0) || (*ofs >= (buf_len-1)))
    return 0;
  while((c == 0x0A) || (c == 0x0D))
  {
    *ofs = *ofs + 1;
    c = Buf[*ofs];
    if((c == 0) || (*ofs >= (buf_len-1)))
      return 0;
  }
  while((c != 0x0A) && (c != 0x0D))
  {
    if(i >= (len-1))
      break;
    S[i++] = c;
    *ofs = *ofs + 1;
    c = Buf[*ofs];
    if((c == 0) || (*ofs >= (buf_len-1)))
      break;
  }
  S[i] = 0;
  return 1;
}

void ProcOnOkCheckUpdate(SU_PAnswer Ans,void *User)
{
  int pos = 0,idx;
  char S[100];
  SU_PList Exec;
  SU_PHTTPActions Act;
  char buf[1024];
  char *tmp;
  char Version[100];
  char FileVersion[100];
  char InstallDir[100];
  FILE *fp;

  if(Ans->Data == NULL)
    return;
  SU_RB_GetStrValue(FFSS_REGISTRY_PATH "CurrentVersion",Version,sizeof(Version),FFSS_VERSION);
  SU_RB_GetStrValue(FFSS_REGISTRY_PATH "InstallDirectory",InstallDir,sizeof(InstallDir),"");
  if(InstallDir[0] == 0)
  {
    MessageBox(NULL,"InstallDirectory key not found in registry. FFSS installation may be corrupted","FFSS Auto Patch Info",MB_OK | MB_ICONEXCLAMATION);
    return;
  }
  while(ReadLine(Ans->Data,&pos,Ans->Data_Length,S,sizeof(S)))
  {
    if((S[0] == 0) || (S[0] == '#'))
      continue;
    /* Check if version matches */
    tmp = strtok(S," ");
    if(tmp == NULL)
      continue;
    if(strncmp(tmp,Version,sizeof(Version)) != 0)
      continue; /* Not for our version of ffss */

    /* Check if file exists locally */
    tmp = strtok(NULL," ");
    if(tmp == NULL)
      continue;
    idx = atoi(tmp);
    if((idx <= 0) || (idx >= FFSS_CHECK_FILES_COUNT))
      continue;
    snprintf(buf,sizeof(buf),"%s%s",InstallDir,FCU_Files[idx].FilePath);
    fp = fopen(buf,"rb");
    if(fp == NULL)
      continue;
    fclose(fp);
    CurrentIdx = idx;

    /* Check local version */
    tmp = strtok(NULL," ");
    if(tmp == NULL)
      continue;
    snprintf(buf,sizeof(buf),"%sPatches\\%s",FFSS_REGISTRY_PATH,FCU_Files[idx].FilePath);
    SU_RB_GetStrValue(buf,FileVersion,sizeof(FileVersion),"");
    if((FileVersion[0] != 0) && (strncmp(tmp,FileVersion,sizeof(FileVersion)) <= 0))
      continue;
    CurrentFileVersion = strdup(tmp);

    DoPatch = true;
    ChangeLog = false;
    /* Get ChangeLog */
    tmp = strtok(NULL," ");
    if(tmp == NULL)
      continue;
    snprintf(buf,sizeof(buf),"%s%s.changelog",FFSS_CHECK_URL_GET_PATCHES,tmp);
    Exec = NULL;
    Act = (SU_PHTTPActions) malloc(sizeof(SU_THTTPActions));
    memset(Act,0,sizeof(SU_THTTPActions));
    Act->Command = ACT_GET;
    strcpy(Act->URL,buf);
    Act->User = (void *)2;
    Act->CB.OnOk = ProcOnOkRedirect;
    Exec = SU_AddElementHead(Exec,Act);
    if(SU_ExecuteActions(Exec) != 0)
    {
      MessageBox(NULL,"Cannot connect to " FFSS_CHECK_URL_BASE ". Check HTTP settings (re-run 'Configure FFSS')","FFSS Auto Patch Info",MB_OK);
      exit(2);
    }
    SU_FreeAction(Act);
    SU_FreeList(Exec);

    /* Get patched file */
    if(DoPatch)
    {
      if(!ChangeLog) /* ChangeLog dialog wasn't displayed */
      {
        snprintf(buf,sizeof(buf),"Ready to patch file '%s%s'. Continue ?",InstallDir,FCU_Files[idx].FilePath);
        if(MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_OKCANCEL) == IDCANCEL)
        {
          snprintf(buf,sizeof(buf),"Aborting file patching for '%s%s'",InstallDir,FCU_Files[idx].FilePath);
          MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_OK);
          continue;
        }
      }
      snprintf(buf,sizeof(buf),"%s%s",FFSS_CHECK_URL_GET_PATCHES,tmp);
      Exec = NULL;
      Act = (SU_PHTTPActions) malloc(sizeof(SU_THTTPActions));
      memset(Act,0,sizeof(SU_THTTPActions));
      Act->Command = ACT_GET;
      strcpy(Act->URL,buf);
      Act->User = (void *)1;
      Act->CB.OnOk = ProcOnOkRedirect;
      Exec = SU_AddElementHead(Exec,Act);
      if(SU_ExecuteActions(Exec) != 0)
      {
        MessageBox(NULL,"Cannot connect to " FFSS_CHECK_URL_BASE ". Check HTTP settings (re-run 'Configure FFSS')","FFSS Auto Patch Info",MB_OK);
        exit(2);
      }
      SU_FreeAction(Act);
      SU_FreeList(Exec);
    }
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
  if(User == NULL)
    Act->CB.OnOk = ProcOnOkCheckUpdate;
  else if(User == (void *)1)
    Act->CB.OnOk = ProcOnOkGotPatch;
  else
    Act->CB.OnOk = ProcOnOkGotChangeLog;
  Act->User = User;
  Exec = SU_AddElementHead(Exec,Act);
  if(SU_ExecuteActions(Exec) != 0)
  {
    MessageBox(NULL,"Cannot connect to " FFSS_CHECK_URL_BASE ". Check HTTP settings (re-run 'Configure FFSS')","FFSS Auto Patch Info",MB_OK);
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

  AP_hInstance = hInstance;
  fl = 2.2;
  if((int)Exec == 666)
    printf("%f\n",fl);

  if(!SU_WSInit(2,2))
  {
    MessageBox(NULL,"Cannot init winsock2 dll","FFSS Auto Patch Info",MB_OK);
    return 1;
  }
  CheckProxy();
  Exec = NULL;
  Act = (SU_PHTTPActions) malloc(sizeof(SU_THTTPActions));
  memset(Act,0,sizeof(SU_THTTPActions));
  Act->Command = ACT_GET;
  strcpy(Act->URL,FFSS_CHECK_URL_PATCHES);
  Act->CB.OnOk = ProcOnOkRedirect;
  Exec = SU_AddElementHead(Exec,Act);
  if(SU_ExecuteActions(Exec) != 0)
  {
    MessageBox(NULL,"Cannot connect to " FFSS_CHECK_URL_BASE ". Check HTTP settings (re-run 'Configure FFSS')","FFSS Auto Patch Info",MB_OK);
    return 2;
  }
  SU_FreeAction(Act);
  SU_FreeList(Exec);
  return 0;
}
