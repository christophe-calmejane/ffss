#include <ffss.h>
#include <wininet.h>
#undef malloc
#include "AutoPatch\\resource.h"

#define DIRECT_URL

#ifdef DIRECT_URL
#define FFSS_CHECK_URL_BASE "http://zekiller.skytech.org/ffss/"
#else /* !DIRECT_URL */
#define FFSS_CHECK_URL_BASE "http://ffss.fr.st/"
#endif /* DIRECT_URL */
#define FFSS_CHECK_URL_PATCHES FFSS_CHECK_URL_BASE "CurrentPatches"
#define FFSS_CHECK_URL_GET_PATCHES FFSS_CHECK_URL_BASE "patches/"
#define FFSS_CHECK_RB_BASE FFSS_LM_REGISTRY_PATH "AutoCheck_"
#define FFSS_CHECK_RB_CONN_TYPE  FFSS_CHECK_RB_BASE "Type"
#define FFSS_CHECK_RB_PROXY_HOST FFSS_CHECK_RB_BASE "Proxy_Host"
#define FFSS_CHECK_RB_PROXY_PORT FFSS_CHECK_RB_BASE "Proxy_Port"
#define FFSS_CHECK_RB_PROXY_USER FFSS_CHECK_RB_BASE "Proxy_User"
#define FFSS_CHECK_RB_PROXY_PWD  FFSS_CHECK_RB_BASE "Proxy_Pwd"

#include "autopatch.h"
#include "language.h"

unsigned int AP_CurrentLang = AP_LANG_ENGLISH;
int CurrentIdx = 0; /* Idx of file to patch */
char *CurrentFileVersion = NULL; /* New file version to set in registry */
HINSTANCE AP_hInstance;
bool DoPatch = true;
bool ChangeLog = false;
bool NeverAgain = false;

void ProcOnOkRedirect(SU_PAnswer Ans,void *User);
HWND AP_hwnd;

#define FFSS_REGISTRY_PATH_PROCESSID FFSS_LM_REGISTRY_PATH "Server\\ProcessId"
#define SLEEP_TIME 100
#define MAX_WAIT 30

void AP_LoadLanguage(void)
{
  char buf[100];
  int i;

  SU_RB_GetStrValue(FFSS_LM_REGISTRY_PATH "FavoriteLanguage",buf,sizeof(buf),"En");
  for(i=0;i<AP_LANG_COUNT;i++)
  {
    if(stricmp(buf,AP_Lang[i][AP_LANGS_COUNTRYCODE]) == 0)
    {
      AP_CurrentLang = i;
      break;
    }
  }
}

LRESULT CALLBACK wndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  switch (message) {
    case WM_COMMAND:
      switch(LOWORD(wParam))
      {
        case ID_BTN1:
          DoPatch = true;
          DestroyWindow(hwnd);
          AP_hwnd = NULL;
          return TRUE;
        case ID_BTN2:
          DoPatch = false;
          if(MessageBox(AP_hwnd,AP_LANG(AP_LANGS_MB1),"FFSS Auto Patch Info",MB_YESNO) != IDYES)
          {
            NeverAgain = true;
          }
          DestroyWindow(hwnd);
          AP_hwnd = NULL;
          return TRUE;
      }
      break;
    case WM_CLOSE:
      return TRUE;
    case WM_DESTROY:
      PostQuitMessage(0);
      return TRUE;
  }
  return FALSE;
}

void ProcOnOkGotChangeLog(SU_PAnswer Ans,void *User)
{
  MSG msg;
  HWND dlg;
  char buf[4096];
  int len;
  char InstallDir[512];
  char FileToPatch[1024];
  RECT rect1,rect2;

  if(SU_nocasestrstr(Ans->Data,"error404") != NULL)
    return;
  SU_RB_GetStrValue(FFSS_LM_REGISTRY_PATH "InstallDirectory",InstallDir,sizeof(InstallDir),"");
#ifndef DEBUG
  if(InstallDir[0] == 0)
    return;
#endif /* !DEBUG */
  snprintf(FileToPatch,sizeof(FileToPatch),"%s\\%s",InstallDir,FCU_Files[CurrentIdx].FilePath);

  AP_hwnd = CreateDialog(AP_hInstance,MAKEINTRESOURCE(IDD_DIALOG1),GetDesktopWindow(),wndProc);
  if(AP_hwnd == NULL)
    return;
  GetWindowRect(GetDesktopWindow(),&rect1);
  GetWindowRect(AP_hwnd,&rect2);
  SetWindowPos(AP_hwnd,HWND_TOPMOST,(rect1.right/2)-((rect2.right-rect2.left)/2),(rect1.bottom/2)-((rect2.bottom-rect2.top)/2),0,0,SWP_NOSIZE);

  SetDlgItemText(AP_hwnd,(int)MAKEINTRESOURCE(ID_BTN1),AP_LANG(AP_LANGS_BTN1));
  SetDlgItemText(AP_hwnd,(int)MAKEINTRESOURCE(ID_BTN2),AP_LANG(AP_LANGS_BTN2));
  snprintf(buf,sizeof(buf),AP_LANG(AP_LANGS_LBL1),FileToPatch);
  SetDlgItemText(AP_hwnd,(int)MAKEINTRESOURCE(IDC_LABEL1),buf);
  SetDlgItemText(AP_hwnd,(int)MAKEINTRESOURCE(IDC_LABEL2),AP_LANG(AP_LANGS_LBL2));
  dlg = GetDlgItem(AP_hwnd,(int)MAKEINTRESOURCE(IDC_EDIT1));
  len = Ans->Data_Length + 1;
  if(len >= sizeof(buf))
    len = sizeof(buf);
  SU_strcpy(buf,Ans->Data,len);
  SetWindowText(dlg,buf);

  ShowWindow(AP_hwnd,SW_SHOW);

  while(GetMessage(&msg,AP_hwnd,0,0))
  {
    if(!IsWindow(AP_hwnd) || !IsDialogMessage(AP_hwnd,&msg))
    {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
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
  int maxtry = 3;

  if(SU_nocasestrstr(Ans->Data,"error404") != NULL)
  {
    snprintf(buf,sizeof(buf),AP_LANG(AP_LANGS_MB12));
    MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_OK | MB_ICONEXCLAMATION);
    return;
  }
  SU_RB_GetStrValue(FFSS_LM_REGISTRY_PATH "InstallDirectory",InstallDir,sizeof(InstallDir),"");
#ifndef DEBUG
  if(InstallDir[0] == 0)
    return;
#endif /* !DEBUG */
  snprintf(FileToPatch,sizeof(FileToPatch),"%s\\%s",InstallDir,FCU_Files[CurrentIdx].FilePath);

  fp = fopen(FileToPatch,"wb");
  while(fp == NULL) /* Cannot open for writing... file in use ? */
  {
    maxtry--;
    if(maxtry == 0)
    {
      snprintf(buf,sizeof(buf),AP_LANG(AP_LANGS_MB5),FileToPatch);
      MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_OK);
      return;
    }
    if(FCU_Files[CurrentIdx].KillServer) /* Server related file... kill server */
    {
      snprintf(buf,sizeof(buf),AP_LANG(AP_LANGS_MB2),FileToPatch);
      if(MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_YESNO) != IDYES)
      {
        snprintf(buf,sizeof(buf),AP_LANG(AP_LANGS_MB3),FileToPatch);
        MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_OK);
        return;
      }
      KillServer();
      Sleep(500);
    }
    else /* Display message box */
    {
      snprintf(buf,sizeof(buf),AP_LANG(AP_LANGS_MB4),FileToPatch);
      if(MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_RETRYCANCEL) == IDCANCEL)
      {
        snprintf(buf,sizeof(buf),AP_LANG(AP_LANGS_MB3),FileToPatch);
        MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_OK);
        return;
      }
    }
    fp = fopen(FileToPatch,"wb");
  }
  fwrite(Ans->Data,Ans->Data_Length,1,fp);
  fclose(fp);
  /* Update File version in registry */
  snprintf(buf,sizeof(buf),"%sPatches\\%s",FFSS_LM_REGISTRY_PATH,FCU_Files[CurrentIdx].FilePath);
  SU_RB_SetStrValue(buf,CurrentFileVersion);
  snprintf(buf,sizeof(buf),AP_LANG(AP_LANGS_MB6),FileToPatch,CurrentFileVersion);
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
  SU_RB_GetStrValue(FFSS_LM_REGISTRY_PATH "CurrentVersion",Version,sizeof(Version),FFSS_VERSION);
  SU_RB_GetStrValue(FFSS_LM_REGISTRY_PATH "InstallDirectory",InstallDir,sizeof(InstallDir),"");
#ifndef DEBUG
  if(InstallDir[0] == 0)
  {
    MessageBox(NULL,AP_LANG(AP_LANGS_MB7),"FFSS Auto Patch Info",MB_OK | MB_ICONEXCLAMATION);
    return;
  }
#endif /* !DEBUG */
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
    if(FCU_Files[idx].FilePath[0] == 0)
      continue;
    snprintf(buf,sizeof(buf),"%s\\%s",InstallDir,FCU_Files[idx].FilePath);
#ifndef DEBUG
    fp = fopen(buf,"rb");
    if(fp == NULL)
      continue;
    fclose(fp);
#endif /* !DEBUG */
    CurrentIdx = idx;

    /* Check local version */
    tmp = strtok(NULL," ");
    if(tmp == NULL)
      continue;
    snprintf(buf,sizeof(buf),"%sPatches\\%s",FFSS_LM_REGISTRY_PATH,FCU_Files[idx].FilePath);
    SU_RB_GetStrValue(buf,FileVersion,sizeof(FileVersion),"");
    if((FileVersion[0] != 0) && (strncmp(tmp,FileVersion,sizeof(FileVersion)) <= 0))
      continue;
    CurrentFileVersion = strdup(tmp);

    DoPatch = true;
    ChangeLog = false;
    NeverAgain = false;
    /* Get ChangeLog */
    tmp = strtok(NULL," ");
    if(tmp == NULL)
      continue;
    snprintf(buf,sizeof(buf),"%s%s.changelog.%s",FFSS_CHECK_URL_GET_PATCHES,tmp,AP_Lang[AP_CurrentLang][0]);
    Exec = NULL;
    Act = (SU_PHTTPActions) malloc(sizeof(SU_THTTPActions));
    memset(Act,0,sizeof(SU_THTTPActions));
    Act->Command = ACT_GET;
    strcpy(Act->URL,buf);
    Act->User = (void *)2;
#ifdef DIRECT_URL
    Act->CB.OnOk = ProcOnOkGotChangeLog;
#else /* !DIRECT_URL */
    Act->CB.OnOk = ProcOnOkRedirect;
#endif /* DIRECT_URL */
    Exec = SU_AddElementHead(Exec,Act);
    if(SU_ExecuteActions(Exec) != 0)
    {
      MessageBox(NULL,AP_LANG(AP_LANGS_MB8),"FFSS Auto Patch Info",MB_OK);
      exit(2);
    }
    SU_FreeAction(Act);
    SU_FreeList(Exec);

    /* Get patched file */
    if(DoPatch)
    {
      if(!ChangeLog) /* ChangeLog dialog wasn't displayed */
      {
        snprintf(buf,sizeof(buf),AP_LANG(AP_LANGS_MB9),InstallDir,FCU_Files[idx].FilePath);
        if(MessageBox(NULL,buf,"FFSS Auto Patch Info",MB_OKCANCEL) == IDCANCEL)
        {
          if(MessageBox(AP_hwnd,AP_LANG(AP_LANGS_MB1),"FFSS Auto Patch Info",MB_YESNO) != IDYES)
          {
            /* Update File version in registry */
            snprintf(buf,sizeof(buf),"%sPatches\\%s",FFSS_LM_REGISTRY_PATH,FCU_Files[CurrentIdx].FilePath);
            SU_RB_SetStrValue(buf,CurrentFileVersion);
          }
          snprintf(buf,sizeof(buf),AP_LANG(AP_LANGS_MB10),InstallDir,FCU_Files[idx].FilePath);
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
#ifdef DIRECT_URL
      Act->CB.OnOk = ProcOnOkGotPatch;
#else /* !DIRECT_URL */
      Act->CB.OnOk = ProcOnOkRedirect;
#endif /* DIRECT_URL */
      Exec = SU_AddElementHead(Exec,Act);
      if(SU_ExecuteActions(Exec) != 0)
      {
        MessageBox(NULL,AP_LANG(AP_LANGS_MB8),"FFSS Auto Patch Info",MB_OK);
        exit(2);
      }
      SU_FreeAction(Act);
      SU_FreeList(Exec);
    }
    else
    {
      if(NeverAgain)
      {
        /* Update File version in registry */
        snprintf(buf,sizeof(buf),"%sPatches\\%s",FFSS_LM_REGISTRY_PATH,FCU_Files[CurrentIdx].FilePath);
        SU_RB_SetStrValue(buf,CurrentFileVersion);
      }
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

  p = strstr(Ans->Data,";URL=");
  if(p == NULL)
    return;
  p += 5;
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
    MessageBox(NULL,AP_LANG(AP_LANGS_MB8),"FFSS Auto Patch Info",MB_OK);
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

  AP_LoadLanguage();
  if(!SU_WSInit(2,2))
  {
    MessageBox(NULL,AP_LANG(AP_LANGS_MB11),"FFSS Auto Patch Info",MB_OK);
    return 1;
  }
  CheckProxy();
  Exec = NULL;
  Act = (SU_PHTTPActions) malloc(sizeof(SU_THTTPActions));
  memset(Act,0,sizeof(SU_THTTPActions));
  Act->Command = ACT_GET;
  strcpy(Act->URL,FFSS_CHECK_URL_PATCHES);
#ifdef DIRECT_URL
  Act->CB.OnOk = ProcOnOkCheckUpdate;
#else /* !DIRECT_URL */
  Act->CB.OnOk = ProcOnOkRedirect;
#endif /* DIRECT_URL */
  Exec = SU_AddElementHead(Exec,Act);
  if(SU_ExecuteActions(Exec) != 0)
  {
    MessageBox(NULL,AP_LANG(AP_LANGS_MB8),"FFSS Auto Patch Info",MB_OK);
    return 2;
  }
  SU_FreeAction(Act);
  SU_FreeList(Exec);
  return 0;
}
