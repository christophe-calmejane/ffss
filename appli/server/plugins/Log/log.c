/* This is the Log plugin for ffss server */
/*    (c) Christophe Calmejane - 2002     */
/*     aka Ze KiLleR / SkyTech            */
/*                                        */
/* http://zekiller.skytech.org            */
/* mailto : zekiller@skytech.org          */

/* TODO :
*/

#define LOG_NAME      "Log Plugin"
#define LOG_VERSION   "0.1"
#define LOG_COPYRIGHT "(c) Ze KiLleR - 2002"
#define LOG_DESCRIPTION "Logs all successful connections and download requests."
#define LOG_FILE_PREFIX "FS_Log"
#define LOG_PLUGIN_REG_KEY FSP_BASE_REG_KEY LOG_NAME

/* The only file we need to include is server.h */
#include "../../src/plugin.h"
#ifdef _WIN32
#include "Log\\resource.h"
#include <Shlobj.h>
#endif /* _WIN32 */
#include <stdarg.h>
#undef malloc
#undef strdup

typedef struct
{
  char *Path;
  bool Log_Conn;
  bool Log_Dwl;
} L_TGlobal, *L_PGlobal;

/* We have to declare a FS_PPlugin structure for our callbacks */
FS_PPlugin Pl;
L_TGlobal L_Gbl;
FSP_TInfos L_Infos;

void * (*PluginQueryFunc)(int Type,...);

FILE *L_fp = NULL;
int L_day = 0;
#ifdef _WIN32
HWND L_hwnd;
HINSTANCE L_hInstance;
#endif /* _WIN32 */

void LoadConfig()
{
#ifdef _WIN32
  char Path[1024];

  SU_RB_GetStrValue(LOG_PLUGIN_REG_KEY "\\LogsPath",Path,sizeof(Path),".");
  L_Gbl.Path = strdup(Path);
  L_Gbl.Log_Conn = SU_RB_GetIntValue(LOG_PLUGIN_REG_KEY "\\Log_Conn",1) == 1;
  L_Gbl.Log_Dwl = SU_RB_GetIntValue(LOG_PLUGIN_REG_KEY "\\Log_Dwl",1) == 1;
#endif /* _WIN32 */
}

void StoreConfig()
{
#ifdef _WIN32
  SU_RB_SetStrValue(LOG_PLUGIN_REG_KEY "\\LogsPath",L_Gbl.Path);
  SU_RB_SetIntValue(LOG_PLUGIN_REG_KEY "\\Log_Conn",L_Gbl.Log_Conn);
  SU_RB_GetIntValue(LOG_PLUGIN_REG_KEY "\\Log_Dwl",L_Gbl.Log_Dwl);
#endif /* _WIN32 */
}

void WriteLog(char *Txt,...)
{
  struct tm *TM;
  time_t Tim;
  va_list argptr;
  char Str[4096];

  Tim = time(NULL);
  TM = localtime(&Tim);
  if(TM->tm_mday != L_day)
  {
    SU_CloseLogFile(L_fp);
    snprintf(Str,sizeof(Str),"%s/%s-%d-%.2d-%.2d.log",L_Gbl.Path,LOG_FILE_PREFIX,TM->tm_year+1900,TM->tm_mon+1,TM->tm_mday);
    L_day = TM->tm_mday;
    L_fp = SU_OpenLogFile(Str);
  }
  va_start(argptr,Txt);
  snprintf(Str,sizeof(Str),"[%.2d:%.2d:%.2d] ",TM->tm_hour,TM->tm_min,TM->tm_sec);
#ifdef _WIN32
  _vsnprintf(Str+strlen(Str),sizeof(Str)-strlen(Str),Txt,argptr);
#else /* !_WIN32 */
  vsnprintf(Str+strlen(Str),sizeof(Str)-strlen(Str),Txt,argptr);
#endif /* _WIN32 */
  if(L_fp != NULL)
    fprintf(L_fp,Str);
}

/* Successfully connected to the share */
void *OnShareConnection(SU_PClientSocket Client,const char ShareName[],const char Login[],const char Password[],long int Compressions)
{
  if(!L_Gbl.Log_Conn)
    return NULL;
  if((Login == NULL) || (Login[0] == 0))
    WriteLog("Share Connection from %s (%s) : %s {id:%u}\n",inet_ntoa(Client->SAddr.sin_addr),SU_NameOfPort(inet_ntoa(Client->SAddr.sin_addr)),ShareName,SU_THREAD_SELF);
  else
    WriteLog("Share Connection from %s (%s) : %s using login %s {id:%u}\n",inet_ntoa(Client->SAddr.sin_addr),SU_NameOfPort(inet_ntoa(Client->SAddr.sin_addr)),ShareName,Login,SU_THREAD_SELF);
  return NULL;
}

/* Download successfully started */
bool OnDownload(SU_PClientSocket Client,const char Path[],FFSS_LongField StartPos,int Port)
{
  if(!L_Gbl.Log_Dwl)
    return true;
  if(StartPos == 0)
    WriteLog("Download request from %s {id:%u} : %s\n",inet_ntoa(Client->SAddr.sin_addr),SU_THREAD_SELF,Path);
  else
    WriteLog("Download request from %s {id:%u} : Resuming %s\n",inet_ntoa(Client->SAddr.sin_addr),SU_THREAD_SELF,Path);
  return true;
}

#ifdef _WIN32
char *GetDirectoryPath(HWND hwnd,char *Buf)
{
  LPITEMIDLIST pidlRoot = NULL;
  LPITEMIDLIST pidlSelected = NULL;
  BROWSEINFO bi = {0};

  bi.hwndOwner = hwnd;
  bi.pidlRoot = NULL;
  bi.pszDisplayName = "Choose a folder to store log files";
  bi.lpszTitle = "Choose a folder to store log files";
  bi.ulFlags = 0;
  bi.lpfn = NULL;
  bi.lParam = 0;

  pidlSelected = SHBrowseForFolder(&bi);

  SHGetPathFromIDList(pidlSelected,Buf);
  return Buf;
}

LRESULT CALLBACK wndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  switch (message) {
    case WM_COMMAND:
      switch(LOWORD(wParam))
      {
        HWND dlg;
        char buf[1024];

        case IDOK:
          L_Gbl.Log_Conn = IsDlgButtonChecked(hwnd,(int)MAKEINTRESOURCE(IDC_CHECK1)) == BST_CHECKED;
          L_Gbl.Log_Dwl = IsDlgButtonChecked(hwnd,(int)MAKEINTRESOURCE(IDC_CHECK2)) == BST_CHECKED;
          dlg = GetDlgItem(hwnd,(int)MAKEINTRESOURCE(IDC_EDIT1));
          GetWindowText(dlg,buf,sizeof(buf));
          if(L_Gbl.Path != NULL)
            free(L_Gbl.Path);
          L_Gbl.Path = strdup(buf);
          /* Store config now */
          StoreConfig();
          /* Close box */
          EndDialog(hwnd,0);
          PostQuitMessage(0);
          return TRUE;
        case IDC_BUTTON1:
          dlg = GetDlgItem(hwnd,(int)MAKEINTRESOURCE(IDC_EDIT1));
          SetWindowText(dlg,GetDirectoryPath(hwnd,buf));
          return TRUE;
      }
      break;
    case WM_CREATE:
      return TRUE;

    case WM_DESTROY:
      PostQuitMessage(0);
      return TRUE;
  }
  return DefWindowProc(hwnd, message, wParam, lParam);
}

void ThreadFunc(void *info)
{
  MSG msg;
  HWND dlg;

  L_hwnd = CreateDialog(L_hInstance,MAKEINTRESOURCE(IDD_DIALOG1),GetDesktopWindow(),wndProc);
  if(L_hwnd == NULL)
    return;

  if(L_Gbl.Log_Conn)
    CheckDlgButton(L_hwnd,(int)MAKEINTRESOURCE(IDC_CHECK1),BST_CHECKED);
  if(L_Gbl.Log_Dwl)
    CheckDlgButton(L_hwnd,(int)MAKEINTRESOURCE(IDC_CHECK2),BST_CHECKED);
  dlg = GetDlgItem(L_hwnd,(int)MAKEINTRESOURCE(IDC_EDIT1));
  SetWindowText(dlg,L_Gbl.Path);
  ShowWindow(L_hwnd,SW_SHOW);
  while(GetMessage(&msg,L_hwnd,0,0))
  {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
}

/* This is the function called when plugin is requested to configure itself */
FS_PLUGIN_EXPORT bool Plugin_Configure(void)
{
  DWORD tmp;
  /* Create a thread to manage messages */

  if(CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThreadFunc,NULL,0,&tmp) == NULL)
    return false;
  return true;
}
#endif /* _WIN32 */

/* This is the Init fonction (Name it CAREFULLY) called on each LoadPlugin call */
FS_PLUGIN_EXPORT FS_PPlugin Plugin_Init(void *Info,void *(*QueryFunc)(int Type,...))
{
  struct tm *TM;
  time_t Tim;
  char S[1024];

  /* Get pointer to plugin query function */
  PluginQueryFunc = QueryFunc;
#ifdef _WIN32
  L_hInstance = (HINSTANCE)Info;
#endif /* _WIN32 */

  /* Setting all callbacks to NULL */
  Pl = (FS_PPlugin) malloc(sizeof(FS_TPlugin));
  if(Pl == NULL)
    return NULL;
  memset(Pl,0,sizeof(FS_TPlugin));

  /* Setting plugin infos */
  Pl->Name = LOG_NAME;
  Pl->Copyright = LOG_COPYRIGHT;
  Pl->Version = LOG_VERSION;

  /* Setting our callbacks */
  Pl->CB.OnShareConnection = OnShareConnection;
  Pl->CB.OnDownload = OnDownload;

  /* Load config options */
  memset(&L_Gbl,0,sizeof(L_TGlobal));
  LoadConfig();

  /* Open log file */
  Tim = time(NULL);
  TM = localtime(&Tim);
  snprintf(S,sizeof(S),"%s/%s-%d-%.2d-%.2d.log",L_Gbl.Path,LOG_FILE_PREFIX,TM->tm_year+1900,TM->tm_mon+1,TM->tm_mday);
  L_day = TM->tm_mday;
  L_fp = SU_OpenLogFile(S);
  if(L_fp == NULL)
    return NULL;

  /* And finaly returning the FS_PPlugin structure to the server.
   * If something goes wrong during this init function, free everything you have allocated and return NULL.
   * UnInit function will not be called in this case.
  */
  Plugin_Configure();
  return Pl;
}

/* This is the UnInit fonction (Name it CAREFULLY) called on each UnLoadPlugin call */
FS_PLUGIN_EXPORT void Plugin_UnInit(void)
{
  //SendMessage(L_hwnd,WM_DESTROY,0,0);
  SU_CloseLogFile(L_fp);
  if(L_Gbl.Path != NULL)
    free(L_Gbl.Path);
}

FS_PLUGIN_EXPORT FSP_PInfos Plugin_QueryInfos(void)
{
  L_Infos.Name = LOG_NAME;
  L_Infos.Version = LOG_VERSION;
  L_Infos.Copyright = LOG_COPYRIGHT;
  L_Infos.Description = LOG_DESCRIPTION;
  return &L_Infos;
}
