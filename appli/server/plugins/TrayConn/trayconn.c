/* This is the Tray Conn plugin for ffss server */
/*    (c) Christophe Calmejane - 2002           */
/*     aka Ze KiLleR / SkyTech                  */
/*                                              */
/* http://zekiller.skytech.org                  */
/* mailto : zekiller@skytech.org                */

#define TRAYCONN_NAME      "Tray Conn Plugin"
#define TRAYCONN_VERSION   "0.1"
#define TRAYCONN_COPYRIGHT "(c) Ze KiLleR - 2002"

/* The only file we need to include is server.h */
#include "../../src/plugin.h"
#undef malloc
#undef strdup

/* We have to declare a FS_PPlugin structure for our callbacks */
FS_PPlugin Pl;

void * (*PluginQueryFunc)(int Type,...);

HWND TC_hwnd;
HINSTANCE TC_hInstance;
HBITMAP TC_bmp,TC_bmp_and;
unsigned char TC_img[16*16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,
                               0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,
                               0,0,0,0,1,1,2,2,2,2,1,1,0,0,0,0,
                               0,0,0,0,1,1,2,3,3,2,1,1,0,0,0,0,
                               0,0,0,0,1,1,2,3,3,2,1,1,0,0,0,0,
                               0,0,0,0,1,1,2,2,2,2,1,1,0,0,0,0,
                               0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,
                               0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4};

#define UWM_SYSTRAY (WM_USER + 1)

void SetNewIcon(const char *Tip)
{
  NOTIFYICONDATA nid;
  ICONINFO ii;
  HICON icon;

  SetBitmapBits(TC_bmp,sizeof(TC_img),&TC_img); /* Update bitmap bits */
  ii.fIcon = TRUE;
  ii.hbmMask = TC_bmp_and;
  ii.hbmColor = TC_bmp;
  icon = CreateIconIndirect(&ii);
  if(icon == NULL)
    return;

  /* Fill out NOTIFYICONDATA structure */
  nid.cbSize = sizeof(NOTIFYICONDATA);
  nid.hWnd = TC_hwnd;
  nid.uID = 1;
  nid.uFlags = NIF_ICON | NIF_TIP;
  nid.hIcon = icon;
  strncpy(nid.szTip,Tip,sizeof(nid.szTip));

  Shell_NotifyIcon(NIM_MODIFY,&nid);
  DestroyIcon(icon);
}

LRESULT CALLBACK wndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  POINT pt;
  HMENU hmenu, hpopup;
  NOTIFYICONDATA nid;

  switch (message) {
    case WM_CREATE:
      SetTimer(hwnd, 0x29a, 2000, NULL);
      return TRUE;

    case WM_TIMER:
      //SetNewIcon("");
      return TRUE;

    case WM_DESTROY:
      nid.cbSize = sizeof(NOTIFYICONDATA);
      nid.hWnd = hwnd;
      nid.uID = 1;
      nid.uFlags = NIF_TIP;
      Shell_NotifyIcon(NIM_DELETE, &nid);
      DeleteObject(TC_bmp);
      DeleteObject(TC_bmp_and);
      PostQuitMessage(0);
      KillTimer(hwnd, 0x29a);
      return TRUE;

  }
  return DefWindowProc(hwnd, message, wParam, lParam);
}

void ThreadFunc(void *info)
{
  char *classname = "TrayConn.Plugin.hWnd";
  WNDCLASS wc;
  NOTIFYICONDATA nid;
  ICONINFO ii;
  HICON icon;
  MSG msg;
  unsigned char img_and[4*16] = {255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255};

  TC_hInstance = (HINSTANCE)info;
  wc.style = 0;
  wc.lpfnWndProc = wndProc;
  wc.cbClsExtra = wc.cbWndExtra = 0;
  wc.hInstance = TC_hInstance;
  wc.hIcon = LoadIcon(TC_hInstance, MAKEINTRESOURCE(101));
  wc.hCursor = LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
  wc.lpszMenuName = NULL;
  wc.lpszClassName = classname;
  if(RegisterClass(&wc) == 0)
    return false;
  TC_hwnd = CreateWindow(classname, classname, WS_POPUP, CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, NULL, NULL, TC_hInstance, NULL);
  if(TC_hwnd == NULL)
    return false;

  TC_bmp_and = CreateBitmap(16,16,1,1,img_and);
  if(TC_bmp_and == NULL)
    return false;
  TC_bmp = CreateBitmap(16,16,1,8,TC_img);
  if(TC_bmp == NULL)
  {
    DeleteObject(TC_bmp_and);
    return false;
  }
  ii.fIcon = TRUE;
  ii.hbmMask = TC_bmp_and;
  ii.hbmColor = TC_bmp;
  icon = CreateIconIndirect(&ii);
  if(icon == NULL)
  {
    DeleteObject(TC_bmp_and);
    DeleteObject(TC_bmp);
    return false;
  }

  /* Fill out NOTIFYICONDATA structure */
  nid.cbSize = sizeof(NOTIFYICONDATA);
  nid.hWnd = TC_hwnd; /* window to receive notifications */
  nid.uID = 1;     /* application-defined ID for icon (can be any UINT value) */
  nid.uFlags = NIF_MESSAGE |  /* nid.uCallbackMessage is valid, use it */
               NIF_ICON |     /* nid.hIcon is valid, use it */
               NIF_TIP;       /* nid.szTip is valid, use it */
  nid.uCallbackMessage = UWM_SYSTRAY; /* message sent to nid.hWnd */
  nid.hIcon = icon;
  nid.szTip[0] = 0;

  if(Shell_NotifyIcon(NIM_ADD,&nid) == 0)
  {
    DeleteObject(TC_bmp_and);
    DeleteObject(TC_bmp);
    DestroyIcon(icon);
    return false;
  }
  DestroyIcon(icon);

  while(GetMessage(&msg,TC_hwnd,0,0))
  {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
  return;
}

/* This is the Init fonction (Name it CAREFULLY) called on each LoadPlugin call */
FS_PLUGIN_EXPORT FS_PPlugin Plugin_Init(void *Info,void *(*QueryFunc)(int Type,...))
{
  DWORD tmp;

  /* Get pointer to plugin query function */
  PluginQueryFunc = QueryFunc;

  /* Setting all callbacks to NULL */
  Pl = (FS_PPlugin) malloc(sizeof(FS_TPlugin));
  if(Pl == NULL)
    return NULL;
  memset(Pl,0,sizeof(FS_TPlugin));

  /* Setting plugin infos */
  Pl->Name = TRAYCONN_NAME;
  Pl->Author = TRAYCONN_COPYRIGHT;
  Pl->Version = TRAYCONN_VERSION;

  /* Setting our callbacks */
  //Pl->OnCheckConfConn = OnCheckConfConn;

  /* Create a thread to manage messages */
  if(CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThreadFunc,Info,0,&tmp) == NULL)
    return NULL;
  /* And finaly returning the FS_PPlugin structure to the server.
   * If something goes wrong during this init function, free everything you have allocated and return NULL.
   * UnInit function will not be called in this case.
  */
  return Pl;
}

/* This is the UnInit fonction (Name it CAREFULLY) called on each UnLoadPlugin call */
FS_PLUGIN_EXPORT void Plugin_UnInit(void)
{
  SendMessage(TC_hwnd,WM_DESTROY,0,0);
}
