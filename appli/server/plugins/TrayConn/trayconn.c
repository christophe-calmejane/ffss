/* This is the Tray Conn plugin for ffss server */
/*    (c) Christophe Calmejane - 2002           */
/*     aka Ze KiLleR / SkyTech                  */
/*                                              */
/* http://zekiller.skytech.org                  */
/* mailto : zekiller@skytech.org                */

/* TODO : dblclic gauche : ouvre manager
          clic droit : menu contextuel : eject tout le monde...
*/

#define TRAYCONN_NAME      "Tray Conn Plugin"
#define TRAYCONN_VERSION   "0.5"
#define TRAYCONN_COPYRIGHT "(c) Ze KiLleR - 2002"
#define TRAYCONN_DESCRIPTION "Displays an icon in the system tray which shows how many connections and downloads are currently running.\n A double clic on the icon opens the share manager, and a right clic pops up a contextual menu which allows to eject everybody, set the server into quiet mode, and shutdown it."

#define MANAGER_PATH_REG_KEY "HKEY_CURRENT_USER\\Software\\FFSS\\Server\\ManagerPath"
#define TIMER_DELAY 5*1000 /* Every 5 sec */
#define TIMER_KEY 0x29a
#include "TrayConn\\resource.h"

/* The only file we need to include is server.h */
#include "../../src/plugin.h"
#undef malloc
#undef strdup

/* We have to declare a FS_PPlugin structure for our callbacks */
FS_PPlugin Pl;
FSP_TInfos TC_Infos;
SU_THREAD_HANDLE TC_Thr;

void * (*PluginQueryFunc)(int Type,...);

#define clNone    0x123456
#define clBlack   0x000000
#define clWhite   0xFFFFFF
#define clBlue    0x0000FF
#define clNavy    0x000080
#define clLime    0x00FF00
#define clGreen   0x008000
#define clRed     0xFF0000
#define clMaroon  0x800000
#define clAqua    0x00FFFF
#define clFuchsia 0xFF00FF
#define clYellow  0xFFFF00
#define clTeal    0x008080
#define clPurple  0x800080
#define clOlive   0x808000
#define clLtGray  0xC0C0C0
#define clDkGray  0x808080


HWND TC_hwnd;
HINSTANCE TC_hInstance;
int TC_State;
HBITMAP TC_bmp,TC_bmp_and;
unsigned long int TC_img_orig[16][16] = {clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,
                                         clLtGray,clDkGray,clDkGray,clDkGray,clDkGray,clDkGray,clWhite,clLtGray,clLtGray,clDkGray,clDkGray,clDkGray,clDkGray,clDkGray,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,clLtGray,clDkGray,clGreen,clGreen,clGreen,clGreen,clWhite,clLtGray,
                                         clLtGray,clDkGray,clWhite,clWhite,clWhite,clWhite,clWhite,clLtGray,clLtGray,clDkGray,clWhite,clWhite,clWhite,clWhite,clWhite,clLtGray,
                                         clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray,clLtGray};
unsigned long int TC_img_over[16][16] = {clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,
                                         clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,
                                         clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,
                                         clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,
                                         clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,
                                         clNone,clNone,clNone,clNone,clNone,clNone,clNone,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clNone,
                                         clNone,clNone,clNone,clNone,clNone,clNone,clNone,clBlack,clBlack,clBlack,clBlack,clBlack,clBlack,clBlack,clNone,clNone,
                                         clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clBlack,clBlack,clBlack,clBlack,clBlack,clBlack,clBlack,clWhite,clNone,
                                         clWhite,clBlack,clBlack,clBlack,clBlack,clBlack,clBlack,clWhite,clWhite,clWhite,clWhite,clBlack,clBlack,clWhite,clWhite,clNone,
                                         clWhite,clBlack,clBlack,clBlack,clBlack,clBlack,clBlack,clWhite,clWhite,clWhite,clBlack,clBlack,clWhite,clWhite,clWhite,clNone,
                                         clWhite,clWhite,clWhite,clWhite,clBlack,clBlack,clWhite,clWhite,clWhite,clBlack,clBlack,clWhite,clWhite,clWhite,clWhite,clNone,
                                         clWhite,clWhite,clBlack,clBlack,clWhite,clWhite,clWhite,clWhite,clBlack,clBlack,clWhite,clWhite,clWhite,clWhite,clWhite,clNone,
                                         clWhite,clBlack,clBlack,clBlack,clBlack,clBlack,clWhite,clBlack,clBlack,clBlack,clBlack,clBlack,clBlack,clBlack,clWhite,clNone,
                                         clWhite,clBlack,clBlack,clBlack,clBlack,clBlack,clWhite,clBlack,clBlack,clBlack,clBlack,clBlack,clBlack,clBlack,clWhite,clNone,
                                         clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clWhite,clNone,
                                         clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone,clNone};
unsigned long int TC_img[16][16] = {clBlack,};
char TC_Tip[64] = {0,};

#define UWM_SYSTRAY (WM_USER + 1)
#define JAUGE_OFS_BOTTOM 13
#define JAUGE_HEIGHT     12
#define JAUGE_OFS_LEFT   2
#define JAUGE_OFS_RIGHT  10
#define JAUGE_WIDTH      4

void DrawJauge(int ofs,int nb)
{
  int i,j;

  for(i=0;i<nb;i++)
  {
    for(j=0;j<JAUGE_WIDTH;j++)
      TC_img[JAUGE_OFS_BOTTOM-i][ofs+j] = clLime;
  }
}

void DrawOverlay(void)
{
  int i,j;

  if(TC_State != FFSS_STATE_QUIET)
    return;
  for(i=0;i<16;i++)
  {
    for(j=0;j<16;j++)
    {
      if(TC_img_over[i][j] != clNone)
        TC_img[i][j] = TC_img_over[i][j];
    }
  }
}

char *DrawBitmap(void)
{
  FS_PGlobal Gbl;
  SU_PList Index,Ptr,Ptr2; /* FS_PShare */
  FS_PShare Shr;
  FS_PConn Conn;
  unsigned int nb_shares = 0;
  unsigned int max_xfers = 0,nb_xfers = 0;
  unsigned int max_conns = 0,nb_conns = 0;

  Index = (SU_PList) PluginQueryFunc(FSPQ_ACQUIRE_INDEX);
  Ptr = Index;
  while(Ptr != NULL)
  {
    Shr = (FS_PShare) Ptr->Data;
    Ptr2 = Shr->Conns;
    while(Ptr2 != NULL)
    {
      Conn = (FS_PConn) Ptr2->Data;
      nb_xfers += SU_ListCount(Conn->XFers); /* Add streaming here... one day */
      Ptr2 = Ptr2->Next;
    }
    max_conns += Shr->MaxConnections;
    nb_shares++;
    Ptr = Ptr->Next;
  }
  PluginQueryFunc(FSPQ_RELEASE_INDEX);

  Gbl = (FS_PGlobal) PluginQueryFunc(FSPQ_ACQUIRE_GLOBAL);
  max_xfers = Gbl->MaxXFerPerConn * nb_shares;
  max_conns = max(Gbl->MaxConn,max_conns);
  nb_conns = Gbl->Conn;
  PluginQueryFunc(FSPQ_RELEASE_GLOBAL);
  
  memcpy(&TC_img,&TC_img_orig,sizeof(TC_img)); /* Reset icon */
  DrawJauge(JAUGE_OFS_LEFT,(int)(nb_conns/(float)max_conns*JAUGE_HEIGHT));
  DrawJauge(JAUGE_OFS_RIGHT,(int)(nb_xfers/(float)max_xfers*JAUGE_HEIGHT));
  DrawOverlay();

  snprintf(TC_Tip,sizeof(TC_Tip),"FFSS : %d conn(s) - %d xfer(s)",nb_conns,nb_xfers);
  return TC_Tip;
}

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
      SetTimer(hwnd,TIMER_KEY,TIMER_DELAY,NULL);
      return TRUE;

    case WM_TIMER:
      TC_State = (int) PluginQueryFunc(FSPQ_GET_STATE);
      SetNewIcon(DrawBitmap());
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
      KillTimer(hwnd,TIMER_KEY);
      return TRUE;

    case UWM_SYSTRAY: /* We are being notified of mouse activity over the icon */
      switch (lParam)
      {
        MENUITEMINFO mi;
        case WM_RBUTTONUP: /* Let's track a popup menu */
          GetCursorPos(&pt);
          hmenu = LoadMenu(TC_hInstance,MAKEINTRESOURCE(IDR_MENU1));
          memset(&mi,0,sizeof(mi));
          mi.cbSize = sizeof(mi);
          mi.fMask = MIIM_STATE;
          TC_State = (int) PluginQueryFunc(FSPQ_GET_STATE);
          if(TC_State == FFSS_STATE_QUIET)
            mi.fState = MFS_CHECKED;
          else
            mi.fState = MFS_ENABLED;
          SetMenuItemInfo(hmenu,IDM_SILENT,FALSE,&mi);
          hpopup = GetSubMenu(hmenu, 0);
          SetForegroundWindow(hwnd);
          switch (TrackPopupMenu(hpopup,TPM_RETURNCMD | TPM_RIGHTBUTTON,pt.x, pt.y,0,hwnd,NULL))
          {
            case IDM_QUIT:
              if(MessageBox(hwnd,"You are requesting FFSS Server to shut down.\n                     Are you sure ??","Tray Conn Plugin Info",MB_YESNO | MB_DEFBUTTON2 |MB_ICONEXCLAMATION) == IDYES)
                PluginQueryFunc(FSPQ_SHUTDOWN);
              break;
            case IDM_EJECT:
              PluginQueryFunc(FSPQ_EJECT_ALL); /* Eject everybody */
              SetNewIcon(DrawBitmap());
              break;
            case IDM_SILENT:
              if(TC_State == FFSS_STATE_QUIET)
                TC_State = FFSS_STATE_ON;
              else
                TC_State = FFSS_STATE_QUIET;
              PluginQueryFunc(FSPQ_SET_STATE,TC_State); /* Setting new state */
              SetNewIcon(DrawBitmap());
              break;
          }
          PostMessage(hwnd,0,0,0);
          DestroyMenu(hmenu); /* Delete loaded menu and reclaim its resources */
          break;

        case WM_LBUTTONDBLCLK: /* Open Share Manager */
        {
          STARTUPINFO sti;
          PROCESS_INFORMATION pi;
          char buf[1024];

          memset(&sti,0,sizeof(sti));
          memset(&pi,0,sizeof(pi));
          SU_RB_GetStrValue(MANAGER_PATH_REG_KEY,buf,sizeof(buf),"");
          CreateProcess(buf,buf,NULL,NULL,false,0,NULL,NULL,&sti,&pi);
          break;
        }
      }
      return TRUE;

  }
  return DefWindowProc(hwnd, message, wParam, lParam);
}

SU_THREAD_ROUTINE(ThreadFunc,info)
{
  char *classname = "TrayConn.Plugin.hWnd";
  WNDCLASS wc;
  NOTIFYICONDATA nid;
  ICONINFO ii;
  HICON icon;
  MSG msg;
  unsigned char img_and[16*16] = {0,};

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
    return;
  TC_hwnd = CreateWindow(classname, classname, WS_POPUP, CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, NULL, NULL, TC_hInstance, NULL);
  if(TC_hwnd == NULL)
    return;

  TC_bmp_and = CreateBitmap(16,16,1,1,img_and);
  if(TC_bmp_and == NULL)
    return;
  TC_bmp = CreateBitmap(16,16,1,32,TC_img_orig);
  if(TC_bmp == NULL)
  {
    DeleteObject(TC_bmp_and);
    return;
  }
  TC_State = (int) PluginQueryFunc(FSPQ_GET_STATE);
  TC_State = FFSS_STATE_QUIET;
  DrawBitmap();
  ii.fIcon = TRUE;
  ii.hbmMask = TC_bmp_and;
  ii.hbmColor = TC_bmp;
  icon = CreateIconIndirect(&ii);
  if(icon == NULL)
  {
    DeleteObject(TC_bmp_and);
    DeleteObject(TC_bmp);
    return;
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
    return;
  }
  DestroyIcon(icon);

  while(GetMessage(&msg,TC_hwnd,0,0))
  {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
}

/* This is the Init fonction (Name it CAREFULLY) called on each LoadPlugin call */
FS_PLUGIN_EXPORT FS_PPlugin Plugin_Init(void *Info,void *(*QueryFunc)(int Type,...))
{
  /* Get pointer to plugin query function */
  PluginQueryFunc = QueryFunc;

  /* Setting all callbacks to NULL */
  Pl = (FS_PPlugin) malloc(sizeof(FS_TPlugin));
  if(Pl == NULL)
    return NULL;
  memset(Pl,0,sizeof(FS_TPlugin));

  /* Setting plugin infos */
  Pl->size = sizeof(FS_TPlugin);
  Pl->Name = TRAYCONN_NAME;
  Pl->Copyright = TRAYCONN_COPYRIGHT;
  Pl->Version = TRAYCONN_VERSION;

  /* Create a thread to manage messages */
  if(!SU_CreateThread(&TC_Thr,ThreadFunc,Info,true))
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
  SU_KillThread(TC_Thr);
}

FS_PLUGIN_EXPORT FSP_PInfos Plugin_QueryInfos(void)
{
  TC_Infos.Name = TRAYCONN_NAME;
  TC_Infos.Version = TRAYCONN_VERSION;
  TC_Infos.Copyright = TRAYCONN_COPYRIGHT;
  TC_Infos.Description = TRAYCONN_DESCRIPTION;
  return &TC_Infos;
}
