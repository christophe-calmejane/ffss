/* This is the conf conn plugin for ffss server */
/* (c) Christophe Calmejane - 2001'03           */
/*     aka Ze KiLleR / SkyTech                  */
/*                                              */
/* http://zekiller.skytech.org                  */
/* mailto : zekiller@skytech.org                */
/*
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#define CONFCONN_NAME      "Conf Conn"
#define CONFCONN_VERSION   "1.0"
#define CONFCONN_COPYRIGHT "(c) Ze KiLleR - 2002"
#define CONFCONN_PLUGIN_REG_KEY FSP_BASE_REG_KEY CONFCONN_NAME "\\"
#define CONFCONN_CONFIG_FILE "ConfConn.conf"

/* The only file we need to include is server.h */
#include "../../src/plugin.h"
#undef malloc
#undef strdup

#include "confconn.h"

char *CC_Lang[CC_LANG_COUNT][CC_LANGS_COUNT] = {/* English */
                                                {"En",
                                                 "Allows remote hosts to connect (using ip+login+pwd filter) to the server, and manage shares, eject connections, etc...",
                                                 "Remote Configuration Plugin Configuration",
                                                 "&Remove access",
                                                 "&Add",
                                                 "&Close",
                                                 " Add access ",
                                                 "Allowed IP",
                                                 "Login",
                                                 "Password",
                                                 "Login",
                                                 "Hostname",
                                                 "Allowed IP",
                                                 "You must specify a valid IP",
                                                 "You must specify a login",
                                                 "You must specify a password",
                                                 "An entry with same IP and login already exists. Remove it first",
                                                 "Are you sure you want to remove access for %s from %s ?\n",
                                                 "Error removing access"
                                                },
                                                 /* French */
                                                {"Fr",
                                                 "Permet des connexions à distances sur le serveur (en utilisant un filtre ip+login+mdp), afin de contrôler les partages, éjecter des connexions, etc...",
                                                 "Configuration du module de Configuration à Distance",
                                                 "&Retirer l'accès",
                                                 "&Ajouter",
                                                 "&Fermer",
                                                 " Ajouter un accès ",
                                                 "Adresse IP",
                                                 "Utilisateur",
                                                 "Mot de passe",
                                                 "Utilisateur",
                                                 "Hôte",
                                                 "Adresse IP",
                                                 "Vous devez spécifier une adresse IP valide",
                                                 "Vous devez spécifier un nom d'utilisateur",
                                                 "Vous devez spécifier un mot de passe",
                                                 "Une entrée avec la même adresse IP existe déja. Retirez là avant",
                                                 "Etes vous sûr de vouloir retirer l'accès à %s depuis %s ?\n",
                                                 "Erreur lors de la suppression de l'accès"
                                                }
                                               };
#define CC_LANG(x) CC_Lang[CC_CurrentLang][x]

typedef struct
{
  char *IP;
  char *Login;
  char *Pwd;
} CC_TConf, *CC_PConf;

/* We have to declare a FS_PPlugin structure for our callbacks */
FS_PPlugin Pl;
SU_PList CC_Confs = NULL; /* CC_PConf */
FSP_TInfos CC_Infos;
bool CC_AddConf(const char IP[],const char Login[],const char Pwd[]);
bool CC_DelConf(const char IP[],const char Login[]);
bool CC_Crypted = false;
unsigned int CC_CurrentLang = CC_LANG_ENGLISH;

SU_THREAD_HANDLE CC_Thr;
#ifdef _WIN32
#include "ConfConn\\resource.h"
#include "commctrl.h"
HWND CC_hwnd = NULL;
HINSTANCE CC_hInstance;
void ThreadFunc(void *info);
#endif /* _WIN32 */

/* ******************** */
/* OS dependant section */
/* ******************** */
#ifdef _WIN32
void CC_LoadLanguage(void)
{
  char buf[100];
  int i;

  SU_RB_GetStrValue(FFSS_LM_REGISTRY_PATH "FavoriteLanguage",buf,sizeof(buf),"En");
  for(i=0;i<CC_LANG_COUNT;i++)
  {
    if(stricmp(buf,CC_Lang[i][CC_LANGS_COUNTRYCODE]) == 0)
    {
      CC_CurrentLang = i;
      break;
    }
  }
}

bool CC_LoadConfig()
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
    return true;
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
  return true;
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

LRESULT CALLBACK CC_wndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  switch (message) {
    case WM_COMMAND:
      switch(LOWORD(wParam))
      {
        HWND lst,e_ip,e_login,e_pwd;
        char buf[1024],ip[100],login[50],pwd[50];
        int pos;
        DWORD adrs;
        LVITEM item;
        struct in_addr in;
        char *tmp,*host;
        BYTE f_1,f_2,f_3,f_4;

        case IDOK:
          /* Close box */
          DestroyWindow(CC_hwnd);
          CC_hwnd = NULL;
          return TRUE;
        case IDC_BUTTON1:
          lst = GetDlgItem(hwnd,(int)MAKEINTRESOURCE(IDC_LIST1));
          e_ip = GetDlgItem(hwnd,(int)MAKEINTRESOURCE(IDC_IPADDRESS1));
          e_login = GetDlgItem(hwnd,(int)MAKEINTRESOURCE(IDC_EDIT3));
          e_pwd = GetDlgItem(hwnd,(int)MAKEINTRESOURCE(IDC_EDIT4));
          if(SendMessage(e_ip,IPM_GETADDRESS,0,(DWORD)&adrs) != 4)
          {
            MessageBox(hwnd,CC_LANG(CC_LANGS_MB_IP),"Conf Conn Info",MB_OK);
            return TRUE;
          }
          GetWindowText(e_login,login,sizeof(login));
          GetWindowText(e_pwd,pwd,sizeof(pwd));
          if(login[0] == 0)
          {
            MessageBox(hwnd,CC_LANG(CC_LANGS_MB_LOGIN),"Conf Conn Info",MB_OK);
            return TRUE;
          }
          if(pwd[0] == 0)
          {
            MessageBox(hwnd,CC_LANG(CC_LANGS_MB_PWD),"Conf Conn Info",MB_OK);
            return TRUE;
          }
          f_1 = (BYTE)FIRST_IPADDRESS(adrs);f_2 = (BYTE)SECOND_IPADDRESS(adrs);f_3 = (BYTE)THIRD_IPADDRESS(adrs);f_4 = (BYTE)FOURTH_IPADDRESS(adrs);
          adrs = MAKEIPADDRESS(f_4,f_3,f_2,f_1);
          in.S_un.S_addr = adrs;
          tmp = inet_ntoa(in);
          if(!CC_AddConf(tmp,login,pwd))
          {
            MessageBox(hwnd,CC_LANG(CC_LANGS_MB_EXISTS),"Conf Conn Info",MB_OK | MB_ICONEXCLAMATION);
            return TRUE;
          }
          item.mask = LVIF_TEXT;
          item.iItem = 0;
          item.pszText = tmp;
          item.cchTextMax = strlen(item.pszText);
          item.iSubItem = 0;
          ListView_InsertItem(lst,&item);
          host = SU_NameOfPort(tmp);
          if(host == NULL)
            host = tmp;
          item.pszText = host;
          item.cchTextMax = strlen(item.pszText);
          item.iSubItem = 1;
          ListView_SetItem(lst,&item);
          item.pszText = login;
          item.cchTextMax = strlen(item.pszText);
          item.iSubItem = 2;
          ListView_SetItem(lst,&item);
          SendMessage(e_ip,IPM_CLEARADDRESS,0,0);
          SetWindowText(e_login,"");
          SetWindowText(e_pwd,"");
          return TRUE;
        case IDC_BUTTON2:
          lst = GetDlgItem(hwnd,(int)MAKEINTRESOURCE(IDC_LIST1));
          pos = ListView_GetSelectionMark(lst);
          if(pos == -1)
            return TRUE;
          ListView_GetItemText(lst,pos,0,ip,sizeof(ip));
          ListView_GetItemText(lst,pos,2,login,sizeof(login));
          snprintf(buf,sizeof(buf),CC_LANG(CC_LANGS_MB_REMOVE),login,ip);
          if(MessageBox(hwnd,buf,"Conf Conn Question",MB_YESNO) == IDYES)
          {
            if(CC_DelConf(ip,login))
              ListView_DeleteItem(lst,pos);
            else
              MessageBox(hwnd,CC_LANG(CC_LANGS_MB_ERR_REMOVE),"Conf Conn Info",MB_OK | MB_ICONEXCLAMATION);
          }
          return TRUE;
      }
      break;

    case WM_CLOSE:
      /* Close box */
      DestroyWindow(CC_hwnd);
      CC_hwnd = NULL;
      return TRUE;
    case WM_DESTROY:
      PostQuitMessage(0);
      return TRUE;
  }
  return FALSE;
}

SU_THREAD_ROUTINE(ThreadFunc,info)
{
  MSG msg;
  HWND dlg;
  LVCOLUMN col;
  INITCOMMONCONTROLSEX icc;
  CC_PConf Conf;
  SU_PList Ptr;
  LVITEM item;
  char *host;

  icc.dwSize = sizeof(INITCOMMONCONTROLSEX);
  icc.dwICC = ICC_LISTVIEW_CLASSES | ICC_WIN95_CLASSES | ICC_INTERNET_CLASSES;
  InitCommonControlsEx(&icc);
  CC_hwnd = CreateDialog(CC_hInstance,MAKEINTRESOURCE(IDD_DIALOG1),(HWND)info,CC_wndProc);
  if(CC_hwnd == NULL)
    return;

  dlg = GetDlgItem(CC_hwnd,(int)MAKEINTRESOURCE(IDC_LIST1));
  ListView_SetExtendedListViewStyleEx(dlg,LVS_EX_FULLROWSELECT,LVS_EX_FULLROWSELECT);
  col.mask = LVCF_TEXT | LVCF_WIDTH;
  col.cx = 80;
  col.pszText = CC_LANG(CC_LANGS_CLN_LOGIN);
  col.cchTextMax = strlen(col.pszText);
  ListView_InsertColumn(dlg,0,&col);
  col.cx = 180;
  col.pszText = CC_LANG(CC_LANGS_CLN_HOST);
  col.cchTextMax = strlen(col.pszText);
  ListView_InsertColumn(dlg,0,&col);
  col.cx = 100;
  col.pszText = CC_LANG(CC_LANGS_CLN_IP);
  col.cchTextMax = strlen(col.pszText);
  ListView_InsertColumn(dlg,0,&col);
  SetWindowText(CC_hwnd,CC_LANG(CC_LANGS_WND_TITLE));
  SetDlgItemText(CC_hwnd,(int)MAKEINTRESOURCE(IDC_BUTTON2),CC_LANG(CC_LANGS_BTN_REMOVE));
  SetDlgItemText(CC_hwnd,(int)MAKEINTRESOURCE(IDC_BUTTON1),CC_LANG(CC_LANGS_BTN_ADD));
  SetDlgItemText(CC_hwnd,(int)MAKEINTRESOURCE(IDOK),CC_LANG(CC_LANGS_BTN_CLOSE));
  SetDlgItemText(CC_hwnd,(int)MAKEINTRESOURCE(IDC_STATIC1),CC_LANG(CC_LANGS_GBOX_TXT));
  SetDlgItemText(CC_hwnd,(int)MAKEINTRESOURCE(IDC_STATIC2),CC_LANG(CC_LANGS_STC_IP));
  SetDlgItemText(CC_hwnd,(int)MAKEINTRESOURCE(IDC_STATIC3),CC_LANG(CC_LANGS_STC_LOGIN));
  SetDlgItemText(CC_hwnd,(int)MAKEINTRESOURCE(IDC_STATIC4),CC_LANG(CC_LANGS_STC_PWD));

  item.mask = LVIF_TEXT;
  item.iItem = 0;
  Ptr = CC_Confs;
  while(Ptr != NULL)
  {
    Conf = (CC_PConf) Ptr->Data;
    item.pszText = Conf->IP;
    item.cchTextMax = strlen(item.pszText);
    item.iSubItem = 0;
    ListView_InsertItem(dlg,&item);
    host = SU_NameOfPort(Conf->IP);
    if(host == NULL)
      host = Conf->IP;
    item.pszText = host;
    item.cchTextMax = strlen(item.pszText);
    item.iSubItem = 1;
    ListView_SetItem(dlg,&item);
    item.pszText = Conf->Login;
    item.cchTextMax = strlen(item.pszText);
    item.iSubItem = 2;
    ListView_SetItem(dlg,&item);
    Ptr = Ptr->Next;
  }

  ShowWindow(CC_hwnd,SW_SHOW);
  while(GetMessage(&msg,CC_hwnd,0,0))
  {
    if(!IsWindow(CC_hwnd) || !IsDialogMessage(CC_hwnd,&msg))
    {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }
}

/* This is the function called when plugin is requested to configure itself */
FS_PLUGIN_EXPORT bool Plugin_Configure(void *User)
{
  if(IsWindow(CC_hwnd))
    return true;

  /* Create a thread to manage messages */
  if(!SU_CreateThread(&CC_Thr,ThreadFunc,User,true))
    return false;
  return true;
}

#else /* !_WIN32 */
char *CC_Language2Char = NULL;

void CC_LoadLanguage(void)
{
  char buf[100];
  int i;

  if(CC_Language2Char == NULL)
    CC_Language2Char = "En";

  for(i=0;i<CC_LANG_COUNT;i++)
  {
    if(strcasecmp(buf,CC_Lang[i][CC_LANGS_COUNTRYCODE]) == 0)
    {
      CC_CurrentLang = i;
      break;
    }
  }
}

bool CC_LoadConfig()
{
  FILE *fp;
  char Name[512],Value[1024];
  char *ptr,*IP,*Login,*Pwd;

  fp = fopen(CONFCONN_CONFIG_FILE,"rt");
  if(fp == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"ConfConn plugin load error : Cannot open config file %s",CONFCONN_CONFIG_FILE);
    return false;
  }

  while(SU_ParseConfig(fp,Name,sizeof(Name),Value,sizeof(Value)))
  {
    if(strcasecmp(Name,"Crypt") == 0)
    {
#ifndef USE_CRYPT
      FFSS_PrintSyslog(LOG_ERR,"ConfConn plugin load error : Plugin not compiled with Crypt support... password will NOT be crypted");
#endif /* !USE_CRYPT */
      CC_Crypted = true;
    }
    else if(strcasecmp(Name,"Access") == 0)
    {
      ptr = SU_strparse(Value,':');
      if((ptr == NULL) || (ptr[0] == 0))
      {
        FFSS_PrintSyslog(LOG_ERR,"ConfConn plugin load error : Error with line %s (Missing IP)",Value);
        fclose(fp);
        return false;
      }
      IP = ptr;
      ptr = SU_strparse(NULL,':');
      if((ptr == NULL) || (ptr[0] == 0))
      {
        FFSS_PrintSyslog(LOG_ERR,"ConfConn plugin load error : Error with line %s (Missing Login)",Value);
        fclose(fp);
        return false;
      }
      Login = strdup(ptr);
      ptr = SU_strparse(NULL,':');
      if((ptr == NULL) || (ptr[0] == 0))
      {
        FFSS_PrintSyslog(LOG_ERR,"ConfConn plugin load error : Error with line %s (Missing Password)",Value);
        fclose(fp);
        return false;
      }
      Pwd = strdup(ptr);
      CC_AddConf(IP,Login,Pwd);
    }
    else if(strcasecmp(Name,"Language") == 0)
    {
      CC_Language2Char = strdup(Value);
    }
    else
    {
      FFSS_PrintSyslog(LOG_ERR,"ConfConn plugin load error : Unknown option in config file : %s %s",Name,Value);
    }
  }
  fclose(fp);

  return true;
}
void CC_SaveConfig()
{
}
#endif /* _WIN32 */

/* ********************** */
/* OS independant section */
/* ********************** */
void CC_CryptPasswd(const char Passwd[],char Crypted[],int len)
{
  char Key[3];
  char *s;

  if(strlen(Passwd) < 3)
  {
    Key[0] = 'C'; Key[1] = 'C'; Key[2] = 0;
  }
  else
  {
    Key[0] = Passwd[0]; Key[1] = Passwd[1]; Key[2] = 0;
  }
#ifdef USE_CRYPT
  s = crypt(Passwd,Key);
#else
  s = Passwd;
#endif
  if(s != NULL)
    SU_strcpy(Crypted,s,len);
  else
    Crypted[0] = 0;
}

CC_PConf CC_SearchConf(const char IP[],const char Login[])
{
  CC_PConf Conf;
  SU_PList Ptr;

  Ptr = CC_Confs;
  while(Ptr != NULL)
  {
    Conf = (CC_PConf) Ptr->Data;
    if((strcmp(IP,Conf->IP) == 0) && (strcmp(Login,Conf->Login) == 0))
      return Conf;
    Ptr = Ptr->Next;
  }
  return NULL;
}

bool CC_AddConf(const char IP[],const char Login[],const char Pwd[])
{
  CC_PConf Conf;

  if(CC_SearchConf(IP,Login) != NULL)
    return false;
  Conf = (CC_PConf) malloc(sizeof(CC_TConf));
  memset(Conf,0,sizeof(CC_TConf));
  Conf->IP = strdup(IP);
  Conf->Login = strdup(Login);
  Conf->Pwd = strdup(Pwd);
  CC_Confs = SU_AddElementHead(CC_Confs,Conf);
  return true;
}

bool CC_DelConf(const char IP[],const char Login[])
{
  CC_PConf Conf;

  Conf = CC_SearchConf(IP,Login);
  if(Conf == NULL)
    return false;
  CC_Confs = SU_DelElementElem(CC_Confs,Conf);
  return true;
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
  if(res != (int)Length)
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
  if(res != (int)Length)
    return false;
  Pwd[Length] = 0;

  if(CC_Crypted) /* Crypted password */
    CC_CryptPasswd(Pwd,Pwd,sizeof(Pwd));

  /* Now check login/pwd */
  if((strcmp(Login,Conf->Login) != 0) || (strcmp(Pwd,Conf->Pwd) != 0))
    return false;
  /* Ok, identified... accept the connection */
  return true;
}


/* This is the Init fonction (Name it CAREFULLY) called on each LoadPlugin call */
FS_PLUGIN_EXPORT FS_PPlugin Plugin_Init(void *Info,void *(*QueryFunc)(int Type,...))
{
#ifdef _WIN32
  CC_hInstance = (HINSTANCE)Info;
#endif /* _WIN32 */
  /* Setting all callbacks to NULL */
  Pl = (FS_PPlugin) malloc(sizeof(FS_TPlugin));
  if(Pl == NULL)
    return NULL;
  memset(Pl,0,sizeof(FS_TPlugin));

  /* Setting plugin infos */
  Pl->size = sizeof(FS_TPlugin);
  Pl->Name = CONFCONN_NAME;
  Pl->Copyright = CONFCONN_COPYRIGHT;
  Pl->Version = CONFCONN_VERSION;

  /* Setting our callbacks */
  Pl->OnCheckConfConn = OnCheckConfConn;

  CC_LoadLanguage();
  if(!CC_LoadConfig())
  {
    free(Pl);
    return NULL;
  }
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
#ifdef _WIN32
  PostMessage(CC_hwnd,WM_DESTROY,0,0);
  SU_USLEEP(200);
#endif /* !_WIN32 */
  SU_KillThread(CC_Thr);
}

FS_PLUGIN_EXPORT FSP_PInfos Plugin_QueryInfos(void)
{
  CC_LoadLanguage();

  CC_Infos.Name = CONFCONN_NAME;
  CC_Infos.Version = CONFCONN_VERSION;
  CC_Infos.Copyright = CONFCONN_COPYRIGHT;
  CC_Infos.Description = CC_LANG(CC_LANGS_DESCRIPTION);
  return &CC_Infos;
}
