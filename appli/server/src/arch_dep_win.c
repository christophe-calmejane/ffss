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
/* WARNINGS
 *  - No '|' in share names
 *  - No ',' in login or password of users
 */

#include "server.h"

#define FFSS_REGISTRY_PATH_SERVER FFSS_LM_REGISTRY_PATH "Server\\"

HANDLE FS_GlobalMutex = 0;

void FS_MainThread(void)
{
  while(1)
  {
    SU_SLEEP(60);
  }
}

bool FS_IsAlreadyRunning(void)
{
  FS_GlobalMutex = CreateMutex(NULL,false,"FFSSServerGlobalMutex");
  if(FS_GlobalMutex == 0)
    return false;
  if(GetLastError() == ERROR_ALREADY_EXISTS)
    return true;
  return false;
}

bool FS_LoadConfig(const char FileName[])
{
  char Shares[4096];
  char *p,*q,*r,*s;
  char *u_l,*u_p,*u_w;
  char key[4096];
  char Path[1024];
  char Comment[FFSS_MAX_SHARECOMMENT_LENGTH+1];
  char Users[2048];
  SU_PList Ptr;
  FS_PUser Usr;
  int  Writeable;
  int  Private;
  int NoChksum;
  int  MaxConn;
  char GBL_Name[FFSS_MAX_SERVERNAME_LENGTH+1];
  char GBL_Comment[FFSS_MAX_SERVERCOMMENT_LENGTH+1];
  char GBL_Master[1024];
  HKEY HK;

	SU_RB_SetRegistry64Mode(SU_RB_MODE_FORCE_WOW64_KEY);
  SU_SEM_WAIT(FS_SemGbl);
  GetCurrentDirectory(sizeof(Path),Path);
  SU_RB_SetStrValue(FFSS_REGISTRY_PATH_SERVER "ServerDirectory",Path);
  SU_RB_SetIntValue(FFSS_REGISTRY_PATH_SERVER "ProcessId",GetCurrentProcessId());
  SU_RB_SetStrValue(FFSS_LM_REGISTRY_PATH "CurrentVersion",FFSS_VERSION);

  FS_MyGlobal.ConfSock = true;
  SU_DBG_PrintDebug(FS_DBGMSG_GLOBAL,"Loading config from registry");
  SU_RB_GetStrValue(FFSS_REGISTRY_PATH_SERVER "ShareNames",Shares,sizeof(Shares),"");
  if(Shares[0] == 0)
    p = NULL;
  else
  {
    p = Shares;
    s = strchr(Shares,'|');
    if(s != NULL)
    {
      s[0] = 0;
      s++;
    }
  }
  while(p != NULL)
  {
    /* Get Share Path */
    _snprintf(key,sizeof(key),"%s%s_Path",FFSS_REGISTRY_PATH_SERVER,p);
    SU_RB_GetStrValue(key,Path,sizeof(Path),"");
    /* Get Share Comment */
    _snprintf(key,sizeof(key),"%s%s_Comment",FFSS_REGISTRY_PATH_SERVER,p);
    SU_RB_GetStrValue(key,Comment,sizeof(Comment),"");
    /* Get Share Writeable */
    _snprintf(key,sizeof(key),"%s%s_Writeable",FFSS_REGISTRY_PATH_SERVER,p);
    SU_RB_GetIntValue(key,&Writeable,0);
    /* Get Share Private */
    _snprintf(key,sizeof(key),"%s%s_Private",FFSS_REGISTRY_PATH_SERVER,p);
    SU_RB_GetIntValue(key,&Private,0);
    /* Get Share NoChksum */
    _snprintf(key,sizeof(key),"%s%s_NoChksum",FFSS_REGISTRY_PATH_SERVER,p);
    SU_RB_GetIntValue(key,&NoChksum,0);
    /* Get Share MaxConnections */
    _snprintf(key,sizeof(key),"%s%s_MaxConnections",FFSS_REGISTRY_PATH_SERVER,p);
    SU_RB_GetIntValue(key,&MaxConn,0);
    /* Get Share Users */
    Ptr = NULL;
    _snprintf(key,sizeof(key),"%s%s_Users",FFSS_REGISTRY_PATH_SERVER,p);
    SU_RB_GetStrValue(key,Users,sizeof(Users),"");
    q = Users;
    r = strchr(q,',');
    while(r != NULL)
    {
      r[0] = 0; r++;
      u_l = q;
      q = r;
      r = strchr(q,',');
      if(r == NULL)
        break;
      r[0] = 0; r++;
      u_p = q;
      q = r;
      r = strchr(q,',');
      if(r != NULL)
      {
        r[0] = 0; r++;
      }
      u_w = q;
      q = r;
      Usr = (FS_PUser) malloc(sizeof(FS_TUser));
      memset(Usr,0,sizeof(FS_TUser));
      Usr->Login = strdup(u_l);
      Usr->Password = strdup(u_p);
      Usr->Writeable = atoi(u_w);
      Ptr = SU_AddElementHead(Ptr,Usr);
      if(q == NULL)
        break;
      else
        r = strchr(q,',');
    }
    if(Path[0] != 0)
    {
      /* Building index */
      FS_BuildIndex(Path,p,Comment,(bool)Writeable,(bool)Private,(bool)NoChksum,MaxConn,Ptr,false);
    }

    if((s == NULL) || (s[0] == 0))
      p = NULL;
    else
    {
      p = s;
      s = strchr(s,'|');
      if(s != NULL)
      {
        s[0] = 0;
        s++;
      }
    }
  }
  /* Get global Name */
  SU_RB_GetStrValue(FFSS_REGISTRY_PATH_SERVER "Global_Name",GBL_Name,sizeof(GBL_Name),"Nobody");
  FS_MyGlobal.Name = strdup(GBL_Name);
  /* Get global Comment */
  SU_RB_GetStrValue(FFSS_REGISTRY_PATH_SERVER "Global_Comment",GBL_Comment,sizeof(GBL_Comment),"Misconfigured server");
  FS_MyGlobal.Comment = strdup(GBL_Comment);
  /* Get global Master */
  SU_RB_GetStrValue(FFSS_REGISTRY_PATH_SERVER "Global_Master",GBL_Master,sizeof(GBL_Master),"");
  if(GBL_Master[0] != 0)
    FS_MyGlobal.Master = strdup(GBL_Master);
  /* Get global Idle */
  SU_RB_GetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_Idle",&FS_MyGlobal.Idle,5*60);
  /* Get global MaxConn */
  SU_RB_GetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_MaxConn",&FS_MyGlobal.MaxConn,FFSS_DEFAULT_MAX_CONN);
  /* Get global MaxXFerPerConn */
  SU_RB_GetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_MaxXFerPerConn",&FS_MyGlobal.MaxXFerPerConn,FFSS_DEFAULT_MAX_XFER_PER_CONN);
  /* Get global FTP */
  SU_RB_GetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_FTP",&FS_MyGlobal.FTP,0);
  /* Get global FTP MaxConn */
  SU_RB_GetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_FTP_MaxConn",&FS_MyGlobal.FTPMaxConn,10);
  /* Get global XFerInConn */
  SU_RB_GetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_XFerInConn",&FS_MyGlobal.XFerInConn,0);
  /* Get global ReadBufferSize */
  SU_RB_GetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_ReadBufferSize",&FFSS_TransferReadBufferSize,FFSS_TRANSFER_READ_BUFFER_SIZE);
  /* Get global BufferSize */
  SU_RB_GetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_XFerBufferSize",&FFSS_TransferBufferSize,FFSS_TRANSFER_BUFFER_SIZE);

  /* Load plugins */
  _snprintf(key,sizeof(key),"%sPlugins\\",FFSS_REGISTRY_PATH_SERVER);
  HK = SU_RB_OpenKeys(key,KEY_READ);
  if(HK != NULL)
  {
    int idx = 0;
    char buf[4096];
    LONG ret = ERROR_SUCCESS;
    DWORD len,len2;
    FS_PPlugin Pl;

    while(ret != ERROR_NO_MORE_ITEMS)
    {
      len = sizeof(key);
      len2 = sizeof(buf);
      ret = RegEnumValue(HK,idx,key,&len,NULL,NULL,buf,&len2);
      if(ret == ERROR_SUCCESS)
      {
        Pl = FS_LoadPlugin(buf);
        if(Pl != NULL)
          Pl->Startup = true;
      }
      idx++;
    }
    RegCloseKey(HK);
  }
  SU_SEM_POST(FS_SemGbl);
  return true;
}

/* Locks FS_SemShr & FS_SemGbl */
bool FS_SaveConfig(const char FileName[])
{
  char Shares[10000];
  SU_PList Ptr,Ptr2;
  FS_PShare Share;
  char key[10000];
  char Users[2048];
  FS_PUser Usr;

  SU_DBG_PrintDebug(FS_DBGMSG_GLOBAL,"Saving config to registry");
  SU_SEM_WAIT(FS_SemShr);
  Ptr = FS_Index;
  Shares[0] = 0;
  while(Ptr != NULL)
  {
    Share = (FS_PShare) Ptr->Data;
    SU_strcat(Shares,Share->ShareName,sizeof(Shares));
    if(Ptr->Next != NULL)
      SU_strcat(Shares,"|",sizeof(Shares));

    /* Set Share Path */
    _snprintf(key,sizeof(key),"%s%s_Path",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
    SU_RB_SetStrValue(key,Share->Path);
    /* Set Share Comment */
    _snprintf(key,sizeof(key),"%s%s_Comment",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
    SU_RB_SetStrValue(key,Share->Comment);
    /* Set Share Writeable */
    _snprintf(key,sizeof(key),"%s%s_Writeable",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
    SU_RB_SetIntValue(key,Share->Writeable);
    /* Set Share Private */
    _snprintf(key,sizeof(key),"%s%s_Private",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
    SU_RB_SetIntValue(key,Share->Private);
    /* Set Share NoChksum */
    _snprintf(key,sizeof(key),"%s%s_NoChksum",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
    SU_RB_SetIntValue(key,Share->NoChksum);
    /* Set Share MaxConnections */
    _snprintf(key,sizeof(key),"%s%s_MaxConnections",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
    SU_RB_SetIntValue(key,Share->MaxConnections);
    /* Set Share Users */
    Ptr2 = Share->Users;
    Users[0] = 0;
    while(Ptr2 != NULL)
    {
      Usr = (FS_PUser) Ptr2->Data;
      SU_strcat(Users,Usr->Login,sizeof(Users));
      SU_strcat(Users,",",sizeof(Users));
      SU_strcat(Users,Usr->Password,sizeof(Users));
      if(Usr->Writeable)
        SU_strcat(Users,",1",sizeof(Users));
      else
        SU_strcat(Users,",0",sizeof(Users));
      if(Ptr2->Next != NULL)
        SU_strcat(Users,",",sizeof(Users));

      Ptr2 = Ptr2->Next;
    }
    _snprintf(key,sizeof(key),"%s%s_Users",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
    SU_RB_SetStrValue(key,Users);

    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemShr);
  SU_RB_SetStrValue(FFSS_REGISTRY_PATH_SERVER "ShareNames",Shares);

  SU_SEM_WAIT(FS_SemGbl);
  /* Set global Name */
  SU_RB_SetStrValue(FFSS_REGISTRY_PATH_SERVER "Global_Name",FS_MyGlobal.Name);
  /* Set global Comment */
  SU_RB_SetStrValue(FFSS_REGISTRY_PATH_SERVER "Global_Comment",FS_MyGlobal.Comment);
  /* Set global Master */
  if(FS_MyGlobal.Master != NULL)
    SU_RB_SetStrValue(FFSS_REGISTRY_PATH_SERVER "Global_Master",FS_MyGlobal.Master);
  else
    SU_RB_SetStrValue(FFSS_REGISTRY_PATH_SERVER "Global_Master","");
  /* Set global Idle */
  SU_RB_SetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_Idle",FS_MyGlobal.Idle);
  /* Set global MaxConn */
  SU_RB_SetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_MaxConn",FS_MyGlobal.MaxConn);
  /* Set global MaxXFerPerConn */
  SU_RB_SetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_MaxXFerPerConn",FS_MyGlobal.MaxXFerPerConn);
  /* Set global FTP */
  SU_RB_SetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_FTP",FS_MyGlobal.FTP);
  /* Set global FTP MaxConn */
  SU_RB_SetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_FTP_MaxConn",FS_MyGlobal.FTPMaxConn);
  /* Set global XFerInConn */
  SU_RB_SetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_XFerInConn",FS_MyGlobal.XFerInConn);
  /* Set global ReadBufferSize */
  SU_RB_SetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_ReadBufferSize",FFSS_TransferReadBufferSize);
  /* Set global BufferSize */
  SU_RB_SetIntValue(FFSS_REGISTRY_PATH_SERVER "Global_XFerBufferSize",FFSS_TransferBufferSize);

  SU_SEM_POST(FS_SemGbl);
  return true;
}

void FS_RemoveShare(FS_PShare Share)
{
  char key[10000];

  /* Del Share Path */
  _snprintf(key,sizeof(key),"%s%s_Path",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
  SU_RB_DelValue(key);
  /* Del Share Comment */
  _snprintf(key,sizeof(key),"%s%s_Comment",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
  SU_RB_DelValue(key);
  /* Del Share Writeable */
  _snprintf(key,sizeof(key),"%s%s_Writeable",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
  SU_RB_DelValue(key);
  /* Del Share Private */
  _snprintf(key,sizeof(key),"%s%s_Private",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
  SU_RB_DelValue(key);
  /* Del Share NoChksum */
  _snprintf(key,sizeof(key),"%s%s_NoChksum",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
  SU_RB_DelValue(key);
  /* Del Share MaxConnections */
  _snprintf(key,sizeof(key),"%s%s_MaxConnections",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
  SU_RB_DelValue(key);
  /* Del Share Users */
  _snprintf(key,sizeof(key),"%s%s_Users",FFSS_REGISTRY_PATH_SERVER,Share->ShareName);
  SU_RB_DelValue(key);
}

/* Assumes FS_SemShr is locked */
bool FS_CheckDirectoryChanged(FS_PShare Share)
{
  SU_DBG_PrintDebug(FS_DBGMSG_GLOBAL,"Checking for a change in share %s (%s)",Share->ShareName,Share->Path);
  if(Share->NotifyHandle == NULL)
  {
    Share->NotifyHandle = FindFirstChangeNotification(Share->Path,true,FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME);
    if(Share->NotifyHandle == NULL)
      return false;
  }
  if(WaitForSingleObject(Share->NotifyHandle,0) == WAIT_OBJECT_0) /* Change detected */
  {
    FindNextChangeNotification(Share->NotifyHandle);
    return true;
  }
  return false;
}

void FS_AddPluginToStartup(FS_PPlugin Plugin)
{
  char tmp[1024];
  _snprintf(tmp,sizeof(tmp),"%sPlugins\\%s",FFSS_REGISTRY_PATH_SERVER,Plugin->Name);
  SU_RB_SetStrValue(tmp,Plugin->Path);
  Plugin->Startup = true;
}

void FS_RemovePluginFromStartup(FS_PPlugin Plugin)
{
  char tmp[1024];
  _snprintf(tmp,sizeof(tmp),"%sPlugins\\%s",FFSS_REGISTRY_PATH_SERVER,Plugin->Name);
  SU_RB_DelValue(tmp);
  Plugin->Startup = false;
}

void FS_ShuttingDown()
{
  SU_RB_SetIntValue(FFSS_REGISTRY_PATH_SERVER "ProcessId",0);
  CloseHandle(FS_GlobalMutex);
}
