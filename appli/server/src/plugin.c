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
#include "plugin.h"
#include <stdarg.h>

#ifdef _WIN32
extern HINSTANCE FS_hInstance;
extern HWND FS_hwnd;
#endif /* _WIN32 */

void *FS_PluginQuery(int Type,...)
{
  va_list ap;
  void *ret = NULL;

  va_start(ap,Type);
  switch(Type)
  {
    case FSPQ_ACQUIRE_GLOBAL : /* No param */
      SU_SEM_WAIT(FS_SemGbl);
      ret = (void *) &FS_MyGlobal;
      break;
    case FSPQ_RELEASE_GLOBAL : /* No param */
      SU_SEM_POST(FS_SemGbl);
      break;
    case FSPQ_ACQUIRE_INDEX : /* No param */
      SU_SEM_WAIT(FS_SemShr);
      ret = (void *) FS_Index;
      break;
    case FSPQ_RELEASE_INDEX : /* No Param */
      SU_SEM_POST(FS_SemShr);
      break;
      //I = va_arg(ap, BN_PInfo); /* I */

    case FSPQ_GET_FILTER_API : /* No Param */
      if(FFSS_Filter_Api.Initialized)
        ret = (void *)&FFSS_Filter_Api;
      break;

    case FSPQ_GET_QOS_API : /* No Param */
      if(FFSS_QoS_Api.Initialized)
        ret = (void *)&FFSS_QoS_Api;
      break;

    case FSPQ_GET_STATE : /* No Param */
      ret = (void *)FS_MyState;
      break;
    case FSPQ_SET_STATE : /* (int NewState) */
      FS_MyState = va_arg(ap, int);
      SU_SEM_WAIT(FS_SemGbl);
      if(FS_MyGlobal.Master != NULL)
      {
        /* Sending new state to my master */
        FS_SendMessage_State(FS_MyGlobal.Master,FS_MyGlobal.Name,FFSS_GetOS(),FS_MyGlobal.Comment,FS_MyState);
      }
      SU_SEM_POST(FS_SemGbl);
      break;
    case FSPQ_EJECT_ALL :
      FS_EjectAll(true);
      break;

#ifdef _WIN32
    case FSPQ_SHUTDOWN : /* Only supported on Win32 */
      SendMessage(FS_hwnd,WM_CLOSE,0,0);
      break;
#endif /* _WIN32 */

    case FSPQ_DBG_FLAGS : /* (SU_u64 Flags) */
      SU_DBG_SetFlags(va_arg(ap, SU_u64));
      break;
    case FSPQ_DBG_OUTPUT : /* (SU_u16 Output) */
      SU_DBG_SetOutput(va_arg(ap, unsigned int));
      break;
  }
  va_end(ap);
  return ret;
}

/* Locks FS_SemPlugin */
FS_PPlugin FS_LoadPlugin(const char Name[])
{
#ifdef PLUGINS
  SU_DL_HANDLE handle;
  FS_PPlugin (*fonc)(void *,void *(*QueryFunc)(int Type,...));
  void (*fonc2)(void);
  FS_PPlugin Pl;

  handle = SU_DL_OPEN(Name);
  if(!handle)
  {
#ifdef _WIN32
    FFSS_PrintSyslog(LOG_ERR,"Couldn't open %s (%d)\n",Name,GetLastError());
#else /* !_WIN32 */
    FFSS_PrintSyslog(LOG_ERR,"Couldn't open %s (%s)\n",Name,dlerror());
#endif /* _WIN32 */
    return NULL;
  }

  fonc = (FS_PPlugin (*)(void *,void *(*QueryFunc)(int Type,...))) SU_DL_SYM(handle,"Plugin_Init");
  if(fonc == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Couldn't find Plugin_Init function for %s\n",Name);
    return NULL;
  }
#ifdef _WIN32
  Pl = fonc((void *)handle,&FS_PluginQuery);
#else /* !_WIN32 */
  Pl = fonc(NULL,&FS_PluginQuery);
#endif /* _WIN32 */
  if(Pl == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error in Plugin_Init function for %s\n",Name);
    SU_DL_CLOSE(handle);
    return NULL;
  }
  if(Pl->size != sizeof(FS_TPlugin))
  {
    FFSS_PrintSyslog(LOG_ERR,"Plugin_Init function returned a wrong sized FS_PPlugin structure (%d instead of %d)... rebuild plugin %s\n",Pl->size,sizeof(FS_TPlugin),Name);
    fonc2 = (void(*)(void))SU_DL_SYM(handle,"Plugin_UnInit");
    if(fonc2 != NULL)
      fonc2();
    SU_DL_CLOSE(handle);
    return NULL;
  }
  Pl->Configurable = (SU_DL_SYM(handle,"Plugin_Configure") != NULL);
  Pl->Handle = handle;
  Pl->Path = strdup(Name);
  SU_SEM_WAIT(FS_SemPlugin);
  FS_Plugins = SU_AddElementHead(FS_Plugins,(void *)Pl);
  SU_SEM_POST(FS_SemPlugin);
  SU_DBG_PrintDebug(FS_DBGMSG_GLOBAL,"Plugin successfully loaded : %s",Pl->Path);
#endif /* PLUGINS */
  return Pl;
}

/* Locks FS_SemPlugin */
void FS_UnLoadPlugin(SU_DL_HANDLE Handle)
{
#ifdef PLUGINS
  void (*Fonc)(void);
  SU_PList Ptr;
  FS_PPlugin Pl;
  int i;

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  i = 0;
  while(Ptr != NULL)
  {
    Pl = (FS_PPlugin) Ptr->Data;
    if(Handle == Pl->Handle)
    {
      if(Pl->Path != NULL)
        free(Pl->Path);
      /* Do not free anything else.... freed by the plugin itself */
      Fonc = (void(*)(void))SU_DL_SYM(Handle,"Plugin_UnInit");
      if(Fonc != NULL)
        Fonc();
      SU_DL_CLOSE(Handle);
      FS_Plugins = SU_DelElementPos(FS_Plugins,i);
      break;
    }
    i++;
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
#endif /* PLUGINS */
}

/* Locks FS_SemPlugin */
void FS_UnLoadAllPlugin(void)
{
#ifdef PLUGINS
  void (*Fonc)(void);
  SU_PList Ptr;
  FS_PPlugin Pl;

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    Pl = (FS_PPlugin) Ptr->Data;
    if(Pl->Path != NULL)
      free(Pl->Path);
    /* Do not free anything else.... freed by the plugin itself */
    Fonc = (void(*)(void))SU_DL_SYM(Pl->Handle,"Plugin_UnInit");
    if(Fonc != NULL)
      Fonc();
    SU_DL_CLOSE(Pl->Handle);
    Ptr = Ptr->Next;
  }
  SU_FreeList(FS_Plugins);
  FS_Plugins = NULL;
  SU_SEM_POST(FS_SemPlugin);
#endif /* PLUGINS */
}

bool FS_ConfigurePlugin(SU_DL_HANDLE Handle,void *User)
{
  bool ret = true;
#ifdef PLUGINS
  bool (*Fonc)(void *);

  Fonc = (bool(*)(void *))SU_DL_SYM(Handle,"Plugin_Configure");
  if(Fonc != NULL)
    ret = Fonc(User);
  else
    ret = false;
#endif /* PLUGINS */
  return ret;
}

/* Locks FS_SemPlugin */
bool FS_IsPluginValid(FS_PPlugin Plugin)
{
  SU_PList Ptr;

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if((void *)Plugin == Ptr->Data)
    {
      SU_SEM_POST(FS_SemPlugin);
      return true;
    }
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  return false;
}

