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

    case FSPQ_SHUTDOWN :
      SendMessage(FS_hwnd,WM_CLOSE,0,0);
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
    return NULL;
  }
  if(Pl->size != sizeof(FS_TPlugin))
  {
    FFSS_PrintSyslog(LOG_ERR,"Plugin_Init function returned a wrong sized FS_PPlugin... rebuild plugin %s\n",Name);
    return NULL;
  }
  Pl->Handle = handle;
  Pl->Path = strdup(Name);
  SU_SEM_WAIT(FS_SemPlugin);
  FS_Plugins = SU_AddElementHead(FS_Plugins,(void *)Pl);
  SU_SEM_POST(FS_SemPlugin);
#ifdef DEBUG
  printf("Plugin successfully loaded : %s\n",Pl->Path);
#endif /* DEBUG */
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

bool FS_ConfigurePlugin(SU_DL_HANDLE Handle)
{
  bool ret = true;
#ifdef PLUGINS
  bool (*Fonc)(void);

  Fonc = (bool(*)(void))SU_DL_SYM(Handle,"Plugin_Configure");
  if(Fonc != NULL)
    ret = Fonc();
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

