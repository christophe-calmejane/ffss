#include "server.h"
#ifdef PLUGINS
#include <dlfcn.h>
#endif

// bool FS_LoadConfig() is defined in parser.y as this file is only used under unix
// But must be defined here for other architectures

void FS_MainThread(void)
{

  pthread_join(FS_THR_UDP,NULL);
  pthread_join(FS_THR_TCP,NULL);
  printf("joined!\n");

}

bool FS_SaveConfig(const char FileName[])
{
  return true;
}

void FS_RemoveShare(FS_PShare Share) {}

bool FS_LoadPlugin(const char Name[])
{
#ifdef PLUGINS
  void *handle;
  FS_PPlugin (*fonc)(void);
  FS_PPlugin Pl;
  char *error;

  handle = dlopen (Name, RTLD_LAZY);
  if(!handle)
  {
    FFSS_PrintSyslog(LOG_ERR,"Couldn't open %s (%s)\n",Name,dlerror());
    return false;
  }

  fonc = dlsym(handle, "Plugin_Init");
  if((error = dlerror()) != NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Couldn't find Plugin_Init function for %s\n",Name);
    return false;
  }
  Pl = fonc();
  if(Pl == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Error in Plugin_Init function for %s\n",Name);
    return false;
  }
  Pl->Handle = handle;
  FS_Plugins = SU_AddElementHead(FS_Plugins,(void *)Pl);
#ifdef DEBUG
  printf("Successfully loaded %s\n",Pl->Name);
#endif
#endif
  return true;
}

void FS_UnLoadPlugin(void *Handle)
{
#ifdef PLUGINS
  void (*Fonc)(void);
  SU_PList Ptr;
  char *error;
  int i;

  Fonc = dlsym(Handle,"Plugin_UnInit");
  if((error = dlerror()) == NULL)
    Fonc();
  dlclose(Handle);
  Ptr = FS_Plugins;
  i = 0;
  while(Ptr != NULL)
  {
    if(Handle == ((FS_PPlugin)Ptr->Data)->Handle)
    {
      if(((FS_PPlugin)Ptr->Data)->Name != NULL)
        free(((FS_PPlugin)Ptr->Data)->Name);
      free(Ptr->Data);
      FS_Plugins = SU_DelElementPos(FS_Plugins,i);
      break;
    }
    i++;
    Ptr = Ptr->Next;
  }
#endif
}

