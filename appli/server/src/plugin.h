#ifndef __PLUGIN_H__
#define __PLUGIN_H__

#include "server.h"

#define FSP_BASE_REG_KEY "HKEY_CURRENT_USER\\Software\\FFSS\\Server\\PluginsConf\\"

#define FSPQ_ACQUIRE_GLOBAL    1
#define FSPQ_RELEASE_GLOBAL    2
#define FSPQ_ACQUIRE_INDEX     3
#define FSPQ_RELEASE_INDEX     4

#define FSPQ_GET_STATE        20
#define FSPQ_SET_STATE        21
#define FSPQ_EJECT_ALL        22

#define FSPQ_SHUTDOWN         30

void *FS_PluginQuery(int Type,...);

/* Structure returned by Plugin_QueryInfos call */
typedef struct
{
  char *Name;
  char *Version;
  char *Copyright;
  char *Description;
} FSP_TInfos, *FSP_PInfos;


/* Internal prototypes */
FS_PPlugin FS_LoadPlugin(const char Name[]);
void FS_UnLoadPlugin(SU_DL_HANDLE Handle);
void FS_UnLoadAllPlugin(void);
bool FS_ConfigurePlugin(SU_DL_HANDLE Handle);
bool FS_IsPluginValid(FS_PPlugin Plugin);

#endif /* !__PLUGIN_H__ */
