#ifndef __PLUGIN_H__
#define __PLUGIN_H__

#include "server.h"

#define FSPQ_ACQUIRE_GLOBAL    1
#define FSPQ_RELEASE_GLOBAL    2
#define FSPQ_ACQUIRE_INDEX     3
#define FSPQ_RELEASE_INDEX     4

void *FS_PluginQuery(int Type,...);



/* Internal prototypes */
FS_PPlugin FS_LoadPlugin(const char Name[]);
void FS_UnLoadPlugin(SU_DL_HANDLE Handle);
void FS_UnLoadAllPlugin(void);
bool FS_ConfigurePlugin(SU_DL_HANDLE Handle);
bool FS_IsPluginValid(FS_PPlugin Plugin);


#endif /* !__PLUGIN_H__ */
